/*
 * A log of things that were broken and misleading:
 * getting memory returned errors. ???
 * getting regs returned errors. ???
 * looking at gdb with `set debug remote 1`, gdb sends `$Hgp0.0` early on, as well as
 * QStartNoAckMode and vMustReplyEmpty
 * H is a packet to set current process/thread, which probably switches from a non-state to
 * something that can be queried.
 * issuing H myself doesn't workg, gets a long stream of $OK#a9 repeated back?
 * turns out each $OK#a9 is to an input character
 * eventually figure out by looking at another implementation that the client must ack a response
 * so start acking responses with a +, things start working! queries give data! yay
 *
 * also need to support: $qSupported, multiprocess, packetsize, xmlRegisters, requesting and
 * reading target.xml, reading data more than one byte at a time, thread/pid selection, tracing,
 * thread attachment state. granular register selection.
 *
 * inference of arch when missing <architecture> in <target> (ex, arm, mips, ...)
 *
 * anyway here's a doc ref https://sourceware.org/gdb/onlinedocs/gdb/Packets.html#Packets
 */

use debug::Peek;
use std::convert::TryInto;

use std::io;
use std::io::Read;
use std::io::Write;
use std::io::{Error, ErrorKind};
use std::rc::Rc;
use std::cell::RefCell;

use std::time::Duration;
use std::net::{SocketAddr, TcpStream};

fn checksum(s: &[u8]) -> u8 {
    s.iter().fold(0, |x, y| x.wrapping_add(*y))
}

#[test]
pub fn test_try_unhexlify() {
    assert_eq!(try_unhexlify(b"48"), Some(vec![0x48]));
    assert_eq!(try_unhexlify(b"e2"), Some(vec![0xe2]));
}

fn try_unhexlify(bytes: &[u8]) -> Option<Vec<u8>> {
    fn unhex(b: u8) -> Option<u8> {
        if ('0'..='9').contains(&(b as char)) {
            Some(b - '0' as u8)
        } else {
            let lower = b | 0x20;
            if ('a'..='f').contains(&(lower as char)) {
                Some(lower - 'a' as u8 + 10)
            } else {
                None
            }
        }
    }

    if bytes.len() % 2 != 0 {
        return None;
    }

    let mut res = Vec::with_capacity(bytes.len() / 2);

    for b in bytes.chunks(2) {
        let high = match unhex(b[0]) {
            Some(v) => v,
            None => { return None; }
        };
        let low = match unhex(b[1]) {
            Some(v) => v,
            None => { return None; }
        };
        res.push((high << 4) | low);
    }
    Some(res)
}

struct SupportedFeatures {
    // vCont?
    // xmlRegisters?
    // swbreak?
    // hwbreak?
    //
    features: u8
}

#[derive(Debug, PartialEq)]
pub enum BreakpointKind {
    Any,
    Hardware,
    Software
}

#[derive(Debug, PartialEq)]
pub enum ResumeStyle {
    Step,
    Continue,
}

#[derive(Debug, PartialEq)]
pub enum QueryKind {
    /*
    ‘qfThreadInfo’
    ‘qsThreadInfo’
    */
    // ThreadIds
    //   qOffsets
    // SectionRelocations,
}

#[derive(Debug, PartialEq)]
pub enum Command {
    DetectSupport,
    MemoryRead(usize, usize),
    RegsGet,
    SelectThread(usize),
    SetBreakpoint(BreakpointKind, usize),
    RemoveBreakpoint(BreakpointKind, usize),
    Continue(ResumeStyle),
    Signal(u8, ResumeStyle),
    Stop,
//    Query(QueryKind),
}

fn check_checksum(buf: &[u8]) -> Option<&[u8]> {
    if buf.len() < "$#00".len() {
        // not even a valid checksum string
        return None;
    }

    let checksum_str = &buf[buf.len() - 3..];
    if checksum_str[0] != '#' as u8 {
        // tail is not #XX
        return None;
    }

    let body = &buf[..buf.len() - 3][1..];

    let checksum = checksum(body);

    // skip the #
    if &checksum_str[1..] == format!("{:02x}", checksum).as_bytes() {
        Some(body)
    } else {
        None
    }
}

#[allow(unused)]
fn is_digit(b: u8) -> bool {
    b >= '0' as u8 && b <= '9' as u8
}

fn is_err(buf: &[u8]) -> Option<u8> {
    if buf.len() != b"EXX".len() {
        // not the right length
        return None;
    }

    if buf[0] != 'E' as u8 {
        return None;
    }

    try_unhexlify(&buf[1..]).map(|xs| xs[0])
}

enum ReplyCategory {
    Doubled,
    Incomplete,
}

impl Command {
    fn reply_looks_complete(&self, buf: &[u8]) -> Result<(), ReplyCategory> {
        fn reply_looks_complete_inner(cmd: &Command, buf: &[u8]) -> bool {
            // test checksum...
            match cmd {
                /*
                Command::MemoryRead(_, _) |
                Command::RegsGet |
                Command::SelectThread(_) => {
                */
                _ => {
                    check_checksum(buf).is_some()
                }
            }
        }

        if reply_looks_complete_inner(self, buf) {
            Ok(())
        } else {
            // gdb sometimes goes ahead and sends replies twice.
            // specifically observed with `get_regs` and `get_mem`
            // requests on `gdbserver localhost:1234 emacs` before the program has been started,
            // where gdbserver replies `+$E01#a6$E01#a6`.
            //
            // i don't know why.
            let (low, high) = buf.split_at(buf.len() / 2);
            if low == high {
                if reply_looks_complete_inner(self, low) {
                    Err(ReplyCategory::Doubled)
                } else {
                    Err(ReplyCategory::Incomplete)
                }
            } else {
                Err(ReplyCategory::Incomplete)
            }
        }
    }
    #[allow(unused)]
    fn response_size(&self) -> Option<usize> {
        match self {
            Command::MemoryRead(size, len) => {
                // + $ <data> #<checksum>
                Some(1 + 1 + len * 2 + 3)
            },
            Command::DetectSupport |
            Command::SetBreakpoint(_, _) |
            Command::RemoveBreakpoint(_, _) |
            Command::Continue(_) |
            Command::Signal(_, _) |
            Command::Stop |
            Command::SelectThread(_) |
            Command::RegsGet => {
                // lol
                None
            }
        }
    }
    fn serialize(&self) -> String {
        match self {
            Command::DetectSupport => {
                "qSupport:vContSupported;swbreak;hwbreak".to_string()
            }
            Command::MemoryRead(size, len) => {
                format!("m{:x},{:x}", size, len)
            }
            Command::RegsGet => {
                "g".to_string()
            }
            Command::SelectThread(id) => {
                format!("Hg{}", id)
            }
            Command::SetBreakpoint(kind, address) => {
                match kind {
                    BreakpointKind::Any |
                    BreakpointKind::Software => {
                        // TODO: support architecture-specific (ARM and MIPS, really) breakpoint
                        // sizes
                        format!("Z0,{:x},0", address)
                    }
                    BreakpointKind::Hardware => {
                        // TODO: support architecture-specific (ARM and MIPS, really) breakpoint
                        // sizes
                        format!("Z1,{:x},0", address)
                    }
                }
            }
            Command::RemoveBreakpoint(kind, address) => {
                match kind {
                    BreakpointKind::Any |
                    BreakpointKind::Software => {
                        // TODO: support architecture-specific (ARM and MIPS, really) breakpoint
                        // sizes
                        format!("z0,{:x},0", address)
                    }
                    BreakpointKind::Hardware => {
                        // TODO: support architecture-specific (ARM and MIPS, really) breakpoint
                        // sizes
                        format!("z1,{:x},0", address)
                    }
                }
            }
            Command::Continue(style) => {
                match style {
                    ResumeStyle::Step => {
                        "vCont;s".to_string()
                    }
                    ResumeStyle::Continue => {
                        "vCont;c".to_string()
                    }
                }
            }
            Command::Signal(signal, style) => {
                match style {
                    ResumeStyle::Step => {
                        format!("vCont;S {:02x}", signal)
                    }
                    ResumeStyle::Continue => {
                        format!("vCont;C {:02x}", signal)
                    }
                }
            }
            Command::Stop => {
                "vCont;t".to_string()
            }
        }
    }
    pub fn causes_response(&self) -> bool {
        match self {
            Command::DetectSupport |
            Command::SelectThread(_) |
            Command::MemoryRead(_, _) |
            Command::RegsGet |
            Command::SetBreakpoint(_, _) |
            Command::RemoveBreakpoint(_, _) |
            Command::Continue(_) |
            Command::Signal(_, _) |
            Command::Stop => {
                true
            }
        }
    }
}

struct ProtocolFeatures {
    features: u8,
}

impl ProtocolFeatures {
    fn new() -> Self { Self { features: 0 } }
    fn set_vcont(&mut self, supported: bool) {
        self.features &= 0x01;
        self.features |= if supported { 0x01 } else { 0x00 };
    }
    fn vcont(&self) -> bool {
        self.features & 0x01 != 0
    }
}

pub struct GDBRemote {
    features: ProtocolFeatures,
    stream: TcpStream,
    in_flight: Option<Command>
}

impl GDBRemote {
    pub fn connect(addr: &SocketAddr) -> io::Result<GDBRemote> {
        let stream = TcpStream::connect_timeout(addr, Duration::from_millis(1000))?;
        let mut connection = GDBRemote {
            features: ProtocolFeatures::new(),
            stream,
            in_flight: None
        };
        let features = connection.issue_request_with_response(Command::DetectSupport)?;
        eprintln!("got feature report: {:?}", unsafe { std::str::from_utf8(features.as_slice()) });

        Ok(connection)
    }

    fn issue_request(&mut self, cmd: Command) -> io::Result<()> {
        assert_eq!(self.in_flight, None);
        let serialized = cmd.serialize();
        let msg = format!("${}#{:02x}", serialized, checksum(serialized.as_bytes()));
        eprintln!("sending request {:?}: {}", cmd, msg);
        if cmd.causes_response() {
            self.in_flight = Some(cmd);
        }
        self.stream.write_all(msg.as_bytes())
    }

    fn ack(&mut self) {
        // TODO: check for err (socket closed, etc)
        // // TODO: check for err (socket closed, etc)
        self.stream.write_all(b"+").unwrap();
    }

    fn issue_request_with_response(&mut self, cmd: Command) -> io::Result<Vec<u8>> {
//        tracing::event!(tracing::Level::INFO, cmd = ?cmd, "issuing gdb request");
        const PACKET_SIZE: usize = 0x1000;
        let mut result = vec![0; PACKET_SIZE];
        let mut offset = 0;

        self.issue_request(cmd).unwrap();

        assert!(self.in_flight.is_some());

        loop {
            let read = self.stream.read(&mut result[offset..])?;
            offset += read;

            match self.in_flight.as_ref().unwrap().reply_looks_complete(&result[..offset][1..]) {
                Ok(()) => {
                    break;
                }
                Err(ReplyCategory::Doubled) => {
                    offset = (offset + 1) / 2;
                    break;
                }
                Err(ReplyCategory::Incomplete) => {
                    // hope for more bytes lol
                }
            }
        }

        let count = offset;
//        eprintln!("< {}", unsafe { std::str::from_utf8_unchecked(&result) });

        let resp = if result[0] == '+' as u8 {
            // we may be obligated to ack responses
            self.ack();
            // TODO: check checksum
            Ok(result[..(count - 3)][2..].to_vec())
        } else if result[0] == '-' as u8 {
            // we may be obligated to ack responses
            self.ack();
//            panic!("bad result for in-flight {:?}: {:?}", self.in_flight, result);
            assert!(result.len() == 1);
            Err(Error::new(ErrorKind::Other, format!("invalid request: {:?}", self.in_flight)))
        } else {
            tracing::event!(tracing::Level::ERROR, result = ?result, "gdb says invalid");
            Err(Error::new(ErrorKind::Other, format!("invalid response: {:?}", result)))
        };

        // whatever happens, we've gotten an answer...
        self.in_flight = None;

        resp
    }

    pub fn set_thread(&mut self, id: usize) -> io::Result<()> {
        let resp = self.issue_request_with_response(Command::SelectThread(id))?;
        if let Some(errno) = is_err(&resp) {
            Err(Error::new(ErrorKind::Other, format!("set_thread failed: {}", errno)))
        } else {
            Ok(())
        }
    }

    pub fn read_memory(&mut self, mut addr: usize, len: usize) -> io::Result<Vec<u8>> {
//        tracing::event!(tracing::Level::INFO, "reading {} bytes at {} from remote gdb tatrget", len, addr);
//        eprintln!("reading {:#x} bytes at {:#x} from remote gdb tatrget", len, addr);
        if len == 0 { return Ok(vec![]); }

        let mut remaining = len;
        let mut result = Vec::with_capacity(len);

        // sorry for the magic number! this is just what i saw in the debug logs after pointing gdb
        // at a remote target and setting `set remote debug 1`
        let max_request_size = 0xffb;

        while remaining > 0 {
            let request_size = std::cmp::min(max_request_size, remaining);

            // we have to request a(nother) packet of data to build up the buffer..
            let res = self.issue_request_with_response(Command::MemoryRead(addr, request_size))?;

            // if let Some(errno) = is_err(&res) {
//                tracing::event!(tracing::Level::ERROR, "invalid read: {}", errno);
            //     return Err(Error::new(ErrorKind::Other, format!("invalid read: {}", errno)));
            // }

            let buf = match try_unhexlify(
                res.as_slice()
            ) {
                Some(buf) => {
                    buf
                },
                None => {
//                    tracing::event!(tracing::Level::ERROR, "invalid response characters");
                    return Err(Error::new(ErrorKind::Other, "response contained non-hex characters where only hex was expected, or was not an even number of characters"));
                }
            };

            if buf.len() == 0 {
                panic!("got 0 bytes back from gdb?");
            }

            result.extend_from_slice(buf.as_slice());

            addr += request_size;
            remaining -= request_size;
        }

//        tracing::event!(tracing::Level::INFO, "read ok!");
        Ok(result)
    }

    // this is a very low level "get me register state bytes" function
    // and is only register state as the remote debugger stub knows it.
    //
    // these still must be mapped to some useful layout by a layout read
    // from the remote stub, or by an ISA that knows how to map a gdb
    // register blob
    pub fn get_regs(&mut self) -> Vec<u8> {
        let resp = self.issue_request_with_response(Command::RegsGet).unwrap();
        try_unhexlify(&resp).unwrap()
    }

    pub fn step(&mut self) -> io::Result<()> {
        eprintln!("stepping!");
        self.issue_request_with_response(Command::Continue(ResumeStyle::Step))
            .map(|resp| {
                eprintln!("resp: {:?}", std::str::from_utf8(&resp).unwrap());
            //    assert!(resp.len() == 0)
            })
    }
    pub fn cont(&mut self) -> io::Result<()> {
        eprintln!("continuing!");
        self.issue_request_with_response(Command::Continue(ResumeStyle::Continue))
            .map(|resp| {
                eprintln!("resp: {:?}", std::str::from_utf8(&resp).unwrap());
            //    assert!(resp.len() == 0)
            })
    }
    pub fn set_breakpoint(&mut self, addr: usize) -> io::Result<()> {
        eprintln!("breakpointing at {:#x}!", addr);
        self.issue_request_with_response(Command::SetBreakpoint(BreakpointKind::Software, addr))
            .map(|resp| {
                eprintln!("resp: {:?}", std::str::from_utf8(&resp).unwrap());
            //    assert!(resp.len() == 0)
            })
    }
}

impl Peek for Rc<RefCell<GDBRemote>> {
    fn read_bytes<A: yaxpeax_arch::Address, W: std::io::Write>(&self, addr: A, len: u64, buf: &mut W) -> Option<()> {
//        tracing::event!(tracing::Level::INFO, "reading {} bytes at {}", len, addr.stringy());
//        eprintln!("reading {} bytes at {}", len, addr.stringy());
        const CHUNKSIZE: usize = 1024;
        let start = addr.to_linear();
        let mut reader = self.borrow_mut();
        for chunk in 0..(len as usize / CHUNKSIZE) {
            let data = reader.read_memory((start + chunk) as usize, CHUNKSIZE);
            if let Ok(data) = data {
                // TODO: correctly handle wrote-some-but-not-all.
                if buf.write(data.as_slice()).ok() != Some(CHUNKSIZE) {
                    return None;
                }
            } else {
                return None;
            }
        }

        let remainder = len as usize % CHUNKSIZE;
        if remainder == 0 {
            return Some(());
        }
        let data = reader.read_memory(start + len as usize - remainder, remainder);
        if let Ok(data) = data {
            // TODO: correctly handle wrote-some-but-not-all.
            if buf.write(data.as_slice()).ok() != Some(remainder) {
                return None;
            }
        } else {
            return None;
        }
//        tracing::event!(tracing::Level::INFO, "completed read of {} bytes at {}", len, addr.stringy());
        Some(())
    }
    fn read<A: yaxpeax_arch::Address>(&self, addr: A) -> Option<u8> {
        self.borrow_mut().read_memory(addr.to_linear(), 1).unwrap().get(0).map(|x| *x)
    }
    fn read16<A: yaxpeax_arch::Address>(&self, addr: A) -> Option<[u8; 2]> {
        self.borrow_mut().read_memory(addr.to_linear(), 2).unwrap().as_slice().try_into().ok()
    }
    fn read32<A: yaxpeax_arch::Address>(&self, addr: A) -> Option<[u8; 4]> {
        self.borrow_mut().read_memory(addr.to_linear(), 4).unwrap().as_slice().try_into().ok()
    }
    fn read64<A: yaxpeax_arch::Address>(&self, addr: A) -> Option<[u8; 8]> {
        self.borrow_mut().read_memory(addr.to_linear(), 8).unwrap().as_slice().try_into().ok()
    }
}
