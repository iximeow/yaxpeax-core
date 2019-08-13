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

#[derive(Debug, PartialEq)]
pub enum Command {
    MemoryRead(usize, usize),
    RegsGet,
    SelectThread(usize),
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
                Command::MemoryRead(_, _) |
                Command::RegsGet |
                Command::SelectThread(_) => {
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
    fn response_size(&self) -> Option<usize> {
        match self {
            Command::MemoryRead(size, len) => {
                // + $ <data> #<checksum>
                Some(1 + 1 + len * 2 + 3)
            },
            Command::SelectThread(_) |
            Command::RegsGet => {
                // lol
                None
            }
        }
    }
    fn serialize(&self) -> String {
        match self {
            Command::MemoryRead(size, len) => {
                format!("m{:x},{:x}", size, len)
            }
            Command::RegsGet => {
                "g".to_string()
            }
            Command::SelectThread(id) => {
                format!("Hg{}", id)
            }
        }
    }
    pub fn causes_response(&self) -> bool {
        match self {
            Command::SelectThread(_) |
            Command::MemoryRead(_, _) |
            Command::RegsGet => {
                    true
                }
        }
    }
}

pub struct GDBRemote {
    stream: TcpStream,
    in_flight: Option<Command>
}

impl GDBRemote {
    pub fn connect(addr: &SocketAddr) -> io::Result<GDBRemote> {
        let stream = TcpStream::connect_timeout(addr, Duration::from_millis(1000))?;

        Ok(GDBRemote { stream, in_flight: None })
    }

    fn issue_request(&mut self, cmd: Command) -> io::Result<()> {
        assert_eq!(self.in_flight, None);
        let serialized = cmd.serialize();
        if cmd.causes_response() {
            self.in_flight = Some(cmd);
        }
        let msg = format!("${}#{:02x}", serialized, checksum(serialized.as_bytes()));
        self.stream.write_all(msg.as_bytes())
    }

    fn ack(&mut self) {
        // TODO: check for err (socket closed, etc)
        // // TODO: check for err (socket closed, etc)
        self.stream.write_all(b"+").unwrap();
    }

    fn issue_request_with_response(&mut self, cmd: Command) -> io::Result<Vec<u8>> {
        const PACKET_SIZE: usize = 0x1000;
        let mut result = vec![0; PACKET_SIZE];
        let mut offset = 0;

        self.issue_request(cmd);

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

            if let Some(errno) = is_err(&res) {
                return Err(Error::new(ErrorKind::Other, format!("invalid read: {}", errno)));
            }

            let buf = match try_unhexlify(
                res.as_slice()
            ) {
                Some(buf) => buf,
                None => {
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

        Ok(result)
    }

    // this is a very low level "get me register state bytes" function
    // and is only register state as the remote debugger stub knows it.
    //
    // these still must be mapped to some useful layout by a layout read
    // from the remote stub, or by an ISA that knows how to map a gdb
    // register blob
    pub fn get_regs(&mut self) -> Vec<u8> {
        self.issue_request_with_response(Command::RegsGet).unwrap()
    }
}

impl Peek for Rc<RefCell<GDBRemote>> {
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
