use debug::Peek;
use std::convert::TryInto;

use std::io;
use std::io::Read;
use std::io::Write;
use std::rc::Rc;
use std::cell::RefCell;

use std::time::Duration;
use std::net::{SocketAddr, TcpStream};

fn checksum(s: &str) -> u8 {
    s.bytes().fold(0, |x, y| x.wrapping_add(y))
}

fn unhexlify(bytes: &[u8]) -> Vec<u8> {
    fn unhex(b: u8) -> u8 {
        if ('0'..='9').contains(&(b as char)) {
            b - '0' as u8
        } else {
            let lower = b | 0x20;
            if ('a'..='f').contains(&(lower as char)) {
                lower - 'a' as u8
            } else {
                unreachable!();
            }
        }
    }
    assert!(bytes.len() % 2 == 0);
    println!("{:?}", bytes);
    bytes.chunks(2)
        .map(|bs| {
            (unhex(bs[0]) << 4) | unhex(bs[1])
        })
        .collect()
}

#[derive(Debug, PartialEq)]
pub enum Command {
    MemoryRead(usize, usize),
    RegsGet,
}

impl Command {
    fn response_size(&self) -> Option<usize> {
        match self {
            Command::MemoryRead(size, len) => {
                // + $ <data> #<checksum>
                Some(1 + 1 + len * 2 + 3)
            },
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
        }
    }
    pub fn causes_response(&self) -> bool {
        match self {
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
        self.stream.write_all(format!("${}#{:2x}\n", serialized, checksum(&serialized)).as_bytes())

    }

    fn issue_request_with_response(&mut self, cmd: Command) -> io::Result<Vec<u8>> {
        const PACKET_SIZE: usize = 0x1000;
        let mut result = vec![0; PACKET_SIZE];
        self.issue_request(cmd);
        let mut count = if let Some(size) = self.in_flight.as_ref().and_then(|cmd| cmd.response_size()) {
            let mut offset = 0;
            while offset < size {
                let read = self.stream.read(&mut result[offset..]).unwrap();
                offset += read;
            }
            if offset > size {
                size
            } else {
                offset
            }
        } else {
            let count = self.stream.read(&mut result)?;
            if count == 0 {
                panic!("no bytes available");
            }
            count
        };
//        if Some(count) == self.in_flight.as_ref().and_then(|cmd| cmd.response_size()).map(|x| x * 2 - 1) {
            // we hit whatever bug happens where gdb echos the data back twice???
//            count = (count + 1) / 2 - 1;
//        }
        if result[0] == '+' as u8 {
            self.in_flight = None;
            // TODO: check checksum
            println!("got back {} bytes: {:?}", count, &result[..count]);
            Ok(result[2..(count - 3)].to_vec())
        } else {
            panic!("bad result for in-flight {:?}: {:?}", self.in_flight, result);
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
            let buf = unhexlify(
                res.as_slice()
            );

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
    fn get_regs(&mut self) -> Vec<u8> {
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
