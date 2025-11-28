use emacs::{defun, Env, IntoLisp, Result, Value};
use rosc::{OscPacket, OscType};	
use std::net::UdpSocket;
use std::sync::{Arc, Mutex};
use std::thread;

// REQUIRED: Declare GPL compatibility
emacs::plugin_is_GPL_compatible!();

lazy_static::lazy_static! {
    static ref INBOX: Arc<Mutex<Vec<(i32, i32, i32)>>> = Arc::new(Mutex::new(Vec::new()));
}

#[emacs::module]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

fn to_i32(arg:Option<&OscType>) -> Option<i32> {
    match arg {
        Some(OscType::Int(i)) => Some(*i),
        Some(OscType::Float(f)) => Some(*f as i32),
        Some(OscType::String(s)) => s.parse::<i32>().ok(), // Even handle strings if weirdness happens
        _ => None,
    }
}

// Unwraps Bundles to find the actual Message inside
fn handle_packet(packet: OscPacket, inbox: &Arc<Mutex<Vec<(i32, i32, i32)>>>) {
    match packet {
        OscPacket::Message(msg) => {
            if msg.addr == "/editor/highlights" {
                let args = &msg.args;
                // indices 3, 4, 5 are colStart, eventId, colEnd
                if args.len() >= 6 {
                    if let (Some(col_start), Some(event_id), Some(col_end)) = 
                           (to_i32(args.get(3)), to_i32(args.get(4)), to_i32(args.get(5))) 
                    {
                        let mut guard = inbox.lock().unwrap();
                        guard.push((col_start, event_id, col_end));
                    }
                }
            }
        }
        OscPacket::Bundle(bundle) => {
            // Recursively process all packets inside the bundle
            for pkt in bundle.content {
                handle_packet(pkt, inbox);
            }
        }
    }
}

#[defun]
fn start_server(port: i64) -> Result<()> {
    let inbox_clone = INBOX.clone();
    
    thread::spawn(move || {
        let addr = format!("127.0.0.1:{}", port);
        let socket = match UdpSocket::bind(&addr) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Rust UDP Bind Error: {}", e);
                return;
            }
        };

        let mut buf = [0u8; 4096]; // Increased buffer size for Bundles

        loop {
            if let Ok((amt, _)) = socket.recv_from(&mut buf) {
                let buf_slice = &buf[..amt];
                if let Ok((_, packet)) = rosc::decoder::decode_udp(buf_slice) {
                    handle_packet(packet, &inbox_clone);
                }
            }
        }
    });

    Ok(())
}

#[defun]
fn poll_events(env: &Env) -> Result<Value<'_>> {
    let mut guard = INBOX.lock().unwrap();
    
    if guard.is_empty() {
        return env.intern("nil");
    }

    let events: Vec<Value<'_>> = guard.drain(..)
        .map(|(col, id, end_col)| {
            let args = vec![
                col.into_lisp(env).unwrap(),
                id.into_lisp(env).unwrap(),
                end_col.into_lisp(env).unwrap()
            ];
            env.call("list", &args).unwrap()
        })
        .collect();

    env.call("list", &events)
}
