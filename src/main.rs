use std::{
    fs::{self},
    io::{self, Read, Write},
    os::unix::net::{UnixListener, UnixStream},
    path::PathBuf,
};

type Res<A> = Result<A, Box<dyn std::error::Error>>;

const SOCKET_PATH: &str = "/tmp/ej-socket";

fn do_client() -> Res<()> {
    let socket = PathBuf::from(SOCKET_PATH);

    let mut stream = UnixStream::connect(&socket)?;

    let mut pipe_buffer = Vec::with_capacity(1024);
    io::stdin().read_to_end(&mut pipe_buffer)?;

    stream.write(&pipe_buffer).map(|_| ()).map_err(From::from)
}

fn do_server() -> Res<()> {
    let socket = PathBuf::from(SOCKET_PATH);

    // Delete old socket if necessary
    if socket.exists() {
        println!("Deleting old socket");
        fs::remove_file(&socket)?;
    }

    let listener = UnixListener::bind(&socket)?;

    println!("Server started, waiting for clients");

    let mut buffer = String::with_capacity(1024);

    // Iterate over clients, blocks if no client available
    for client in listener.incoming() {
        client?.read_to_string(&mut buffer)?;
        println!("Client said: {buffer}");

        buffer.clear();
    }

    Ok(())
}

fn main() -> Res<()> {
    let mut args = std::env::args();

    args.next(); //exe name

    let mut is_client = false;
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--client" => {
                is_client = true;
            }
            _ => {
                return Err(format!("Unrecognized arg: {arg}").into());
            }
        }
    }

    if is_client {
        do_client()
    } else {
        do_server()
    }
}
