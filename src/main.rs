use std::{
    fs::{self},
    io::{self, ErrorKind, Read, Write},
    os::unix::net::{UnixListener, UnixStream},
    path::PathBuf,
    process::{ExitCode, Termination},
    sync::mpsc::{channel, Receiver, TryRecvError},
    thread,
};

type Res<A> = Result<A, Box<dyn std::error::Error>>;

const SOCKET_PATH: &str = "/tmp/ej-socket";

#[derive(Debug)]
enum ClientError {
    Io(io::Error),
}

impl core::fmt::Display for ClientError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        use ClientError::*;
        match self {
            Io(_) => write!(f, "I/O error"),
        }
    }
}

impl std::error::Error for ClientError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        use ClientError::*;
        match self {
            Io(e) => Some(e),
        }
    }
}

impl From<io::Error> for ClientError {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

fn do_client() -> Result<(), ClientError> {
    let socket = PathBuf::from(SOCKET_PATH);

    let mut stream = UnixStream::connect(&socket)?;

    let mut pipe_buffer = Vec::with_capacity(1024);
    io::stdin().read_to_end(&mut pipe_buffer)?;

    stream.write(&pipe_buffer).map(|_| ()).map_err(From::from)
}

#[derive(Debug)]
enum ServerError {
    Io(io::Error),
    TryRecv(TryRecvError)
}

impl core::fmt::Display for ServerError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        use ServerError::*;
        match self {
            Io(_) => write!(f, "I/O error"),
            TryRecv(_) => write!(f, "TryRecv error"),
        }
    }
}

impl std::error::Error for ServerError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        use ServerError::*;
        match self {
            Io(e) => Some(e),
            TryRecv(e) => Some(e),
        }
    }
}

impl From<io::Error> for ServerError {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

fn do_server() -> Result<(), ServerError> {
    use ServerError::*;

    let socket = PathBuf::from(SOCKET_PATH);

    // Delete old socket if necessary
    if socket.exists() {
        println!("Deleting old socket");
        fs::remove_file(&socket)?;
    }

    let listener = UnixListener::bind(&socket)?;
    listener.set_nonblocking(true)?;

    fn setup_terminal() -> io::Result<()> {
        use std::os::fd::AsRawFd;

        let tty;
        let fd = if unsafe { libc::isatty(libc::STDIN_FILENO) } == 1 {
            libc::STDIN_FILENO
        } else {
            tty = fs::File::open("/dev/tty")?;

            tty.as_raw_fd()
        };

        let mut ptr = core::mem::MaybeUninit::uninit();

        if unsafe { libc::tcgetattr(fd, ptr.as_mut_ptr()) } == 0 {
            let mut termios = unsafe { ptr.assume_init() };
            let c_oflag = termios.c_oflag;

            unsafe { libc::cfmakeraw(&mut termios); }
            termios.c_oflag = c_oflag;
            // Enable signal handling again after cfmakeraw disabled it
            termios.c_lflag |= libc::ISIG;

            if unsafe { libc::tcsetattr(fd, libc::TCSADRAIN, &termios) } == 0 {
                return Ok(());
            }
        }

        Err(io::Error::last_os_error())
    }

    setup_terminal()?;

    println!("Server started, waiting for clients");

    let mut buffer = String::with_capacity(1024);
    let mut jump_points = Vec::with_capacity(16);
    let mut index = 0;

    struct JumpPoint {
        path: PathBuf,
    }

    fn parse_jump_points(jump_points: &mut Vec<JumpPoint>, to_parse: &str) {
        // TODO actual parsing
        //for line in to_parse.lines() {
            //
        //}
    }

    let stdin_channel: Receiver<u8> = {
        let (tx, rx) = channel::<u8>();
        thread::spawn(move || {
            let mut stdin = io::stdin();
            let mut buffer = [0; 1];
            loop {
                stdin.read_exact(&mut buffer).unwrap();
                tx.send(buffer[0]).unwrap();
            }
        });
        rx
    };

    loop {
        // Iterate over clients, blocks if no client available
        match listener.accept() {
            Ok((mut stream, _)) => {
                stream.read_to_string(&mut buffer)?;
                println!("Client said: {buffer}");

                parse_jump_points(&mut jump_points, &buffer);

                buffer.clear();
            },
            Err(e) if e.kind() == ErrorKind::WouldBlock => {}
            Err(e) => println!("accept function failed: {e:?}"),
        }

        const SPACE: u8 = 32;

        match stdin_channel.try_recv() {
            Ok(SPACE) => match jump_points.get(index) {
                Some(_jp) => println!("TODO implement jumping"),
                None => println!("No jump point found"),
            },
            Ok(key) => println!("Received: {}", key),
            Err(TryRecvError::Empty) => {},
            Err(e) => Err(TryRecv(e))?,
        }

        // TODO accept input and display UI
        // UI:
        //     List of jump points
        //         Space to trigger current selection and move to next
        //         Arrows to move along list
        //     Some way to view the raw input?

        // TODO calculate amount to sleep based on how long things took
        thread::sleep(std::time::Duration::from_millis(16));
    }

    Ok(())
}

struct Terminator(Res<()>);

impl Termination for Terminator {
    fn report(self) -> ExitCode {
        match self.0 {
            Ok(val) => ExitCode::SUCCESS,
            Err(error) => {
                use std::fmt::Write;
                let mut buffer = String::with_capacity(256);

                let mut indent = 0;
                let mut err: Option<&dyn std::error::Error> = Some(&*error);
                while let Some(e) = err {
                    for _ in 0..indent {
                        buffer.push(' ');
                    }
                    let _ = write!(buffer, "{e}\n");

                    err = e.source();

                    indent += 4;
                }

                eprintln!("{buffer}");

                ExitCode::FAILURE
            }
        }
    }
}

fn main() -> Terminator {
    Terminator(res_main())
}

fn res_main() -> Res<()> {
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
        do_client().map_err(From::from)
    } else {
        do_server().map_err(From::from)
    }
}
