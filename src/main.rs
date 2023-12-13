#![deny(unreachable_patterns)]

use std::{
    fs::{self},
    io::{self, ErrorKind, Read, Write},
    os::unix::net::{UnixListener, UnixStream},
    path::PathBuf,
    process::{Command, ExitCode, Termination},
    sync::mpsc::{channel, Receiver, TryRecvError},
    thread,
};

mod printer {
    #[derive(Default)]
    pub struct Printer {
        enable_alt: &'static str,
        disable_alt: &'static str,
        clear: &'static str,
        move_home: &'static str,
    }

    impl Printer {
        pub fn ansi() -> Self {
            Self {
                enable_alt: "\u{001b}[?1049h",
                disable_alt: "\u{001b}[?1049l",
                clear: "\u{001b}[2J",
                move_home: "\u{001b}[H",
            }
        }

        pub fn enable_alternate_screen(&self) {
            print!("{}", self.enable_alt);
        }

        pub fn disable_alternate_screen(&self) {
            print!("{}", self.disable_alt);
        }

        pub fn clear(&self) {
            print!("{}", self.clear);
        }

        pub fn move_home(&self) {
            print!("{}", self.move_home);
        }
    }
}
use printer::Printer;

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

fn get_row_count() -> usize {
    // TODO get from terminal
    36
}

mod jump_points {
    use super::*;
    use tinyjson::JsonValue;

    // 64k error messages ought to be enough for anybody!
    pub type DuplicateIndex = u16;
    pub type DuplicateIndexesLen = u8;

    #[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
    pub struct DuplicateIndexes {
        len: DuplicateIndexesLen,
        // 7 so this structure takes a round number of bytes, and because we
        // don't expect to need even that many in practice.
        indexes: [DuplicateIndex; 7],
    }

    impl DuplicateIndexes {
        pub fn push(&mut self, index: DuplicateIndex) {
            let free_index = usize::from(self.len);
            if free_index >= self.indexes.len() {
                debug_assert!(false, "Ran out of space in DuplicateIndex::push!");
                return
            }
            self.indexes[free_index] = index;
            self.len += 1;
        }

        pub fn len(&self) -> DuplicateIndexesLen {
            self.len
        }
    }

    // TODO sort line numbers properly instead of lexicographically.
    // (97 < 400 for example)
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub struct JumpPoint {
        // As of this writing, this only affects the sort/deduping
        pub uninformative_number: bool,
        // As of this writing, this only affects the sort/deduping
        pub external_lib: bool,
        pub path: PathBuf,
        pub message: String,
        pub duplicate_indexes: DuplicateIndexes,
    }

    pub fn parse(jump_points: &mut Vec<JumpPoint>, to_parse: &str) {
        use std::fmt::Write;
        use JsonValue::*;

        jump_points.clear();

        for line in to_parse.lines() {
            match line.parse::<JsonValue>() {
                Ok(Object(object)) => {
                    let Some(Object(message)) = object.get("message") else { continue };

                    let rendered_message = message.get("rendered")
                        .and_then(|r| match r {
                            String(s) => Some(s),
                            _ => None,
                        });

                    let start_len = jump_points.len();

                    macro_rules! process_spans {
                        ($root: expr) => {

                            if let Some(Array(spans)) = $root.get("spans") {
                                for span_value in spans {
                                    let Object(span) = span_value else { continue };

                                    let Some(String(ref path)) = span.get("file_name") else { continue };
                                    let Some(Number(line_number)) = span.get("line_start") else { continue };

                                    let external_lib = path.contains("rustup");

                                    let mut path = path.clone();
                                    let _ = write!(path, ":{line_number}");
                                    jump_points.push(JumpPoint {
                                        path: PathBuf::from(path),
                                        message: rendered_message.cloned().unwrap_or_default(),
                                        // We want NaN to map to true.
                                        uninformative_number: !(*line_number > 1.),
                                        external_lib,
                                        duplicate_indexes: <_>::default(),
                                    });
                                }
                            }
                        }
                    }

                    process_spans!(message);

                    let Some(Array(children)) = message.get("children") else { continue };

                    for child_value in children {
                        let Object(child) = child_value else { continue };
                        process_spans!(child);
                    }

                    let end_len = jump_points.len();
                    assert!(start_len <= end_len);
                    if let Ok(end_len) = jump_points.len().try_into() {
                        let start_len = start_len.try_into().unwrap();

                        // If at least two were pushed
                        if start_len + 1 < end_len {
                            // At least one was pushed, and it would have gotten
                            // that index.
                            let start_index = start_len;

                            for point_i in start_index..end_len {
                                let point: &mut JumpPoint =
                                    &mut jump_points[usize::from(point_i)];

                                for pushed_i in start_index..end_len {
                                    if pushed_i == point_i { continue }
                                    point.duplicate_indexes.push(pushed_i);
                                }
                            }
                        }
                    }
                },
                _ => {}
            }
        }

        jump_points.sort();
        jump_points.dedup();
    }

    #[test]
    fn parse_handles_line_1_entries_properly() {
        // We get some line 1 entries that are duplicated later with a more
        // informative line number. We don't want to see those at the top
        // of the list.
        // TODO check for entries where another has the same message and
        // if the line number is uninformative on less than all of a given
        // one of a given group with the same message, remove those entries
        // with uninformative line numbers.

        let to_parse = r#"
{"message": {"rendered": "msg", "spans": [{"file_name": "example.rs", "line_start": 1}]}}
{"message": {"rendered": "msg", "spans": [{"file_name": "example.rs", "line_start": 123}]}}
"#;

        const EXPECTED_COUNT: usize = 2;

        let mut jump_points = Vec::with_capacity(EXPECTED_COUNT);

        parse(&mut jump_points, to_parse);

        assert_eq!(jump_points.len(), EXPECTED_COUNT);
        assert!(jump_points[1].uninformative_number);
        assert!(!jump_points[0].uninformative_number);
    }

    #[test]
    fn parse_handles_rustup_entries_properly() {
        // We get some entries that point to, for example the rust std library,
        // locally on disk. We don't want those to be at the top of the list.
        // The place they appear on disk in a usual installation on linux, ends
        // up being in the ~/.rustup folder. Hopefully the Windows equivalent
        // contains the string rustup too.
        // TODO confirm that for Windows, etc.

        let to_parse = r#"
{"message": {"rendered": "msg", "spans": [{"file_name": "rustup/example.rs", "line_start": 123}]}}
{"message": {"rendered": "msg", "spans": [{"file_name": "example.rs", "line_start": 123}]}}
"#;

        const EXPECTED_COUNT: usize = 2;

        let mut jump_points = Vec::with_capacity(EXPECTED_COUNT);

        parse(&mut jump_points, to_parse);

        assert_eq!(jump_points.len(), EXPECTED_COUNT);
        assert!(jump_points[1].external_lib);
        assert!(jump_points[1].path.to_string_lossy().contains("rustup"));
        assert!(!jump_points[0].external_lib);
        assert!(!jump_points[0].path.to_string_lossy().contains("rustup"));
    }

    #[test]
    fn parse_collects_all_spans_on_this_children_example() {
        let to_parse = r#"
{"message": {"rendered": "msg", "spans": [{"file_name": "rustup/example.rs", "line_start": 123}], "children": [{"spans": [{"file_name": "rustup/example.rs", "line_start": 123}]}]}}
{"message": {"rendered": "msg", "spans": [{"file_name": "example.rs", "line_start": 123}]}}
"#;

        const EXPECTED_COUNT: usize = 3;

        let mut jump_points = Vec::with_capacity(EXPECTED_COUNT);

        parse(&mut jump_points, to_parse);
        assert_eq!(jump_points.len(), EXPECTED_COUNT);
    }

    #[test]
    fn parse_collects_duplicate_indexes_on_this_example() {
        let to_parse = r#"
{"message": {"rendered": "msg", "spans": [{"file_name": "rustup/example.rs", "line_start": 123}], "children": [{"spans": [{"file_name": "rustup/example.rs", "line_start": 123}, {"file_name": "rustup/example_2.rs", "line_start": 456}]}]}}
"#;

        const EXPECTED_COUNT: usize = 3;

        let mut jump_points = Vec::with_capacity(EXPECTED_COUNT);

        parse(&mut jump_points, to_parse);

        macro_rules! dup_ind {
            ($array: expr) => ({
                let mut output = DuplicateIndexes::default();

                for el in $array {
                    output.push(el);
                }

                output
            })
        }

        assert_eq!(jump_points.len(), EXPECTED_COUNT);
        // Recall that the jump_points get sorted!
        assert_eq!(
            jump_points[0].duplicate_indexes,
            dup_ind!([0, 2]),
        );
        assert_eq!(
            jump_points[1].duplicate_indexes,
            dup_ind!([1, 2]),
        );
        assert_eq!(
            jump_points[2].duplicate_indexes,
            dup_ind!([0, 1]),
        );
    }
}
use jump_points::JumpPoint;

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
    let p = Printer::ansi();

    p.enable_alternate_screen();
    let output = do_server_inner(&p);
    p.disable_alternate_screen();

    output
}

fn do_server_inner(p: &Printer) -> Result<(), ServerError> {
    use ServerError::*;

    let socket = PathBuf::from(SOCKET_PATH);

    // Delete old socket if necessary
    if socket.exists() {
        println!("Deleting old socket");
        fs::remove_file(&socket)?;
    }

    let listener = UnixListener::bind(&socket)?;
    listener.set_nonblocking(true)?;

    setup_terminal()?;

    println!("Server started, waiting for clients");

    let mut listener_buffer = String::with_capacity(1024);
    let mut jump_points = Vec::with_capacity(16);
    let mut index = 0;
    let mut scroll_skip_lines: usize = 0;

    #[derive(Copy, Clone)]
    enum Input {
        Byte(u8),
        Up,
        Down,
        Right,
        Left,
        ScrollUp,
        ScrollDown,
        End,
        Home,
        DebugDumpToFile,
    }

    impl core::fmt::Display for Input {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            use Input::*;
            match self {
                Byte(b) => write!(f, "Byte({b})"),
                Up => write!(f, "Up"),
                Down => write!(f, "Down"),
                Right => write!(f, "Right"),
                Left => write!(f, "Left"),
                ScrollUp => write!(f, "ScrollUp"),
                ScrollDown => write!(f, "ScrollDown"),
                End => write!(f, "End"),
                Home => write!(f, "Home"),
                DebugDumpToFile => write!(f, "DebugDumpToFile"),
            }
        }
    }

    let stdin_channel: Receiver<Input> = {
        let (tx, rx) = channel::<Input>();
        thread::spawn(move || {
            use Input::*;
            macro_rules! send {
                ($input: expr) => {
                    tx.send($input).unwrap()
                }
            }

            let mut stdin = io::stdin();
            let mut byte_buffer = [0; 1];
            loop {
                stdin.read_exact(&mut byte_buffer).unwrap();
                let byte = byte_buffer[0];
                match byte {
                    // Device Control Two. Often sent with Ctrl-r
                    0x12 => { send!(DebugDumpToFile); },
                    // ANSI escape
                    0x1B => {
                        let mut escape_buffer = [0; 2];
                        stdin.read_exact(&mut escape_buffer).unwrap();
                        match escape_buffer {
                            [91, 65] => send!(Up),
                            [91, 66] => send!(Down),
                            [91, 67] => send!(Right),
                            [91, 68] => send!(Left),
                            [91, 70] => send!(End),
                            [91, 72] => send!(Home),
                            [79, 65] => send!(ScrollUp),
                            [79, 66] => send!(ScrollDown),
                            _ => {
                                send!(Byte(byte));
                                send!(Byte(escape_buffer[0]));
                                send!(Byte(escape_buffer[1]));
                            },
                        }
                    },
                    _ => { send!(Byte(byte)); }
                }
            }
        });
        rx
    };

    let row_count = get_row_count();

    let mut unhandled_keys = [None; 8];

    loop {
        p.clear();
        p.move_home();

        let mut lines_count = 0;
        // wrap println and count lines and only actually print lines after
        // count exceededs scroll_skip_lines value.
        macro_rules! pln {
            ($($format_args: tt)+) => {{
                let string = format!($($format_args)+);

                let one_past_max = scroll_skip_lines + row_count;

                for line in string.lines() {
                    // Think of the scroll_skip_lines values as being
                    // between the lines
                    if lines_count >= scroll_skip_lines
                    && lines_count < one_past_max {
                        println!("{line}");
                    }

                    lines_count += 1;
                }
            }}
        }

        // Iterate over clients, blocks if no client available
        match listener.accept() {
            Ok((mut stream, _)) => {
                stream.read_to_string(&mut listener_buffer)?;

                jump_points::parse(&mut jump_points, &listener_buffer);

                listener_buffer.clear();
            },
            Err(e) if e.kind() == ErrorKind::WouldBlock => {}
            Err(e) => pln!("accept function failed: {e:?}"),
        }

        // This can happen when the stream changes the length of `jump_points`.
        if index > jump_points.len() {
            index = jump_points.len().saturating_sub(1);
        }

        for i in 0..index {
            let Some(jump_point) = jump_points.get(i) else { break };
            pln!("{}", jump_point.path.display());
        }

        let mut jump_point_opt = None;
        jump_point_opt = jump_points.get(index);
        if let Some(jump_point) = jump_point_opt {
            pln!("----vvvv----");
            pln!("{}", jump_point.message);
            pln!("{}", jump_point.path.display());
            let len = jump_point.duplicate_indexes.len();
            if len > 0 {
                pln!("{len} duplicate(s)");
            }
            pln!("----^^^^----");
        }

        for i in index + 1..jump_points.len() {
            let Some(jump_point) = jump_points.get(i) else { break };
            pln!("{}", jump_point.path.display());
        }

        let mut skip_clearing_unhandled = false;

        const SPACE: u8 = 32;

        match stdin_channel.try_recv() {
            Ok(Input::Byte(SPACE)) => match jump_point_opt {
                Some(jump_point) => {
                    match Command::new("e")
                        .args([&jump_point.path])
                        .output() {
                        Ok(output) => {
                            pln!("{}", output.status.success());
                            pln!("{:?}", std::str::from_utf8(&output.stdout));
                            pln!("{:?}", std::str::from_utf8(&output.stderr));
                        }
                        Err(e) => {
                            pln!("{e}");
                        }
                    }
                },
                None => pln!("No jump point found"),
            },
            Ok(Input::Up) => {
                index = index.saturating_sub(1);
            },
            Ok(Input::Down) => {
                index += 1;
                let len = jump_points.len();
                if index >= len {
                    index = len.saturating_sub(1);
                }
            },
            Ok(Input::ScrollUp) => {
                scroll_skip_lines = scroll_skip_lines.saturating_sub(1);
            },
            Ok(Input::ScrollDown) => {
                scroll_skip_lines += 1;
                let len = jump_points.len();
                if scroll_skip_lines >= len {
                    scroll_skip_lines = len.saturating_sub(1);
                }
            },
            Ok(Input::End) => {
                let last_index = jump_points.len().saturating_sub(1);
                index = last_index;
                scroll_skip_lines = last_index;
            },
            Ok(Input::Home) => {
                index = 0;
                scroll_skip_lines = 0;
            },
            Ok(Input::DebugDumpToFile) => {
                use std::time::SystemTime;

                let path = PathBuf::from(
                    format!(
                        "/tmp/ej-debug-dump-{}",
                        match SystemTime::now()
                            .duration_since(SystemTime::UNIX_EPOCH)
                        {
                            Ok(n) => format!("{}", n.as_nanos()),
                            Err(_) => "before-unix-epoch-somehow".to_string(),
                        }
                    )
                );

                let result = std::fs::write(
                    &path,
                    format!("{jump_points:#?}").as_bytes(),
                );
                // TODO make these message last on screen longer
                match result {
                    Ok(_) => {
                        pln!("Wrote to {}", path.display());
                    }
                    Err(e) => {
                        pln!("When debug dumping: {e}");
                    }
                }
            },
            Ok(key) => {
                skip_clearing_unhandled = true;

                let free_index = unhandled_keys
                    .iter()
                    .position(|e| e.is_none());
                match free_index {
                    Some(index) => {
                        unhandled_keys[index] = Some(key);
                    }
                    None => {
                        unhandled_keys.rotate_left(1);
                        unhandled_keys[
                            unhandled_keys.len() - 1
                        ] = Some(key);
                    }
                }
            },
            Err(TryRecvError::Empty) => {
                skip_clearing_unhandled = true;
            },
            Err(e) => Err(TryRecv(e))?,
        }

        pln!("index: {index}, scroll_skip_lines: {scroll_skip_lines}");

        if !skip_clearing_unhandled {
            unhandled_keys = <_>::default();
        }

        if unhandled_keys.iter().any(|e| e.is_some()) {
            pln!(
                "Received: {}{}{}{}{}{}{}{}",
                if let Some(key) = unhandled_keys[0] { format!("{key}") } else { "".to_string() },
                if let Some(key) = unhandled_keys[1] { format!("{key}") } else { "".to_string() },
                if let Some(key) = unhandled_keys[2] { format!("{key}") } else { "".to_string() },
                if let Some(key) = unhandled_keys[3] { format!("{key}") } else { "".to_string() },
                if let Some(key) = unhandled_keys[4] { format!("{key}") } else { "".to_string() },
                if let Some(key) = unhandled_keys[5] { format!("{key}") } else { "".to_string() },
                if let Some(key) = unhandled_keys[6] { format!("{key}") } else { "".to_string() },
                if let Some(key) = unhandled_keys[7] { format!("{key}") } else { "".to_string() },
            );
        }

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
    let mut socket_debug = false;
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--client" => {
                is_client = true;
            }
            "--socket-debug" => {
                socket_debug = true;
            }
            _ => {
                return Err(format!("Unrecognized arg: {arg}").into());
            }
        }
    }

    if socket_debug {
        if is_client {
            do_socket_debug_client().map_err(From::from)
        } else {
            do_socket_debug_server().map_err(From::from)
        }
    } else {
        if is_client {
            do_client().map_err(From::from)
        } else {
            do_server().map_err(From::from)
        }
    }
}

// TODO? Make this configurable?
const SOCKET_DEBUG_PATH: &str = SOCKET_PATH;

fn do_socket_debug_client() -> Result<(), ClientError> {
    let socket = PathBuf::from(SOCKET_DEBUG_PATH);

    let mut stream = UnixStream::connect(&socket)?;

    let mut pipe_buffer = Vec::with_capacity(1024);
    io::stdin().read_to_end(&mut pipe_buffer)?;

    stream.write(&pipe_buffer).map(|_| ()).map_err(From::from)
}

fn do_socket_debug_server() -> Result<(), ServerError> {
    let p = Printer::ansi();

    p.enable_alternate_screen();
    let output = do_socket_debug_server_inner(&p);
    p.disable_alternate_screen();

    output
}

fn do_socket_debug_server_inner(p: &Printer) -> Result<(), ServerError> {
    use ServerError::*;

    let socket = PathBuf::from(SOCKET_DEBUG_PATH);

    // Delete old socket if necessary
    if socket.exists() {
        println!("Deleting old socket");
        fs::remove_file(&socket)?;
    }

    let listener = UnixListener::bind(&socket)?;
    listener.set_nonblocking(true)?;

    setup_terminal()?;

    println!("Server started, waiting for clients");

    let mut listener_buffer = String::with_capacity(1024);

    let row_count = get_row_count();

    loop {
        // Iterate over clients, blocks if no client available
        match listener.accept() {
            Ok((mut stream, _)) => {
                p.clear();
                p.move_home();

                stream.read_to_string(&mut listener_buffer)?;

                for line in listener_buffer.lines() {
                    println!("{line}");
                }

                listener_buffer.clear();
            },
            Err(e) if e.kind() == ErrorKind::WouldBlock => {}
            Err(e) => println!("accept function failed: {e:?}"),
        }

        // TODO calculate amount to sleep based on how long things took
        thread::sleep(std::time::Duration::from_millis(16));
    }

    Ok(())
}