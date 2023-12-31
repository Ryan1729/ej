#![deny(unreachable_patterns)]

use std::{
    collections::{HashSet},
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

#[derive(Copy, Clone, PartialEq, Eq)]
enum Digit {
    Zero,
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
}

impl Digit {
    pub fn u8(self) -> u8 {
        use Digit::*;
        match self {
            Zero => 0,
            One => 1,
            Two => 2,
            Three => 3,
            Four => 4,
            Five => 5,
            Six => 6,
            Seven => 7,
            Eight => 8,
            Nine => 9,
        }
    }

    pub fn usize(self) -> usize {
        usize::from(self.u8())
    }
}

impl core::fmt::Display for Digit {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.u8())
    }
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
        own_index: DuplicateIndex,
        // 6 so this structure takes a round number of bytes, and because we
        // don't expect to need even that many in practice.
        indexes: [DuplicateIndex; 6],
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

        #[cfg(test)]
        pub fn len(&self) -> DuplicateIndexesLen {
            self.len
        }

        pub fn own_index(&self) -> DuplicateIndex{
            self.own_index
        }

        pub fn as_slice(&self) -> &[DuplicateIndex] {
            &self.indexes[..usize::from(self.len)]
        }

        pub fn get_1_based(&self, index: Digit) -> Option<DuplicateIndex> {
            if index == Digit::Zero {
                return None
            }
            self.as_slice()
                .get(index.usize() - 1)
                .copied()
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

                    macro_rules! process_spans {
                        ($root: expr) => {

                            if let Some(Array(spans)) = $root.get("spans") {
                                for span_value in spans {
                                    let Object(span) = span_value else { continue };

                                    let Some(String(ref path)) = span.get("file_name") else { continue };
                                    let Some(Number(line_number)) = span.get("line_start") else { continue };

                                    let mut external_lib = false;
                                    for needle in ["rustup", "rustc", ".cargo"] {
                                        external_lib = path.contains(needle);
                                        if external_lib {
                                            break;
                                        }
                                    }

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
                },
                _ => {}
            }
        }

        jump_points.sort();
        jump_points.dedup();

        // Setting the duplicate indexes being O(N^2) isn't great, but I
        // don't see another way to pass the duplicate_indexes tests.

        let Ok(len) = DuplicateIndex::try_from(jump_points.len()) else {
            // If we do have more errors than fits inside a DuplicateIndex
            // gracefully degrading to just not tracking the duplicates seems
            // preferable to not displaying the jump points at all.
            return
        };
        let mut duplicated_indexes =
            // Usually there won't even be any duplicates. Could track the
            // max chain of duplicates in the spans collection loop I guess?
            // But we will use at least one to store the root in each loop.
            Vec::with_capacity(1);
        // This is used to avoid pushing the same duplicate indexes onto a
        // jump point more than once.
        let mut previous_duplicate_indexes = HashSet::with_capacity(usize::from(len));
        for root_i in 0..len {
            if previous_duplicate_indexes.contains(&root_i) {
                continue
            }

            duplicated_indexes.clear();
            {
                // TODO slice::split after root_i to avoid clone
                //let message = &jump_points[usize::from(root_i)].message;
                let message = jump_points[usize::from(root_i)].message.clone();

                duplicated_indexes.push(root_i);

                for check_i in (root_i + 1)..len {
                    if jump_points[usize::from(check_i)].message == message {
                        duplicated_indexes.push(check_i);
                        previous_duplicate_indexes.insert(check_i);
                    }
                }
            }

            if duplicated_indexes.len() > 1 {
                let mut own_index = 0;

                for &point_i in &duplicated_indexes {
                    let point: &mut JumpPoint =
                        &mut jump_points[usize::from(point_i)];

                    for &pushed_i in &duplicated_indexes {
                        point.duplicate_indexes.push(pushed_i);
                    }

                    point.duplicate_indexes.own_index = own_index;

                    own_index += 1;
                }
            }
        }
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
    fn parse_handles_external_entries_properly() {
        // We get some entries that point to, for example the rust std library,
        // locally on disk. We don't want those to be at the top of the list.
        // The place they appear on disk in a usual installation on linux, ends
        // up being in the ~/.rustup folder. Hopefully the Windows equivalent
        // contains the string rustup too.
        // TODO confirm that for Windows, etc.

        let to_parse = r#"
{"message": {"rendered": "msg", "spans": [{"file_name": ".cargo/example.rs", "line_start": 123}]}}
{"message": {"rendered": "msg", "spans": [{"file_name": "rustc/example.rs", "line_start": 123}]}}
{"message": {"rendered": "msg", "spans": [{"file_name": "rustup/example.rs", "line_start": 123}]}}
{"message": {"rendered": "msg", "spans": [{"file_name": "example.rs", "line_start": 123}]}}
"#;

        const EXPECTED_COUNT: usize = 4;

        let mut jump_points = Vec::with_capacity(EXPECTED_COUNT);

        parse(&mut jump_points, to_parse);

        assert_eq!(jump_points.len(), EXPECTED_COUNT);
        assert!(jump_points[1].external_lib, "{jump_points:?}");
        assert!(jump_points[1].path.to_string_lossy().contains(".cargo"));
        assert!(jump_points[2].external_lib);
        assert!(jump_points[2].path.to_string_lossy().contains("rustc"));
        assert!(jump_points[3].external_lib);
        assert!(jump_points[3].path.to_string_lossy().contains("rustup"));
        assert!(!jump_points[0].external_lib);
        assert!(!jump_points[0].path.to_string_lossy().contains("rustup"));
        assert!(!jump_points[0].path.to_string_lossy().contains(".cargo"));
        assert!(!jump_points[0].path.to_string_lossy().contains("rustc"));
    }

    #[test]
    fn parse_collects_all_spans_on_this_no_children_example() {
        let to_parse = r#"
{"message": {"rendered": "msg A", "spans": [{"file_name": "example.rs", "line_start": 123}]}}
{"message": {"rendered": "msg B", "spans": [{"file_name": "example.rs", "line_start": 123}]}}
{"message": {"rendered": "msg C", "spans": [{"file_name": "example.rs", "line_start": 123}]}}
"#;

        const EXPECTED_COUNT: usize = 3;

        let mut jump_points = Vec::with_capacity(EXPECTED_COUNT);

        parse(&mut jump_points, to_parse);
        assert_eq!(jump_points.len(), EXPECTED_COUNT, "{jump_points:?}");
    }

    #[test]
    fn parse_collects_all_spans_on_this_children_example() {
        let to_parse = r#"
{"message": {"rendered": "msg", "spans": [{"file_name": "rustup/example.rs", "line_start": 123}], "children": [{"spans": [{"file_name": "rustup/example_2.rs", "line_start": 123}]}]}}
{"message": {"rendered": "msg", "spans": [{"file_name": "example.rs", "line_start": 123}]}}
"#;

        const EXPECTED_COUNT: usize = 3;

        let mut jump_points = Vec::with_capacity(EXPECTED_COUNT);

        parse(&mut jump_points, to_parse);
        assert_eq!(jump_points.len(), EXPECTED_COUNT, "{jump_points:?}");
    }

    #[test]
    fn parse_collects_duplicate_indexes_on_this_example() {
        let to_parse = r#"
{"message": {"rendered": "different_msg", "spans": [{"file_name": "example.rs", "line_start": 123}]}}
{"message": {"rendered": "msg", "spans": [{"file_name": "example.rs", "line_start": 123}], "children": [{"spans": [{"file_name": "rustup/example.rs", "line_start": 123}, {"file_name": "rustup/example_2.rs", "line_start": 456}]}]}}
"#;

        const EXPECTED_COUNT: usize = 4;

        let mut jump_points = Vec::with_capacity(EXPECTED_COUNT);

        parse(&mut jump_points, to_parse);

        macro_rules! dup_ind {
            ($array: expr, $own_index: literal) => ({
                let mut output = DuplicateIndexes::default();

                output.own_index = $own_index;

                for el in $array {
                    output.push(el);
                }

                output
            })
        }

        assert_eq!(jump_points.len(), EXPECTED_COUNT);
        assert_eq!(
            jump_points[0].duplicate_indexes.len(),
            0
        );
        // Recall that the jump_points get sorted. Note though that we want
        // the own indexes, (which should be relative to the indexes array)
        // to be in ascending order in the final list.
        assert_eq!(
            jump_points[1].duplicate_indexes,
            dup_ind!([1, 2, 3], 0),
        );
        assert_eq!(
            jump_points[2].duplicate_indexes,
            dup_ind!([1, 2, 3], 1),
        );
        assert_eq!(
            jump_points[3].duplicate_indexes,
            dup_ind!([1, 2, 3], 2),
        );
    }
}

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
        DigitKey(Digit)
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
                DigitKey(digit) => write!(f, "DigitKey({digit})"),
            }
        }
    }

    let stdin_channel: Receiver<Input> = {
        let (tx, rx) = channel::<Input>();
        thread::spawn(move || {
            use Digit::*;
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
                    // ASCII 0 - 9
                    0x30 => { send!(DigitKey(Zero)); },
                    0x31 => { send!(DigitKey(One)); },
                    0x32 => { send!(DigitKey(Two)); },
                    0x33 => { send!(DigitKey(Three)); },
                    0x34 => { send!(DigitKey(Four)); },
                    0x35 => { send!(DigitKey(Five)); },
                    0x36 => { send!(DigitKey(Six)); },
                    0x37 => { send!(DigitKey(Seven)); },
                    0x38 => { send!(DigitKey(Eight)); },
                    0x39 => { send!(DigitKey(Nine)); },
                    _ => { send!(Byte(byte)); }
                }
            }
        });
        rx
    };

    let row_count = get_row_count();

    let mut unhandled_keys = [None; 8];

    let mut fade_message = String::with_capacity(1024);
    let mut fade_counter: u16 = 0;

    loop {
        p.clear();
        p.move_home();

        let mut lines_count = 0;
        // wrap println and count lines and only actually print lines after
        // count exceededs scroll_skip_lines value.
        macro_rules! pln {
            ($($format_args: tt)+) => ({
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
            })
        }

        // Like println but write to the fading message instead.
        // Resets the fade counter, ensuring the message is shown
        // for a while.
        macro_rules! fade_println {
            ($($format_args: tt)+) => ({
                use std::fmt::Write;
                // If we use this macro on multiple lines in sequence,
                // things will work fine, so we don't want a warning.
                {
                    #![allow(unused_assignments)]
                    fade_counter = 300;
                }
                let _ = writeln!(&mut fade_message, $($format_args)+);
            })
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

        let jump_point_opt = jump_points.get(index);
        if let Some(jump_point) = jump_point_opt {
            pln!("----vvvv----");
            pln!("{}", jump_point.message);
            pln!("{}", jump_point.path.display());
            let duplicates = jump_point.duplicate_indexes.as_slice();
            let len = duplicates.len();
            if len > 0 {
                let own_index = usize::from(
                    jump_point.duplicate_indexes.own_index()
                );
                // 4 for "[N] ", minus 1 for not outputting a trailing spce
                let mut s = String::with_capacity((len * 4) - 1);
                let mut sep = "";
                for zero_based_index in 0..len {
                    use std::fmt::Write;

                    if zero_based_index == own_index {
                        continue
                    }

                    let one_based_index = zero_based_index + 1;
                    let _ = write!(&mut s, "{sep}[{one_based_index}]");
                    sep = " ";
                }
                pln!("{s}");
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
                            fade_println!("{}", output.status.success());
                            match std::str::from_utf8(&output.stdout) {
                                Ok(s) => { fade_println!("{s}"); }
                                Err(err) => { fade_println!("Error: {err}"); }
                            }
                            match std::str::from_utf8(&output.stderr) {
                                Ok(s) => { fade_println!("{s}"); }
                                Err(err) => { fade_println!("Error: {err}"); }
                            }
                        }
                        Err(e) => {
                            fade_println!("{e}");
                        }
                    }
                },
                None => fade_println!("No jump point found"),
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

                match result {
                    Ok(_) => {
                        fade_println!("Wrote to {}", path.display());
                    }
                    Err(e) => {
                        fade_println!("When debug dumping: {e}");
                    }
                }
            },
            Ok(Input::DigitKey(digit)) => match jump_point_opt {
                Some(jump_point) => {
                    match jump_point
                        .duplicate_indexes
                        .get_1_based(digit) {
                        Some(duplicate_index) => {
                            let duplicate_index = usize::from(duplicate_index);
                            index = duplicate_index;
                            scroll_skip_lines = duplicate_index;
                        },
                        None => fade_println!("No duplicate for {digit}"),
                    }
                },
                None => fade_println!("No jump point found for digit jump"),
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

        if !fade_message.is_empty() {
            pln!("{fade_message}");
        }

        fade_counter = fade_counter.saturating_sub(1);
        if fade_counter == 0 {
            fade_message.clear();
        }

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
}

struct Terminator(Res<()>);

impl Termination for Terminator {
    fn report(self) -> ExitCode {
        match self.0 {
            Ok(()) => ExitCode::SUCCESS,
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
}