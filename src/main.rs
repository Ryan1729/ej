use std::{
    fs::File,
    io::{self, Read},
    os::unix::io::FromRawFd,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut f = unsafe { File::from_raw_fd(3) };
    let mut input = String::new();
    f.read_to_string(&mut input)?;

    println!("I read: {}", input);

    Ok(())
}


//fn main() -> Result<(), Box<dyn std::error::Error>> {
    //let mut pipe_buffer = Vec::with_capacity(1024);
    //io::stdin().read_to_end(&mut pipe_buffer)?;
//
    //println!("{:?}", String::from_utf8(pipe_buffer));
//
    //let mut keys_buffer = String::with_capacity(256);
//
    //while let Ok(count) = io::stdin().read_line(&mut keys_buffer) {
        //if count == 0 { continue }
        //println!("\rkeys_buffer: {keys_buffer}");
        //if keys_buffer.starts_with("q") {
            //println!("got quit command");
            //return Ok(())
        //}
//
        //keys_buffer.clear();
    //}
//
    //println!("fell out of loop");
//
    //Ok(())
//}