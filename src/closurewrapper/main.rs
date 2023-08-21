use std::convert::TryInto;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::thread;

fn main() {
    let own_path = Path::new("/proc/self/exe").read_link().unwrap();

    let init_fifo_path = own_path.with_file_name("init");
    let channels_fifo_path = own_path.with_file_name("channels");

    // TODO: Figure out how to lock this to prevent race conditions with concurrent access.
    // These should be pretty rare at least
    let mut init_fifo = File::create(&init_fifo_path).unwrap();
    let mut channel_fifo = File::open(&channels_fifo_path).unwrap();

    let mut args = std::env::args();
    // We don't want to pass argv[0] to the closure since it doesn't make much sense
    // so we skip it entirely
    let _ = args.next().unwrap();

    // We register this as a new execution of the parent's closure by sending program arguments through init
    // The arguments are formatted like this (with every number being 64bit little endian):
    // [argument count] [argument1.len()] argument1 ... [argumentN.len()] argumentN
    let arg_length_bytes = args.len().to_le_bytes();
    init_fifo.write(&arg_length_bytes).unwrap();
    for arg in args {
        let arg_length_bytes = arg.len().to_le_bytes();
        init_fifo.write(&arg_length_bytes).unwrap();
        init_fifo.write(arg.as_bytes()).unwrap();
    }

    // The parent sends the corresponding id for the actual closure fifos.
    // The id is sent as a 64bit little endian number
    let mut id_buffer = [0u8; 8];
    channel_fifo.read_exact(&mut id_buffer).unwrap();
    let id = u64::from_le_bytes(id_buffer);
    // TODO: release the lock again

    // By now, the parent should have created two new pipes for stdin and stdout
    let stdin_pipe_path = own_path.with_file_name(format!("{}_stdin", id));
    let stdout_pipe_path = own_path.with_file_name(format!("{}_stdout", id));
    let exit_code_pipe_path = own_path.with_file_name(format!("{}_exit_code", id));

    let mut stdin_pipe = File::create(&stdin_pipe_path).unwrap();
    let mut stdout_pipe = File::open(&stdout_pipe_path).unwrap();

    // TODO: Can we use dup2 here to alias the file descriptors and avoid manually copying anything?
    thread::spawn(move || {
        std::io::copy(&mut std::io::stdin(), &mut stdin_pipe).unwrap();
    });
    thread::spawn(move || {
        std::io::copy(&mut stdout_pipe, &mut std::io::stdout()).unwrap();
    });
    
    let mut exit_code_pipe = File::open(&exit_code_pipe_path).unwrap();
    let mut exit_code_buffer = [0u8; 8];
    exit_code_pipe.read_exact(&mut exit_code_buffer).unwrap();
    let exit_code = u64::from_le_bytes(exit_code_buffer);
    std::process::exit(exit_code.try_into().unwrap());
}
