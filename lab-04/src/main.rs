use std::{
    collections::BTreeSet,
    fs,
    hint::black_box,
    io::{self, Read, Write},
    net::{TcpListener, TcpStream},
    num::NonZero,
    path::PathBuf,
    str::FromStr,
    time::Instant,
};

fn divisors(n: NonZero<u32>) -> BTreeSet<NonZero<u32>> {
    let mut set = BTreeSet::new();
    for i in 2..=n.get() {
        if n.get().is_multiple_of(i) {
            let val = unsafe { NonZero::new_unchecked(i) };
            set.insert(val);
        }
    }
    set
}

fn benchmark_divisors() {
    const TESTS_COUNT: usize = 100;
    let mut total_sum = 0;

    for i in 1..=TESTS_COUNT {
        let start = Instant::now();
        black_box(divisors(black_box(NonZero::new(i as _).unwrap())));
        let duration = start.elapsed();
        total_sum += duration.as_micros();
    }

    let average = total_sum / TESTS_COUNT as u128;

    let milis = average / 1000;
    let frac = average % 1000;

    println!("mean time: {milis},{frac} milisecs");
}

fn assert_sorted(vec: &Vec<u32>) {
    let pairs = vec.windows(2);
    for pair in pairs {
        if pair[0] > pair[1] {
            panic!("{:?} not sorted", vec)
        }
    }
}

fn bulk_read(stream: &mut TcpStream, size: usize) -> io::Result<Vec<u8>> {
    let mut buffer = vec![0; size];
    let mut remaining = size;
    while remaining != 0 {
        let read = stream.read(&mut buffer)?;
        remaining -= read;
    }
    Ok(buffer)
}

fn bulk_write(stream: &mut TcpStream, buf: &[u8]) -> io::Result<()> {
    let mut buffer = buf;
    let mut written = 0;
    while written != buf.len() {
        let write = stream.write(buffer)?;
        written += write;
        buffer = &buf[written..];
    }
    Ok(())
}

fn handle_client(mut stream: TcpStream) -> io::Result<()> {
    println!("handling client...");
    let len = bulk_read(&mut stream, 2)?[0];
    println!("received len: {len}");
    let path_bytes = bulk_read(&mut stream, len as _)?;
    let path_str = match String::from_utf8(path_bytes) {
        Ok(s) => s.trim().to_string(),
        Err(_) => {
            let err = "Incorrect utf8 string";
            bulk_write(&mut stream, err.as_bytes())?;
            return Ok(());
        }
    };
    let path = match PathBuf::from_str(&path_str) {
        Ok(p) => p,
        Err(_) => {
            let err = format!("Incorrect path\n {:?}", path_str.as_bytes());
            bulk_write(&mut stream, err.as_bytes())?;
            return Ok(());
        }
    };
    println!("received path: {:?}", path);

    let mut responses = vec![];

    for entry in fs::read_dir(&path)? {
        let dir = entry?;
        let path = dir.path();
        responses.push(path.to_str().unwrap().to_string());
    }

    let response = responses.join("\n");
    bulk_write(&mut stream, response.as_bytes())?;

    stream.flush()?;

    Ok(())
}

fn main() {
    benchmark_divisors();

    let vec1 = vec![1, 2, 3];
    assert_sorted(&vec1);

    let listener = TcpListener::bind("127.0.0.1:8080").unwrap();

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                if let Err(e) = handle_client(stream) {
                    eprintln!("[SERVER] Error while handling client: {e}");
                }
            }
            Err(e) => {
                eprintln!("[SERVER] Failed to handle client: {e}");
            }
        }
    }
}
