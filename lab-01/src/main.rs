use std::{
    fs::File,
    io::{self, Write},
};

fn main() {
    let result = run_main_loop();
    match result {
        true => println!("an error occurred"),
        false => println!("user stopped the program"),
    }
    let (int, float) = run_bonus_loop();
    println!("Got int {int}, got float {float}");
}

fn run_main_loop() -> bool {
    let mut input = String::new();
    loop {
        println!("Podaj liczbę");
        input.clear();
        io::stdin()
            .read_line(&mut input)
            .expect("should read line from stdin");
        let x = match input.trim().parse::<u64>() {
            Ok(x) => x,
            Err(_) => {
                break true;
            }
        };
        if x == 0 {
            break false;
        }
        let r: u64 = rand::random_range(0..=5);
        let new_x = x + r;
        println!("New x: {new_x}");
        let powers = find_powers(new_x);
        let collatz = check_collacts_arr(&powers);
        if save_to_file(&collatz) {
            break true;
        }
    }
}

fn run_bonus_loop() -> (i32, f32) {
    let mut x = 2;
    'outer: loop {
        x += 1;
        loop {
            x += 2;
            if x > 15 {
                break 'outer (x, x as f32 * 2.5);
            }
            if x > 7 {
                break;
            }
        }
    }
}

const ARR_SIZE: usize = 10;

fn find_powers(x: u64) -> [u64; ARR_SIZE] {
    let mut arr = [1; ARR_SIZE];
    arr[0] = x;
    for i in 1..ARR_SIZE {
        arr[i] = arr[i - 1] * x;
    }
    arr
}

fn check_collacts_arr(input: &[u64; ARR_SIZE]) -> [bool; ARR_SIZE] {
    let mut result = [false; ARR_SIZE];
    for (idx, &i) in input.iter().enumerate() {
        result[idx] = check_collatz(i);
    }
    result
}

fn check_collatz(x: u64) -> bool {
    const MAX_ITERS: usize = 100;

    let mut value = x;

    for _ in 0..MAX_ITERS {
        if value == 1 {
            return true;
        }
        let is_even = value & 1 == 0;
        match is_even {
            true => value /= 2,
            false => value = value * 3 + 1,
        }
    }

    false
}

fn save_to_file(input: &[bool]) -> bool {
    const FILE_NAME: &str = "xyz.txt";
    let mut file = File::create(FILE_NAME).expect("should create the file");
    let buffer = input
        .iter()
        .map(|b| match b {
            true => b'1',
            false => b'0',
        })
        .collect::<Vec<u8>>();
    file.write_all(&buffer).is_err()
}
