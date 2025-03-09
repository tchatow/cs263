use  std::time::Instant;

fn fibonacci_recursive(n: u64) -> u64 {
    match n {
        0 => 0,
        1 => 1,
        _ => fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2),
    }
}

fn main() {
    for _i in 0..10 {
        let start = Instant::now();
        fibonacci_recursive(40);
        let end = Instant::now();
        println!("{}", (end - start).as_micros());
    }
}
