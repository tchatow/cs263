use  std::time::Instant;

fn stats(data: &[f32]) -> (f32, f32) {
    let n = data.len() as f32;
    let avg: f32 = data.iter().sum::<f32>() / n;
    let stddev = (data.iter().map(|x| (*x - avg).powf(2.0)).sum::<f32>() / n).sqrt();
    (avg, stddev)
}

fn fibonacci_recursive(n: u64) -> u64 {
    match n {
        0 => 0,
        1 => 1,
        _ => fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2),
    }
}

fn main() {
    let mut times = [0.0; 10];
    for i in 0..10 {
        let start = Instant::now();
        fibonacci_recursive(40);
        let end = Instant::now();
        
        times[i] = (end - start).as_micros() as f32;
    }
    
    let (avg, stddev) = stats(&times);
    println!("{}\t{}", avg, stddev);
}
