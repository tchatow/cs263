use std::env;
use std::fs;
use std::time::Instant;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    let search = &args[2];
    let data = fs::read_to_string(&args[1]).unwrap();
    
    let mut start = 0;

   let t1 = Instant::now();
    while let Some(m) = data[start..].find(search) {
        println!("Found match at byte {}", m + start);
        start += m + search.len();
    }
    let t2 = Instant::now();
    println!("{}", (t2 - t1).as_micros());
}
