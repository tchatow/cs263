use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    let search = &args[2];
    let data = fs::read_to_string(&args[1]).unwrap();
    
    let mut start = 0;
    
    while let Some(m) = data[start..].find(search) {
        println!("Found match at byte {}", m + start);
        start += m + search.len();
    }
}
