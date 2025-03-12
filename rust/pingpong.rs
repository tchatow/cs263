use std::sync::{Condvar, Mutex};
use std::thread;
use std::time::Instant;

fn main() {
    let cv = Condvar::new();
    let m = Mutex::new(0);

    const K_ITERS: i32 = 10000000;

    let t1 = Instant::now();
    thread::scope(|s| {
        s.spawn(|| -> _ {
            let mut l = m.lock().unwrap();

            while *l < K_ITERS {
                // Wakeup on evens
                while *l % 2 != 0 {
                    l = cv.wait(l).unwrap();
                }

                *l += 1;
                //println!("t1 {}", *l);
                cv.notify_one();
            }
        });

        s.spawn(|| -> _ {
            let mut l = m.lock().unwrap();

            while *l < K_ITERS {
                // Wakeup on odds
                while *l % 2 != 1 {
                    l = cv.wait(l).unwrap();
                }

                *l += 1;
                //println!("t2 {}", *l);
                cv.notify_one();
            }
        });

        cv.notify_one();
    });
    let t2 = Instant::now();

    let dt = t2 - t1;

    println!(
        "Sent {} messages in {:?}, {}m/s",
        K_ITERS,
        dt,
        (K_ITERS as f32) * 1000000.0 / (dt.as_micros() as f32)
    );
}
