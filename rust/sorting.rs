use rand::prelude::*;
use rand::Rng;
use std::time::Instant;

fn bubble_sort(arr: &mut [i32]) {
    let n = arr.len();
    if n <= 1 {
        return;
    }

    for i in 0..n {
        let mut swapped = false;

        for j in 0..n - i - 1 {
            if arr[j] > arr[j + 1] {
                arr.swap(j, j + 1);
                swapped = true;
            }
        }

        if !swapped {
            break;
        }
    }
}

fn merge_sort(arr: Vec<i32>) -> Vec<i32> {
    let len = arr.len();
    if len <= 1 {
        return arr;
    }

    let mid = len / 2;
    let left = merge_sort(arr[..mid].to_vec());
    let right = merge_sort(arr[mid..].to_vec());

    merge(left, right)
}

fn merge(left: Vec<i32>, right: Vec<i32>) -> Vec<i32> {
    let mut merged = Vec::new();
    let mut left_idx = 0;
    let mut right_idx = 0;

    while left_idx < left.len() && right_idx < right.len() {
        if left[left_idx] <= right[right_idx] {
            merged.push(left[left_idx]);
            left_idx += 1;
        } else {
            merged.push(right[right_idx]);
            right_idx += 1;
        }
    }

    while left_idx < left.len() {
        merged.push(left[left_idx]);
        left_idx += 1;
    }

    while right_idx < right.len() {
        merged.push(right[right_idx]);
        right_idx += 1;
    }

    merged
}

fn generate_random_array(size: usize) -> Vec<i32> {
    let mut rng = rand::rng();
    let mut arr = Vec::with_capacity(size);

    for _ in 0..size {
        arr.push(rng.gen_range(0..(size as i32)));
    }

    arr
}

fn stats(data: &[f32]) -> (f32, f32) {
    let n = data.len() as f32;
    let avg: f32 = data.iter().sum::<f32>() / n;
    let stddev = (data.iter().map(|x| (*x - avg).powf(2.0)).sum::<f32>() / n).sqrt();
    (avg, stddev)
}

fn main() {
    const K: usize = 10;
    let mut bubble_times = [[0.0; K]; 24];
    let mut merge_times = [[0.0; K]; 24];

    for n in 0..10 {
        for i in 0..17 {
            let t1 = Instant::now();
            let arr = generate_random_array(2usize.pow(i));
            let arr_len = arr.len();
            let mut arr1 = arr.clone();
            bubble_sort(&mut arr1);
            let t2 = Instant::now();
            merge_sort(arr);
            let t3 = Instant::now();

            bubble_times[i as usize][n] = (t2 - t1).as_micros() as f32;
            merge_times[i as usize][n] = (t3 - t2).as_micros() as f32;

            /*println!(
                "{}\t{}\t{}",
                arr_len,
                (t2 - t1).as_micros(),
                (t3 - t2).as_micros()
            );*/
        }
        println!("n={}", n);
    }

    println!("Results");
    for i in 0..=24 {
        let (a0, s0) = stats(&bubble_times[i]);
        let (a1, s1) = stats(&merge_times[i]);
        println!("{}\t{}\t{}\t{}", a0, s0, a1, s1);
    }
}
