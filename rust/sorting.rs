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

fn main() {
    
    let mut arr = [5,2,7,8,1,2];
    bubble_sort(&mut arr);
    println!("{:?}", arr);

    let arr2 = vec![5,2,7,8,1,2];
    println!("{:?}", merge_sort(arr2));
}