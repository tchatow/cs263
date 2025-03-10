use std::time::Instant;

enum Tree {
    Empty,
    Node { left: Box<Tree>, right: Box<Tree> },
}

fn init_tree(depth: u32) -> Tree {
    if depth == 0 {
        Tree::Empty
    } else {
        Tree::Node {
            left: Box::new(init_tree(depth - 1)),
            right: Box::new(init_tree(depth - 1)),
        }
    }
}

fn node_count(tree: &Tree) -> u32 {
    match tree {
        Tree::Empty => 0,
        Tree::Node { left, right } => 1 + node_count(left) + node_count(right),
    }
}

fn stats(data: &[f32]) -> (f32, f32) {
    let n = data.len() as f32;
    let avg: f32 = data.iter().sum::<f32>() / n;
    let stddev = (data.iter().map(|x| (*x - avg).powf(2.0)).sum::<f32>() / n).sqrt();
    (avg, stddev)
}

fn main() {
    const K: usize = 10;
    let mut times = [[0.0; K]; 22];
    
    for i in 0..K {
        let start = Instant::now();
        for depth in 0..=21 {
            let runs = Instant::now();
            let tree = init_tree(depth);
            let count = node_count(&tree);
            let ends = Instant::now();

            //    println!("Depth {}: Nodes = {}, Runtime = {:?}", depth, count, (ends - runs));
            // println!("{}\t{}\t{}", depth, count, (ends - runs).as_micros());
            
            times[depth as usize][i] = (ends - runs).as_micros() as f32;
            println!("{}", (ends - runs).as_micros());
        }

        let end = Instant::now();
        println!("Took {:?}", (end - start));
    }
    
    println!("Results");
    for i in 0..=21 {
        let (avg, stddev) = stats(&times[i]);
        println!("{}\t{}", avg, stddev);
    }
}
