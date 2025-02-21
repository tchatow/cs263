use std::time::{Instant};
 
enum Tree {
    Empty,
    Node {
        left: Box<Tree>,
        right: Box<Tree>,
    }
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
        Tree::Node { left, right } => 1 + node_count(left) + node_count(right)
    }
}

fn main() {
    let start = Instant::now();
    
    for depth in 0..=21 {
        let tree = init_tree(depth); 
        let count = node_count(&tree);

        println!("Depth {}: Nodes = {}", depth, count);
    }
    
    let end = Instant::now();
    println!("Took {:?}", (end - start));
}
