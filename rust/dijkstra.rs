use csv;
use rand::Rng;
use std::{cmp, error::Error, fs::File, io, process, collections::HashSet};

#[derive(Clone, Default)]
struct Node {
    adj: Vec<u32>,
    adj_len: Vec<f32>,
}

#[derive(Default)]
struct Graph {
    nodes: Vec<Node>,
}

impl Graph {
    fn load(&mut self, filename: &str) {
        let mut rdr = csv::Reader::from_reader(File::open(filename).unwrap());
        let mut rows = 0;
        for result in rdr.records() {
            let record = result.unwrap();
            let start_id = record[2].parse::<u32>().unwrap() - 1;
            let end_id = record[3].parse::<u32>().unwrap() - 1;
            let len = record[5].parse::<f32>().unwrap();

            self.nodes
                .resize((cmp::max(start_id, end_id) + 1) as usize, Node::default());

            self.nodes[start_id as usize].adj.push(end_id);
            self.nodes[start_id as usize].adj_len.push(len);
            self.nodes[end_id as usize].adj.push(start_id);
            self.nodes[end_id as usize].adj_len.push(len);

            rows += 1;
            
            if rows > 10000 {
                break;
            }
        }

        println!("{} Nodes, {} Edges", self.nodes.len(), rows);
    }

    fn shortest_path(&self, start: i32, end: i32) -> Vec<i32> {
        let mut q: HashSet<usize> = HashSet::new();

        let mut dist = vec![f32::INFINITY; self.nodes.len()];
        let mut prev = vec![-1; self.nodes.len()];

        for node in 0..self.nodes.len() {
            q.insert(node);
        }
        dist[start as usize] = 0.0;

        while !q.is_empty() {
            let &u = q
                .iter()
                .min_by(|&x, &y| dist[*x].total_cmp(&dist[*y]))
                .unwrap();
            q.remove(&u);

            for (vi, &v) in self.nodes[u].adj.iter().enumerate() {
                if !q.contains(&(v as usize)) {
                    continue;
                }
                
                let new_dist = dist[u] + self.nodes[u].adj_len[vi];
                if new_dist < dist[v as usize] {
                    dist[v as usize] = new_dist;
                    prev[v as usize] = u as i32;
                }
            }
        }
        
        let mut seq = Vec::new();
        let mut u = end;
        
        if prev[u as usize] >= 0 || u == start {
            while u >= 0 {
                seq.push(u);
                u = prev[u as usize] as i32;
            }
        }
        
        seq
    }
}

fn main() {
    let mut g = Graph::default();

    g.load("../data/NewYork_Edgelist.csv");
    
    let mut rng = rand::rng();
    
    
    for i in 0..1000 {
        let max = g.nodes.len() as i32;
        let start = rng.random_range(0..max);
        let end = rng.random_range(0..max);
    
        let seq = g.shortest_path(start, end);
        println!("Shortest path from {} to {}: {:?}", start, end, seq);
    }
}
