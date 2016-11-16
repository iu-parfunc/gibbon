// Tested with rustc 1.8.0-nightly (d63b8e539 2016-01-23)

// #![feature(time2)]

use std::env;
use std::process;
use std::time::Instant;

enum Tree {
    Leaf(i32),
    Node(Box<Tree>, Box<Tree>),
}

fn build_tree(n : i32) -> Box<Tree> {
    build_tree_iter(1, n)
}

fn build_tree2(n : i32) -> Box<Tree> {
    build_tree_iter2(1, n)
}

fn build_tree_iter(root : i32, n : i32) -> Box<Tree> {
    if n == 0 {
        Box::new(Tree::Leaf(root))
    } else {
        Box::new(Tree::Node(build_tree_iter(root, n - 1),
                            build_tree_iter(root + 2^(n - 1), n - 1)))
    }
}

fn build_tree_iter2(root : i32, n : i32) -> Box<Tree> {
    if n == 0 {
        Box::new(Tree::Leaf(root))
    } else {
        Box::new(Tree::Node(build_tree_iter2(root, n - 1),
                            build_tree_iter2(root, n - 1)))
    }
}

fn add_1_tree(tree : &Tree) -> Box<Tree> {
    match *tree {
        Tree::Leaf(i) =>
            Box::new(Tree::Leaf(i + 1)),
        Tree::Node(ref left, ref right) =>
            Box::new(Tree::Node(add_1_tree(&*left), add_1_tree(&*right)))
    }
}

fn sum_tree(tree : &Tree) -> i32 {
   match *tree {
       Tree::Leaf(i) => i,
       Tree::Node(ref left, ref right) => (sum_tree(left) + sum_tree(right))
   }
}

fn leftmost(tree : &Tree) -> i32 {
    match *tree {
        Tree::Leaf(i) => i,
        Tree::Node(ref left, _) => leftmost(left)
    }
}

fn main() {
    let args : Vec<String> = env::args().collect();
    if args.len() < 4 {
        println!("Bad command line args. Expected one string and two numbers (exponent).");
        process::exit(1);
    }

    let mode: String = args[1].parse().unwrap();
    let power : i32 = args[2].parse().unwrap();
    let iters : u32 = args[3].parse().unwrap();
    
    if mode == "build".to_string() {
      let start = Instant::now();
			
      for _ in 0 .. iters {
        let tree = build_tree2(power);		

        println!("Test, leftmost leaf in output: {}", leftmost(&*tree));
        // println!("Took {:?}.", elapsed);
      }
      let elapsed = start.elapsed();
      println!("Took {}.{:09}", elapsed.as_secs(), elapsed.subsec_nanos());
      println!("BATCHTIME: {}.{}", elapsed.as_secs(), elapsed.subsec_nanos());

      let average = elapsed / iters;
      println!("Average of {} runs: {}.{:09} seconds.",
             iters, average.as_secs(), average.subsec_nanos());
    
    } else if mode == "sum".to_string() {
      let tree = build_tree(power);
      let start = Instant::now();
			
      for _ in 0 .. iters {
        let tree2 = sum_tree(&*tree);

	println!("Sum {}", tree2);
        // println!("Took {:?}.", elapsed);
      }
      let elapsed = start.elapsed();
      println!("Took {}.{:09}", elapsed.as_secs(), elapsed.subsec_nanos());
      println!("BATCHTIME: {}", elapsed.as_secs());

      let average = elapsed / iters;
      println!("Average of {} runs: {}.{:09} seconds.",
             iters, average.as_secs(), average.subsec_nanos());    

    } else {   
      let tree = build_tree(power);
      let start = Instant::now();
			
      for _ in 0 .. iters {
        let tree2 = add_1_tree(&*tree);

        println!("Test, leftmost leaf in output: {}", leftmost(&*tree2));
        // println!("Took {:?}.", elapsed);
      }
      let elapsed = start.elapsed();
      println!("Took {}.{:09}", elapsed.as_secs(), elapsed.subsec_nanos());
      println!("BATCHTIME: {}", elapsed.as_secs());

      let average = elapsed / iters;
      println!("Average of {} runs: {}.{:09} seconds.",
             iters, average.as_secs(), average.subsec_nanos());
    }
}