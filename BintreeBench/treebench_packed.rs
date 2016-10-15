// Tested with rustc 1.8.0-nightly (d63b8e539 2016-01-23)

#![feature(alloc_system)]

extern crate alloc_system;

use std::env;
use std::process;
use std::time::Instant;
use std::time::Duration;


enum Foo {
    A,
    B(i32),
}


/*
enum Tree {
    Leaf(i32),
    Node(Box<Tree>, Box<Tree>),
}

fn build_tree(n : i32) -> Box<Tree> {
    build_tree_iter(1, n)
}

fn build_tree_iter(root : i32, n : i32) -> Box<Tree> {
    if n == 0 {
        Box::new(Tree::Leaf(root))
    } else {
        Box::new(Tree::Node(build_tree_iter(root, n - 1),
                            build_tree_iter(root + 2^(n - 1), n - 1)))
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

fn leftmost(tree : &Tree) -> i32 {
    match *tree {
        Tree::Leaf(i) => i,
        Tree::Node(ref left, _) => leftmost(left)
    }
}

fn main() {
    let args : Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Bad command line args. Expected one number (exponent).");
        process::exit(1);
    }

    let power : i32 = args[1].parse().unwrap();

    let times = 10;
    let mut total : Duration = Duration::new(0, 0);

    for _ in 0 .. times {
        let tree = build_tree(power);

        let start = Instant::now();
        let tree2 = add_1_tree(&*tree);
        let elapsed = start.elapsed();

        println!("Test, leftmost leaf in output: {}", leftmost(&*tree2));
        // println!("Took {:?}.", elapsed);
        println!("Took {}.{:09}", elapsed.as_secs(), elapsed.subsec_nanos());

        // Uh, += doesn't work but this works.
        total = total + elapsed;
    }

    let average = total / times;
    println!("Average of {} runs: {}.{:09} seconds.",
             times, average.as_secs(), average.subsec_nanos());
}
*/

fn main() {

    //    Vec<i32> v;

    let v  = vec![1, 2, 3, 4, 5]; // v: Vec<i32>    
    // let v2 = vec![ A(), B(3) ];

    println!("Test ... {:?}", v2);
}
