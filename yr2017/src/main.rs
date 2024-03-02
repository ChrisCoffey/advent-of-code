use std::env;
use std::fs;
use std::fmt;
use std::collections::HashMap;

fn main() {
    let initial_grid = Vec::from([
                                 String::from(".#."),
                                 String::from("..#"),
                                 String::from("###")
                                 ]);

    let result = solve_day_21(
        "../data/2017/21.tst",
        initial_grid.clone(),
        2
        );
    println!("{result}");
}


fn solve_day_21(fp: &str, initial_state: Vec<String>, num_iterations: i32) -> String {
    let transformation_rules = load_day_21_data(fp);
    //for (from, to) in load_day_21_data(fp).iter() {
    //    println!("{from} -> {to}");
    //}

    let mut grid_state = initial_state;
    for i in 0..num_iterations {
        grid_state = run_iteration(transformation_rules.clone(), grid_state);
    }

    return String::from("success?");
}

fn run_iteration(rules: HashMap<Vec<String>, Vec<String>>, grid: Vec<String>) -> Vec<String> {
    let grid_size = grid.len();
    let num_squares: usize;
    let square_size: usize;
    if grid_size % 2 == 0 { square_size = 2; }
    else {square_size = 3; }
    num_squares = grid_size / square_size;

    for i in 0..num_squares {
        for j in 0..grid[i* square_size].len() / square_size {
            let mut sub_square = Vec::new();
            for k in 0..square_size {
                let start = k * square_size;
                let end = start + square_size;
            }
        }
    }

    grid
}

fn load_day_21_data(fp: &str) -> HashMap<Vec<String>, Vec<String>>{
    println!("{fp}");
    let raw_data = fs::read_to_string(fp);
    match raw_data {
        Result::Ok(res) => {
            let mut mapping_rules: HashMap<Vec<String>, Vec<String>> = HashMap::new();
            let rules = res.split('\n');
            for rule in rules {
                if rule.len() > 1 { // Ignore trailng newlines
                    add_rule_to_index(& mut mapping_rules, rule);
                }
            }
            mapping_rules
        }
        Result::Err(err) => {
            println!("{err}");
            return HashMap::new()
        }
    }
}

fn as_string_vec(str: &str, delim: &str) -> Vec<String> {
    str.split(delim).map(|s| String::from(s)).collect()
}

fn add_rule_to_index<'a>(hm: &'a mut HashMap<Vec<String>, Vec<String>>, rule: &str) {
    let rule_stages: Vec<&str> = rule.split(" => ").collect();
    let input: Vec<String> = as_string_vec(rule_stages[0], "/");
    let output: Vec<String> = as_string_vec(rule_stages[1], "/");

    let input2 = rotate_grid(input.clone());
    let input3 = rotate_grid(input2.clone());
    let input4 = rotate_grid(input3.clone());

    hm.insert(input, output.clone());
    hm.insert(input2, output.clone());
    hm.insert(input3, output.clone());
    hm.insert(input4, output.clone());
}

// Rotate a grid 90 degrees counter-clockwise
fn rotate_grid(grid: Vec<String>) -> Vec<String>{
    let mut result: Vec<String> = Vec::new();
    let mut n = grid.len();
    for row in 0..grid.len() {
        n = n-1;
        let mut row_str = String::new();
        for i in 0..grid.len() {
            let char_opt = grid[i].chars().nth(n);
            match char_opt {
                Some(c) => row_str.push(c),
                None => println!("Error!! Grid index out of bounds somehow.")
            }
        }
        result.push(row_str);
    }

    result
}
