use std::env;
use std::fs;
use std::fmt;
use std::collections::HashMap;

type Row = Vec<char>;
type Grid = Vec<Row>;
type Rules = HashMap<Grid, Grid>;

fn main() {
    let initial_grid = Vec::from([
                                 ".#.".chars().collect(),
                                 "..#".chars().collect(),
                                 "###".chars().collect()
                                 ]);

    let result = solve_day_21(
        "../data/2017/21.txt",
        initial_grid.clone(),
        18
        );
    println!("{result}");
}


fn solve_day_21(fp: &str, initial_state: Grid, num_iterations: i32) -> String {
    let transformation_rules = load_day_21_data(fp);
    //for (from, to) in load_day_21_data(fp).iter() {
    //    println!("{from} -> {to}");
    //}

    let mut grid_state = initial_state;
    match transformation_rules.get(&grid_state) {
        Some(pattern) => grid_state = pattern.clone(),
        None => println!("ERROR!! coulddn't match initial grid")
    }

    for i in 0..num_iterations-1 {
        grid_state = run_iteration(transformation_rules.clone(), grid_state);
    }

    let grid_bits = grid_set_bits(grid_state.clone());
    return format!("Results:\n {:?} \n\n {}", grid_state, grid_bits);
}

fn grid_set_bits(grid: Grid) -> i32 {
    let mut count = 0;
    for row in grid {
        for col in row {
            if col == '#' { count +=1 ; }
        }
    }
    count
}

fn run_iteration(rules: Rules, grid: Grid) -> Grid {
    let grid_size = grid.len();
    let squares_per_row : usize;
    let square_size: usize;
    if grid_size % 2 == 0 { square_size = 2; } else {square_size = 3; }
    squares_per_row = grid_size / square_size;

    println!("{:?}", grid);
    // Subdivide the input grid, and push it into the next step
    let mut parts: Vec<Grid> = Vec::new();
    for r in 0..squares_per_row {
        for c in 0..squares_per_row {
            // Build up the sub matrix
            let mut sub_grid: Grid = Vec::new();
            for x in (r*square_size)..((r+1)*square_size) {
                let mut row: Row = Vec::new();
                for y in (c*square_size)..((c+1)*square_size) {
                    row.push(grid[x][y]);
                }
                sub_grid.push(row);
            }
            // Store the sub matrix to process in a moment
            match rules.get(&sub_grid) {
                Some(pattern) => parts.push(pattern.clone()),
                None => println!("Oh no, the grid doesn't match!! c:{:?} {:?} \n=>\n {:?}", c, sub_grid, rules)
            }
        }
    }

    println!("{:?}", parts);

    // Push all the parts back together
    let new_width = (parts.len() as f32).sqrt().floor() as usize;
    let subgrid_size = parts[0].len();

    let mut result_grid: Grid = vec![Vec::new(); new_width * subgrid_size];
    // Given the list of new sub-grids in parts, pull off `new_width` elems
    // For each "row" of grids
    //  For each row in each grid, push it onto the result grid
    for row in 0..new_width {
        let slice = &parts[row*new_width..((row+1)*new_width)];
        for col in 0..new_width {
            for row_slice in 0..subgrid_size {
                // Push the row slice onto the result grid
                result_grid[(row*subgrid_size) + row_slice].append(&mut slice[col][row_slice].clone());
            }
        }
    }

    result_grid
}

fn load_day_21_data(fp: &str) -> Rules {
    println!("{fp}");
    let raw_data = fs::read_to_string(fp);
    match raw_data {
        Result::Ok(res) => {
            let mut mapping_rules: Rules = HashMap::new();
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

fn as_string_vec(str: &str, delim: &str) -> Vec<Row> {
    str.split(delim).map(|s| s.chars().collect()).collect()
}

fn add_rule_to_index<'a>(hm: &'a mut Rules, rule: &str) {
    let rule_stages: Vec<&str> = rule.split(" => ").collect();
    let input: Grid = as_string_vec(rule_stages[0], "/");
    let output: Grid = as_string_vec(rule_stages[1], "/");

    do_add_rule(hm, input.clone(), output.clone());
    do_add_rule(hm, flip(input), output.clone());
}

fn do_add_rule<'a>(hm: &'a mut Rules, input: Grid, output: Grid) {
    let input2 = rotate_grid(input.clone());
    let input3 = rotate_grid(input2.clone());
    let input4 = rotate_grid(input3.clone());

    hm.insert(input, output.clone());
    hm.insert(input2, output.clone());
    hm.insert(input3, output.clone());
    hm.insert(input4, output.clone());
}

// Rotate a grid 90 degrees counter-clockwise
fn rotate_grid(grid: Grid) -> Grid{
    let mut result: Grid = Vec::new();
    let mut n = grid.len();
    for row in 0..grid.len() {
        n = n-1;
        let mut new_row: Row = Vec::new();
        for i in 0..grid.len() {
            let c = grid[i][n];
            new_row.push(c);
        }
        result.push(new_row);
    }

    result
}

fn flip(grid: Grid) -> Grid {
    let mut flipped: Grid = Vec::new();
    for v in grid {
        let mut v2 = v.clone();
        v2.reverse();
        flipped.push(v2);
    }
    flipped
}
