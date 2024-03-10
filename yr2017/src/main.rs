use std::env;
mod day_21;
mod day_22;


fn main() {
    run_day_21();
}

fn run_day_21() {
    let initial_grid = Vec::from([
                                 ".#.".chars().collect(),
                                 "..#".chars().collect(),
                                 "###".chars().collect()
                                 ]);

    let result = day_21::solve(
        "../data/2017/21.txt",
        initial_grid.clone(),
        18
        );
    println!("{result}");
}
