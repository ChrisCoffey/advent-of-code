use std::fs;

pub type Row = Vec<bool>;
pub type Grid = Vec<Row>;
pub struct Point {
    x: usize,
    y: usize
}
pub struct State {
    location: Point,
    direction: Direction,
    grid: Grid,
    infections:usize
}
pub enum Direction  {
    Up,
    Down,
    Left,
    Right
}

fn run_burst(state: State) -> State {
    let current_cell = state.grid[state.location.y][state.location.x];
    let next_location: Point;
    let next_direction: Direction;

    match (state.direction, current_cell) {
        (Up, true) => {
            next_direction = Direction::Right;
            next_location = Point {y: state.location.y, x: state.location.x + 1 }
        }
        (Up, false ) => {
            next_direction = Direction::Left;
            next_location = Point {y: state.location.y, x: state.location.x - 1 }
        }

        (Right, true) => {
            next_direction = Direction::Down;
            next_location = Point {y: state.location.y + 1, x: state.location.x }
        }
        (Right, false) => {
            next_direction = Direction::Up;
            next_location = Point {y: state.location.y - 1, x: state.location.x }
        }

        (Down, true) => {
            next_direction = Direction::Left;
            next_location = Point {y: state.location.y, x: state.location.x - 1 }
        }
        (Down, false) => {
            next_direction = Direction::Right;
            next_location = Point {y: state.location.y, x: state.location.x + 1 }
        }

        (Left, true) => {
            next_direction = Direction::Up;
            next_location = Point {y: state.location.y - 1, x: state.location.x }
        }
        (Left, false) => {
            next_direction = Direction::Down;
            next_location = Point {y: state.location.y + 1, x: state.location.x }
        }
    }

    let updated_grid = grid.&mut

    State {
        location: next_location,
        direction: next_direction,
        grid: state.grid,
        infections: state.infections + (if !current_cell { 1 } else {0} )
    }
}


pub fn load_data(padding: usize, fp: &str) -> Grid {
    let raw_data = fs::read_to_string(fp);
    match raw_data {
        Result::Ok(res) => {
            let mut grid: Grid = Vec::new();
            for line in res.lines() {
                let before = vec![false; padding];
                let after = vec![false; padding];
                let full_line = [before, parse_line(line), after].concat();
                grid.push(full_line);
            }
            grid
        }
        Result::Err(err) => {
            println!("Error loadig data: {err}");
            return Vec::new();
        }
    }
}

fn parse_line(line: &str) -> Vec<bool> {
    line.chars().map(|c| if c == '#' {true} else {false}).collect()
}
