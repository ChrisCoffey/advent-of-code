use std::fs;

pub type Row = Vec<bool>;
pub type Grid = Vec<Row>;
pub struct Point {
    x: usize,
    y: usize
}
pub struct State<'a> {
    location: Point,
    direction: Direction,
    grid: &'a mut Grid,
    infections:usize
}
pub enum Direction  {
    Up,
    Down,
    Left,
    Right
}

pub fn solve_part1(fp: &str) -> usize {
    let iterations: usize = 10000;
    let mut initial_grid = load_data(&iterations, fp);

    let mut state = State {
        location: initial_location(&initial_grid),
        direction: Direction::Up,
        grid: &mut initial_grid,
        infections: 0
    };

    for _ in 0..iterations {
        state = run_burst(state);
    }

    state.infections
}

fn run_burst(state: State) -> State {
    let current_cell = state.grid[state.location.y][state.location.x];
    let next_location: Point;
    let next_direction: Direction;

    match (&state.direction, current_cell) {
        (Direction::Up, true) => {
            next_direction = Direction::Right;
            next_location = Point {y: state.location.y, x: state.location.x + 1 }
        }
        (Direction::Up, false ) => {
            next_direction = Direction::Left;
            next_location = Point {y: state.location.y, x: state.location.x - 1 }
        }

        (Direction::Right, true) => {
            next_direction = Direction::Down;
            next_location = Point {y: state.location.y + 1, x: state.location.x }
        }
        (Direction::Right, false) => {
            next_direction = Direction::Up;
            next_location = Point {y: state.location.y - 1, x: state.location.x }
        }

        (Direction::Down, true) => {
            next_direction = Direction::Left;
            next_location = Point {y: state.location.y, x: state.location.x - 1 }
        }
        (Direction::Down, false) => {
            next_direction = Direction::Right;
            next_location = Point {y: state.location.y, x: state.location.x + 1 }
        }

        (Direction::Left, true) => {
            next_direction = Direction::Up;
            next_location = Point {y: state.location.y - 1, x: state.location.x }
        }
        (Direction::Left, false) => {
            next_direction = Direction::Down;
            next_location = Point {y: state.location.y + 1, x: state.location.x }
        }
    }

    state.grid[state.location.y][state.location.x] = !current_cell;

    State {
        location: next_location,
        direction: next_direction,
        grid: state.grid,
        infections: state.infections + (if !current_cell { 1 } else {0} )
    }
}


pub fn load_data(padding: &usize, fp: &str) -> Grid {
    let raw_data = fs::read_to_string(fp);
    match raw_data {
        Result::Ok(res) => {
            let mut grid: Grid = Vec::new();
            for line in res.lines() {
                let before = vec![false; *padding];
                let after = vec![false; *padding];
                let full_line = [before, parse_line(line), after].concat();
                grid.push(full_line);
            }
            let row_width = grid[0].len();
            let before = vec![
                vec![false; row_width];
                *padding
                ];
            let after = vec![
                vec![false; row_width];
                *padding
                ];
            grid = [before, grid, after].concat();
            // Grid is now the correct width, but not the correct height. Pad vertically
            println!("{}\n{}", grid.len(), grid[0].len());
            grid
        }
        Result::Err(err) => {
            println!("Error loadig data: {err}");
            panic!();
        }
    }
}

fn initial_location(grid: &Grid) -> Point {
    let y_axis = (*grid).len();
    let x_axis = (*grid)[0].len();

    Point {y: y_axis / 2, x: x_axis /2 }
}

fn parse_line(line: &str) -> Vec<bool> {
    line.chars().map(|c| if c == '#' {true} else {false}).collect()
}
