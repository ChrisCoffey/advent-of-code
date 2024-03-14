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


pub struct StateEvovle<'a> {
    location: Point,
    direction: Direction,
    grid: &'a mut GridEvolve,
    infections: usize
}
pub type RowEvolve = Vec<InfectionStatus>;
pub type GridEvolve = Vec<RowEvolve>;

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum InfectionStatus {
    Clean,
    Weak,
    Infected,
    Flagged
}

pub fn solve_part1(fp: &str) -> usize {
    let iterations: usize = 10000;
    let mut initial_grid: Grid = load_data(&true, &false, &iterations, fp);

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

pub fn solve_part2(fp: &str) -> usize {
    let iterations: usize = 10000000;
    println!("start");
    let mut initial_grid: GridEvolve =  load_data(&InfectionStatus::Infected, &InfectionStatus::Clean, &(iterations/ 10000), fp);

    println!("grid built");

    let mut state = StateEvovle {
        location: initial_location(&initial_grid),
        direction: Direction::Up,
        grid: &mut initial_grid,
        infections: 0
    };

    println!("Starting to loop");
    for i in 0..iterations {
        state = run_burst_evolved(state);
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

fn run_burst_evolved(state: StateEvovle) -> StateEvovle {
    let current_cell = state.grid[state.location.y][state.location.x];
    let next_location: Point;
    let next_direction: Direction;

    match (&state.direction, &current_cell) {
        // Up
        (Direction::Up, InfectionStatus::Clean) => {
            next_direction = Direction::Left;
            next_location = Point {y: state.location.y, x: state.location.x - 1 }
        }
        (Direction::Up, InfectionStatus::Weak) => {
            next_direction = Direction::Up;
            next_location = Point {y: state.location.y - 1, x: state.location.x }
        }
        (Direction::Up, InfectionStatus::Infected) => {
            next_direction = Direction::Right;
            next_location = Point {y: state.location.y, x: state.location.x + 1 }
        }
        (Direction::Up, InfectionStatus::Flagged) => {
            next_direction = Direction::Down;
            next_location = Point {y: state.location.y + 1, x: state.location.x }
        }


        // Right
        (Direction::Right, InfectionStatus::Clean) => {
            next_direction = Direction::Up;
            next_location = Point {y: state.location.y -1, x: state.location.x }
        }
        (Direction::Right, InfectionStatus::Weak) => {
            next_direction = Direction::Right;
            next_location = Point {y: state.location.y, x: state.location.x + 1}
        }
        (Direction::Right, InfectionStatus::Infected) => {
            next_direction = Direction::Down;
            next_location = Point {y: state.location.y + 1, x: state.location.x }
        }
        (Direction::Right, InfectionStatus::Flagged) => {
            next_direction = Direction::Left;
            next_location = Point {y: state.location.y, x: state.location.x - 1 }
        }

        // Down
        (Direction::Down, InfectionStatus::Clean) => {
            next_direction = Direction::Right;
            next_location = Point {y: state.location.y, x: state.location.x + 1 }
        }
        (Direction::Down, InfectionStatus::Weak) => {
            next_direction = Direction::Down;
            next_location = Point {y: state.location.y + 1, x: state.location.x }
        }
        (Direction::Down, InfectionStatus::Infected) => {
            next_direction = Direction::Left;
            next_location = Point {y: state.location.y, x: state.location.x - 1 }
        }
        (Direction::Down, InfectionStatus::Flagged) => {
            next_direction = Direction::Up;
            next_location = Point {y: state.location.y - 1, x: state.location.x }
        }

        // Left
        (Direction::Left, InfectionStatus::Clean) => {
            next_direction = Direction::Down;
            next_location = Point {y: state.location.y + 1, x: state.location.x }
        }
        (Direction::Left, InfectionStatus::Weak) => {
            next_direction = Direction::Left;
            next_location = Point {y: state.location.y, x: state.location.x - 1}
        }
        (Direction::Left, InfectionStatus::Infected) => {
            next_direction = Direction::Up;
            next_location = Point {y: state.location.y-1, x: state.location.x }
        }
        (Direction::Left, InfectionStatus::Flagged) => {
            next_direction = Direction::Right;
            next_location = Point {y: state.location.y, x: state.location.x + 1}
        }
    }

    state.grid[state.location.y][state.location.x] = evolve_cell(&current_cell);

    StateEvovle {
        location: next_location,
        direction: next_direction,
        grid: state.grid,
        infections: state.infections + (if current_cell == InfectionStatus::Weak { 1 } else {0} )
    }

}

fn evolve_cell(status: &InfectionStatus) -> InfectionStatus {
    match status {
        InfectionStatus::Clean => InfectionStatus::Weak,
        InfectionStatus::Weak => InfectionStatus::Infected,
        InfectionStatus::Infected => InfectionStatus::Flagged,
        InfectionStatus::Flagged => InfectionStatus::Clean
    }
}

pub fn load_data<A: Clone + Copy>(t: &A, f: &A, padding: &usize, fp: &str) -> Vec<Vec<A>> {
    let raw_data = fs::read_to_string(fp);
    match raw_data {
        Result::Ok(res) => {
            let mut grid: Vec<Vec<A>> = Vec::new();
            for line in res.lines() {
                let before = vec![*f; *padding];
                let after = vec![*f; *padding];
                let full_line = [before, parse_line(*t, *f, line), after].concat();
                grid.push(full_line);
            }
            let row_width = grid[0].len();
            let before = vec![
                vec![*f; row_width];
                *padding
                ];
            let after = vec![
                vec![*f; row_width];
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

fn initial_location<A>(grid: &Vec<Vec<A>>) -> Point {
    let y_axis = (*grid).len();
    let x_axis = (*grid)[0].len();

    Point {y: y_axis / 2, x: x_axis /2 }
}

fn parse_line<A: Copy>(t: A, f: A, line: &str) -> Vec<A> {
    line.chars().map(|c| if c == '#' {t} else {f}).collect()
}
