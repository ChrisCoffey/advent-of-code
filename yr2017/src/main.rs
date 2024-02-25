use std::env;
use std::fs;
use std::collections::HashMap;

#[derive(Hash, Eq, PartialEq, Debug)]
enum Grid {
    TwoBy(String, String),
    ThreeBy(String, String, String),
}

fn main() {
    println!("Hello, world!");
    let result = solve_day_21("../data/2017/21.tst");
    println!("{result}");
}


fn solve_day_21(fp: &str) -> String {
    let data = load_day_21_data(fp);

    return "{data}".to_string();
}

fn load_day_21_data(fp: &str) -> HashMap<Grid, Grid>{
    println!("{fp}");
    let raw_data = fs::read_to_string(fp);
    match raw_data {
        Result::Ok(res) => {
            let mapping_rules: HashMap<Grid, Grid> = HashMap::new();
            let rules = res.split('\n');
            for rule in rules {
                add_rule_to_index(&mapping_rules, rule);
            }
            mapping_rules
        }
        Result::Err(err) => {
            println!("{err}");
            return HashMap::new()
        }
    }
}

fn add_rule_to_index<'a>(hm: &'a HashMap<Grid, Grid>, rule: &str) -> &'a HashMap<Grid,Grid> {
    let rule_stages: Vec<&str> = rule.split("=>").collect();
    let input = to_grid(rule_stages[0].split('/').collect());
    let output = to_grid(rule_stages[1].split('/').collect());

    let input2 = rotate_grid(input);
    let input3 = rotate_grid(input2);
    let input4 = rotate_grid(input3);

    hm.insert(input, output);
    hm.insert(input2, output);
    hm.insert(input3, output);
    hm.insert(input4, output);

    hm
}

fn to_grid(raw: Vec<&str>) -> Grid {
    let res: Grid;
    if raw.len() == 2 {
        res = Grid::TwoBy(
            String::from(raw[0]),
            String::from(raw[1])
            );
    } else {
        res = Grid::ThreeBy(
            String::from(raw[0]),
            String::from(raw[1]),
            String::from(raw[2])
            );
    }
    res
}

// Rotate a grid 90 degrees counter-clockwise
fn rotate_grid(grid: Grid) -> Grid {
    match grid {
        Grid::TwoBy(r1, r2) => {
            let letters1: Vec<char> = r1.chars().collect();
            let letters2: Vec<char> = r2.chars().collect();

            let letters1_ = [letters1[1],letters2[1]].iter().collect();
            let letters2_ = [letters1[0],letters2[0]].iter().collect();

            Grid::TwoBy(letters1_, letters2_)
        }
        Grid::ThreeBy(r1, r2, r3) => {
            let letters1: Vec<char> = r1.chars().collect();
            let letters2: Vec<char> = r2.chars().collect();
            let letters3: Vec<char> = r3.chars().collect();

            let letters1_ = [letters1[2], letters2[2], letters3[2]].iter().collect();
            let letters2_ = [letters1[1],letters2[1], letters3[1]].iter().collect();
            let letters3_ = [letters1[0],letters2[0], letters3[0]].iter().collect();

            Grid::ThreeBy(letters1_, letters2_, letters3_)
        }
    }
}
