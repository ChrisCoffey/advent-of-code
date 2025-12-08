use std::fmt;
use std::fs;


pub enum Val {
    Reg(usize),
    Val(usize)
}

pub type Registers = vec![0; 8];
pub type Instructions = Array<Instruction>;
pub enum Instruction {
    Set(usize, Val),
    Sub(usize, Val),
    Mul(usize, Val),
    Jnz(Val, ),
}

pub fn load_data(fp: &str) -> Instructions {
    let raw_data = fs::read_to_string(fp);
    let lines = raw_data.lines();
    let result = lines.map( |inst| )

    result
}

fn parse_instruction(line: &str) -> Instruction {
}

fn map_register(r: char) -> usize {
    (r as u8) - b'a'
}
