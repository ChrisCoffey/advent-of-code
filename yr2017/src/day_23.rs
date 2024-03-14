use std::fmt;
use std::fs;


pub enum Val {
    Reg(usize),
    Val(usize)
}

pub type Registers = vec![0; 8];
pub enum Instruction {
    Set(usize, Val),
    Sub(usize, Val),
    Mul(usize, Val),
    Jnz(Val, ),
}

pub fn load_data(fp: &str) -> {

}

fn map_register(r: char) -> usize {
    (r as u8) - b'a'
}
