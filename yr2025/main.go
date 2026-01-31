package main

import "aoc25/days"
import "fmt"

func main() {
  day4Input := days.ImportDay4("data/day4.input")
  fmt.Println(day4Input)
  score := days.SolveDay4Part2(day4Input)
  fmt.Println(score)
}
