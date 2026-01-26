package main

import "aoc25/days"
import "fmt"

func main() {
  day3Input := days.ImportDay3("data/day3.input")
  fmt.Println(day3Input)
  score := days.SolveDay3Part2(day3Input)
  fmt.Println(score)
}
