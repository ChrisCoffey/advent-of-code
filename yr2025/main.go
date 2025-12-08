package main

import "aoc25/days"
import "fmt"

func main() {
  day2Input := days.ImportDay2("data/day2.input")
  fmt.Println(day2Input)
  score, score2 := days.SolveDay2(day2Input)
  fmt.Println(score)
  fmt.Println(score2)
}
