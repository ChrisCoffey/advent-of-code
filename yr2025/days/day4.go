package days

import (
	"strings"
  "slices"
)

type Point struct{Y, X int}
const roll = '@'

func SolveDay4Part1(grid []string) int {
  
  result := 0
  for row, str := range(grid) {
    for col, c := range(str) {
      if c == roll && CanMove(grid, Point{row, col})  {
        result++
      }
    }
  }

  return result
}

func SolveDay4Part2(grid []string) int {
  var result []string
  moved := 0
  
  for row, str := range(grid) {
    row_str := ""
    for col, c := range(str) {
      if c == roll && CanMove(grid, Point{row, col}) {
        row_str += string('.') 
        moved++ 
      } else {
        row_str += string(c)
      }
    }
    result = append(result, row_str)
  }

  
  if (!slices.Equal(grid, result)){
    moved += SolveDay4Part2(result)
  }
    return moved
}

func CanMove(grid []string, focusedIndex Point) bool {
  numNeighbors := 0
 
  rows := Filter(
    []int{focusedIndex.Y-1, focusedIndex.Y, focusedIndex.Y+1}, 
    func(x int) bool { return x >= 0 && x < len(grid) },
  ) 
  columns := Filter(
    []int{focusedIndex.X-1, focusedIndex.X, focusedIndex.X+1},
    func(x int) bool { return x >=0 && x < len(grid[0])},
  )

  for _, row := range rows {
    for _, column := range columns {
      if row == focusedIndex.Y && column == focusedIndex.X {
        continue
      }

      if (grid[row][column] == roll) { numNeighbors++}
    }
  }

  return (numNeighbors < 4)
}


func ImportDay4(file string) []string {
  lines := ReadLines(file)
  for i:=0; i< len(lines); i++ {
    lines[i] = strings.TrimSpace(lines[i])
  }
  return lines
}
