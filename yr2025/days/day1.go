package days

import "strconv"

func SolveDay1(turns []Turn) int {
  currentFocus := 50
  numZeros := 0
  
  var tinyTurns []Turn
  for _, turn := range(turns) {
    innerTurns := expandTurn(turn)
    for _, iturn := range(innerTurns) { tinyTurns = append(tinyTurns, iturn) }
  }

  for _, turn := range(tinyTurns) {
    currentFocus = do_turn(currentFocus, turn)
    if currentFocus == 0 {
      numZeros++
    }
  }

  return numZeros
}



type Turn struct {
  amount int
}

func do_turn(state int, t Turn) int {
  intermediate := state + t.amount
  x := ((intermediate % 100) + 100) % 100

  return x
}

func ImportDay1(file string) []Turn {
  lines := ReadLines(file)

  result := make([]Turn, len(lines))
  for i, line := range lines {
    result[i] = to_turn(line)
  }

  return result
}

func to_turn(line string) Turn {
  var signum int
  if line[0] == 'L' {
    signum = -1
  } else {
    signum = 1
  }
 
  n, err := strconv.Atoi(line[1:])
  if err != nil {
    n = -10000
  }

  return Turn{ signum * n }
}

func expandTurn(turn Turn) []Turn {
  sign := Sign(turn.amount)
  result := make([]Turn, Abs(turn.amount))
  for i := 0; i < Abs(turn.amount); i++ {
    result[i] = Turn{ 1 * sign }
  }
  
  return result
}
