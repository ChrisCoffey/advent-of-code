package days

import "strings"
import "fmt"
import "math"
import "slices"

type BatterySequence struct {
  batteries []int  
}

type BatteryPair struct  {
  left int
  right int
}

func (bp BatteryPair) setLeft (nums []int) (BatteryPair, int) {
  // Start with 2nd from the last so there's always _some_ possibility of a 2-digit number
  idx := len(nums) - 2
  for i := len(nums) - 2; i >= 0; i-- {
    if(nums[i] >= nums[idx]) {
      idx = i
    }
  }

  bp.left = nums[idx]
  return bp, idx
}

func (bp BatteryPair) setRight (nums []int) (BatteryPair, int) {
  idx := 0
  
  for i, n := range nums {
    if nums[idx] <= n {
      idx = i
    }
  }

  bp.right = nums[idx]
  return bp, idx
}

func ImportDay3(file string) []BatterySequence {
  lines := ReadLines(file)  
  
  result := make([]BatterySequence, len(lines))
  for li, line := range lines {
    cleaned := strings.TrimSpace(line)
    nums := make([]int, len(cleaned)) 
    for i, c := range cleaned {
      nums[i] = int(c - '0')
    }

    result[li] = BatterySequence { batteries:  nums }
  }

  return result
}

// NOTE: debug the indexing logic in this code, and the setLeft/setRight
// The idea is to scan right -> left first, and find the largest value. 

// 16888 is too low. In looking at the results from a single example, it's apparent
// that the code is not allowing duplicates of the same value. It should be <=
func SolveDay3(sequences []BatterySequence) int {
  score := 0
  for _, bseq := range sequences {
    bp := BatteryPair {left: -1, right: -1}
    bp2, n := bp.setLeft(bseq.batteries)
    bp3, _ := bp2.setRight(bseq.batteries[n+1:])
    score += bp3.Score()
    fmt.Println(bp3)
  }
  return score
}

func (bp BatteryPair) Score() int {
  return (bp.left*10) + bp.right
}

func LargestJoltage(size int, seq []int) int {
  if size == 1 { return slices.Max(seq) }
  // of the leftmost (seq.length - size) elements
  // choose the largest (note it's index)
  // Add that number * 10^(size -1) to `joltage`
  // Recursively call LargestJoltage until size = 0
  score := 0 
  idx := 0
  fmt.Println(size, seq);
  for i, n := range seq[0:len(seq) - size+1] {
    if n > score {
      score = n
      idx = i
    } 
  }
  score*= int(math.Pow(10, float64(size-1)))
  score+= LargestJoltage(size -1, seq[idx+1:])

  return score
}

func SolveDay3Part2(sequences []BatterySequence) int {
  score := 0
  for _, bseq := range sequences {
    joltage := LargestJoltage(12, bseq.batteries)
    score += joltage
    fmt.Println("Joltage: ", joltage, " Score: ", score)
  }
  return score
}
