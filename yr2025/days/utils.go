package days

import "os"
import "strings"

func ReadLines(path string) []string {
  data, _ := os.ReadFile(path)
  return strings.Split(strings.TrimSpace(string(data)), "\n")
}

func Abs(x int) int {
  if x < 0 { return -x }
  return x
}

func Sign(x int) int {
  if x < 0 { return -1 } 
  return 1
}
