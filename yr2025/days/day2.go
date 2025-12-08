package days

import "strconv"
import "strings"
import "os"


func SolveDay2(ranges []ProductRange) (int, int) {
  score := 0
  for _, r := range(ranges) {
    for _, i := range(findInvalidIds(r)) {
      score += i
    }
  }

  score2 := 0
  for _, r := range(ranges) {
    for _, i := range(findInvalidIdsPart2(r)) {
      score2 += i
    }
  }

  return score, score2
}

func findInvalidIds(rng ProductRange) []int {
  l, r := rng.ToInts()
  
  var invalidIds []int
  for i := l; i <= r; i++ {
    s := strconv.Itoa(i)
    // Because the invalid Ids always result in a pattern repeated twice, 
    // there must be an even number of digits
    n := len(s)
    //fmt.Println(l, r, i, n%2 == 0, s[0:n/2], s[n/2:], n%2 ==0 && s[0:n/2] == s[n/2:])
    if n%2 ==0 && s[0:n/2] == s[n/2:] {
     // fmt.Println(i)
      invalidIds = append(invalidIds, i)
    }
  }

  return invalidIds
}

func findInvalidIdsPart2(rng ProductRange) []int {
  l, r := rng.ToInts()

  var invalidIds []int
  for i := l; i<= r; i++ {
    s := strconv.Itoa(i)
    doubled := s+s
    if strings.Contains(doubled[1:len(doubled)-1], s) {
      invalidIds = append(invalidIds, i)
    }
  }
  
  return invalidIds
}

type ProductRange struct {
  left string
  right string
}

func (pr ProductRange) ToInts() (int, int) {
  l, _ := strconv.Atoi(pr.left)
  r, _ := strconv.Atoi(pr.right)

  return l, r
}

func ImportDay2(file string) []ProductRange {
  data, _ := os.ReadFile(file)
  rawRanges := strings.Split(strings.TrimSpace(string(data)), ",")

  var result []ProductRange
  for _, r := range(rawRanges) {
    result = append(result, toRange(r))
  }
  return result
}

func toRange(str string) ProductRange {
  parts := strings.Split(str, "-") 
  return ProductRange{ 
    left: parts[0], 
    right: parts[1],
  }
}
