package adventhelper

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func ReadStrFile(fname string) (strs []string, err error) {
	b, err := ioutil.ReadFile(fname)
	if err != nil { return nil, err }

	lines := strings.Split(string(b), "\n")
	for _, l := range lines {
		// Empty line occurs at the end of the file when we use Split.
		if len(l) == 0 { continue }
		strs = append(strs, l)
	}

	return strs, nil
}

// Read a file to an array of integers.
func ReadIntFile(fname string) (nums []int, err error) {
	b, err := ioutil.ReadFile(fname)
	if err != nil { return nil, err }

	lines := strings.Split(string(b), "\n")
	// Assign cap to avoid resize on every append.
	nums = make([]int, 0, len(lines))

	for _, l := range lines {
		// Empty line occurs at the end of the file when we use Split.
		if len(l) == 0 { continue }
		// Atoi better suits the job when we know exactly what we're dealing
		// with. Scanf is the more general option.
		n, err := strconv.Atoi(l)
		if err != nil { return nil, err }
		nums = append(nums, n)
	}

	return nums, nil
}

// Mimics Haskell's filter function
func Filter(test func(interface{}) bool, ss []interface{}) (ret []interface{}) {
		for _, s := range ss {
				if test(s) {
						ret = append(ret, s)
				}
		}
		return
}

// This func must be Exported, Capitalized, and comment added.
// Print the solution in a standardised format.
func PrintSoln(day int, soln1 interface{}, soln2 interface{}) {
	fmt.Println("Day", day)
	fmt.Println("  Part 1:", soln1)
	fmt.Println("  Part 2:", soln2)
}

// a^b
func PowInt(a int, b int) (n int) {
	n = 1
	for i := 0; i < b; i++ {
		n *= a
	}
	return n
}