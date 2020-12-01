package adventhelper

import (  
    "fmt"
    "io/ioutil"
    "strconv"
    "strings"
)

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

// This func must be Exported, Capitalized, and comment added.
// Print the solution in a standardised format.
func PrintSoln(part int, soln interface{}) {
	fmt.Println("Part", part, ":", soln)
}