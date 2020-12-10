package main

import Helper "./adventhelper"

import (
	"sort"
)

func Jumps(jolts []int) (ones int, threes int) {
	ones, threes = 0, 1 // we know the final jump will be three

	for i := 0; i < len(jolts) - 1; i++ {
		if jolts[i] - jolts[i+1] == 1 {
			ones++
		} else if jolts[i] - jolts[i+1] == 3 {
			threes++
		}
	}

	return 
}

func RoutesTo(jolts []int) (routes int) {
	seen := make(map[int]int)
	seen[jolts[0]] = 1 // there is always just one final step

	for i := 1; i < len(jolts); i++ {
		t := 0
		for j := 0; j < 4; j++ {
			t += seen[jolts[i] + j]
		}
		seen[jolts[i]] = t
	}

	return seen[0]
}

func main() {
	js, _ := Helper.ReadIntFile("../input/input10.txt")
	sort.Sort(sort.Reverse(sort.IntSlice(js))) // this is the way
	js = append(js, 0)
	ones, threes := Jumps(js)

	Helper.PrintSoln(10, ones*threes, RoutesTo(js))

	return
}
