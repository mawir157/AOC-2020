package main

import (
	"sort"
	"strings"
)

import Helper "./adventhelper"

func binSearch(v string, s string, l int) (n int) {
	n = 0
	vv := strings.Split(v, "")
	for i, c := range s {
			if (string(c) == vv[1]) {
	    	n += Helper.PowInt(2, l - 1 - i)
	  	}
	}
	return n
}

func getSeat(s string) (n int) {
	return (8 * binSearch("FB", s[:7], 7)) + binSearch("LR", s[7:], 3)
}

func missing(is []int) (n int) {
	for i := 0; i < len(is) - 1; i++ {
		if (is[i] + 1) != is[i+1] {
			return is[i] + 1
		}
	}
	return -1
}

func main() {
	ss, _ := Helper.ReadStrFile("../input/input05.txt")
	var seats []int
	for _,s := range(ss) {
		seats = append(seats, getSeat(s))
	}
	sort.Ints(seats[:])

	Helper.PrintSoln(5, seats[len(seats) - 1], missing(seats))

	return
}
