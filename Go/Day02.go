package main

import (
	"strconv"
	"strings"
)

import Helper "./adventhelper"

func CheckString(str string) (ok1 bool, ok2 bool) {
	parts := strings.Split(str, " ")
	lh    := strings.Split(parts[0], "-")
	lo, _ := strconv.Atoi(lh[0])
	hi, _ := strconv.Atoi(lh[1])
	c     := parts[1][:1]
	pw    := parts[2]
	ct    := strings.Count(pw, c)

	return ((ct >= lo) && (ct <= hi)),
	       ((pw[(lo-1):lo] == c) != (pw[(hi-1):hi] == c))
}

func Count(pws []string) (count1 int, count2 int) {
	count1 = 0;
	count2 = 0;
	for _, pw := range pws {
		ok1, ok2 := CheckString(pw)
		if (ok1) {
			count1 += 1
		}
		if (ok2) {
			count2 += 1
		}
	}

	return
}

func main() {
	strs, _ := Helper.ReadStrFile("../input/input02.txt")
	p1, p2 := Count(strs)

	Helper.PrintSoln(2, p1, p2)

	return
}
