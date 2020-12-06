package main

import (
	"strings"
)

import Helper "./adventhelper"

func eachAnswer(ss string, split string) (n int, n2 int) {
	count := make(map[string]int)
	lines := strings.Split(ss, split)

	for _, l := range lines {
		for _, c := range l {
			count[string(c)] += 1
		}
	}

	n2 = 0
	for _, v := range count {
		if v == len(lines) {
			n2 += 1
		}
	}

	return len(count), n2
}

func main() {
	ss, _ := Helper.ParseLineGroups("../input/input06.txt", ":")

	part1 := 0
	part2 := 0
	for _, s := range ss {
		v1, v2 := eachAnswer(s, ":")
		part1 += v1
		part2 += v2
	}


	Helper.PrintSoln(6, part1, part2)

	return
}
