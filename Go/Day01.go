package main

import AH "./adventhelper"

func part1(v []int, t int) int {
	for r, i := range v {
		for _, j := range v[r:] {
			if i + j == t {
				return i * j
			}			
		}
	}

	return -1
}

func part2(v []int, t int) int {
	for r, i := range v {
		for s, j := range v[r:] {
			if (i + j > t) {
				continue;
			}
			for _, k := range v[s:] {
				if i + j + k == t {
					return i * j * k
				}
			}
		}		
	}

	return -1
}

func main() {
	ints, _ := AH.ReadIntFile("../input/input01.txt")

	AH.PrintSoln(1, part1(ints, 2020), part2(ints, 2020))

	return
}
