package main

import Helper "./adventhelper"

func part1(v []int, t int) int {
	for _, i := range v {
		for _, j := range v {
			if i + j == t {
				return i * j
			}			
		}
	}

	return -1
}

func part2(v []int, t int) int {
	for _, i := range v {
		for _, j := range v {
			if (i + j > t) {continue;} // micro optimisation
			for _, k := range v {
				if i + j + k == t {
					return i * j * k
				}
			}
		}		
	}

	return -1
}

func main() {
	ints, _ := Helper.ReadIntFile("../input/input01.txt")

	Helper.PrintSoln(1, part1(ints, 2020), part2(ints, 2020))

	return
}
