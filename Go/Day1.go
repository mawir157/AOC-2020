package main

import (  
    "fmt"
    Helper "./adventhelper"
)

func part1(values []int, target int) int {
	for i := 0; i < len(values); i++ {
		for j := 0; j < len(values); j++ {
			if values[i] + values[j] == target {
				return values[i] * values[j]
			}
		}		
	}
	return -1
}

func part2(values []int, target int) int {
	for i := 0; i < len(values); i++ {
		for j := 0; j < len(values); j++ {
			for k := 0; k < len(values); k++ {
				if values[i] + values[j] + values[k] == target {
					return values[i] * values[j] * values[k]
				}
			}
		}		
	}
	return -1
}

func main() {
	ints, err := Helper.ReadIntFile("../input/input01.txt")

  if err != nil {
    fmt.Println("File reading error", err)
    return
  }

	fmt.Println("Day 1")
	Helper.PrintSoln(1, part1(ints, 2020))
	Helper.PrintSoln(2, part2(ints, 2020))

	return
}