package main

import Helper "./adventhelper"

func test(arr []int, offset int, size int) (bool) {
	if len(arr) < offset + size + 1 {
		return false
	}

	for i := 0; i < size; i++ {
		for j := i + 1; j < size; j++ {
			if arr[offset+i] + arr[offset+j] == arr[offset + size] {
				return true
			}
		}		
	}

	return false
}

func findRange(arr []int, target int, blocksize int) ([]int) {
	block := arr[:blocksize]
	s := 0
	for _, i := range block {
		s += i
	}

	if s == target {
		return block
	} else if s > target {
		return findRange(arr[1:], target, 2)
	} else {
		return findRange(arr, target, blocksize+1)
	}
}

func main() {
	is, _ := Helper.ReadIntFile("../input/input09.txt")
	size := 117

	var part1 int
	for i := 0; i < len(is) - size; i++ {
		if !test(is, i, size) {
			part1 = is[i+size]
			break
		}
	}
	max, min := Helper.MaxAndMin(findRange(is, part1, 2))

	Helper.PrintSoln(9, part1, max + min)

	return
}
