package main

import AH "./adventhelper"

// b^a mod p
func modPow(b int, a int, p int) (x int) {
	x = 1;

	for ; a > 0; {
		if (a % 2 == 1) {
			a -= 1
			x *= b
			x %= p
		}
		a /= 2
		b *= b
		b %= p
	}

	return
}

// b^a = x mod p
func dicreteLog(b int, x int, p int) (a int) {
	temp := b
	for a = 1; ; a++ {
		if temp == x {
			break
		}
		temp *= b
		temp %= p
	}
	return a
}

func main() {
	is, _ := AH.ReadIntFile("../input/input25.txt")

	AH.PrintSoln(25, modPow(is[0], dicreteLog(7, is[1], 20201227), 20201227),
	                 "See you next year, I love you.")

	return
}