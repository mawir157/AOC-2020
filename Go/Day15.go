package main

import AH "./adventhelper"

import (

)

func MakeGameState(inputs []int) (gameState map[int]int) {
	gameState = make(map[int]int)
	for i, v :=	range inputs {
		gameState[v] = i + 1
	}
	return
}

func playGame(game map[int]int, n int, seen bool, turns int, offset int) (next int) {
	for turn := offset; turn <= turns; turn++ {
		if seen {
 			next = turn - game[n] - 1
		} else { // if not say zero
			next = 0
		}
		_, seen = game[next]
		game[n] = turn - 1
		n = next
	}

	return
}

func main() {
	input15 := []int{7,14,0,17,11,1,2}

	q := MakeGameState(input15)
	init := 0
	if _, ok := q[0]; ok {
		init = input15[len(input15) - 1]
	}
	s := playGame(q, init, false, 2020, len(input15) + 1)

	q = MakeGameState(input15) // need to reset the map
	t := playGame(q, init, false, 30000000, len(input15) + 1)

	AH.PrintSoln(15, s, t)

	return
}
