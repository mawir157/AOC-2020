package main

import AH "./adventhelper"

import (
	"strconv"
)

func playHand(player1 []int, player2 []int) ([]int, []int) {
	if len(player1) == 0 || len(player2) == 0 {
		return player1, player2
	}

	if player1[0] > player2[0] {
		return append(player1[1:], player1[0], player2[0]), player2[1:]
	} else {
		return player1[1:], append(player2[1:], player2[0], player1[0])
	}
}

func playGame(player1 []int, player2 []int) ([]int, []int) {
	if len(player1) == 0 || len(player2) == 0 {
		return player1, player2
	}

	new1, new2 := playHand(player1, player2)
	return playGame(new1, new2)
}

func score(deck []int) (total int) {
	m := len(deck)
	total = 0
	for i, v := range deck {
		total += (m-i) * v
	}
	return
}

func playHand2(deck1 []int, deck2 []int) ([]int, []int) {
	if len(deck1) == 0 || len(deck2) == 0 {
		return deck1, deck2
	}

	c1, c2 := deck1[0], deck2[0]

	if c1 < len(deck1) && c2 < len(deck2) { // we are playing a sub game
		// there is some shared memory shenanigans that I don't fully understand
		// so we are going to make a copy of the slices to avoid the unpleasantness
		subDeck1 := make([]int, c1)
		subDeck2 := make([]int, c2)
		copy(subDeck1, deck1[1:(c1+1)])
		copy(subDeck2, deck2[1:(c2+1)])
		_, _, p1Wins := playGame2(subDeck1, subDeck2)
		if (p1Wins) {
			return append(deck1[1:], deck1[0], deck2[0]), deck2[1:]
		} else {
			return deck1[1:], append(deck2[1:], deck2[0], deck1[0])
		}
	} else {
		if c1 > c2 {
			return append(deck1[1:], deck1[0], deck2[0]), deck2[1:]
		} else {
			return deck1[1:], append(deck2[1:], deck2[0], deck1[0])
		}
	}
}

func seenBefore(cur []int, prev [][]int) bool {
	for _, arr := range prev {
		if len(cur) != len(arr) {
			continue
		}
		ok := true
		for i := 0; i < len(cur); i++ {
			if cur[i] != arr[i] {
				ok = false
				break
			}
		}
		if ok {
			return true
		}
	}
	return false
}

func playGame2(deck1 []int, deck2 []int) ([]int, []int, bool) {
	prevDeck1 := [][]int{}
	prevDeck2 := [][]int{}
	for {
		if len(deck1) == 0 || len(deck2) == 0 {
			return deck1, deck2, (len(deck2) == 0)
		}

		if seenBefore(deck1, prevDeck1) && seenBefore(deck2, prevDeck2) {
			return deck1, deck2, true
		}

		prevDeck1 = append(prevDeck1, deck1)
		prevDeck2 = append(prevDeck2, deck2)

		deck1, deck2 = playHand2(deck1, deck2)
	}
}

func main() {
	ss, _ := AH.ReadStrFile("../input/input22.txt")
	player1 := false
	deck1 := []int{}
	deck2 := []int{}
	for _, s := range ss {
		if AH.FirstRune(s) == rune('P') {
			player1 = !player1
			continue
		}

		n, _ := strconv.Atoi(s)

		if player1 {
			deck1 = append(deck1, n)
		} else {
			deck2 = append(deck2, n)
		}
	}

	newDeck1, newDeck2 := playGame(deck1, deck2)
	part1 := 0
	if (len(newDeck1) > 0) {
		part1 = score(newDeck1)
	} else {
		part1 = score(newDeck2)
	}

	newDeck1, newDeck2, _ = playGame2(deck1, deck2)
	part2 := 0
	if (len(newDeck1) > 0) {
		part2 = score(newDeck1)
	} else {
		part2 = score(newDeck2)
	}

	AH.PrintSoln(22, part1, part2)

	return
}
