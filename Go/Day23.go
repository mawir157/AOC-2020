package main

import AH "./adventhelper"

import (
 "strconv"
)

type Game struct {
	Cups   map[int]int
	Ptr    int
	MaxElm int
}

func buildGame(size int, input []int) (map[int]int) {
	game := make(map[int]int)

	if size == len(input) {
		for i := 0; i < len(input); i++ {
			game[input[i]] = input[(i+1) % len(input)]
		}
	} else {
		for i := 0; i < len(input) - 1; i++ {
			game[input[i]] = input[(i+1)]
		}
		game[input[len(input) - 1]] = len(input) + 1
		for i := len(input); i < size; i++ {
			game[i+1] = i + 2
		} 
		game[size] = input[0]
	}

	return game
}

func (game *Game) tick() () {
	nbr1 := game.Cups[game.Ptr]
	nbr2 := game.Cups[nbr1]
	nbr3 := game.Cups[nbr2]
	nbr4 := game.Cups[nbr3]

	dest := game.Ptr - 1
	if dest <= 0 {
		dest = game.MaxElm
	}
	for ; dest == nbr1 || dest == nbr2 || dest == nbr3; {
		dest--
		if dest <= 0 {
			dest = game.MaxElm
		}
	}

	afterDest := game.Cups[dest]

	game.Cups[game.Ptr]  = nbr4
	game.Cups[dest]      = nbr1
	game.Cups[nbr3]      = afterDest

	game.Ptr = nbr4
}

func (game *Game) print(start int) (str string) {
	str = ""
	for i := start; ;  {
		temp := game.Cups[i]

		if temp == start {
			break
		}
		tempStr := strconv.Itoa(temp)
		str = str + tempStr
		i = temp
	}	
	return
}

func (game *Game) part2(start int) (int) {
	nbr1 := game.Cups[start]
	nbr2 := game.Cups[nbr1]

	return nbr1 * nbr2
}

func main() {
	input := []int{5,2,3,7,6,4,8,1,9}
	cups1 := buildGame(9, input)
	game1 := Game{Cups:cups1, Ptr:input[0], MaxElm:9}

	cups2 := buildGame(1000000, input)
	game2 := Game{Cups:cups2, Ptr:input[0], MaxElm:1000000}	

	for i := 0; i < 100; i++ {
		game1.tick()
	}

	for i := 0; i < 10000000; i++ {
		game2.tick()
	}

	AH.PrintSoln(23, game1.print(1), game2.part2(1))

	return
}
