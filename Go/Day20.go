package main

import AH "./adventhelper"

import (
	"regexp"
	"strconv"
)

type Tile struct {
	Id       int
	Pattern  []string
	NbrIds   []int
}

type Pos struct {
	X, Y     int
}

type Dir int

const(
	Top Dir = iota
	Bottom
	Left
	Right
)

func opp(d Dir) Dir {
	switch d {
		case Top: return Bottom
		case Bottom: return Top
		case Left: return Right
		case Right: return Left
	}
	return Top
}

func (t Tile) side(d Dir) string {
	switch d {
		case Top: return t.Pattern[0]
		case Bottom: return t.Pattern[len(t.Pattern) - 1]
		case Left: {
			side := ""
			for _, s := range t.Pattern {
				side = side + string(AH.FirstRune(s))
			}
			return side
		}
		case Right: {
			side := ""
			for _, s := range t.Pattern {
				side = side + AH.FinalRune(s)
			}
			return side
		}
	}
	return "ERROR!"
}

func rotateArray(ss []string) []string {
	newArray := []string{}
	for i := 0; i < len(ss); i++ {
		newLine := ""
		for _,s := range ss {
			newLine = newLine + s[i:i+1]
		}
		newLine = AH.ReverseString(newLine)
		newArray = append(newArray, newLine)
	}
	return newArray
}

func flipArray(ss []string) []string {
	newArray := []string{}
	for i := 0; i < len(ss); i++ {
		newArray = append(newArray, AH.ReverseString(ss[i]))
	}
	return newArray
}

// clockwise!
func (t* Tile) rotate() {
	newPattern := rotateArray(t.Pattern)
	t.Pattern = newPattern
}

func (t* Tile) flip() {
	newPattern := flipArray(t.Pattern)
	t.Pattern = newPattern
}

func parseInput(ss []string) map[int]Tile {
	// tiles := []Tile{}
	tiles := make(map[int]Tile)

	for i := 0; i < len(ss); i++ {
		header := AH.TrimLastRune(AH.Drop(ss[i], 5))

		n, _ := strconv.Atoi(header)

		pattern := []string{}
		for j := 0; j < 10; j++ {
			i++
			pattern = append(pattern, ss[i])
		}

		tiles[n] = Tile{Id:n, Pattern:pattern}
	}
	return tiles
}

func countSingle(tile1 *Tile, tile2 Tile, d Dir) (count int, piece Tile) {
	count = 0
	toMatch := tile1.side(d)
	dr := opp(d)

	for i := 0; i < 4; i++ {
		if tile2.side(dr) == toMatch {
			return 1, tile2
		}
		tile2.rotate()
	}
	tile2.flip()
	for i := 0; i < 4; i++ {
		if tile2.side(dr) == toMatch {
			return 1, tile2
		}
		tile2.rotate()
	}
	return 0, tile2
}

func countNbrs(allTiles map[int]Tile, tile *Tile, add bool) (u int, l int, b int, r int) {
	u, l, b, r = 0, 0, 0, 0
	for id, t := range allTiles {
		if tile.Id == id { // a tile connot neighbour itself
			continue
		}

		u1, _ := countSingle(tile, t, Top)
		if u1 > 0 {
			u += u1
			if add {
				tile.NbrIds = append(tile.NbrIds, id)
			}
		}

		l1, _ := countSingle(tile, t, Left)
		if l1 > 0 {
			l += l1
			if add {
				tile.NbrIds = append(tile.NbrIds, id)
			}
		}

		b1, _ := countSingle(tile, t, Bottom)
		if b1 > 0 {
			b += b1
			if add {
				tile.NbrIds = append(tile.NbrIds, id)
			}
		}

		r1, _ := countSingle(tile, t, Right)
		if r1 > 0 {
			r += r1
			if add {
				tile.NbrIds = append(tile.NbrIds, id)
			}
		}
	}
	return
}

func permutateToTopLeft (allTiles map[int]Tile, tile *Tile)  {
	for i := 0; i < 4; i++ {
		u,l,b,r := countNbrs(allTiles, tile, false)
		if u == 0 && l == 0 && b == 1 && r == 1 {
			return
		}
		tile.rotate()
	}
	tile.flip()
	for i := 0; i < 4; i++ {
		u,l,b,r := countNbrs(allTiles, tile, false)
		if u == 0 && l == 0 && b == 1 && r == 1 {
			return
		}
	}
	return
}

func fillRowLR(grid map[Pos]Tile, tiles map[int]Tile, rowN int) {
	leftPiece, _ := grid[Pos{X:0, Y:rowN}]
	for i := 1; i < 12; i++ {
		for _, k := range leftPiece.NbrIds {
			t := tiles[k]


			hit, tile := countSingle(&leftPiece, t, Right)
			temp := tile
			if (hit == 1) {
				grid[Pos{X:i, Y:rowN}] = temp
				leftPiece = tile
				break
			}
		}
	}
	return
}

func fillColTB(grid map[Pos]Tile, tiles map[int]Tile, colN int) {
	abovePiece, _  := grid[Pos{X:colN, Y:0}]
	for i := 1; i < 12; i++ {
		for _, k := range abovePiece.NbrIds {
			t := tiles[k]

			hit, tile := countSingle(&abovePiece, t, Bottom)
			temp := tile
			if (hit == 1) {
				grid[Pos{X:colN, Y:i}] = temp
				abovePiece = tile
				break
			}
		}
	}
	return
}

func completePuzzle(grid map[Pos]Tile, tiles map[int]Tile) {
	fillRowLR(grid, tiles, 0)
	fillColTB(grid, tiles, 0)

	for i := 1; i < 12; i++ {
		fillRowLR(grid, tiles, i)
	}	
}

func trimEdgesAndGlue(grid map[Pos]Tile) (ss []string) {
	for r := 0; r < 12; r++ {
		s8 := []string{"","","","","","","",""}
		for c := 0; c < 12; c++ {
			t,_ := grid[Pos{X:c, Y:r}]
			for i := 0; i < 8; i++ {
				s8[i] = s8[i] + AH.TrimLastRune(AH.TrimFirstRune(t.Pattern[i + 1]))
			}
		}
		for _, s := range s8 {
				ss = append(ss,s)	
		}
	}
	return
}

func countRough(ss []string) (rough int) {
	rough = 0
	for _, s := range ss {
		for _, r := range s {
			if r == rune('#') {
				rough++
			}
		}
	}
	return
}

func dragonAt(regex string, ss []string, r int, c int) bool {
	test := AH.Take(AH.Drop(ss[r], c), 20)
	ok, _ := regexp.Match(regex, []byte(test))

	return ok
}

func dragonHunt(ss []string) (dragons int) {
	dragons = 0
	for r := 0; r < len(ss)-2; r++ {
		for c := 0; c < len(ss)-22; c++ {
			if (dragonAt(".#.{2}#.{2}#.{2}#.{2}#.{2}#.{3}" , ss, r+2, c)) {
				if (dragonAt("#.{4}##.{4}##.{4}###" , ss, r+1, c)) {
					if (dragonAt(".{18}#", ss, r, c)) {
						dragons++
					}				
				}
			}
		}
	}

	return
}

func part2(ss []string) int {
	dragonCounts := []int{
		dragonHunt(ss),
		dragonHunt(rotateArray(ss)),
		dragonHunt(rotateArray(rotateArray(ss))),
		dragonHunt(rotateArray(rotateArray(rotateArray(ss)))),
		dragonHunt(flipArray(ss)),
		dragonHunt(flipArray(rotateArray(ss))),
		dragonHunt(flipArray(rotateArray(rotateArray(ss)))),
		dragonHunt(flipArray(rotateArray(rotateArray(rotateArray(ss))))),
	}
	m, _ := AH.MaxAndMin(dragonCounts)

	return countRough(ss) - 15 * m
}

func main() {
	ss, _ := AH.ReadStrFile("../input/input20.txt")
	ts := parseInput(ss)

	part1 := 1
	topLeft := Tile{}

	for i, t := range ts {
		temp := t
		u,l,b,r := countNbrs(ts, &temp, true)
		ts[i] = temp
		if u+l+b+r == 2 {
			part1 *= i
			topLeft = ts[i]
		}
	}
	// fmt.Println(topLeft)
	// fmt.Println(ts[3793])

	// rotate topLeft so that it only has neighnours below and to the right...
	permutateToTopLeft(ts, &topLeft)
	// ...and insert it into the puzzle
	puzzle := make(map[Pos]Tile)
	puzzle[Pos{X:0, Y:0}] = topLeft
	// fill in the puzzle
	completePuzzle(puzzle, ts)
	completed := trimEdgesAndGlue(puzzle)

	AH.PrintSoln(20, part1, part2(completed))

	return
}
