package main

import Helper "./adventhelper"

type Code int

const (
	Open	Code = iota
	Full
	Void
)

type Pos struct {
	X, Y int
}

var	dirs [8]Pos = [8]Pos{ {X:-1, Y:-1}, {X:-1, Y:0},  {X:-1, Y:1}, {X:0, Y:-1},
                          {X:0, Y:1},   {X: 1, Y:-1}, {X: 1, Y:0}, {X:1, Y:1} }

func BuildSeatMap (ss []string) (seats map[Pos]Code, filled int) {
	seats = make(map[Pos]Code)
	filled = 0
	for i, s := range ss {
		for j, c := range s {
			switch c {
				case '.': seats[Pos{X:i, Y:j}] = Void
				case '#': seats[Pos{X:i, Y:j}] = Full; filled++
				case 'L': seats[Pos{X:i, Y:j}] = Open
			}

		}
	}
	return
}

func LookInDir(seats map[Pos]Code, pos Pos, dir Pos, upto int) (int) {
	for i := 0; i < upto; i++ {
		pos = Pos{pos.X + dir.X, pos.Y + dir.Y}
		if status, ok := seats[pos]; ok {
				switch status {
					case Void: // do nothing
					case Full: return 1
					case Open: return 0
				}
		} else {
			return 0 // we've gone off the end of the grid without seeing a seat
		}
	}
	return 0
}

func Update(seats map[Pos]Code, sight int, threshold int) (newSeats map[Pos]Code, filled int) {
	newSeats = make(map[Pos]Code)	
	filled = 0

	for pos, status := range seats {
		nbrs := 0
		for _, d := range dirs {
				nbrs += LookInDir(seats, pos, d, sight)
		}

		switch status {
			case Void: newSeats[pos] = Void
			case Full: 
				if nbrs >= threshold {
					newSeats[pos] = Open
				} else {
					newSeats[pos] = Full; filled++
				}
			case Open: 
				if nbrs == 0 {
					newSeats[pos] = Full; filled++
				} else {
					newSeats[pos] = Open
				}
		}
	}

	return
}

func UpdateUntilStable(seats map[Pos]Code, prev int, sight int, threshold int) (cur int) {
	var newSeats map[Pos]Code
	newSeats, cur = Update(seats, sight, threshold)

	if cur == prev {
		return
	} else {
		return UpdateUntilStable(newSeats, cur, sight, threshold)
	}
}

func main() {
	js, _ := Helper.ReadStrFile("../input/input11.txt")
	seats, filled := BuildSeatMap(js)

	Helper.PrintSoln(11, UpdateUntilStable(seats, filled, 1, 4),
	                     UpdateUntilStable(seats, filled, 1000, 5))

	return
}
