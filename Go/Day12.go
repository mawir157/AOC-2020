package main

import AH "./adventhelper"

import (
	"strconv"
)

type Instruction struct {
	Code   rune
	Value  int
}

type Pos struct {
	X, Y int
}

type Robot struct {
	Locn   Pos
	Dirn   int
	WayPnt Pos
}

func ParseLine (ss string) (i Instruction) {
	r  := AH.FirstRune(ss)
	rs := AH.TrimFirstRune(ss)
	n, _ := strconv.Atoi(rs)

	if r == rune('L') || r == rune('R') {
		n /= 90
	}

	return Instruction{Code: r, Value: n}
}

func (rbt *Robot) Move(ins Instruction) () {
	switch ins.Code {
		case rune('N'): rbt.Locn.Y += ins.Value
		case rune('S'): rbt.Locn.Y -= ins.Value
		case rune('E'): rbt.Locn.X += ins.Value
		case rune('W'): rbt.Locn.X -= ins.Value
		case rune('L'): rbt.Dirn = (rbt.Dirn + ins.Value) % 4
		case rune('R'): rbt.Dirn = (4 + rbt.Dirn - ins.Value) % 4
		case rune('F'): {
			switch rbt.Dirn {
				case 0: rbt.Locn.X += ins.Value
				case 1: rbt.Locn.Y += ins.Value
				case 2: rbt.Locn.X -= ins.Value
				case 3: rbt.Locn.Y -= ins.Value
			}
		}
	}

	return 
}

func (rbt *Robot) MoveAlt(ins Instruction) () {
	switch ins.Code {
		case rune('N'): rbt.WayPnt.Y += ins.Value
		case rune('S'): rbt.WayPnt.Y -= ins.Value
		case rune('E'): rbt.WayPnt.X += ins.Value
		case rune('W'): rbt.WayPnt.X -= ins.Value
		case rune('L'):
			switch ins.Value {
				case 1: rbt.WayPnt.X, rbt.WayPnt.Y = -rbt.WayPnt.Y,  rbt.WayPnt.X
				case 2: rbt.WayPnt.X, rbt.WayPnt.Y = -rbt.WayPnt.X, -rbt.WayPnt.Y
				case 3: rbt.WayPnt.X, rbt.WayPnt.Y =  rbt.WayPnt.Y, -rbt.WayPnt.X
			}			
		case rune('R'):
			switch ins.Value {
				case 1: rbt.WayPnt.X, rbt.WayPnt.Y =  rbt.WayPnt.Y, -rbt.WayPnt.X
				case 2: rbt.WayPnt.X, rbt.WayPnt.Y = -rbt.WayPnt.X, -rbt.WayPnt.Y
				case 3: rbt.WayPnt.X, rbt.WayPnt.Y = -rbt.WayPnt.Y,  rbt.WayPnt.X
			}			
		case rune('F'):
			rbt.Locn.X += ins.Value * rbt.WayPnt.X
			rbt.Locn.Y += ins.Value * rbt.WayPnt.Y
	}

	return 
}

func (rbt Robot) ManDis() (distance int) {
	return AH.AbsInt(rbt.Locn.X) + AH.AbsInt(rbt.Locn.Y)
}

func main() {
	ss, _ := AH.ReadStrFile("../input/input12.txt")

	var is []Instruction
	for _, s := range ss {
		is = append(is, ParseLine(s))
	}

	starbug := Robot{Locn: Pos{X:0, Y:0}, Dirn: 0}
	for _, ins := range is {
		starbug.Move(ins)
	}

	lexx := Robot{Locn: Pos{X:0, Y:0}, Dirn: 0, WayPnt: Pos{X:10, Y:1}}
	for _, ins := range is {
		lexx.MoveAlt(ins)
	}

	AH.PrintSoln(12, starbug.ManDis(), lexx.ManDis())

	return
}
