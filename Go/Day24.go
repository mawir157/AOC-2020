package main

import AH "./adventhelper"

type Pos struct {
	X, Y int
}

func (pt Pos) nbrs() (ns []Pos) {
	ns = []Pos{Pos{X:pt.X + 1, Y:pt.Y    },
	           Pos{X:pt.X,     Y:pt.Y + 1},
	           Pos{X:pt.X - 1, Y:pt.Y    },
	           Pos{X:pt.X,     Y:pt.Y - 1},
	           Pos{X:pt.X - 1, Y:pt.Y + 1},
	           Pos{X:pt.X + 1, Y:pt.Y - 1}}

	return
}

func parseLine(x0 int, y0 int, s string) (Pos) {
	x1, y1 := x0, y0

	rs := []rune(s)

	for i := 0; i < len(rs); i++ {
		// grab first char
		h := rs[i]
		switch h {
			case rune('e') : x1++
			case rune('w') : x1--
			case rune('n') : {
				i++
				hh := rs[i]
				switch hh {
					case rune('e') : y1++
					case rune('w') : x1--; y1++		
				}	
			}
			case rune('s') : {
				i++
				hh := rs[i]
				switch hh {
					case rune('e') : x1++; y1--
					case rune('w') : y1--
				}
			}
		}
	}

	return Pos{X:x1, Y:y1}
}

func (pt Pos) nbrsCount(space map[Pos]int) (count int) {
	count = 0
	ns := pt.nbrs()
	for _, n := range ns {
		if _, ok := space[n]; ok {
			count++
		}
	}
	return
}

func tick (space map[Pos]int) (newSpace map[Pos]int) {
	newSpace = make(map[Pos]int)

	for pos, _ := range space {
		// check if this cell surives
		nCount := pos.nbrsCount(space) // the number of neighbours
		if nCount == 1 || nCount == 2 {
			newSpace[pos] = 1
		}
		// now check its neighbours
		neighbours := pos.nbrs()
		for _, nPos := range neighbours {
			nCount = nPos.nbrsCount(space) // the number of neighbours
			// if we've already done this point...
			if _, ok := newSpace[nPos]; ok {
				// do nothing..
			} else { // we haven't checked this point yet
				if _, ok := space[nPos]; ok { // Is it currently alive..
					if nCount == 1 || nCount == 2 { //..and has 2 or 3 live neighbours...
						newSpace[nPos] = 1 // ..send it to next generation
					} // otherwise it dies here
				} else { // It is NOT currently alive...
					if nCount == 2 { //..and has exactly 3 live neighbours...
						newSpace[nPos] = 1 // ..send it to next generation
					} // otherwise it dies here
				}
			}
		}
	}

	return
}

func main() {
	ss, _ := AH.ReadStrFile("../input/input24.txt")

	points := make(map[Pos]int)

	for _, s := range ss {
		pt := parseLine(0,0,s)
		if _, ok := points[pt]; ok {
			delete(points, pt)
		} else {
			points[pt] = 1
		}
	}
	part1 := len(points)

	for i := 0; i < 100; i++ {
		points = tick(points)
	}	
	part2 := len(points)

	AH.PrintSoln(24, part1, part2)

	return
}