package main

import AH "./adventhelper"

type Pos struct {
	X, Y, Z, T int
}

func (pt Pos) nbrs(dimn int) (ns []Pos) {
	ns = []Pos{}

	for x := -1; x < 2; x++ {
		for y := -1; y < 2; y++ {
			for z := -1; z < 2; z++ {
				if dimn == 3 {
					if x == 0 && y == 0 && z == 0 {
						continue
					}
					ns = append(ns, Pos{X:pt.X + x, Y:pt.Y + y, Z:pt.Z + z})
				}
				if dimn == 4 {
					for t := -1; t < 2; t++ {
						if x == 0 && y == 0 && z == 0 && t == 0 {
							continue
						}
						ns = append(ns, Pos{X:pt.X + x, Y:pt.Y + y,
						                    Z:pt.Z + z, T:pt.T + t})
					}
				}
			}
		}
	}

	return
}

func (pt Pos) nbrsCount(space map[Pos]int, dimn int) (count int) {
	count = 0
	ns := pt.nbrs(dimn)
	for _, n := range ns {
		if _, ok := space[n]; ok {
			count++
		}
	}
	return
}

func BuildNSpace (ss []string, dimn int) (space map[Pos]int) {
	space = make(map[Pos]int)
	for i, s := range ss {
		for j, c := range s {
			if c == '#' {
				if dimn	 == 3 {
					space[Pos{X:i, Y:j, Z:0}] = 1
				} else if dimn == 4 {
					space[Pos{X:i, Y:j, Z:0, T:0}] = 1
				}
			}
		}
	}
	return
}

func tick (space map[Pos]int, dimn int) (newSpace map[Pos]int) {
	newSpace = make(map[Pos]int)

	for pos, _ := range space {
		// check if this cell surives
		nCount := pos.nbrsCount(space, dimn) // the number of neighbours
		if nCount == 2 || nCount == 3 {
			newSpace[pos] = 1
		}
		// now check its neighbours
		neighbours := pos.nbrs(dimn)
		for _, nPos := range neighbours {
			nCount = nPos.nbrsCount(space, dimn) // the number of neighbours
			// if we've already done this point...
			if _, ok := newSpace[nPos]; ok {
				// do nothing..
			} else { // we haven't checked this point yet
				if _, ok := space[nPos]; ok { // Is it currently alive..
					if nCount == 2 || nCount == 3 { //..and has 2 or 3 live neighbours...
						newSpace[nPos] = 1 // ..send it to next generation
					} // otherwise it dies here
				} else { // It is NOT currently alive...
					if nCount == 3 { //..and has exactly 3 live neighbours...
						newSpace[nPos] = 1 // ..send it to next generation
					} // otherwise it dies here
				}
			}
		}
	}

	return
}

func main() {
	ss, _ := AH.ReadStrFile("../input/input17.txt")
	space3 := BuildNSpace (ss, 3)
	space4 := BuildNSpace (ss, 4)

	for i := 0; i < 6; i++ {
		space3 = tick(space3, 3)
		space4 = tick(space4, 4)
	}

	AH.PrintSoln(17, len(space3), len(space4))

	return
}
