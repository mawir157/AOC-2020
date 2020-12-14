package main

import AH "./adventhelper"

func tbg(w int, ss []string, dx int, dy int) (trees int) {
	trees = 0;
	for i, y:= 0, 0; i < len(ss); i, y = (i + dx), ((y + dy) % w) {
		if ss[i][y:(y+1)] == "#" {
			trees += 1
		}
	}

	return
}

func main() {
	mnt, _ := AH.ReadStrFile("../input/input03.txt")
	w  := len(mnt[0])
	t1 := tbg(w, mnt, 1,1)
	t2 := tbg(w, mnt, 1,3)
	t3 := tbg(w, mnt, 1,5)
	t4 := tbg(w, mnt, 1,7)
	t5 := tbg(w, mnt, 2,1)

	AH.PrintSoln(3, t2, t1*t2*t3*t4*t5)

	return
}
