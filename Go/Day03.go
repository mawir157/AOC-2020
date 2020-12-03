package main

import Helper "./adventhelper"

func tbg(w int, ss []string, dx int, dy int) (trees int) {
	trees = 0;
	y := 0
	for i, s := range ss {
		if (i % dx != 0) {
			continue
		} 

		if s[y:(y+1)] == "#" {
			trees += 1
		}

		y = (y + dy) % w
	}
	return trees
}

func main() {
  mnt, _ := Helper.ReadStrFile("../input/input03.txt")
  w  := len(mnt[0])
  t1 := tbg(w, mnt, 1,1)
  t2 := tbg(w, mnt, 1,3)
  t3 := tbg(w, mnt, 1,5)
  t4 := tbg(w, mnt, 1,7)
  t5 := tbg(w, mnt, 2,1)

  Helper.PrintSoln(3, t2, t1*t2*t3*t4*t5)

  return
}
