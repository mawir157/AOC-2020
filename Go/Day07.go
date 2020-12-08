package main

import Helper "./adventhelper"

import (
	"strconv"
	"strings"
)

func tbg(w int, ss []string, dx int, dy int) (trees int) {
	trees = 0;
	for i, y:= 0, 0; i < len(ss); i, y = (i + dx), ((y + dy) % w) {
		if ss[i][y:(y+1)] == "#" {
			trees += 1
		}
	}

	return
}

type Pair struct {
	Name     string
	Quantity int
}

type Bag struct {
	Name string;
	Contents []Pair;
}

func parseLine(s string) (b Bag) {
	parts := strings.Split(s, " contain")
	parents := strings.Split(parts[0], " ")
	parent := parents[0] + " " + parents[1]

	children := strings.Split(parts[1], ",")
	cs := make([]Pair, 0, len(children))
	for _, c := range children {
		temp := strings.Split(strings.TrimSpace(c), " ")
		n, _ := strconv.Atoi(temp[0])
		c_name := temp[1] + " " + temp[2]

		p := Pair{Name: c_name, Quantity: n}
		cs = append(cs, p)
	}

	b = Bag{Name: parent, Contents: cs}

	return b;
}

func bagHas(b Bag, name string) (bool) {
	for _, p := range b.Contents {
		if p.Name == name{
			return true
		}
	}
	return false
}
////////////////////////////////////////////////////////////////////////////////
func part1(bs []Bag, targets []string) (int) {
	parents := make([]string, 0, len(bs))
	temp := targets
	l := 1

	for l > 0 {
		up := mapUpstream(bs, temp)

		for _, e := range up {
			if !Helper.ContainsStr(parents, e) {
				parents = append(parents, e)
			}
		}

		l = len(up)
		temp = up
	}

	return len(parents)
}

func mapUpstream(bs []Bag, targets []string) ([]string) {
	contains := make([]string, 0, len(bs))

	for _, t := range targets {
		temp := upstream(bs, t)
		for _, e := range temp {
			if !Helper.ContainsStr(contains, e) {
				contains = append(contains, e)
			}
		}
	}
	return contains
}

func upstream(bs []Bag, target string) ([]string) {
	contains := make([]string, 0, len(bs))

	for _, b := range bs {
		if bagHas(b, target) {
			contains = append(contains, b.Name)
		}
	}
	return contains
}
////////////////////////////////////////////////////////////////////////////////
func collapse(bags []Bag, counter int, top Bag) (newCounter int, new Bag) {
	// iterate through the set of all bags
	newContents := make([]Pair, 0)
	for _,b := range bags {
		// b is bag
		// does this bag have the same name as a bag in top?
		for _, j := range top.Contents {
			// j =  {Colour, Count}
			// b =  {Colour, []Pair}
			if b.Name == j.Name { // we need to replace j with j.Quantity * b.Contents
				counter += j.Quantity
				// fmt.Println("Expanding" , j.Quantity, j.Name, "bags into")
				for _,k := range b.Contents {
					// fmt.Println("\t" , k.Quantity, k.Name, "bags")
					tempPair := Pair{Name: k.Name, Quantity: (j.Quantity * k.Quantity)}
					newContents = append(newContents, tempPair)
				}
			}
		}
	}

	return counter, Bag{Name: top.Name, Contents:newContents}
}

func part2(bags []Bag, counter int, top Bag) (newCounter int) {
	for len(top.Contents) > 0 {
		counter, top = collapse(bags, counter, top)
	}

	return counter
}

func main() {
	ss, _ := Helper.ReadStrFile("../input/input07.txt")

	bags := make([]Bag, 0, len(ss))
	var sg Bag
	for _,s := range ss {
		q := parseLine(s)
		bags = append(bags, q)
		if q.Name == "shiny gold" {
			sg = q
		}
	}

	Helper.PrintSoln(7, part1(bags, []string{"shiny gold"}),
	                                part2(bags, 0, sg))

	return
}