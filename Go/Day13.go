package main

import AH "./adventhelper"

import (
	"strconv"
	"strings"
)

type Pair struct {
	A, P int
}

func ParseLine (ss string) (pairs []Pair) {
	parts := strings.Split(ss, ",") 

	for i, s := range parts {
		if (s != "x") {
			p, _ := strconv.Atoi(s)
			pairs = append(pairs, Pair{A: i, P:p})
		}
	}

	return
}

func part1(now int, pairs []Pair) (int) {
	best := Pair{A:1000, P:0}
	for _, pair := range pairs {
		m := pair.P - (now % pair.P) 
		if m < best.A {
			best = Pair{A:m, P:pair.P}
		}
	}

	return (best.A * best.P)
}

func ChiRemThm(p1 Pair, p2 Pair) (Pair) {
	for n := 1; n <= p2.P; n++ {
		c := p1.A + n * p1.P
		if c % p2.P == p2.A % p2.P {
			return Pair{A:c, P:(p1.P*p2.P)}
		}
	}

	return Pair{A:0, P:1} // can never be hit but we can't compile without it!
}

func part2(pairs []Pair) (int) {
	out := Pair{A:0, P:1}
	for _, pair := range pairs {
		out = ChiRemThm(out, pair)
	}

	return out.P - out.A
}

func main() {
	ss, _ := AH.ReadStrFile("../input/input13.txt")
	now, _ := strconv.Atoi(ss[0])
	pairs := ParseLine(ss[1])

	AH.PrintSoln(13, part1(now, pairs), part2(pairs))

	return
}
