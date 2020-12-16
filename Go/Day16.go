package main

import AH "./adventhelper"

import (
	"strconv"
	"strings"
)

type DoubleRange struct {
	LhsLo, LhsHi int
	RhsLo, RhsHi int
	ValidIndices []int
}

func findAndDelete(s []int, item int) []int {
	index := 0
	for _, i := range s {
		if i != item {
			s[index] = i
			index++
		}
	}
	return s[:index]
}

func (dr DoubleRange) Ok(i int) (ok bool) {
	return (dr.LhsLo <= i && i <= dr.LhsHi) || (dr.RhsLo <= i && i <= dr.RhsHi)
}

func (dr *DoubleRange) Possible(ticket []int) () {
	newValid := []int{}
	for _, i := range dr.ValidIndices {
		if dr.Ok(ticket[i]) {
			newValid = append(newValid, i)
		}		
	}

	dr.ValidIndices = newValid
	return
}

func (dr *DoubleRange) Delete(i int) {
	dr.ValidIndices = findAndDelete(dr.ValidIndices, i) 
}

func GetInvalidFields(intervals []DoubleRange, ticket []int) (invalid []int) {
	invalid = []int{}
	for _, v := range ticket {
		ok := false
		for _, inter := range intervals {
			ok = ok || inter.Ok(v)
		}

		if !ok {
			invalid = append(invalid, v)
		}
	}

	return
}

func part1(intervals []DoubleRange, tickets [][]int) (invalid int) {
	invalid = 0
	for _, t := range tickets {
		inv := GetInvalidFields(intervals, t)
		for _,v := range inv {
			invalid += v
		}
	}

	return
}

func part2(intervals []DoubleRange, tickets [][]int, me []int) (prod int) {
	validTickets := [][]int{}
	// step 1 remove the invalid tickets
	for _, t := range tickets {
		if len(GetInvalidFields(intervals, t)) == 0 {
			validTickets = append(validTickets, t)
		}
	}

	// step 2 for each range find the valid indices
	for i, _ := range intervals {
		for _, t := range validTickets{
			intervals[i].Possible(t)
		}
	}

	// step 3 reduce the valids indices to singletons
	outIndices := [20]int{}
	for i := 0; i <= 20; i++ {
		kill := -1
		// find a Double Range with only on possible
		for j, q := range intervals { 
			if len(q.ValidIndices) == 1 { // set this to the final set of indices
				outIndices[q.ValidIndices[0]] = j
				kill = q.ValidIndices[0]
				break
			}
		}
		// now remove this value from all the other invervals
		for i, _ := range intervals { 
			intervals[i].Delete(kill)
		}
	}

	prod = 1
	for i, v := range outIndices {
		if v < 6 {
			prod *= me[i]
		}
	}

	return prod
}

func main() {
	ss, _ := AH.ReadStrFile("../input/input16.txt")
	intervals := []DoubleRange{}
	me := []int{}
	tickets := [][]int{}
	// this is tedious
	for i, s := range ss {
		if i < 20 { // get the valid ranges - price: 30-889 or 914-949
			parts := strings.Split(s, ": ") 
			pair := strings.Split(parts[1], " or ")
			lhs := strings.Split(pair[0], "-")
			rhs := strings.Split(pair[1], "-")
			lhslo, _ := strconv.Atoi(lhs[0])
			lhshi, _ := strconv.Atoi(lhs[1])
			rhslo, _ := strconv.Atoi(rhs[0])
			rhshi, _ := strconv.Atoi(rhs[1])

			rng := DoubleRange{LhsLo:lhslo, LhsHi:lhshi,
			                   RhsLo:rhslo, RhsHi:rhshi,
			                   ValidIndices: []int{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
			                                       10,11,12,13,14,15,16,17,18,19}}
			intervals = append(intervals, rng)
		} else if i == 21 {
			parts := strings.Split(s, ",")
			for _, p := range parts {
				j,_ := strconv.Atoi(p)
				me = append(me, j)
			}
		} else if i > 22 {
			parts := strings.Split(s, ",")
			tempTicket := []int{}
			for _, p := range parts {
				j,_ := strconv.Atoi(p)
				tempTicket = append(tempTicket, j)
			}
			tickets = append(tickets, tempTicket)
		}
	}

	AH.PrintSoln(16, part1(intervals, tickets),
	                 part2(intervals, tickets, me))

	return
}
