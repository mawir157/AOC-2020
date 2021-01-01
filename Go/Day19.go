package main

import AH "./adventhelper"

import (
	"strconv"
	"strings"
)

type RuleType string

const(
	Literal RuleType = "Literal"
	Sing             = "Sing"
	Recur            = "Recur"
)

type Rule struct {
	Mode         RuleType
	Lit          string
	Single       []int 
	SubRules     []Rule
}

func parseToRule(s string) (rules [][]int) {
	subrules := strings.Split(s, " | ")
	for _, r := range subrules {
		subArray := []int{}
		sub := strings.Split(r, " ")
		for _, s := range sub {
			i,_ := strconv.Atoi(s)
			subArray = append(subArray, i)
		}
		rules = append(rules, subArray)
	}
	return
}

func parseInput(ss []string) (rs map[int]Rule, ws []string) {
	rs = make(map[int]Rule)
	for _, s := range ss {
		h := AH.FirstRune(s)
		if h != rune('a') && h != rune('b') {
			parts := strings.Split(s, ": ")
			id,_ := strconv.Atoi(parts[0])
			// fmt.Println(parts[1])

			if AH.FirstRune(parts[1]) == rune('"') { // 121: "a"
				// lit := AH.TrimFirstRune(parts[1])
				lit := string(parts[1][1])
				rs[id] = Rule{Mode:Literal, Lit:lit}
			} else {
				subRules := parseToRule(parts[1])
				if len(subRules) == 1 {
					rs[id] = Rule{Mode:Sing, Single:subRules[0]}
				} else {
					recur := []Rule{}
					for _, r := range subRules {
						recur = append(recur, Rule{Mode:Sing, Single:r})
					}
					rs[id] = Rule{Mode:Recur, SubRules:recur}
				}
			}
			
			
		} else {
			ws = append(ws, s)
		}
	}
	return
}

func eval(rules map[int]Rule, s string, r Rule) []string {
	if r.Mode == Literal {
		if len(s) == 0 {
			return make([]string, 0)
		} else if AH.Take(s,1) == r.Lit {
			return []string{AH.TrimFirstRune(s)} 
		} else {
			return make([]string, 0)
		}
	}

	if r.Mode == Sing {
		reduced := []string{s}
		for _, i := range r.Single {
			foldStrings := make([]string, 0)
			for _, ss := range reduced {
				vv := eval(rules, ss, rules[i])
				for _, v := range vv {
					foldStrings = append(foldStrings, v)
				}
			}
			reduced = foldStrings
		}
		return reduced
	}

	if r.Mode == Recur {
		reduced := make([]string, 0)
		for _, ri := range r.SubRules {
			vv := eval(rules, s, ri)
			for _, v := range vv {
					reduced = append(reduced, v)
			}
		}
		return reduced
	}

	return make([]string, 0)
}

func anyLengthZero(ss []string) bool {
	for _, s := range ss {
		if len(s) == 0 {
			return true
		}
	}
	return false
}


func main() {
	ss, _ := AH.ReadStrFile("../input/input19.txt")
	rs, ws := parseInput(ss)

	rs2 := make(map[int]Rule)
	for k,v := range rs {
	  rs2[k] = v
	}

	// add the extra rules
	rs2[8] = Rule{Mode:Recur,
	              SubRules:[]Rule{Rule{Mode:Sing, Single:[]int{42}},
	                              Rule{Mode:Sing, Single:[]int{42, 8}}}}
	rs2[11] = Rule{Mode:Recur,
	               SubRules:[]Rule{Rule{Mode:Sing, Single:[]int{42, 31}},
	                              Rule{Mode:Sing, Single:[]int{42, 11, 31}}}}

	total1 := 0
	total2 := 0
	for _, w := range ws {
		q  := eval(rs, w, rs[0])
		q2 := eval(rs2, w, rs[0])
		if (anyLengthZero(q)) {
			total1++
		}
		if (anyLengthZero(q2)) { 
			total2++
		}
	}

	AH.PrintSoln(19, total1, total2)

	return
}
