package main

import AH "./adventhelper"

import (
	"regexp"
	"strconv"
	"strings"
)

// a1[*/+]a2[*/+]a3[*/+]a4[*/+]a5[*/+]a6
// resolves operators left to right
func parseLR(exp string) (string) {
	prevExp := ""
	re := regexp.MustCompile("[0-9]+[\\*\\+][0-9]+")
	for {
		if exp == prevExp {
			break
		}
		prevExp = exp
		found := re.FindString(exp)
		if found != "" {
			newValue := ""
			if AH.ContainsChar(found, '*') {
				parts := strings.Split(found, "*")
				lhs, _ := strconv.Atoi(parts[0])
				rhs, _ := strconv.Atoi(parts[1])
				newValue = strconv.Itoa(lhs * rhs)
			}

			if AH.ContainsChar(found, '+') {
				parts := strings.Split(found, "+")
				lhs, _ := strconv.Atoi(parts[0])
				rhs, _ := strconv.Atoi(parts[1])
				newValue = strconv.Itoa(lhs + rhs)
			}

			exp = strings.Replace(exp, found, newValue, 1)
		}
	}

	return exp
}

// a1[*/+]a2[*/+]a3[*/+]a4[*/+]a5[*/+]a6
// resolves all additions the multiplications
func parsePM(exp string) (string) {
	prevExp := ""
	reAdd := regexp.MustCompile("[0-9]+[\\+][0-9]+")
	reMul := regexp.MustCompile("[0-9]+[\\*][0-9]+")

	for {
		if (exp == prevExp) {
			break
		}
		prevExp = exp

		// look for an addition term
		found := reAdd.FindString(exp)
		if found != "" {
			parts := strings.Split(found, "+")
			lhs, _ := strconv.Atoi(parts[0])
			rhs, _ := strconv.Atoi(parts[1])
			newValue := strconv.Itoa(lhs + rhs)

			exp = strings.Replace(exp, found, newValue, 1)
			continue
		}
		// if we get here we haven't found an addition term,
		// so look for a multiplcation
		found = reMul.FindString(exp)
		if found != "" {
			parts := strings.Split(found, "*")
			lhs, _ := strconv.Atoi(parts[0])
			rhs, _ := strconv.Atoi(parts[1])
			newValue := strconv.Itoa(lhs * rhs)

			exp = strings.Replace(exp, found, newValue, 1)
			continue
		}

		return exp
	}

	return exp
}

// find a bracket that does not conatin any other brackets
func parseBracket(exp string, lr bool) (string) {
	prevExp := ""
	re := regexp.MustCompile("\\([0-9+*]+\\)")
	for {
		if exp == prevExp {
			break
		}
		prevExp = exp
		found := re.FindString(exp)
		// have we found a bracket with no nested brackets inside?
		if found != "" {
			// if so reduce it, based on the rule given in the instructions
			newValue := ""
			if lr {
				newValue = parseLR(AH.TrimLastRune(AH.TrimFirstRune(found)))
			} else {
				newValue = parsePM(AH.TrimLastRune(AH.TrimFirstRune(found)))
			}

			exp = strings.Replace(exp, found, newValue, 1)
		}
	}
	return exp
}

func Part1(exp string) (value int) {
	value, _ = strconv.Atoi(parseLR(parseBracket(exp, true)))
	return
}

func Part2(exp string) (value int) {
	value, _ = strconv.Atoi(parsePM(parseBracket(exp, false)))
	return
}

func main() {
	ss, _ := AH.ReadStrFile("../input/input18.txt")
	expressions := []string{}
	for _, s := range ss {
		temp := strings.Replace(s, " ", "", -1) // remove the spaces!
		expressions = append(expressions, temp)
	}

	total1, total2 := 0, 0
	for _, e := range expressions {
		total1 += Part1(e)
		total2 += Part2(e)
	}

	AH.PrintSoln(18, total1, total2)

	return
}
