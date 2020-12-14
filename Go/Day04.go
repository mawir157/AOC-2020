package main

import (
	"regexp"
	"strings"
)

import AH "./adventhelper"

func checkId(s string) (part1 bool, part2 bool) {
	part1 = true
	part2 = true
	ss := strings.Split(s, " ")

	// a bit hacky because we don't have sets
	seen := make(map[string]bool)
	fields := []string{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}
	for _, key := range fields {
		seen[key] = false
	}

	for _, q := range ss {
		p := strings.Split(q, ":")

		key := p[0]
		value := p[1]
		seen[key] = true

		ok := true
		switch key {
			case "byr":
				ok, _ = regexp.Match("(19[2-9][0-9]|200[0-2])", []byte(value))
			case "iyr":
				ok, _ = regexp.Match("(201[0-9]|2020)", []byte(value))
			case "eyr":
				ok, _ = regexp.Match("(202[0-9]|2030)", []byte(value))
			case "hgt":
				ok, _ = regexp.Match("(59in|6[0-9]in|7[0-6]in|1[5-8][0-9]cm|19[0-3]cm)", []byte(value))
			case "hcl":
				ok, _ = regexp.Match("^#([0-9a-f]){6}$", []byte(value))
			case "ecl":
				ok, _ = regexp.Match("(amb|blu|brn|gry|grn|hzl|oth)", []byte(value))
			case "pid":
				ok, _ = regexp.Match("^([0-9]){9}$", []byte(value))
			case "cid":
				ok = true
		}
		part2 = part2 && ok
	}

	for _, key := range fields {
		part1 = part1 && seen[key]
	}

	return
}

func main() {
	ss, _ := AH.ParseLineGroups("../input/input04.txt", " ")

	part1 := 0
	part2 := 0
	for _, s := range ss {
		ok1, ok2 := checkId(s)
		if ok1 {
			part1 += 1
			if ok2 {
				part2 += 1
			}
		}
	}

	AH.PrintSoln(4, part1, part2)

	return
}
