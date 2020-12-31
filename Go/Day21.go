package main

import AH "./adventhelper"

import (
	"sort"
	"strings"
)

type Recipe struct {
	Ingredient  map[string]int
	Allegerns   map[string]int
}

type Pair struct {
	Ingredient  string
	Allegern    string
}

func parseToRecipe(s string) Recipe {
	halves := strings.Split(s, " (contains ")
	is := strings.Split(halves[0], " ")
	ism := make(map[string]int)
	for _, i := range is {
		ism[i] = 1
	}
	as := strings.Split(AH.TrimLastRune(halves[1]), ", ")
	asm := make(map[string]int)
	for _, a := range as {
		asm[a] = 1
	}

	return Recipe{Ingredient:ism, Allegerns:asm}
}

func intersection(lhs map[string]int, rhs map[string]int) map[string]int {
	is := make(map[string]int)
	for k, _ := range lhs {
		if _, ok := rhs[k]; ok {
			is[k] = 1
		}
	}
	return is
}

func recipeCollect(rs []Recipe) (Recipe) {
	is := make(map[string]int)
	as := make(map[string]int)

	for _, r := range rs {
		for k, _ := range r.Ingredient {
			is[k] = 1
		}

		for k, _ := range r.Allegerns {
			as[k] = 1
		}
	}

	return Recipe{Ingredient:is, Allegerns:as}
}

func IngredientWithAllegern(rs []Recipe, all string) []string {
	cs := recipeCollect(rs)
	filtered := cs.Ingredient
	for _, r := range rs {
		if _, ok := r.Allegerns[all]; ok {
			filtered = intersection(filtered, r.Ingredient)
		}
	}

	unique := make([]string, 0, len(filtered))
	for key := range filtered {
		unique = append(unique, key)
	}

	return unique
}

func DeleteIngAndAll(rs []Recipe, i string, a string) []Recipe {
	for _, r := range rs {
		delete(r.Ingredient, i)
		delete(r.Allegerns, a)
	}

	return rs
}

func kill(rs []Recipe) []Recipe {
	collect := recipeCollect(rs)
	as := collect.Allegerns

	for len(as) > 0 {
		for a, _ := range as {
			match := IngredientWithAllegern(rs, a)

			if len(match) == 1 {
				rs = DeleteIngAndAll(rs, match[0], a)
			}
		}

		collect = recipeCollect(rs)
		as = collect.Allegerns
	}
	return rs
}

func extract(rs []Recipe) []Pair {
	collect := recipeCollect(rs)
	as := collect.Allegerns

	prs := []Pair{}
	for len(as) > 0 {
		for a, _ := range as {
			match := IngredientWithAllegern(rs, a)

			if len(match) == 1 {
				rs = DeleteIngAndAll(rs, match[0], a)
				prs = append(prs, Pair{Ingredient:match[0], Allegern:a})
			}
		}

		collect = recipeCollect(rs)
		as = collect.Allegerns
	}
	return prs
}

func part1Count(rs []Recipe) (total int, is []string) {
	total = 0

	unique := make(map[string]int)
	for _, r := range rs {
		total += len(r.Ingredient)
		for i, _ := range r.Ingredient {
			unique[i] = 1
		}
	}

	is = make([]string, 0, len(unique))
	for key := range unique {
		is = append(is, key)
	}
	return
}

func part2String(prs []Pair) (out string) {
	sort.SliceStable(prs, func(i, j int) bool {
		return prs[i].Allegern < prs[j].Allegern
	})

	out = ""
	for _, pr := range prs {
		out = out + pr.Ingredient + ","
	}
	out = AH.TrimLastRune(out)
	return
}

func main() {
	is, _ := AH.ReadStrFile("../input/input21.txt")
	// we need two copies as the functions modify the original!
	rs1 := []Recipe{}
	rs2 := []Recipe{}

	for _, s := range is {
		rs1 = append(rs1, parseToRecipe(s))
		rs2 = append(rs2, parseToRecipe(s))
	}

	kill(rs1)
	count, clean := part1Count(rs1)

	for _, i := range clean {
		rs2 = DeleteIngAndAll(rs2, i, "NULL")
	}
	test := extract(rs2)

	AH.PrintSoln(21, count, part2String(test))

	return
}
