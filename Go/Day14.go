package main

import Helper "./adventhelper"

import (
	"strconv"
	"strings"
)

type Pair struct {
	A, P int
}

type Machine struct {
	Mask   string
	Memory map[int]int
}

// Sets the bit at pos in the integer n.
func setBit(n int, pos uint) int {
	n |= (1 << pos)
	return n
}

// Sets the bit at pos in the integer n.
func clearBit(n int, pos uint) int {
	n &^= (1 << pos)
	return n
}

func applyMaskToValue(mask string, value int) (newValue int) {
	newValue = value
	for i, c := range mask {
		switch c {
			case rune('X'): // do nothing
			case rune('0'): newValue = clearBit(newValue, uint(i))
			case rune('1'): newValue = setBit  (newValue, uint(i))
		}		
	}
	return 
}

func applyMaskToRegistries(mask string, index int, regs []int) (newRegs []int) {
	if index >= len(mask) {
		return regs
	}

	rs := []rune(mask)
	c := rs[index]
	switch c {
		case rune('X'):
			for _, r := range regs {
				newRegs = append(newRegs, setBit  (r, uint(index)))
				newRegs = append(newRegs, clearBit(r, uint(index)))
			}
		case rune('0'):
			 newRegs = regs
		case rune('1'):
			for _, r := range regs {
				newRegs = append(newRegs, setBit(r, uint(index)))
			}
	}	

	return applyMaskToRegistries(mask, index + 1, newRegs)
}

func (m *Machine) Apply(instruction string, part1 bool) () {
	parts := strings.Split(instruction, " = ") 
	if (parts[0] == "mask") { // set mask
		m.Mask = Helper.ReverseString(parts[1])
	} else { // write to memory parts
		reg, _ := strconv.Atoi(Helper.TrimLastRune(Helper.Drop(parts[0], 4)))
		val, _ := strconv.Atoi(parts[1])
		if (part1) {
			val = applyMaskToValue(m.Mask, val)
			m.Memory[reg] = val
		} else {
			regs := applyMaskToRegistries(m.Mask, 0, []int{reg})
			for _, r := range regs {
				m.Memory[r] = val
			}
		}
	}
}

func (m Machine) Count() (total int) {
	for _, element := range m.Memory {
		total += element
	}
	return
}

func main() {
	ss, _ := Helper.ReadStrFile("../input/input14.txt")

	skynet := Machine{Mask:"", Memory:make(map[int]int)}
	holly := Machine{Mask:"", Memory:make(map[int]int)}
	for _, s := range ss {
		skynet.Apply(s, true)
		holly.Apply(s, false)
	}

	Helper.PrintSoln(14, skynet.Count(), holly.Count())

	return
}
