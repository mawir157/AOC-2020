package main

import AH "./adventhelper"

import (
	"strconv"
	"strings"
)

type Command struct {
	Instruction string
	Value       int
}

func (c *Command) flip() {
	if c.Instruction == "acc" {
		// do nothing
	} else if c.Instruction == "nop" {
		c.Instruction = "jmp"
	} else if c.Instruction == "jmp" {
		c.Instruction = "nop"
	}
}

type Machine struct {
	Accumulator int
	History     []int
	Pointer     int
	Program     []Command
}

func (m Machine) at() (Command) {
	return m.Program[m.Pointer]
}

// exit codes
// 0  - still running
// 1  - halted (pointer out of bounds)
// -1 - entered an infinite loop
// -2 - unrecognised command
func (m *Machine) tick() (exitCode int) {
	if m.Pointer >= len(m.Program) || m.Pointer < 0 {
		return 1 // the pointer is Out of Bounds
	} else if AH.ContainsInt(m.History, m.Pointer) {
		return -1 // we've enetered an infinite loop
	} else { // we are expecting a valid instruction
		ins := m.at().Instruction
		val := m.at().Value
		m.History = append(m.History, m.Pointer)
		if ins == "acc" {
			m.Accumulator += val
			m.Pointer += 1
		} else if ins == "jmp" {
			m.Pointer += val
		} else if ins == "nop" {
			m.Pointer += 1
		} else {
			return -2 // we have a INVALID instruction
		}
		return 0 // Carry on...
	}
}

func (m *Machine) run() (exitCode int) {
  exitCode = 0

	for exitCode == 0 {
		exitCode = m.tick()
	}

	return exitCode	
}

func parseLine(s string) (com Command) {
	parts := strings.Split(s, " ")
	v,_ := strconv.Atoi(parts[1])

	com = Command{Instruction: parts[0], Value: v}
	return com
}

func main() {
	ss, _ := AH.ReadStrFile("../input/input08.txt")
	part1 := 0
	part2 := 0

	prg := make([]Command, 0, len(ss))
	for _,s := range ss {
		q := parseLine(s)
		prg = append(prg, q)
	}

	skynet := Machine{Accumulator: 0,
	                  History:     make([]int, 0, 1000),
	                  Pointer:     0,
	                  Program:     prg}

	skynet.run()
	part1 = skynet.Accumulator
	
	cp := make([]Command, len(prg))
	for i := 0; i < len(prg); i++ {
		copy(cp, prg)
		cp[i].flip()

		tempM := Machine{Accumulator: 0,
		                 History:     make([]int, 0, 1000),
		                 Pointer:     0,
		                 Program:     cp}

		code := tempM.run()

		if code == 1 {
			part2 = tempM.Accumulator
			break
		}
	}

	AH.PrintSoln(8, part1, part2)

	return
}
