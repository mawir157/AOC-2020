package main

import Helper "./adventhelper"

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
// -3 - Complete faliure of logic!
func (m *Machine) tick() (exitCode int) {
	if m.Pointer >= len(m.Program) || m.Pointer < 0 {
		// the pointer is Out of Bounds
		return 1
	} else if Helper.Contains(m.History, m.Pointer) {
		// we've enetered an infinite loop
		return -1
	} else {
		// we have a valid instruction
		ins := m.at().Instruction
		val := m.at().Value
		m.History = append(m.History, m.Pointer)
		if ins == "acc" {
			m.Accumulator += val
			m.Pointer += 1
			return 0
		} else if ins == "jmp" {
			m.Pointer += val
			return 0
		} else if ins == "nop" {
			m.Pointer += 1
			return 0
		}	
		return -2	
	}
	return -3
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
	ss, _ := Helper.ReadStrFile("../input/input08.txt")
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
	

	cp :=  make([]Command, len(prg))
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

	Helper.PrintSoln(8, part1, part2)

	return
}
