#include "AH.h"

namespace Day08
{

	enum instruction { acc = 0, nop, jmp };

	class Command
	{
	public:
		Command(const instruction i, const int64_t v);
		void flip();

		instruction Instr;
		int64_t     Value;
		
	};

	Command::Command(const instruction i, const int64_t v) :
		Instr(i), Value(v) {}

	void Command::flip()
	{
		switch(Instr)
		{
		case instruction::acc:
			break;
		case instruction::nop:
			Instr = instruction::jmp;
			break;
		case instruction::jmp:
			Instr = instruction::nop;
			break;
		}
	}

	class Machine
	{
	public:
		Machine(const std::vector<Command>& cmds);

		Command at() const { return Program[Pointer]; }
		int64_t tick();
		int64_t run();
		int64_t output() { return Accumulator; }

	private:
		int64_t               Accumulator;
		std::vector<uint64_t> History;
		uint64_t              Pointer;
		std::vector<Command>  Program;
		
	};

	Machine::Machine(const std::vector<Command>& cmds) :
		Accumulator(0), Pointer(0), Program(cmds) {}

	int64_t Machine::tick()
	{
		if (Pointer >= Program.size() || Pointer < 0)
			return 1;
		else if (std::find(History.begin(), History.end(), Pointer) != History.end())
			return -1;
		else
		{
			auto ins = Program[Pointer].Instr;
			auto val = Program[Pointer].Value;

			History.push_back(Pointer);
			switch(ins)
			{
			case instruction::acc:
				Accumulator += val;
				Pointer++;
				break;
			case instruction::nop:
				Pointer++;
				break;
			case instruction::jmp:
				Pointer += val;
				break;
			default:
				return -2;		
			}
			return 0;
		}
	}

	int64_t Machine::run()
	{
		int64_t exitCode = 0;
		while (exitCode == 0)
			exitCode = tick();

		return exitCode;
	}

	Command parseLine(const std::string s)
	{
		auto parts = AH::Split(s, ' ');
		int64_t v = std::stoi(parts[1]);
		instruction i = instruction::acc;
		if (parts[0] == "acc")
			i = instruction::acc;
		else if (parts[0] == "jmp")
			i = instruction::jmp;
		else if (parts[0] == "nop")
			i = instruction::nop;

		return Command(i, v);
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);

		std::vector<Command> cmds;
		std::transform(inputLines.begin(), inputLines.end(),
		               std::back_inserter(cmds),
		               [](std::string s) -> Command { return parseLine(s); });

		Machine m1(cmds);
		m1.run();

		int64_t part2 = 0;
		for (size_t i = 0; i < cmds.size(); ++i)
		{
			if (cmds[i].Instr == instruction::acc)
				continue;

			cmds[i].flip();
			Machine m2(cmds);
			m2.run();

			auto code = m2.run();
			if (code == 1)
			{
				part2 = m2.output();
				break;
			}
			cmds[i].flip();
		}

		AH::PrintSoln(8, m1.output(), part2);

		return 0;
	}

}
