#include "AH.h"

namespace Day14
{

	class Machine
	{
	public:
		Machine();
		void Apply(const std::string& instruction, const bool part1);
		uint64_t Count() const;

	private:
		std::string                  Mask;
		std::map<uint64_t, uint64_t> Memory;
	};

	Machine::Machine()
	{

	}

	uint64_t applyMaskToValue(const std::string& mask, const uint64_t value)
	{
		auto newValue = value;
		size_t i = 0;
		for (auto c : mask)
		{
			if (c == '0')
				newValue &= ~(1ull << i);
			else if (c == '1')
				newValue |= (1ull << i);

			++i;
		}
		return newValue;
	}

	std::vector<uint64_t> applyMaskToRegistries(const std::string& mask,
		     const uint64_t index, const std::vector<uint64_t> regs)
	{
		std::vector<uint64_t> newRegs;
		if (index >= mask.length())
			return regs;

		auto c = mask.at(index);

		if (c == 'X')
		{
			for (auto r : regs)
			{
				auto t1 = r | (1ull << index);
				newRegs.push_back(t1);
				auto t2 = r & ~(1ull << index);
				newRegs.push_back(t2);
			}

		}
		else if (c == '0')
		{
			newRegs = regs;
		}
		else if (c == '1')
		{
			for (auto r : regs)
			{
				auto t1 = r | (1ull << index);
				newRegs.push_back(t1);
			}
		}

		return applyMaskToRegistries(mask, index + 1, newRegs);
	}

	void Machine::Apply(const std::string& instruction, const bool part1)
	{
		auto parts = AH::SplitOnString(instruction, " = ");
		auto p0 = parts[0];
		auto p1 = parts[1];
		if (p0 == "mask")
		{
			std::reverse(p1.begin(), p1.end());
			Mask = p1;
		}
		else
		{
			p0.erase(0, 4);
			p0.erase(p0.size() - 1);
			uint64_t reg = std::stoi(p0);
			uint64_t val = std::stoi(p1);
			if (part1)
			{
				val = applyMaskToValue(Mask, val);
				Memory[reg] = val;
			}
			else
			{
				std::vector<uint64_t> empty{reg};
				auto regs = applyMaskToRegistries(Mask, 0, empty);
				for (auto r : regs)
					Memory[r] = val;
			}
		}
	}

	uint64_t Machine::Count() const
	{
		uint64_t total = 0;
		for (auto [k, v] : Memory)
		{
			total += v;
		}
		return total;
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		Machine m1;
		Machine m2;

		for (auto s : inputLines)
		{
			m1.Apply(s, true);
			m2.Apply(s, false);
		}

		AH::PrintSoln(14, m1.Count(), m2.Count());

		return 0;
	}

}
