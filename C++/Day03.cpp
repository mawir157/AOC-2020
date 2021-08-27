#include "AH.h"

namespace Day03
{

	uint64_t toboggan(const std::vector<std::string>& s, const size_t dx, const size_t dy)
	{
		uint64_t trees = 0;
		size_t w = s[0].length();
		for (size_t i = 0, y = 0; i < s.size(); i += dx, y = ((y + dy) % w))
		{
			trees += (s[i].at(y) == '#') ? 1 : 0;
		}

		return trees;
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);

		auto t1 = toboggan(inputLines, 1, 1);
		auto t2 = toboggan(inputLines, 1, 3);
		auto t3 = toboggan(inputLines, 1, 5);
		auto t4 = toboggan(inputLines, 1, 7);
		auto t5 = toboggan(inputLines, 2, 1);

		AH::PrintSoln(3, t2, t1*t2*t3*t4*t5);

		return 0;
	}

}
