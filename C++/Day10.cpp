#include "AH.h"

namespace Day10
{

	int jumps(const std::vector<int>& jolts)
	{
		int ones = 0, threes = 1;

		for (size_t i = 0; i < jolts.size(); ++i)
		{
			auto diff = jolts[i] - jolts[i+1];
			ones   += (diff == 1) ? 1 : 0;
			threes += (diff == 3) ? 1 : 0;
		}

		return ones * threes;
	}

	uint64_t routes(const std::vector<int>& jolts)
	{
		std::map<int, uint64_t> seen;
		seen[jolts[0]] = 1;

		for (auto j : jolts)
		{
			uint64_t t = 0;
			for (size_t i = 0; i < 4; ++i)
			{
				t += seen[j + i];
			}
			seen[j] = t;
		}

		return seen[0];
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		
		std::vector<int> values;
		std::transform(inputLines.begin(), inputLines.end(),
			             std::back_inserter(values),
	                 [](std::string s) -> int { return std::stoi(s); });

		std::sort(values.begin(), values.end(), std::greater<>());
		values.push_back(0);

		AH::PrintSoln(10, jumps(values), routes(values));

		return 0;
	}

}
