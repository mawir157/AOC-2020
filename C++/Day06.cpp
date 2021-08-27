#include "AH.h"

namespace Day06
{

	std::tuple<uint64_t, uint64_t> each_answer(const std::string s, const char c)
	{
		std::map<char, size_t> count;
		auto lines = AH::Split(s, c);

		for (auto l : lines)
			for (auto cc: l)
				count[cc] += 1;

		uint64_t n = 0;
		for (auto [k,v] : count)
			if (v == lines.size())
				n++;

		return std::make_tuple( count.size(), n);
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		auto lineGroups = AH::ParseLineGroups(inputLines, ':');

		uint64_t p1 = 0, p2 = 0;
		for (auto pw : lineGroups)
		{
			auto [v1, v2] = each_answer(pw, ':');
			p1 += v1;
			p2 += v2;
		}

		AH::PrintSoln(6, p1, p2);

		return 0;
	}

}
