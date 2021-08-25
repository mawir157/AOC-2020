#include "AH.h"

namespace Day13
{

	typedef std::pair<uint64_t, uint64_t> IntPair;

	std::vector<IntPair> parseLine(const std::string& s)
	{
		std::vector<IntPair> v;
		auto ss = AH::Split(s, ',');
		for (size_t i = 0; i < ss.size(); ++i)
		{
			if (ss[i] != "x")
			{
				uint64_t p = std::stoi(ss[i]);
				v.push_back(std::make_pair(i,p));
			}
		}

		return v;
	}

	uint64_t part1(const uint64_t now, const std::vector<IntPair>& pairs)
	{
		IntPair best = std::make_pair(1000, 0);
		for (auto pair : pairs)
		{
			auto p = std::get<1>(pair);
			auto m = p - (now % p);
			if (m < std::get<0>(best))
				best = std::make_pair(m , p);
		}

		return std::get<0>(best) * std::get<1>(best);
	}

	IntPair CRT(const IntPair p1, const IntPair p2)
	{
		for (uint64_t n = 1; n <= std::get<1>(p2); ++n)
		{
			auto c = std::get<0>(p1) + n * std::get<1>(p1);
			if ((c % std::get<1>(p2)) == (std::get<0>(p2) % std::get<1>(p2))) 
				return std::make_pair(c , std::get<1>(p1) * std::get<1>(p2));
		}

		return std::make_pair(0, 1);
	}

	uint64_t part2(const std::vector<IntPair>& pairs)
	{
		IntPair out = std::make_pair(0, 1);
		for (auto pair : pairs)
			out = CRT(out, pair);

		return std::get<1>(out) - std::get<0>(out);
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		uint64_t now = std::stoi(inputLines[0]);

		auto pairs = parseLine(inputLines[1]);

		AH::PrintSoln(13, part1(now, pairs), part2(pairs));

		return 0;
	}

}
