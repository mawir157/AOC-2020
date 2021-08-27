#include "AH.h"

namespace Day04
{

	std::tuple<bool, bool> check_id(const std::string& s)
	{
		
		bool part1 = true, part2 = true;
		auto parts = AH::Split(s, ' ');

		std::set<std::string> seen;
		std::vector<std::string> fields{"byr", "iyr", "eyr", "hgt",
		                                "hcl", "ecl", "pid"};

		for (auto pw : parts)
		{
		  	auto p = AH::Split(pw, ':');
		  	auto key = p[0];
		  	auto value = p[1];
				seen.insert(p[0]);

	  		std::smatch m;

			if (key == "byr")
				part2 &= (std::regex_search (value, m, std::regex("(19[2-9][0-9]|200[0-2])")));
			else if (key == "iyr")
				part2 &= (std::regex_search (value, m, std::regex("(201[0-9]|2020)")));
			else if (key == "eyr")
				part2 &= (std::regex_search (value, m, std::regex("(202[0-9]|2030)")));
			else if (key == "hgt")
				part2 &= (std::regex_search (value, m,
				                             std::regex("(59in|6[0-9]in|7[0-6]in|1[5-8][0-9]cm|19[0-3]cm)")));
			else if (key == "hcl")
				part2 &= (std::regex_search (value, m, std::regex("#([0-9a-f]){6}")));
			else if (key == "ecl")
				part2 &= (std::regex_search (value, m,
				                             std::regex("(amb|blu|brn|gry|grn|hzl|oth)")));
			else if (key == "pid")
				part2 &= (std::regex_search (value, m, std::regex("^([0-9]){9}$")));
		}

		for (auto f : fields)
	  		part1 &= (seen.find(f) != seen.end());

		return std::make_tuple( part1, part1 & part2);

	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		auto lineGroups = AH::ParseLineGroups(inputLines);

		uint64_t p1 = 0, p2 = 0;
		for (auto pw : lineGroups)
		{
			auto [ok1, ok2] = check_id(pw);
			p1 += ok1 ? 1 : 0;
			p2 += ok2 ? 1 : 0;
		}

		AH::PrintSoln(4, p1, p2);

		return 0;
	}

}
