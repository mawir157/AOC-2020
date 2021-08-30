#include "AH.h"

namespace Day18
{

	std::string parseLR(const std::string s)
	{
		std::string exp = s;
		std::string prevExp = "";
		auto re = std::regex("[0-9]+[\\*\\+][0-9]+");
		while (exp != prevExp)
		{
			prevExp = exp;
			std::smatch m;
			if (regex_search(exp, m, re))
			{
				std::string found = m[0];

				std::string newValue = "";
				if (found.find('*') < found.length())
				{
					auto parts = AH::Split(found, '*');
					int64_t lhs = std::stoll(parts[0]);
					int64_t rhs = std::stoll(parts[1]);
					newValue = std::to_string(lhs * rhs);
				}

				if (found.find('+') < found.length())
				{
					auto parts = AH::Split(found, '+');
					int64_t lhs = std::stoll(parts[0]);
					int64_t rhs = std::stoll(parts[1]);
					newValue = std::to_string(lhs + rhs);
				}

				exp = std::regex_replace(exp, re, newValue,
					                       std::regex_constants::format_first_only);
			}
		}

		return exp;
	}

	std::string parsePM(const std::string s)
	{
		std::string exp = s;
		std::string prevExp = "";
		auto reAdd = std::regex("[0-9]+[\\+][0-9]+");
		auto reMul = std::regex("[0-9]+[\\*][0-9]+");
		while (exp != prevExp)
		{
			prevExp = exp;
			std::smatch m;
			if (regex_search(exp, m, reAdd))
			{
 				std::string found = m[0];
				auto parts = AH::Split(found, '+');
				int64_t lhs = std::stoll(parts[0]);
				int64_t rhs = std::stoll(parts[1]);
				std::string newValue = std::to_string(lhs + rhs);
				exp = std::regex_replace(exp, reAdd, newValue,
					                       std::regex_constants::format_first_only);
				continue;
			}

			if (regex_search(exp, m, reMul))
			{
				std::string found = m[0];
				auto parts = AH::Split(found, '*');
				int64_t lhs = std::stoll(parts[0]);
				int64_t rhs = std::stoll(parts[1]);
				std::string newValue = std::to_string(lhs * rhs);
				exp = std::regex_replace(exp, reMul, newValue,
					                       std::regex_constants::format_first_only);
				continue;
			}
		}

		return exp;
	}

	std::string parseBracket(const std::string s, const bool lr)
	{
		std::string exp = s;
		std::string prevExp = "";
		auto re = std::regex("\\([0-9+*]+\\)");

		while (exp != prevExp)
		{
			prevExp = exp;
			std::smatch m;
			if (regex_search(exp, m, re))
			{
				std::string found = m[0];
				found = found.substr(1, found.size() - 2);

				std::string newValue = "";
				if (lr)
				{
					newValue = parseLR(found);
				}
				else
				{
					newValue = parsePM(found);
				}
				exp = std::regex_replace(exp, re, newValue,
					                       std::regex_constants::format_first_only);
			}

		}

		return exp;
	}

	int64_t Evaluate(const std::string exp, const bool part1)
	{
		auto brackets = parseBracket(exp, part1);

		if (part1)
			return std::stoll(parseLR(brackets));
		else
			return std::stoll(parsePM(brackets));
	}



	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		std::vector<std::string> expressions;
		for ( auto& e : inputLines)
		{
			e.erase(remove_if(e.begin(), e.end(), isspace), e.end());
			expressions.push_back(e);
		}

		int64_t part1 = 0, part2 = 0;
		for ( auto e : expressions )
		{
			part1 += Evaluate(e, true);
			part2 += Evaluate(e, false);
		}

		AH::PrintSoln(18, part1, part2);

		return 0;
	}

}
