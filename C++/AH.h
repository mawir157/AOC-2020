#pragma once

#include "includes.h"

namespace AH
{

	void PrintSoln(const int day, const uint64_t soln1, const uint64_t soln2);
	std::vector<std::string> ReadTextFile(const std::string& filename);
	std::vector<std::string> ParseLineGroups(const std::vector<std::string>& ss,
		                                       const char sep=' ');
	std::vector<std::string> Split(const std::string &s, char delim);

}
