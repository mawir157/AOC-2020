#pragma once

#include "includes.h"

namespace AH
{
	// I/0
	void PrintSoln(const int day, const uint64_t soln1, const uint64_t soln2);
	void PrintSolnFinal(const int day, const uint64_t soln1);
	std::vector<std::string> ReadTextFile(const std::string& filename);
	std::vector<std::string> ParseLineGroups(const std::vector<std::string>& ss,
		                                       const char sep=' ');
	std::vector<std::string> Split(const std::string &s, char delim);
	std::vector<std::string> SplitOnString(const std::string &s, 
		                                     const std::string delim);
	// Maths
	uint64_t IntPow(const uint64_t x, const uint64_t p);
}
