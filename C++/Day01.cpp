#include "Day01.h"
#include "AH.h"

namespace Day01
{

	int Part1(const std::vector<int> v, const int target)
	{
		for (int i = 0; i < v.size(); ++i)
	  {
			for (int j = i+1; j < v.size(); ++j)
		  {
		  	if (v[i] + v[j] == target)
		  	{
		  		return v[i] * v[j];
		  	}
		  }
	  }

	  return -1;
	}

	int Part2(const std::vector<int> v, const int target)
	{
		for (int i = 0; i < v.size(); ++i)
	  {
			for (int j = i+1; j < v.size(); ++j)
		  {
		  	for (int k = j+1; k < v.size(); ++k)
			  {
			  	if (v[i] + v[j] + v[k] == target)
			  	{
			  		return v[i] * v[j]* v[k];
			  	}
		  	}
		  }
	  }

	  return -1;
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		
		// convert lines to int
		std::vector<int> values;
	  std::transform(inputLines.begin(), inputLines.end(),
	  	             std::back_inserter(values),
	                 [](std::string s) -> int { return std::stoi(s); });

	  AH::PrintSoln(1, Day01::Part1(values, 2020), Day01::Part2(values, 2020));

		return 0;
	}

}
