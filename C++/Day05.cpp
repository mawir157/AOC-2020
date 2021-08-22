#include "AH.h"

namespace Day05
{

	uint64_t get_seat(const std::string& s, const std::string& ones)
	{
		uint64_t n = 0, i = 0;
		for (auto c : s)
	  {
	  	if (ones.find(c) != std::string::npos)
	  	{
	  		n += AH::IntPow(2, s.length() - 1 - i);
	  	}
	  	++i;
	  }
	  return n;
	}

	uint64_t missing(std::vector<uint64_t>& seats)
	{
		for (size_t i = 0; i < seats.size() - 1; ++i)
		{
			if ((seats[i] + 1) != seats[i+1])
			{
				return seats[i] + 1;
			}
		}
		return -1;
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);

		std::vector<uint64_t> seats;
	  std::transform(inputLines.begin(), inputLines.end(),
	  	             std::back_inserter(seats),
	                 [](std::string s) -> uint64_t { return get_seat(s, "BR"); });
		
	  std::sort(seats.begin(), seats.end());

	  AH::PrintSoln(5, seats.back(), missing(seats));

		return 0;
	}

}
