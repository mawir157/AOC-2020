#include "Day02.h"
#include "AH.h"

namespace Day02
{

	std::tuple<bool, bool> check_string(const std::string& s)
	{
		auto parts = AH::Split(s, ' ');
		auto lh    = AH::Split(parts[0], '-');
		auto lo    = std::stoi(lh[0]);
		auto hi    = std::stoi(lh[1]);
		auto c     = parts[1].front();// first char
		auto pw    = parts[2];
		auto ct    = std::count(pw.begin(), pw.end(), c); 

		return std::make_tuple( ((ct >= lo) && (ct <= hi)),
		                          ((pw.at(lo-1) == c) != (pw.at(hi-1) == c)) );
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);

	  int p1 = 0, p2 = 0;
	  std::cout << p1 << std::endl;
	  for (auto pw : inputLines)
	  {
	  	auto [ok1, ok2] = check_string(pw);
	  	p1 += ok1 ? 1 : 0;
	  	p2 += ok2 ? 1 : 0;
	  }

	  AH::PrintSoln(2, p1, p2);

		return 0;
	}

}
