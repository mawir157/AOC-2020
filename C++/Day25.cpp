#include "AH.h"

namespace Day25
{

	uint64_t modPow(uint64_t b, uint64_t a, uint64_t p)
	{
		uint64_t x = 1;

		while (a > 0)
		{
			if (a % 2 == 1)
			{
				a -= 1;
				x *= b;
				x %= p; 
			}
			a /= 2;
			b *= b;
			b %= p;
		}

		return x;
	}

	uint64_t discreteLog(uint64_t b, uint64_t x, uint64_t p)
	{
		uint64_t temp = b;
		uint64_t a = 1;
		while( temp != x)
		{
			temp *= b;
			temp %= p;
			++a;
		}

		return a;
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		uint64_t b = std::stoi(inputLines[0]);
		uint64_t x = std::stoi(inputLines[1]);

		AH::PrintSolnFinal(25, modPow(b, discreteLog(7, x, 20201227), 20201227));

		return 0;
	}

}
