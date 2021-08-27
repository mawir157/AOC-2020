#include "AH.h"

namespace Day09
{
	bool test(const std::vector<uint64_t>& arr, const size_t offset,
		        const size_t size)
	{
		if (arr.size() < offset + size + 1)
		{
			return false;
		}

		for (size_t i = 0; i < size; ++i)
		{
			for (size_t j = i + 1; j < size; ++j)
			{
				if (arr[offset + i] + arr[offset + j] == arr[offset + size])
				{
					return true;
				}
			}
		}

		return false;
	}
	std::tuple<size_t, size_t> findRange(const std::vector<uint64_t>& arr, 
		                                   const size_t target,
		                                   const size_t blockStart,
		                                   const size_t blockSize)
	{
		uint64_t s = 0;
		for (size_t i = blockStart; i < blockStart + blockSize; ++i)
		{
			s += arr[i];
		}

		if (s == target)
		{
			return std::make_tuple( blockStart, blockSize);
		}
		else if (s > target)
		{
			return findRange(arr, target, blockStart + 1, 2);
		}
		else
		{
			return findRange(arr, target, blockStart, blockSize + 1);
		}
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		size_t size = 117;

		std::vector<uint64_t> values;
		std::transform(inputLines.begin(), inputLines.end(),
		               std::back_inserter(values),
		               [](std::string s) -> uint64_t { return std::strtoull(s.c_str(), NULL, 0); });

		uint64_t part1 = 0;
		for (size_t i = 0; i < values.size(); ++i)
		{
			if (!test(values, i, size))
			{
				part1 = values[i + size];
				break;
			}
		}

		auto [a,b] = findRange(values, part1, 0, 2);

		uint64_t min = 1, max = 0;
		min <<= 62;
		for (size_t i = a; i < a + b; ++i)
		{
			if (values[i] < min)
				min = values[i];
			else if (values[i] > max)
				max = values[i];
		}

		AH::PrintSoln(9, part1, max + min);

		return 0;
	}
}
