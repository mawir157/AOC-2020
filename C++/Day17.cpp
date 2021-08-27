#include "AH.h"

namespace Day17
{

	typedef std::tuple<int64_t, int64_t, int64_t, int64_t> Pos;

	std::vector<Pos> nbrs(const Pos pt, const int64_t dimn)
	{
		std::vector<Pos> ns;
		ns.reserve(1 << dimn);
		for (int64_t x = -1; x < 2; ++x)
		{
			for (int64_t y = -1; y < 2; ++y)
			{
				for (int64_t z = -1; z < 2; ++z)
				{
					if (dimn == 3)
					{
						if (x == 0 && y == 0 && z == 0)
							continue;

						ns.push_back(std::make_tuple(std::get<0>(pt) + x,
						                             std::get<1>(pt) + y,
						                             std::get<2>(pt) + z, 0));
					}
					if (dimn == 4)
					{
						for (int64_t t = -1; t < 2; ++t)
						{
							if (x == 0 && y == 0 && z == 0 && t == 0)
								continue;

							ns.push_back(std::make_tuple(std::get<0>(pt) + x,
							                             std::get<1>(pt) + y,
							                             std::get<2>(pt) + z,
							                             std::get<3>(pt) + t));
						}
					}
				}
			}
		}

		return ns;
	}

	uint64_t nbrsCount(const Pos pt, const std::set<Pos> space, const uint64_t dimn)
	{
		uint64_t count = 0;
		auto ns = nbrs(pt, dimn);
		for (auto n : ns)
		{
			if ( space.find(n) != space.end() )
				count++;
		}
		return count;
	}

	std::set<Pos> BuildNSpace(const std::vector<std::string> ss)
	{
		std::set<Pos> space;
		int64_t i = 0, j = 0;
		for (auto s : ss)
		{
			for (auto c : s)
			{
				if (c == '#')
				{
					auto tpl = std::make_tuple(i, j, 0, 0);
					space.insert(tpl);
				}
				++j;
			}
			++i;
			j = 0;
		}
		return space;
	}

	std::set<Pos> tick(const std::set<Pos> space, const uint64_t dimn)
	{
		std::set<Pos> newSpace;
		// iterate over the live cells
		for (auto pos : space)
		{
			// check if this cell survives
			auto nCountHere = nbrsCount(pos, space, dimn);
			if (nCountHere == 2 || nCountHere == 3)
				newSpace.insert(pos);

			// no check this point's neighbours
			auto neighbours = nbrs(pos, dimn);
			for (auto nPos : neighbours)
			{
				auto nCountThere = nbrsCount(nPos, space, dimn);
				// we've already checked this point...
				if ( newSpace.find(nPos) != newSpace.end() )
				{
					// do nothing
				}
				// we haven't checked this point
				else
				{
					if ( space.find(nPos) != space.end() )
					{
						if (nCountThere == 2 || nCountThere == 3)
							newSpace.insert(nPos);
					}
					else
					{
						if (nCountThere == 3)
							newSpace.insert(nPos);
					}
				}
			}
		}

		return newSpace;
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		auto space3 = BuildNSpace(inputLines);

		auto space4 = BuildNSpace(inputLines);
		for (size_t i = 0; i < 6; ++i)
		{
			space3 = tick(space3, 3);
			space4 = tick(space4, 4);
		}

		AH::PrintSoln(17, space3.size(), space4.size());

		return 0;
	}

}
