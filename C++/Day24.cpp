#include "AH.h"

namespace Day24
{

	class Pos
	{
	public:
		Pos(const int64_t x, const int64_t y) : X(x), Y(y) {};
		int64_t X;
		int64_t Y;

		std::vector<Pos> nbrs() const;
		uint64_t nbrsCount(const std::set<Pos>& space) const;
		bool operator<(const Pos& rhs) const;
	};

	bool Pos::operator<(const Pos& rhs) const
	{
		if (X != rhs.X)
			return (X < rhs.X);

		if (Y != rhs.Y)
			return (Y < rhs.Y);

		return false;
	}

	std::vector<Pos> Pos::nbrs() const
	{
		std::vector<Pos> ns{Pos(X+1,Y), Pos(X,Y+1),Pos(X-1,Y),Pos(X,Y-1),
		                    Pos(X-1,Y+1),Pos(X+1,Y-1)};

		return ns;
	}

	Pos parseLine(int64_t x, int64_t y, const std::string& s)
	{
		for (size_t i = 0; i < s.size(); ++i)
		{
			auto h = s.at(i);
			if (h == 'e') { x++; }
			else if (h == 'w') { x--; }
			else if (h == 'n')
			{
				i++;
				auto hh = s.at(i);
				if (hh == 'e') { y++; }
				else if (hh == 'w') { x--; y++; }
			}
			else if (h == 's')
			{
				i++;
				auto hh = s.at(i);		
				if (hh == 'e') { x++; y--; }
				else if (hh == 'w') { y--; }		
			}
		}

		return Pos(x, y);  
	}

	uint64_t Pos::nbrsCount(const std::set<Pos>& space) const
	{
		uint64_t count = 0;
		auto ns = nbrs();

		for ( auto n : ns)
			if (space.find(n) != space.end())
				count++;		

		return count;
	}

	void tick(std::set<Pos>& space)
	{
		std::set<Pos> newSpace;
		for ( auto pt : space )
		{
			auto nCount = pt.nbrsCount(space);
			if ((nCount == 1) || (nCount == 2))
				newSpace.insert(pt);

			auto ns = pt.nbrs();
			for ( auto nPos : ns)
			{
				nCount = nPos.nbrsCount(space);
				if (newSpace.find(nPos) != newSpace.end())
				{
					// do nothing
				}
				else
				{
					if (space.find(nPos) != space.end())
					{
						if ((nCount == 1) || (nCount == 2))
							newSpace.insert(nPos);
					}
					else
					{
						if (nCount == 2)
							newSpace.insert(nPos);
					}
				}
			}
		}

		space = newSpace;
		return;
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		std::set<Pos> space;

		for (auto s: inputLines)
		{
			auto pt = parseLine(0,0,s);
			if (space.find(pt) != space.end())
				space.erase(pt);
			else
				space.insert(pt);
		}
		auto part1 = space.size();

		for (size_t i = 0; i < 100; ++i)
			tick(space);

		auto part2 = space.size();;

		AH::PrintSoln(24, part1, part2);

		return 0;
	}

}
