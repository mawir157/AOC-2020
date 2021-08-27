#include "AH.h"

namespace Day11
{
	struct Pos
	{
	public:
		Pos(const int X, const int Y);

		int X,Y;
	};

	Pos::Pos(const int X, const int Y) :
		X(X), Y(Y)
	{}

	std::vector<Pos> dirs {Pos(-1,-1), Pos(-1, 0), Pos(-1, 1),
	                       Pos( 0,-1),             Pos( 0, 1),
	                       Pos( 1,-1), Pos( 1, 0), Pos( 1, 1)};

	enum seat { none = 1, full, open };

	typedef	std::vector<std::vector<seat>> SeatingPlan;

	std::tuple<SeatingPlan, uint64_t>
	                              BuildSeatMap(const std::vector<std::string>& ss)
	{
		uint64_t filled = 0;
		auto rows = ss.size();
		auto cols = ss[0].length();
		SeatingPlan sp(rows, std::vector<seat> (cols));

		int i = 0, j = 0;
		for (auto s : ss)
		{
			j = 0;
			for (auto c : s)
			{
				if (c == '.')
				{
					sp[i][j] = seat::none;
				}
				else if (c == '#')
				{
					sp[i][j] = seat::full;
					filled++; 
				}
				else if (c == 'L')
				{
					sp[i][j] = seat::open;
				}
				j++;
			}
			i++;
		}

		return { sp, filled };
	}

	bool LookInDir(const SeatingPlan& seats, const size_t row, const size_t col,
	               const size_t dr, const size_t dc, const size_t upto)
	{
		auto r = row;
		auto c = col;
		for (size_t i = 0; i < upto; ++i)
		{
			r += dr;
			c += dc;
			if ((r >= 0) && (c >= 0) && (r < seats.size()) && (c < seats[0].size()))
			{
				auto status = seats[r][c];
				switch(status)
				{
					case seat::none: // void - do nothing
						break;
					case seat::full: // full
						return true;
						break;
					case seat::open: // open
						return false;
						break;
				}
			} else {
				return false; //we've gone off the end of the grid without seeing a seat
			}
		}
		return false;
	}

	uint64_t Update(const SeatingPlan& seats, SeatingPlan& spNew,
	                const uint64_t sight, const uint64_t threshold)
	{
		uint64_t filled = 0;

		for (size_t r = 0; r < seats.size(); ++r)
		{
			for (size_t c = 0; c < seats[0].size(); ++c)
			{
				auto status = seats[r][c];
				size_t nbrs = 0;
				for (auto d : dirs)
				{
					nbrs += (LookInDir(seats, r, c, d.X, d.Y, sight) ? 1 : 0);
				}
				switch(status)
				{
					case seat::none:
						spNew[r][c] = seat::none;
						break;
					case seat::full:
						if (nbrs >= threshold)
						{
							spNew[r][c] = seat::open;
						}
						else
						{
							spNew[r][c] = seat::full;
							filled++;
						}
						break;
					case seat::open:
						if (nbrs == 0)
						{
							spNew[r][c] = seat::full;
							filled++;
						}
						else
						{
							spNew[r][c] = seat::open;
						}
						break;
				}
			}
		}

		return filled;
	}

	uint64_t UpdateUntilStable(const SeatingPlan& seats, const int sight,
	                           const int threshold)
	{
		auto buffer1 = seats;
		auto buffer2 = seats;
		uint64_t prev = 0;

		auto cur = Update(buffer1, buffer2, sight, threshold);

		while (cur != prev)
		{
			prev = cur;
			buffer1 = buffer2;
			cur = Update(buffer1, buffer2, sight, threshold);
		}

		return cur;
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		auto [sp, filled] = BuildSeatMap(inputLines);

		AH::PrintSoln(11, UpdateUntilStable(sp, 1, 4),
		                  UpdateUntilStable(sp, 1000, 5));

		return 0;
	}

}
