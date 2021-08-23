#include "AH.h"

namespace Day11
{
	struct Pos
	{
	public:
		Pos(const int X, const int Y);

		int X,Y;
		void shift(const Pos& dir);
		
		bool operator <(const Pos& rhs) const
	  {
	    if (this->X != rhs.X)
	    {
	     	return (this->X < rhs.X);
	    }

			if (this->Y != rhs.Y)
	    {
	     	return (this->Y < rhs.Y);
	    }

	    return false;
	  }

	};

	Pos::Pos(const int X, const int Y) :
		X(X), Y(Y)
	{}

	void Pos::shift(const Pos& dir)
	{
		X += dir.X;
		Y += dir.Y;
	}

	std::vector<Pos> dirs {Pos(-1,-1), Pos(-1, 0), Pos(-1, 1),
										  	 Pos( 0,-1),             Pos( 0, 1),
												 Pos( 1,-1), Pos( 1, 0), Pos( 1, 1)};

	enum seat { none = 1, full, open };

	uint64_t BuildSeatMap(const std::vector<std::string> ss,
		                    std::map<Pos, size_t>& seats)
	{
		uint64_t filled = 0;
		int i = 0, j = 0;
		for (auto s : ss)
		{
			j = 0;
			for (auto c : s)
			{
				Pos temp = Pos(i,j);
				if (c == '.')
				{
					// seats[temp] = seat::none; // Void
				}
				else if (c == '#')
				{
					seats[temp] = seat::full; // full
					filled++; 
				}
				else if (c == 'L')
				{
					seats[temp] = seat::open; // open
				}
				j++;
			}
			i++;
		}

		return filled;
	}

	bool LookInDir(const std::map<Pos, size_t> seats,
		             const Pos pos, const Pos dir, const size_t upto)
	{
		auto temp = Pos(pos.X, pos.Y);
		for (size_t i = 0; i < upto; ++i)
		{
			temp.shift(dir);
			if (seats.count(temp) == 1)
			{
				auto status = seats.find(temp)->second;
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
				return false; // we've gone off the end of the grid without seeing a seat
			}
		}
		return false;
	}

	uint64_t Update(std::map<Pos, size_t>& seats,
		              const int sight, const int threshold)
	{
		std::map<Pos, size_t> newSeats;
		uint64_t filled = 0;

		for (auto [pos, status] : seats)
		{
			size_t nbrs = 0;
			for (auto d : dirs)
			{
				nbrs += (LookInDir(seats, pos, d, sight) ? 1 : 0);
			}

			switch(status)
			{
				case seat::none:
					// newSeats[pos] = seat::none;
					break;
				case seat::full:
					if (nbrs >= threshold)
					{
						newSeats[pos] = seat::open;
					}
					else
					{
						newSeats[pos] = seat::full;
						filled++;
					}
					break;
				case seat::open:
					if (nbrs == 0)
					{
						newSeats[pos] = seat::full;
						filled++;
					}
					else
					{
						newSeats[pos] = seat::open;
					}
					break;
			}
		}
		seats = newSeats;

		return filled;
	}

	uint64_t UpdateUntilStable(std::map<Pos, size_t>& seats,
		                         const uint64_t prev,
		                         const int sight,
		                         const int threshold){
		auto cur = Update(seats, sight, threshold);
		std::cout << cur << std::endl;
		if (cur == prev) {
			return cur;
		} else {
			return UpdateUntilStable(seats, cur, sight, threshold);
		}
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		std::map<Pos, size_t> seats;
		auto filled = BuildSeatMap(inputLines, seats);

		std::cout << Update(seats, 1, 4) << std::endl;
		// std::cout << std::endl;

		// size_t nbrs = 0;
		// for (auto d : dirs)
		// {
		// 	nbrs += (LookInDir(seats, Pos(0,0), d, 1) ? 1 : 0);
		// 	// std::cout<< nbrs << "->";
		// }

		// std::cout << Update(seats, 1, 4) << std::endl;
		// std::cout << Update(seats, 1, 4) << std::endl;

		AH::PrintSoln(11, UpdateUntilStable(seats, filled, 1, 4), 2);

		return 0;
	}

}
