#include "AH.h"

namespace Day12
{

	typedef std::tuple<char, int> Instruction;

	struct Pos
	{
	public:
		Pos();
		Pos(const int X, const int Y);

		int X,Y;
	};

	Pos::Pos(const int X, const int Y) :
		X(X), Y(Y)
	{}

	Pos::Pos() : X(0), Y(0) {}

	struct Robot
	{
	public:
		Pos Locn;
		int Dir;
		Pos WayP;

		Robot(const int x, const int y, const int dir);
		Robot(const int x, const int y, const int dir, const int wx, const int wy);

		void move(const Instruction ins);
		void moveWithWayPoint(const Instruction ins);
		int dist() const { return std::abs(Locn.X) + std::abs(Locn.Y); }

	};

	Robot::Robot(const int x, const int y, const int dir)
	{
		Locn = Pos(x,y);
		Dir = dir;
		WayP = Pos(0, 0);
	}

		Robot::Robot(const int x, const int y, const int dir,
		             const int wx, const int wy)
	{
		Locn = Pos(x,y);
		Dir = dir;
		WayP = Pos(wx, wy);
	}

	void Robot::move(const Instruction ins)
	{
		auto code  = std::get<0>(ins);
		auto value = std::get<1>(ins);
		if (code == 'N')
			Locn.Y += value;
		else if (code == 'S')
			Locn.Y -= value;
		else if (code == 'E')
			Locn.X += value;
		else if (code == 'W')
			Locn.X -= value;
		else if (code == 'L')
			Dir = (Dir + value) % 4;
		else if (code == 'R')
			Dir = (4 + Dir - value) % 4;
		else if (code == 'F')
		{
			switch(Dir)
			{
				case 0:
					Locn.X += value;
					break;
				case 1:
					Locn.Y += value;
					break;
				case 2:
					Locn.X -= value;
					break;
				case 3:
					Locn.Y -= value;
					break;
			}
		}
		return;
	}

	void Robot::moveWithWayPoint(const Instruction ins)
	{
		auto code  = std::get<0>(ins);
		auto value = std::get<1>(ins);
		if (code == 'N')
			WayP.Y += value;
		else if (code == 'S')
			WayP.Y -= value;
		else if (code == 'E')
			WayP.X += value;
		else if (code == 'W')
			WayP.X -= value;
		else if (code == 'L')
		{
			switch (value)
			{
				case 1:
					std::swap(WayP.X, WayP.Y);
					WayP.X *= -1;
					break;
				case 2:
					WayP.X *= -1; WayP.Y *= -1;
					break;
				case 3:
					std::swap(WayP.X, WayP.Y);
					WayP.Y *= -1;
					break;
			}
		}
		else if (code == 'R')
		{
			switch (value)
			{
				case 1:
					std::swap(WayP.X, WayP.Y);
					WayP.Y *= -1;
					break;
				case 2:
					WayP.X *= -1; WayP.Y *= -1;
					break;
				case 3:
					std::swap(WayP.X, WayP.Y);
					WayP.X *= -1;
					break;
			}
		}
		else if (code == 'F')
		{
			Locn.X += value * WayP.X;
			Locn.Y += value * WayP.Y;
		}
		return;
	}

	Instruction parseLine(const std::string& s)
	{
		auto r = s.front();
		auto rs = s.substr(1);
		auto n = std::stoi(rs);

		if ((r == 'L') || (r == 'R'))
			n /= 90;

		return {r, n};
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		std::vector<Instruction> ins;
		std::transform(inputLines.begin(), inputLines.end(),
		               std::back_inserter(ins),
		               [](const std::string s) -> Instruction { return parseLine(s); });

		Robot starbug(0, 0, 0);
		for (auto i : ins)
			starbug.move(i);

		Robot firefly(0,0,0,10,1);
		for (auto i : ins)
			firefly.moveWithWayPoint(i);

		AH::PrintSoln(12, starbug.dist(), firefly.dist());

		return 0;
	}

}
