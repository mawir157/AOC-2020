#include "AH.h"

namespace Day23
{

	class Game
	{
	public:
		Game(const uint64_t size, const std::vector<uint64_t>& input);
		void runFor(const uint64_t iters);
		uint64_t output(const bool part1=true) const;
		
	private:
		void tick();

		std::unordered_map<uint64_t, uint64_t> Cups;
		uint64_t Pointer;
		uint64_t MaxEm;
	};

	Game::Game(const uint64_t size, const std::vector<uint64_t>& input)
	{
		if (size == input.size())
		{
			for (size_t i = 0; i < input.size(); ++i)
				Cups[input[i]] = input[(i+1) % input.size()];
		}
		else
		{
			for (size_t i = 0; i < input.size() - 1; ++i)
				Cups[input[i]] = input[i+1];

			Cups[input[input.size() - 1]] = input.size() + 1;
			for (size_t i = input.size(); i < size; ++i)
				Cups[i+1] = i + 2;

			Cups[size] = input[0];
		}
		Pointer = input[0];
		MaxEm = size;
	}

	void Game::tick()
	{
		auto nbr1 = Cups.at(Pointer);
		auto nbr2 = Cups.at(nbr1);
		auto nbr3 = Cups.at(nbr2);
		auto nbr4 = Cups.at(nbr3);

		uint64_t dest = 0;
		if (Pointer > 1)
			dest = Pointer - 1;
		else
			dest = MaxEm;

		while ((dest == nbr1) || (dest == nbr2) || (dest == nbr3))
		{
			if (dest > 1)
				--dest;
			else
				dest = MaxEm;
		}
		auto postDest = Cups.at(dest);

		Cups[Pointer] = nbr4;
		Cups[dest]    = nbr1;
		Cups[nbr3]    = postDest; 

		Pointer = nbr4;
	}

	void Game::runFor(const uint64_t iters)
	{
		for (size_t i = 0; i <  iters; ++i)
			tick();
	}

	uint64_t Game::output(const bool part1) const
	{
		uint64_t ret = 0;
		if (part1)
		{
			for (uint64_t i = 1; ; )
			{
				auto v = Cups.at(i);

				if (v == 1)
					break;

				ret *= 10;
				ret += v;

				i = v;
			}
		}
		else
		{
			auto nbr1 = Cups.at(1);
			auto nbr2 = Cups.at(nbr1);	

			ret = nbr1 * nbr2;		
		}

		return ret;
	}

	int Run(const std::string& filename)
	{
		std::vector<uint64_t> input{5,2,3,7,6,4,8,1,9};
		Game game1(9, input);
		Game game2(1000000, input);

		game1.runFor(100);
		game2.runFor(10000000);

		AH::PrintSoln(23, game1.output(true), game2.output(false));

		return 0;
	}

}
