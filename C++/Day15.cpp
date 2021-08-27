#include "AH.h"

namespace Day15
{
	std::map<uint64_t, uint64_t> makeGameState(const std::vector<uint64_t>& inputs)
	{
		std::map<uint64_t, uint64_t> gameState;
		for (uint64_t i = 0; i < inputs.size(); ++i)
			gameState[inputs[i]] = i + 1;

		return gameState;
	}

	uint64_t playGame(std::map<uint64_t, uint64_t>& game, uint64_t n,
	                  bool seen, const uint64_t turns, const uint64_t offset)
	{
		uint64_t next = 0;
		for (uint64_t turn = offset; turn <= turns; ++turn)
		{
			if (seen)
				next = turn - game[n] - 1;
			else
				next = 0;

			seen = ( game.find(next) != game.end() );
			game[n] = turn - 1;
			n = next;
		}

		return n;
	}

	int Run(const std::string& filename)
	{
		std::vector<uint64_t> input{7,14,0,17,11,1,2};
		auto q1 = makeGameState(input);
		auto q2 = makeGameState(input);
		uint64_t init = 0;

		bool seenZero = ( q1.find(0) != q1.end() );
		if (seenZero)
			init = input.back();

		auto s = playGame(q1, init, seenZero, 2020,     input.size() + 1);
		auto t = playGame(q2, init, seenZero, 30000000, input.size() + 1);

		AH::PrintSoln(15, s, t);

		return 0;
	}

}
