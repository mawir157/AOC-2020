#include "AH.h"

namespace Day22
{
	typedef std::deque<uint64_t> Deck;

	class Game
	{
	public:
		Game(const Deck& d1, const Deck& d2);
		uint64_t score() const;
		void playGame1();
		bool playGame2();
		
	private:
		void playHand1();
		void playHand2();
		uint64_t deckScore(const Deck& d) const;
		Deck d1;
		Deck d2;
	};

	Game::Game(const Deck& d1, const Deck& d2) : d1(d1), d2(d2) {}

	void Game::playHand1()
	{
		if (d1.size() == 0 || d2.size() == 0)
			return;

		auto f1 = d1.front();
		auto f2 = d2.front();
		if (d1[0] > d2[0])
		{
			d1.pop_front();  d1.push_back(f1);  d1.push_back(f2);
			d2.pop_front();
		}
		else
		{
			d1.pop_front();    
			d2.pop_front();  d2.push_back(f2);  d2.push_back(f1);
		}
	}

	void Game::playGame1()
	{
		while ((d1.size() > 0) && (d2.size() > 0))
			playHand1();

		return;
	}

	uint64_t Game::deckScore(const Deck& d) const
	{
		auto m = d.size();
		uint64_t total = 0;
		for (uint64_t i = 0; i < m; ++i)
			total += (m-i) * d[i];

		return total;
	}

	uint64_t Game::score() const
	{
		if (d1.size() > 0)
			return deckScore(d1);
		else
			return deckScore(d2);
	}

	void Game::playHand2()
	{
		if (d1.size() == 0 || d2.size() == 0)
			return;		

		auto f1 = d1.front();
		auto f2 = d2.front();

		if ((f1 < d1.size()) && (f2 < d2.size()))
		{
			auto d1Copy = d1;
			d1Copy.pop_front();
			d1Copy.resize(f1);
			auto d2Copy = d2;
			d2Copy.pop_front();
			d2Copy.resize(f2);

			Game subG(d1Copy, d2Copy);
			auto p1Wins = subG.playGame2();
			
			if (p1Wins)
			{
				d1.pop_front();  d1.push_back(f1);  d1.push_back(f2);
				d2.pop_front();
			}
			else
			{
				d1.pop_front();    
				d2.pop_front();  d2.push_back(f2);  d2.push_back(f1);
			}
		} else {
			if (f1 > f2)
			{
				d1.pop_front();  d1.push_back(f1);  d1.push_back(f2);
				d2.pop_front();
			}
			else
			{
				d1.pop_front();    
				d2.pop_front();  d2.push_back(f2);  d2.push_back(f1);
			}	
		}
		return;
	}

	bool seenBefore(const Deck& cur, const std::vector<Deck>& prev)
	{
		for (auto dq : prev)
		{
			if (dq.size() != cur.size())
				continue;
			bool ok = true;
			for (size_t i = 0; i < dq.size(); ++i)
			{
				if (cur[i] != dq[i])
				{
					ok = false;
					break;
				}
			}
			if (ok)
				return true;
		}
		return false;
	}

	bool Game::playGame2()
	{
		std::vector<Deck> prevD1s;
		std::vector<Deck> prevD2s;

		while (true)
		{
			if ((d1.size() == 0) || (d2.size() == 0))
				return (d2.size() == 0);

			if ( seenBefore(d1, prevD1s) || seenBefore(d2, prevD2s) )
				return true;

			prevD1s.push_back(d1);
			prevD1s.push_back(d2);

			playHand2();
		}
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		bool player1 = false;
		Deck deck1;
		Deck deck2;

		for (auto s: inputLines)
		{
			if (s.front() == 'P')
			{
				player1 = !player1;
				continue;
			}

			if (s.length() == 0)
				continue;

			uint64_t n = std::stoi(s);

			if (player1)
				deck1.push_back(n);
			else
				deck2.push_back(n);
		}

		Game g1(deck1, deck2);
		g1.playGame1();

		Game g2(deck1, deck2);
		g2.playGame2();

		AH::PrintSoln(22, g1.score(), g2.score());

		return 0;
	}

}
