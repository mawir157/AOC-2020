#include "Day07.h"
#include "AH.h"

namespace Day07
{
	class Bag
	{
	public:
		Bag();
		Bag(const std::string& s);
		Bag(const std::string& name,
			  const std::vector<std::pair<std::string, uint64_t>> cts);
	// private:
		std::string Name;
		std::vector<std::pair<std::string, uint64_t>> Contents;
		bool Has(const std::string name) const;
	};

	Bag::Bag(const std::string& s)
	{
		auto parts = AH::SplitOnString(s, " contain ");
		auto parent = parts[0].substr(0, parts[0].size()-5);
		std::vector<std::pair<std::string, uint64_t>> cs;

		if (parts[1].front() != 'n')
		{
			auto children = AH::SplitOnString(parts[1].substr(0, parts[1].size()-1), ", ");			
			for (auto c : children)
			{
				auto temp = AH::Split(c, ' ');
				auto n = std::stoi(temp[0]);
				auto c_name = temp[1] + " " + temp[2];

				cs.emplace_back(c_name, n);
			}
		}

		Name = parent;
		Contents = cs;
	}

	Bag::Bag()
	{};

	Bag::Bag(const std::string& name,
			  const std::vector<std::pair<std::string, uint64_t>> cts) :
		Name(name), Contents(cts)
	{};

	bool Bag::Has(const std::string name) const
	{
		for (auto p : Contents)
		{
			if (std::get<0>(p) == name)
			{
				return true;
			}
		}
		return false;
	}

	std::set<std::string> upstream(const std::vector<Bag>& bags,
		             								const std::string& target) {
		std::set<std::string> contains;

		for (auto b : bags)
		{
			if (b.Has(target))
			{
				contains.insert(b.Name);
			}
		}

		return contains;
	}

	std::set<std::string> mapUpstream(const std::vector<Bag>& bags,
		             									  const std::set<std::string>& targets) {
		std::set<std::string> contains;

		for (auto t : targets)
		{
			auto up = upstream(bags, t);
			contains.insert(up.begin(), up.end());
		}

		return contains;
	}

	uint64_t part1(const std::vector<Bag>& bags, const std::string& base)
	{
		std::set<std::string> parents;
		auto temp = std::set<std::string>{base};
		uint64_t l = 1;

		while (l > 0)
		{
			auto up = mapUpstream(bags, temp);
			parents.insert(up.begin(), up.end());

			l = up.size();
			temp = up;
		}

		return parents.size();
	}

	std::tuple<uint64_t, Bag> collapse(const std::vector<Bag>& bags,
		                                 uint64_t counter,
		                                 const Bag topBag)
	{
		std::vector<std::pair<std::string, uint64_t>> newContents;
		for (auto b : bags)
		{
			for (auto j : topBag.Contents)
			{
				if (b.Name == std::get<0>(j))
				{
					counter += std::get<1>(j);
					for (auto k : b.Contents)
					{
						newContents.emplace_back(std::get<0>(k), std::get<1>(j) * std::get<1>(k));
					}
				}
			}
		}

		return std::make_tuple( counter, Bag(topBag.Name, newContents) );
	}

	uint64_t part2(const std::vector<Bag>& bags, uint64_t counter, Bag topBag)
	{
		while (topBag.Contents.size() > 0)
		{
			auto temp = collapse(bags, counter, topBag);
			counter = std::get<0>(temp);
			topBag = std::get<1>(temp);
		}

		return counter;
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);

		std::vector<Bag> bags;
		Bag sg;
		for (auto l : inputLines)
		{
			auto q = Bag(l);
			bags.emplace_back(l);
			if (q.Name == "shiny gold")
				sg = q;
		}

		AH::PrintSoln(7, part1(bags, "shiny gold"), part2(bags, 0, sg));

		return 0;
	}

}
