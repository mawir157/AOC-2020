#include "AH.h"

namespace Day16
{
	typedef std::vector<uint64_t> Ticket;


	class DoubleRange
	{
	public:
		DoubleRange(const uint64_t ll, const uint64_t lh,
			          const uint64_t rl, const uint64_t rh) :
		LhsLo (ll), LhsHi(lh), RhsLo(rl), RhsHi(rh) {}
		bool Ok(const uint64_t i) const { return (LhsLo <= i && i <= LhsHi) ||
			                                       (RhsLo <= i && i <= RhsHi); }
		void Possible(const Ticket& ticket);
		void Delete(const uint64_t item);
		size_t size() const { return ValidIndices.size(); }
		uint64_t first() const { return ValidIndices[0]; }

	private:
		const uint64_t LhsLo;
		const uint64_t LhsHi;
		const uint64_t RhsLo;
		const uint64_t RhsHi;
		
		std::vector<uint64_t> ValidIndices{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
			                                 10,11,12,13,14,15,16,17,18,19};
	};

	void DoubleRange::Possible(const Ticket& ticket)
	{
		std::vector<uint64_t> newValid;
		newValid.reserve(ValidIndices.size());
		for (auto i : ValidIndices)
			if (Ok(ticket[i]))
				newValid.push_back(i);

		ValidIndices = newValid;
	}

	void DoubleRange::Delete(const uint64_t item)
	{
		ValidIndices.erase(std::remove(ValidIndices.begin(), ValidIndices.end(), item),
			                 ValidIndices.end());
	}

	std::vector<uint64_t> GetInvalidFields(const std::vector<DoubleRange>& intervals,
		                                     const std::vector<uint64_t>& ticket)
	{
		std::vector<uint64_t> invalid;
		for (auto v : ticket)
		{
			bool ok = false;
			for (auto i : intervals)
				ok |= i.Ok(v);

			if (!ok)
				invalid.push_back(v);
		}

		return invalid;
	}

	uint64_t part1(const std::vector<DoubleRange>& intervals,
                 const std::vector<Ticket>& tickets)
	{
		uint64_t invalid = 0;
		for (auto t : tickets)
		{
			auto inv = GetInvalidFields(intervals, t);
			for (auto v : inv)
				invalid += v;
		}

		return invalid;
	}

	uint64_t part2(std::vector<DoubleRange>& intervals,
                 const std::vector<Ticket>& tickets,
                 const std::vector<uint64_t>& me)
	{
		uint64_t prod = 1;
		std::vector<Ticket> validTickets;
		// step 1 remove the invalid tickets
		for (auto t :  tickets)
			if (GetInvalidFields(intervals, t).size() == 0)
				validTickets.push_back(t);

		// step 2 for each range find the valid indices
		for (auto& i : intervals)
			for (auto t : validTickets)
				i.Possible(t);

		// step 3 reduce the valids indices to singletons
		std::vector<uint64_t> outIndices(20);
		for (size_t i = 0; i < 20; ++i)
		{
			uint64_t kill = 1000;
			// find a Double Range with only on possible
			for (size_t j = 0; j < intervals.size(); ++j)
			{
				auto q = intervals[j];
				if (q.size() == 1)
				{
					outIndices[q.first()] = j;
					kill = q.first();
					break;
				}
			}
			// now remove this value from all the other invervals
			for (auto& in : intervals)
				in.Delete(kill);
		}

		for (size_t i; i < outIndices.size(); ++i)
			if (outIndices[i] < 6)
				prod *= me[i];

		return prod;
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		std::vector<DoubleRange> intervals;
		std::vector<uint64_t> me;
		std::vector<std::vector<uint64_t>> tickets;

		for (size_t i = 0; i < inputLines.size(); ++i)
		{
			if (i < 20)
			{
				auto parts = AH::SplitOnString(inputLines[i], ": ");
				auto pair  = AH::SplitOnString(parts[1], " or ");
				auto lhs   = AH::Split(pair[0], '-');
				auto rhs   = AH::Split(pair[1], '-');

				DoubleRange range(std::stoi(lhs[0]), std::stoi(lhs[1]),
					                std::stoi(rhs[0]), std::stoi(rhs[1]));

				intervals.push_back(range);
			}
			else if (i == 22)
			{
				auto parts = AH::Split(inputLines[i], ',');
				for (auto p : parts)
				{
					uint64_t j = std::stoi(p);
					me.push_back(j);
				}
			}
			else if (i > 24)
			{
				auto parts = AH::Split(inputLines[i], ',');
				std::vector<uint64_t> tempTicket;
				for (auto p : parts)
				{
					uint64_t j = std::stoi(p);
					tempTicket.push_back(j);
				}
				tickets.push_back(tempTicket);
			}
		}

		AH::PrintSoln(16, part1(intervals, tickets), part2(intervals, tickets, me));

		return 0;
	}

}
