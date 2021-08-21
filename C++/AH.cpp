#include "AH.h"

namespace AH
{

    void PrintSoln(const int day, const uint64_t soln1, const uint64_t soln2)
    {
        std::cout << "Day "       << day   << std::endl;
        std::cout << "  Part 1: " << soln1 << std::endl;
        std::cout << "  Part 2: " << soln2 << std::endl;

        return;
    }

    std::vector<std::string> ReadTextFile(const std::string& filename)
    {
        std::string line;
        std::vector<std::string> lines;
        lines.reserve(10000);

        std::ifstream data_file(filename);

        while(getline(data_file, line))
        {
            lines.push_back(line);
        }

        data_file.close();

        return lines;
    }

    template <typename Out>
    void split(const std::string &s, char delim, Out result)
    {
        std::istringstream iss(s);
        std::string item;
        while (std::getline(iss, item, delim))
        {
            *result++ = item;
        }
    }

    std::vector<std::string> Split(const std::string &s, char delim)
    {
        std::vector<std::string> elems;
        AH::split(s, delim, std::back_inserter(elems));
        return elems;
    }


}