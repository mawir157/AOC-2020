#include "AH.h"

namespace AH
{

    void PrintSolnFinal(const int day, const uint64_t soln1)
    {
        std::cout << "Day "       << day   << std::endl;
        std::cout << "  Part 1: " << soln1 << std::endl;
        std::cout << "  Part 2: " << "See you next year, I love you." << std::endl;

        return;
    }

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

    std::vector<std::string> ParseLineGroups(const std::vector<std::string>& ss,
                                             const char sep)
    {
        std::vector<std::string> lineGroups;

        std::string temp = "";
        for (auto l : ss)
        {
            if (l.length() != 0)
            {
                if (temp.length() != 0)
                {
                    temp += sep;
                }
                temp += l;
            }
            else
            {
                lineGroups.push_back(temp);
                temp = "";
            }
        }
        lineGroups.push_back(temp);

        return lineGroups;
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

    std::vector<std::string> SplitOnString(const std::string &s,
                                           const std::string delim)
    {
        std::vector<std::string> elems;
        std::string scopy(s);

        size_t pos = 0;
        std::string token;
        while ((pos = scopy.find(delim)) != std::string::npos) {
            token = scopy.substr(0, pos);
            elems.push_back(token);
            scopy.erase(0, pos + delim.length());
        }

        if (scopy.length() > 0)
        {
            elems.push_back(scopy);
        }

        return elems;
    }

    uint64_t IntPow(const uint64_t x, const uint64_t p)
    {
        if (p == 0)
        {
            return 1;
        }
        if (p == 1)
        {
            return x;
        }
      
        int tmp = IntPow(x, p/2);
        
        if (p%2 == 0)
        {
            return tmp * tmp;
        }
        else
        {
            return x * tmp * tmp;
        }
    }

}