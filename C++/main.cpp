#include "days.h"

int main(int argc, char const *argv[])
{
	#ifdef DAY01
	Day01::Run("../input/input01.txt");
	#endif

	#ifdef DAY02
	Day02::Run("../input/input02.txt");
	#endif

	#ifdef DAY03
	Day03::Run("../input/input03.txt");
	#endif

	#ifdef DAY04
	Day04::Run("../input/input04.txt");
	#endif

	#ifdef DAY05
	Day05::Run("../input/input05.txt");
	#endif

	#ifdef DAY06
	Day06::Run("../input/input06.txt");
	#endif

	#ifdef DAY07
	Day07::Run("../input/input07.txt");
	#endif

	#ifdef DAY09
	Day09::Run("../input/input09.txt");
	#endif

	#ifdef DAY10
	Day10::Run("../input/input10.txt");
	#endif

	#ifdef DAY11
	Day11::Run("../input/input11.txt");
	#endif

	#ifdef DAY12
	Day12::Run("../input/input12.txt");
	#endif

	#ifdef DAY13
	Day13::Run("../input/input13.txt");
	#endif

	#ifdef DAY14
	Day14::Run("../input/input14.txt");
	#endif

	#ifdef DAY15
	Day15::Run("NO INPUT FILE");
	#endif

	#ifdef DAY16
	Day16::Run("../input/input16.txt");
	#endif

	#ifdef DAY17
	Day17::Run("../input/input17.txt");
	#endif

	return 0;
}
