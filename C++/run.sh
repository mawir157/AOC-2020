# g++ *.cpp -O2 -o aoc2020 -std=c++17
# ./aoc2020
# rm -rf aoc2020

if [ $# -gt 0 ]
then
  day=$(printf "%02d" $1)
else
  day=-1
fi

if [ $day -gt 0 ]
then
	if test -f Day$day.cpp;
	then
		g++ *.cpp -O2 -o aoc2020 -std=c++17 -DDAY$day -Wall
		./aoc2020
		rm -rf aoc2020
	else
		echo "Day" $day "does not exist"
	fi
else
	missing=""
	COMPILERSTRING=" "
	for i in $(seq -f "%02g" 1 25)
	do
		if test -f Day$i.cpp;
		then
			# go run Day$i.go
			COMPILERSTRING+="-DDAY$i "
		else
			if [ "$missing" = "" ]
			then
				missing=$i
			else
				missing=$missing","$i
			fi
		fi
	done
	g++ *.cpp -O2 -o aoc2020 -std=c++17 $COMPILERSTRING
	./aoc2020
	rm -rf aoc2020
	if [ "$missing" != "" ]
	then
		echo "Missing days = ["$missing"]"
	fi
fi
