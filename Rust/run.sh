cd aoc-2020

if [ $# -gt 0 ]
then
  day=$(printf "%02d" $1)
else
  day=-1
fi

cargo run $day

if [ $day -lt 0 ]
then
	missing=""
	for i in $(seq -f "%02g" 1 25)
	do
		if test ! -f src/day$i.rs;
		then
			if [ "$missing" = "" ]
			then
				missing=$i
			else
				missing=$missing","$i
			fi
		fi
	done
	if [ "$missing" != "" ]
	then
		echo "Missing days = ["$missing"]"
	fi
fi

cd ..