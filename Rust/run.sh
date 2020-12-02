if [ $# -gt 0 ]
then
  day=$(printf "%02d" $1)
else
  day=-1
fi

if [ $day -gt 0 ]
then
	if test -f Day$day.rs;
	then
		rustc Day$day.rs
		./Day$day
		rm Day$day
	else
		echo "Day " $day " does not exist"
	fi
else
	missing=""
	for i in $(seq -f "%02g" 1 25)
	do
		if test -f Day$i.rs;
		then
			rustc Day$i.rs > /dev/null
			./Day$i
			rm Day$i
		else
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
