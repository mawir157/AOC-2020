if [ $# -gt 0 ]
then
  day=$(printf "%02d" $1)
else
  day=-1
fi

if [ $day -gt 0 ]
then
	if test -f Day$day.go;
	then
		go run Day$day.go
	else
		echo "Day " $day " does not exist"
	fi
else
	for i in $(seq -f "%02g" 1 25)
	do
		if test -f Day$i.go;
		then
			go run Day$i.go
		else
			echo "Day " $i " does not exist"
		fi
	done
fi
