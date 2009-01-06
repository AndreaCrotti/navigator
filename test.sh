#!/usr/bin/env bash

# i is the number of cities
i=2
while [ $i -lt "10" ]
	do
	j=1
	while [ $j -lt $i ]
		do
		echo "running over $i cities and a path with len $j"
		cat percorso.txt stradario.txt
		./gen_input.py -r $j -n $i stradario.txt percorso.txt
		PY=$(./nav.py)
		LI=$(./runlisp.sh)
		cat /dev/null > last.pl
		cat /dev/null > min_dist.pl
		PL=$(./nav.pl)
		# FIXME test always true, it shouldn't be like that
		if [ "$PY" != "$LI" -o "$PY" != "$PL" -o "$LI" != "$PL" ]
		then
			echo "we have some differences"
			echo "python: $PY\n lisp: $LI\n prolog: $PL"
		else
			echo "$PY"
		fi
		j=$(($j+1))
		echo "**************************************************"
	done
	i=$(($i+1))
done
