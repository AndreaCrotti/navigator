#!/usr/bin/env bash
#filename = data.$program

LANGS="python prolog lisp"
# 
function get_prog {
	case $1 in
		"python" )
			echo "./nav.py";;
		"prolog" )
			echo "./nav.pl";;
		"lisp" )
			echo "./runlisp.sh";;
	esac
}


# FIXME make this bloody thing working
function plot_all {
	S="plot "
	for p in $(echo $LANGS)
		do
		S=$S"\"data.$p\" smooth csplines title \"$p\""
		if [ "$p" != "lisp" ]
			then S=$S", "
		fi
	done
	
gnuplot << EOF
$S
EOF
}

function build_stats {
	# gets as input filename and program to execute, and times 
	FNAME=$1
	PROG=$2
	TIMES=$3
	COMPL=$4
	TOT=0
	# for ((i=0; $i<$TIMES; i=$i+1))
		# do
	T=$((time -p $PROG > /dev/null) 2>&1 | grep 'real' | cut -d ' ' -f 2)
		# TOT=$(($TOT + $T))
	# done
	AVG=$(($TOT / $TIMES)) # gets the avarage
	echo "$COMPL $T	" >> $FNAME
}

function clean {
	rm data.*
	# a preliminar interaction
	cat /dev/null > last.pl
	# ./gen_input.py
	# ./nav.py > /dev/null
	# ./nav.pl > /dev/null
	# ./runlisp.sh > /dev/null
}

function graph_cities {
	# input, <min> <max> <step> <parameter> <other>
	for ((i=$1; $i<$2; i=$i+$3))
		do
		cat /dev/null > last.pl
		cat /dev/null > min_dist.pl
		echo "generates input for $i"
		./gen_input.py $4 $i
		for l in $(echo $LANGS)
			do
			echo $l"==> "
			echo "generating stats"
			build_stats "data.$l" $(get_prog $l) 1 $i
		done
	done
	echo "generating plots"
	plot_all	#statements
}


clean
graph_cities 2 35 1 "-n"
clean
graph_cities 1 100 1 "-r"
clean
# graph_cities 1 10 1 "-g" "-n 25 -r 25"
