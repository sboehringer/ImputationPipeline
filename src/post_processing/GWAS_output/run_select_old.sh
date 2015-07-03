#!/bin/sh

j=2
while [ $j -lt 263 ] #set here the number of phenotypes * 2 + 1
do
	if [ $j -lt 10 ]
	then
		i=imputation_0$j/LLS_Offspring_Partners_Final_37_Overlap*
	else
		i=imputation_$j/LLS_Offspring_Partners_Final_37_Overlap*
	fi
	#suffix=${i##*_}
	#if [[ $suffix =~ ^[0-9]+$ ]]; then
		#echo ${i%qb*}_lambda.txt
		#if [ ! -f ${i%qb*}qb_lambda.txt ]
		#then
			#echo qsub select_parallel.sh $i
			qsub select_parallel.sh $i
		#fi
	#fi
	let j=$j+2
done

