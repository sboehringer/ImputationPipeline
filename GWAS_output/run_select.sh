#!/bin/sh

for i in imputation_02_*/LLS_Offspring_Partners_Final_37_Overlap*
do
	#suffix=${i##*_}
	#if [[ $suffix =~ ^[0-9]+$ ]]; then
		#echo ${i%qb*}_lambda.txt
		#if [ ! -f ${i%qb*}qb_lambda.txt ]
		#then
			echo qsub select_parallel.sh $i
			qsub select_parallel.sh $i
		#fi
	#fi
done

