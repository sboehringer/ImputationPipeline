#!/bin/bash

#mkdir qsub_jobOutputs
for j in imputation_02_*sorted
do
	arr=(${j//_/ })
	arr2=${arr[2]}
	arr2split=(${arr2//-/ })
	#echo $arr
	#if [ ! -f plots_qnorm_"${arr[2]}".qq.pdf ]
	#then
		qsub qsub_manhattan_epacts.sh "${arr2split[0]}"
	#fi
done
