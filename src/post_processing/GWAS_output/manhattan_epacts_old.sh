#!/bin/bash

#mkdir qsub_jobOutputs
for j in *sorted
do
	arr=(${j//-/ })
	#arr2=${arr[2]}
	#arr2split=(${arr2//-/ })
	#echo $arr
	#if [ ! -f plots_qnorm_"${arr[2]}".qq.pdf ]
	#then
		qsub qsub_manhattan_epacts.sh "${arr[2]}"
	#fi
done
