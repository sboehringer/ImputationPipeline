#!/bin/bash
#$ -j Y
#$ -N manhattan
#$ -o qsub_jobOutputs
#$ -S /bin/bash
#$ -cwd
#$ -V
#$ -p -512
/home/qhelmer/epacts/bin/epacts-plot2 -in imputation_02_$1-sorted -out plots_qnorm_$1 -title $1 -jpg

