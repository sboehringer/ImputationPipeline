#!/bin/bash
#$ -j Y
#$ -N manhattan
#$ -o qsub_jobOutputs
#$ -S /bin/bash
#$ -cwd
#$ -V
#$ -p -512
/home/qhelmer/epacts/bin/epacts-plot2 -in LLS_Offspring_Partners_Final_37_Overlap_chr10_imputed-1-$1-sorted -out plots_qnorm_$1 -title $1 -jpg

