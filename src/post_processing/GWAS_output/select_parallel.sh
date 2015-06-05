#!/bin/sh
# Options
#$ -q MolEpi_HighMem.q
#$ -cwd 
#$ -V 
#$ -S /bin/sh

echo $1
./selectsign_qc_assoc_output_lambda_parallel.py $1

