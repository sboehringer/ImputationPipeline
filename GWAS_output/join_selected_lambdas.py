#!/usr/bin/python

import glob
infiles=glob.glob("*lambda.txt")
out1=open("LLS_Offspring_Partners_Final_37_Overlap_Maldi_complexphen_selected_pval1e-6.txt",'w')
outlambda=open("Maldi_allcomplex_lambdas.txt_statuscorr",'w')

i=0
for infile in infiles:
	input1=open(infile)
	input2=open(infile[:infile.rfind('_')]+"_selected_pval1e-6.txt")
	header=input1.readline()
	if i==0:
		outlambda.write(header)
	lambda1=input1.readline()
	outlambda.write(lambda1)
	input1.close()
	header=input2.readline()
	if i==0:
		out1.write(header)
	for line in input2:
		out1.write(line)
	input2.close()
	i+=1
out1.close()
outlambda.close()

