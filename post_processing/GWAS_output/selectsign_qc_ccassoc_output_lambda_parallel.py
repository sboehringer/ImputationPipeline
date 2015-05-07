#!/usr/bin/python

import sys,glob,os.path

def median(alist):

    srtd = sorted(alist,key=lambda x: x[9]) # returns a sorted copy
    mid = len(alist)/2   # remember that integer division truncates

    if len(alist) % 2 == 0:  # take the avg of middle two
        return (srtd[mid-1][9] + srtd[mid][9]) / 2.0
    else:
        return srtd[mid][9]

#dataset=sys.argv[1] #"imputation_02/LLS_Offspring_Partners_Final_37_Overlap_m_phen1.qtassoc_rsnumbers.gz"
R2Tmin=40.0 #sets R2T threshold (snps with R2T lower than threshold will not appear in output)
mafmin=0.01 #sets allele frequency threshold (snps with maf in controls or cases lower than threshold will not appear in output)
pval=0.000001 #sets p-value threshold 

infile=sys.argv[1]
lines=open(infile)
header=lines.readline()
outfile=infile[:infile.index("qb")+2]+"-sorted"
out2=open(outfile.split('/')[-1],'w')
out2.write(header)
mainlist=[]
lambdalist=[]
for line in lines:
	lineg=line.split()
	if float(lineg[16]) >= R2Tmin and float(lineg[14]) >= mafmin and float(lineg[14]) <= 1-mafmin and float(lineg[15]) >= mafmin and float(lineg[15]) <= 1-mafmin:
		lineg[9]=float(lineg[9]) #test_stat_loss
		lambdalist.append(lineg)
		if float(lineg[11]) <= pval:
			mainlist.append(lineg)
		out2.write(line)
lines.close()
out2.close()		
print "After selection",len(mainlist),"markers left"

outfile2=infile[:infile.index("qb")+2]+"_lambda.txt"
outl=open(outfile2.split('/')[-1],'w')
lambd=median(lambdalist)/0.456
outl.write("phenotype\tlambda\n")
outl.write(outfile.split("-")[-2]+"\t")
outl.write(str(lambd)+'\n')
outl.close()

outfile2=infile[:infile.index("qb")+2]+"_selected_pval1e-6.txt"
out=open(outfile2.split('/')[-1],'w')
out.write(header.split('\t')[0]+" phenotype\n")
mainlist.sort(key=lambda x: x[9], reverse=True)
for line in mainlist:
	line[9]=str(line[9]) #test_stat_loss
	out.write(' '.join(line)+' '+outfile.split("-")[-2]+'\n')
out.close()

