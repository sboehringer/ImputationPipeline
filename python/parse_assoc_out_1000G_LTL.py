#!/usr/bin/python

import glob, sys, gzip

header="SNP\tSTRAND\tBUILD\tCHR\tPOS\tEFFECT_ALLELE\tNON_EFFECT_ALLELE\tN\tN0\tN1\tN2\tEAF\tHWE_P\tCALL_RATE\tBETA\tSE\tPVAL\tIMPUTED\tINFO\n"
filelist=glob.glob(sys.argv[1])
outname=sys.argv[2]
print len(filelist)
infofile=open("../QC_SNPs_LLS_Offspring_Partners_Final_37_Overlap_QCOK.dat") 
infofile.readline() #header
for filename in filelist:
    print "Processing file",filename
    input=open(filename)
    out=gzip.open("LLS_"+outname+"_AUT_10APR13_JD.txt.gz",'w')
    out.write(header)
    outmono=gzip.open("LLS_MONO_"+outname+"_AUT_10APR13_JD.txt.gz",'w')
    outmono.write("SNP\n")
    line=input.readline()
    N=int(line.split()[-2])
    found=True
    #lineno=0
    for line in input: #j in range(len(results1)):
    	#lineno+=1
    	#if lineno==10000:
    	#	break
    	lineg=line.split() #results1[j]
    	if len(lineg)>3:
    		position=int(lineg[2])
    		chromosome=int(lineg[1])
    		while not found and len(ilineg)>3 and ((ilineg[2]==lineg[1] and position>int(ilineg[3])) or int(ilineg[2])<chromosome): #check pos for markers not in lineg
    			ilineg=infofile.readline().split('\t')
    			#print ilineg[:6], lineg[:5]
	    	if found:
    			ilineg=infofile.readline().split('\t')
    			found=False
   			#print ilineg[1],ilineg[-2],ilineg[-1]
    		if len(lineg)>0 and len(ilineg)>0:
    			#print lineg[0]
			if ilineg[2]==lineg[1] and ilineg[3].strip()==lineg[2]:
				found=True
				hwe=ilineg[8]
				callrate=ilineg[6]
				if hwe==' ':
					hwe='.'
				if callrate==' ':
					callrate='1'
			else:
				rsid='.'
				hwe='.'
				callrate='1'
			chrom=lineg[1]
			pos=lineg[2]
			snplabel=lineg[0]
			Nind=N-int(lineg[12])
			if lineg[14]=='100.0':
				typed='0'
			else:
				typed='1'
			info=float(lineg[14])/100.0
			eaf=float(lineg[13])
			if eaf==0 or eaf==1:
				outmono.write(snplabel+'\n')
			else:
				varlist=[snplabel,'+','37',chrom,pos,lineg[4],lineg[3],str(Nind),'.','.','.',lineg[13],hwe,callrate,lineg[15],lineg[16],lineg[11],typed,str(info)]
				out.write('\t'.join(varlist)+'\n')
    out.close()
    outmono.close()
    input.close()
infofile.close()

