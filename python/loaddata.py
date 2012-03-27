#!/usr/bin/python
import sys
import os
import optparse
import glob
import propertyList
import tempfile

##adapted from examples/required_1.py in the optparse source distribution 
class OptionParser (optparse.OptionParser):

    def check_required (self, opt):
        option = self.get_option(opt)

        # Assumes the option's 'default' is set to None!
        if getattr(self.values, option.dest) is None:
            self.error("%s option not supplied" % option)

def fsplitChromosomes (prefix,outputDir,logonly,chrX,chrlist):
    specfiles=[]
    if os.access(prefix+".ped",os.F_OK):
        inputOption="--file "
        peddatafile=prefix+".ped"
    elif os.access(prefix+".bed",os.F_OK):
        inputOption="--bfile "
        peddatafile=prefix+".fam"
    else:
        sys.exit("Error: No plink input file.\n")
    #print jidfile
    pedcommand="qsub.pl --unquote --jid "+jidfile+" -- 'csv.pl -s --select 0:5 --no-header "+peddatafile+" > "+outputDir+"/pedfile'"
    if logonly==False:
        os.system(pedcommand)
    if chrX:
        plinkcommand=" qsub.pl --jid "+jidfile+" -- plink "+inputOption+prefix+" --chr 23 --recode --out "+outputDir+"/"+prefix+"_chr23"
        if logonly==False:
            os.system(plinkcommand)
        specfiles.append({'name' : outputDir+"/"+prefix+"_chr23", 'chromosome' : "23" })
    else:
        for chrom in chrlist:
            plinkcommand=" qsub.pl --jid "+jidfile+" -- plink "+inputOption+prefix+" --chr "+str(chrom)+" --recode --out "+outputDir+"/"+prefix+"_chr"+str(chrom)
            if logonly==False:
                os.system(plinkcommand)
            specfiles.append({'name' : outputDir+"/"+prefix+"_chr"+str(chrom), 'chromosome' : str(chrom) })
    return specfiles

##main
parser = OptionParser()
parser.add_option("-m", "--mapfile", default=None, help="map file prefix (required)")
parser.add_option("-o", "--output", help="output directory")
parser.add_option("--logonly",action="store_true", default=False)
parser.add_option("--outputSpec", default=None, help="output specification file")
parser.add_option("--chrX",action="store_true", default=False)
parser.add_option("--chromosomes", default="1-22")
(options, args) = parser.parse_args()

parser.check_required("-m")

chrlist=[]
chrs=options.chromosomes.split(",")
for i in chrs:
	j=i.split("-")
	if len(j)==1:
		try:
			if int(j[0])>0 and int(j[0])<=22:
				if int(j[0]) not in chrlist:
					chrlist.append(int(j[0]))
			else:
				print "Skipped non-existing chromosome:",j[0]
		except ValueError:
			print "Skipped non-existing chromosome:",j[0]
	else:
		try:
			if int(j[0])>0 and int(j[1])<=22 and int(j[1])>int(j[0]):
				for k in range(int(j[0]),int(j[1])+1):
					if int(k) not in chrlist:
						chrlist.append(int(k))
			else:
				print "Skipped non-existing chromosome range:",i
		except ValueError:
			print "Skipped non-existing chromosome range:",i

if not options.chrX:
	print "Chromosomes to be analyzed:", chrlist
else:
	print "Chromosome to be analyzed: chr X"
if options.output:
    outputDir=options.output
    if not (os.access(outputDir,os.F_OK)):
        os.mkdir(outputDir)
else:
    outputDir=tempfile.mkdtemp(prefix="gwas_",dir="./")
jidfile=tempfile.mkstemp(prefix="gwasconvertbatchJids",dir="/tmp")[1]

specs={'type':"plink"}
if len(chrlist)>0 or options.chrX:
	specs['files']=fsplitChromosomes(options.mapfile,outputDir,options.logonly,options.chrX,chrlist)
else:
	sys.exit("Error: No chromosomes to run.\n")
if os.access(jidfile,os.F_OK):
    qidfile=open(jidfile)
    specs['jids']=qidfile.readlines()
    for qid in range(len(specs['jids'])):
        specs['jids'][qid]=specs['jids'][qid].strip()
    qidfile.close()
if type(options.outputSpec)!=str:
    specfile=open(outputDir+"/files.spec",'w')
else:
    specfile=open(options.outputSpec,'w')
specfile.write(propertyList.stringFromProperty(specs,0)) 
specfile.close()
