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

hapmap3 ="/data/data0/hapmap3/hapmap3map.csv"
hapmap2 ="/data/data0/hapmap2/phased/genotypes_CEU_r21_nr_fwd_legend"
hapmap2 = "/home/pingu/tmp/pipeline/hapmap2/genotypes_CEU_r21_nr_fwd_legend"
hapmap3 = "/home/pingu/tmp/pipeline/hapmap3/hapmap3map.csv"
hapmap2 = "/home/pingu/tmp/pipeline/hapmap2/all_markers.map"
hapmap3 = "/home/pingu/tmp/pipeline/hapmap3/all_markers.map"

def runCsv2(input,hapmap,outputdir,jids):
    wait=''
    if len(jids)>0:
        wait="--waitForJids '"+','.join(jids)+"'"
    outfile=input.split('/')[-1]
    if hapmap=="hapmap2":
        qsubcsv2="qsub.pl "+wait+" --jid "+jidfile+" -- csv2.pl --no-outputHeader --sepo T --o "+outputdir+"/"+outfile+".map -- [sep=S,header=T]:"+hapmap2+" [sep=T,header=F]:"+input+".map --op setHeader=chr,idRs,map,pos0 --op joinOuterLeft --op expr='TTOP$pos[is.na(TTOP$pos)] = TTOP$pos0[is.na(TTOP$pos)]' --op project=chr,idRs,map,pos"
    elif hapmap=="hapmap3":
        qsubcsv2="qsub.pl "+wait+" --jid "+jidfile+" -- csv2.pl --no-outputHeader --sepo T --o "+outputdir+"/"+outfile+".map -- [sep=\',\',header=T]:"+hapmap3+" [sep=T,header=F]:"+input+".map --op setHeader=chr,idRs,map,pos0 --op joinOuterLeft --op expr='TTOP$pos[is.na(TTOP$pos)] = TTOP$pos0[is.na(TTOP$pos)]' --op project=chr,idRs,map,pos"
    else:
        sys.exit("Non-existing hapmap version:"+hapmap+"\n")
    os.system(qsubcsv2)
    os.system("ln -s "+os.path.abspath(input+".ped")+" "+outputdir+"/"+outfile+".ped")
    #print qsubcsv2

##main
parser = OptionParser()
parser.add_option("-s", "--spec", default=None, help="specifications file (required)")
parser.add_option("-i", "--hapmap", default='hapmap2', help="Hapmap version (use hapmap2 or hapmap3; default=hapmap2)")
parser.add_option("-o", "--output", default="./", help="output directory")
parser.add_option("--logonly",action="store_true", default=False)
parser.add_option("--outputSpec", default=None, help="output specification file")
(options, args) = parser.parse_args()

parser.check_required("-s")
if options.output:
    outputDir=options.output
    if not (os.access(outputDir,os.F_OK)):
        os.mkdir(outputDir)
else:
    outputDir=tempfile.mkdtemp(prefix="gwas_",dir="./")
jidfile=tempfile.mkstemp(prefix="gwasconvertbatchJids",dir="/tmp")[1]

specfile=open(options.spec)
specstring='\n'.join(specfile.readlines())
specfile.close()
specs_in=propertyList.propertyFromStringFast(specstring)
if specs_in.has_key("jids"):
    jids=specs_in["jids"]
else:
    jids=[]

specs={'type':"plink",'files':[]}
for infile in specs_in["files"]:
    #if os.access(infile["name"]+".map",os.F_OK):
        if (options.logonly==False):
            runCsv2(infile["name"],options.hapmap,outputDir,jids)
        specs["files"].append({'name' : options.output+"/"+infile["name"].split('/')[-1], 'chromosome' : infile["chromosome"] })
    #else:
    #    print "Warning: map for chromosome "+infile["chromosome"]+" not found"
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
