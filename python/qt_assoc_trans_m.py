#!/usr/bin/python
import sys
import os
import optparse

##ccassoc:cmd	\
#	cc_assoc_trans.py ccassoc:Options ccassoc:ExtraOptions \
#	-n STRATUM --cols OUTPUT_DIR/variables.cols -c 'ccassoc:Covariates' \
#	-p OUTPUT_DIR/variables \
#	-i ccassoc:PEDFILE \
#	-j INPUT.ccassoc:INPUT_EXT \
#	-o OUTPUT


##adapted from examples/required_1.py in the optparse source distribution 
class OptionParser (optparse.OptionParser):

    def check_required (self, opt):
        option = self.get_option(opt)

        # Assumes the option's 'default' is set to None!
        if getattr(self.values, option.dest) is None:
            self.error("%s option not supplied" % option)
##main
parser = OptionParser()
parser.add_option("--assocParameters", help="extra options")
parser.add_option("-n", help="phenotype names; comma-separated")
parser.add_option("--cols", help="variables.cols file (required)")
parser.add_option("-c", help="covariate names; comma-separated")
parser.add_option("-o", help="output dir")
parser.add_option("-i", help="pedigree file")
parser.add_option("-j", help="input gens file")
parser.add_option("-p", help="phenotype file")
parser.add_option("--logonly",action="store_true", default=False)
(options, args) = parser.parse_args()

parser.check_required("--cols")
cols=open(options.cols)
column_names=[]
for line in cols:
    column_names.append(line.strip())
cols.close()

#try:
#    pheno=column_names.index(options.n)-1
#except ValueError:
#    sys.exit("Phenotype "+options.n+" not in variables file.")

phenotypes=options.n.split(',')
if len(phenotypes)==0:
    sys.exit("No phenotype found.")
else:
    phenos=[]
    for name in phenotypes:
        try:
            phenos.append(str(column_names.index(name)-1))
        except ValueError:
            sys.stderr.write("Phenotype "+name+" not in variables file; Skipping this phenotype.\n")
    if len(phenos)==0:
        sys.exit("All phenotypes in"+options.n+" not in variables file.")
covariates=options.c.split(',')
if len(covariates)==0:
    cov=" "
else:
    covs=[]
    for name in covariates:
        try:
            covs.append(str(column_names.index(name)-1))
        except ValueError:
            sys.stderr.write("Covariate "+name+" not in variables file; Skipping this covariate.\n")
    if len(covs)==0:
        cov=" "
    else:
        cov=" -c "+','.join(covs)

scriptname=sys.argv[0].split('/')[-1]
executable=scriptname[:-3]
for pheno in phenos:
    try: 
        os.makedirs(options.o+'/'+pheno)
    except OSError:
        if not os.path.isdir(options.o+'/'+pheno):
            raise
    command=executable+' '+options.assocParameters+" -n "+str(pheno)+cov+" -p "+options.p+" -i "+options.i+" -j "+options.j+" -o "+options.o+'/'+pheno+'/'
    sys.stderr.write(command+'\n')
    if (options.logonly==False):
        os.system(command)
