#!/bin/sh

cd $PIPELINEBASE/src/maintenance

#
# <p> Rprivate
#

R=~/src/Rprivate
( cd $R ; ./exportR.sh )
FILES="Rlibraries.R Rdata.R Rsystem.R Rmeta.R Rgraphics.R Rreporting.R Rfunctions.R Rstatistic.R Rpatches.R Rdataset.R Rsimulation.R RpropertyList.R Rlinux.R RsowReap.R RgenericAll.R RgenericAllRaw.R"
echo "copying from $R."
for i in $FILES; do
	echo "copying $i ->" $PIPELINEBASE/src/R/Rprivate
	cp $R/$i $PIPELINEBASE/src/R/Rprivate
done

#
# <p> Rscripts
#

echo copying ~/src/Rscripts/gwas '->' $PIPELINEBASE/src/R
mkdir $PIPELINEBASE/src/R/gwas
cp -r ~/src/Rscripts/gwas/gwas* ~/src/Rscripts/gwas/report* `realpath ~/src/Rscripts/gwas/setup*` $PIPELINEBASE/src/R/gwas
