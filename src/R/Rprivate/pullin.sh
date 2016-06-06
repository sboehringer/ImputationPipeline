#!/bin/sh

R=~/src/Rprivate
( cd $R ; ./exportR.sh )
FILES="Rlibraries.R Rdata.R Rsystem.R Rmeta.R Rgraphics.R Rreporting.R Rfunctions.R Rstatistic.R Rpatches.R Rdataset.R Rsimulation.R RpropertyList.R Rlinux.R RsowReap.R RgenericAll.R RgenericAllRaw.R"
echo "copying from $R."
for i in $FILES; do
	echo "copying $i ->" `pwd`
	cp $R/$i .
done
