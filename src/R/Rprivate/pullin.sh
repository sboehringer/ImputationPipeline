#!/bin/sh

R=~/src/Rprivate
( cd $R ; ./exportR.sh )
FILES="Rlibraries.R Rdata.R Rsystem.R Rmeta.R Rgraphics.R Rreporting.R Rfunctions.R Rstatistic.R Rpatches.R Rdataset.R Rsimulation.R RpropertyList.R Rlinux.R RsowReap.R"
for i in $FILES; do
	cp $R/$i .
done
