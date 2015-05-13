#!/bin/sh

cp ~/src/Rprivate/*.R $PIPELINEBASE/R/Rprivate
( cd ~/src/scripts ; cp System/qsub.pl Conversion/csv.pl Conversion/csv2.pl $PIPELINEBASE/perl )

( cd ~/src/privatePerl ; cp TempFileNames.pm Set.pm PropertyList.pm $PIPELINEBASE/perl-lib )
