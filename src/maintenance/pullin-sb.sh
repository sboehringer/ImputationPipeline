#!/bin/sh

cp ~/src/Rprivate/*.R $PIPELINEBASE/src/R/Rprivate
( cd ~/src/scripts ; cp System/qsub.pl System/qwait.pl Conversion/csv.pl Conversion/csv2.pl $PIPELINEBASE/src/perl )

( cd ~/src/privatePerl ; cp TempFileNames.pm Set.pm PropertyList.pm BatchQueue.pm $PIPELINEBASE/src/perl-lib )
