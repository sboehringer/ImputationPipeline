#!/bin/sh

cp ~/src/Rprivate/*.R $PIPELINEBASE/R/Rprivate
cp ~/src/scripts/System/qsub.pl $PIPELINEBASE/perl

( cd ~/src/privatePerl ; cp TempFileNames.pm Set.pm PropertyList.pm $PIPELINEBASE/perl-lib )
