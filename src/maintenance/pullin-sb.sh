#!/bin/sh

cd $PIPELINEBASE/src/maintenance

# <p> R scripts
./pullin-R-sb.sh

# <p> perl scripts
( cd ~/src/scripts ; cp System/qsub.pl System/R.pl System/qwait.pl Conversion/csv.pl Conversion/csv2.pl $PIPELINEBASE/src/perl )

# <p> perl libraries
( cd ~/src/privatePerl ; cp TempFileNames.pm Set.pm PropertyList.pm BatchQueue.pm $PIPELINEBASE/src/perl-lib )
( cd ~/src/privatePerl ; mkdir $PIPELINEBASE/src/perl-lib/Statistics ; cp Statistics/R.pm Statistics/Rsession.pm  $PIPELINEBASE/src/perl-lib/Statistics )
