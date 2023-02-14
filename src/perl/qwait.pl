#!/usr/bin/env perl
#
#DATE

use TempFileNames;
use BatchQueue;
use Set;

# default options
$main::d = { sleep => 10, type => 'slurm' };
# options
$main::o = ['sleep=i', 'type=s' ];
$main::usage = 'pid1 ...';
$main::helpText = <<HELP_TEXT;
	Options:
	--sleep=i	sleep interval in seconds [10]

$TempFileNames::GeneralHelp
HELP_TEXT

$template = '#!/bin/sh
#$ -hold_jid %s
echo';

sub waitForJids_SGE { my ($c, @ids) = @_;
	my $tmpcmd = tempFileName("/tmp/qwait_$ENV{USER}");
	writeFile($tmpcmd, sprintf($template, join(',', @ids)), { fileMode => 0770 });
	System("qrsh $tmpcmd", 3);
}

my %typeMap = ( ogs => 'OGS', slurm => 'Slurm' );

sub waitForJids { my ($c, @wids) = @_;
	my @jids;
	my $className = 'BatchQueue'. $typeMap{$c->{type}};
	my $bq = $className->new();
	Log("Waiting for jids: ". join(', ', @wids), 5);
	while (1) {
		@jids = $bq->queuedJobs();
		Log("Active jids: ". join(', ', @jids), 5);
		my @idcs = which_indeces([@wids], [@jids], 1);
		last if (!grep { defined($_) } which_indeces([@wids], [@jids], 1));
		sleep($c->{sleep});
	}
}

#main $#ARGV @ARGV %ENV
	initLog(4);
	my $c = StartStandardScript($main::d, $main::o);
	waitForJids($c, @ARGV);
exit(0);
