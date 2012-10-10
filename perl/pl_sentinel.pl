#!/usr/bin/perl
#
#Wed Apr 25 15:25:11 CEST 2012

use TempFileNames;
use PropertyList;
use Data::Dumper;
use Set;
use PipelineFileset;

# copy pipeline output

# default options
$main::d = { 'wrap' => \&wrapSelf, callTriggers => 1, specFile => 'files.spec' };
# options
$main::o = [
	'waitForJids=s'
];
$main::usage = 'pipeline_dir';
$main::helpText = <<'HELP_TEXT'.$TempFileNames::GeneralHelp;
	Internal pipeline command to wait on all jobs in a pipeline step and
	to gather job information from that step
	
	Options:
	--waitForJids id1,id2,...	wait for specified jids
	--wrap	qsub pl_sentinel.pl to wait for jobs in dir

	Examples:
	# qsub
	pl_sentinel.pl --wrap pipeline_01
	# wait
	pl_sentinel.pl pipeline_01

HELP_TEXT

sub gatherJobs { my ($c) = @_;
}

sub wrapSelf { my ($c, $outputDir) = @_;
	Log("Wrapping directory $outputDir", 4);

	# <p> schedule
	my $scheduler = BatchQueueSGE->new();
	my @jids = split(/\s*,\s*/, $c->{waitForJids});
	my $jid = $scheduler->submit("pl_sentinel.pl $outputDir", 4, waitJids => [@jids])->{jid};

	# <p> update config in a backwards compatible way
	my $specPath = "$outputDir/$c->{specFile}";
	my $s = propertyFromString(readFile($specPath));
	$s->{jobs}{allIds} = $s->{jids};
	$s->{jids} = [$jid];
	writeFile($specPath, stringFromProperty($s));

	return;
}

#main $#ARGV @ARGV %ENV
	#initLog(2);
	my $c = StartStandardScript($main::d, $main::o);
	gatherJobs($c, @ARGV);
exit(0);
