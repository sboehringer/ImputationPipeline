#!/usr/bin/perl
#
#Tue Nov  9 15:38:38 CET 2010
# derived from GWASconvert.pl

use TempFileNames;
use Set;
use Statistics::R;
use PropertyList;

$helpText = <<HELP_TEXT;

	Example:
	GWASconvertBatch.pl --type 3col input.pipe

$TempFileNames::GeneralHelp
HELP_TEXT


sub qsubCommandFromSpecication { my ($s, $jids) = @_;
	my $wait = int(@{$s->{jids}})? "--waitForPids '". join(',', @{$s->{jids}}). "'": '';
	my $storeJids = defined($jids)? "--jids $jids": '';
	my $cmd = "qsub.pl $wait $storeJids --";
	return $cmd;
}

# $i: input
# $o: options
# <!> plist errors
# other input formats
sub specificationFromArgument { my ($a, $o) = @_;
	return propertyFromString(readFile($a));
}

#main $#ARGV @ARGV %ENV
#	initLog(2);
	$o = { qsub => 0 };
	$result = GetOptionsStandard($o,
		'qsub!',
		# output operations
		'type=s',
		# other output options
		'output|o=s'
	);
	$result = GetOptionsStandard($o);
	if ($o->{help} || !$result) {
		printf("USAGE: %s [--output outputDir --type conversion] input\n$helpText", ($0 =~ m{/?([^/]*)$}o));
		exit(!$result);
	}
	#$c = readConfigFile($o->{config});


	my @files = @ARGV;
	foreach $inp (@files) {
		my $s = specificationFromArgument($inp, $o);
		$o->{qsubcmd} = $o->{qsub}
		? qsubCommandFromSpecication($s, tempFileName('/tmp/gwasconvertbatchJids')): '';
		my $sn = convertFiles($s, $o);
	}
exit(0);
