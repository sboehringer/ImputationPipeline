#!/usr/bin/perl
#
#Tue Nov  9 11:47:05 CET 2010
# derived from GWASconvert.pl

use TempFileNames;
use Set;
use Statistics::R;
use PropertyList;
use Data::Dumper;

$helpText = <<HELP_TEXT;

	Example:
	GWASconvertBatch.pl --type 3col input.pipe

$TempFileNames::GeneralHelp
HELP_TEXT

%conversion2type = ( 'ped2tped' => 'tped', 'tped2tped3col' => 'tped3col' );

sub qsubCommandFromSpecication { my ($s, $jids) = @_;
}

%QSub = ( use => 0 );
sub qsubActivate { my ($jidsWait, $jidFile) = @_;
	$QSub->{use} = 1;
	$QSub->{jidFile} = $jidFile;
	$QSub->{jidsWait} = [@$jidsWait];
}

sub QSystem { my ($cmd, $logLevel, @opts) = @_;
	if ($QSub->{use}) {
		my $wait = int(@{$QSub->{jidsWait}})? "--waitForJids '". join(',', @{$QSub->{jidsWait}}). "'": '';
		my $storeJids = defined($QSub->{jidFile})? "--jid $QSub->{jidFile}": '';
		$cmd = "qsub.pl $wait $storeJids -- $cmd";
	}
	System($cmd, $logLevel, @opts);
}

# $i: input
# $o: options
sub convertFiles { my ($i, $o) = @_;
	my $f = 'convert2'.$o->{type};

Log(Dumper($i));
	my $outFiles = [map { my $f = $_;
		my $meta = stringFromProperty({%$f, name => undef }, { noFormatting => 1 });
		QSystem("GWASconvert.pl --type $o->{type} $f->{name} --o $o->{output} "
			."--meta ".qs($meta), 4);
		#return propertyFromString(readFile($specFile));
		# <p> assume the 1-by-1 conversions
		my $r = { %$f, name => "$o->{output}/". splitPathDict($f->{name})->{base} };
		$r
	} @{$i->{files}}];
	Log(Dumper([@outFiles]));

	my $spec = { type => $conversion2type{$o->{type}}, files => $outFiles };
	$spec->{jids} = [split("\n", readFile($QSub->{jidFile}))] if ($o->{qsub});
Log(Dumper($spec));
	return $spec;
}

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
		'output|outputDir|o=s',
		# output specificationFromArgument
		'outputSpec=s'
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
		# <p> defaults
		my $outputDir = firstDef($o->{output},
			tempFileName("gwas_$o->{type}", '', { mkDir => 0, dontDelete => 1 }));
		Mkpath($outputDir) if (! -e $outputDir);
		my $outputSpec = "$outputDir/files.spec" if (!defined($o->{outputSpec}));
		my $to = { %$o, output => $outputDir, outputSpec => $outputSpec };

		qsubActivate($s->{jids}, tempFileName('/tmp/gwasconvertbatchJids')) if ($to->{qsub});

		my $sn = convertFiles($s, $to);
		if (defined($to->{outputSpec})) {
			writeFile($to->{outputSpec}, stringFromProperty($sn));
		} else {
			print(stringFromProperty($to->{outputSpec}));
		}
	}
exit(0);
