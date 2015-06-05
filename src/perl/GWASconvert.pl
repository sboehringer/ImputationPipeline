#!/usr/bin/perl
#
#Tue Jan 27 17:29:47 CET 2009
# derived from plink.pl

use TempFileNames;
use Set;
use Statistics::R;
use PropertyList;
use Data::Dumper;

$helpText = <<HELP_TEXT;

	Example:
	GWASconvert.pl --type 3col input.pipe

$TempFileNames::GeneralHelp
HELP_TEXT

sub convertPed2tped { my ($input, $outputDir, $o, $m) = @_;
	my $out = $outputDir. '/'. splitPathDict($input)->{base};
	my $plink = qq{plink --noweb --file $input $plinkRange --recode --transpose }
		.qq{--out $out};
	System($plink, 2);
	return $out;
}

	#sub convert2impute { my ($prefix, $o) = @_;
sub convertTped2tped3col { my ($input, $outputDir, $o, $m) = @_;
	my $out = $outputDir. '/'. splitPathDict($input)->{base}. '.gens';
	Log("$o->{inputDir}/$input.tped [tped] --> $out [tped3col]", 2);
	my $tped = "$o->{inputDir}/$input.tped";
	open($tped, $tped);
	open($out, ">$out");
	my $i = 1;
	while (<$tped>) {
		my @snp = split(/ /, substr($_, 0, -1));
		my ($chr, $rs, $posGen, $posPhy) = splice(@snp, 0, 4);
		my @alleles = sort { $a cmp $b } @{minus([unique(@snp)], ['0'])};
		# <p> only missings
		next if (!@alleles);
		# <p> monomorphic situation
		if (@alleles == 1) { @alleles = ($alleles[0], $alleles[0]); }
		my @out = (sprintf("snp-%03d-%s", $i++, $chr), $rs, $posPhy, @alleles);
		my %gtD = (
			$alleles[0].$alleles[0] => [1, 0, 0],
			$alleles[0].$alleles[1] => [0, 1, 0],
			$alleles[1].$alleles[1] => [0, 0, 1],
			'00' => [0, 0, 0]
		);
		while (int(@snp) > 0) {
			my $gt = join('', sort { $a cmp $b } splice(@snp, 0, 2));
			Log("Undefined genotype '$gt'.", 1) if (!defined($gtD{$gt}));
			push(@out, @{$gtD{$gt}});
		}
		print $out join(' ', @out), "\n";
	}
	close($out);
	close($tped);
	return splitPathDict($out)->{basePath};
}

# $i: input
# $o: options
sub convertFile { my ($i, $o, $m) = @_;
	my $f = 'convert'.ucfirst($o->{type});

	my @outFiles = $f->($i, $o->{outputDir} ,$o, $m);
	return @outFiles;
}

#main $#ARGV @ARGV %ENV
#	initLog(2);
	$o = {};
	$result = GetOptionsStandard($o,
		# output operations
		'type=s',
		# other output options
		'output|o=s',
		# file meta information
		'meta=s'
	);
	$result = GetOptionsStandard($o);
	if ($o->{help} || !$result) {
		printf("USAGE: %s [--output outputDir --type fileType] input\n$helpText", ($0 =~ m{/?([^/]*)$}o));
		exit(!$result);
	}
	#$c = readConfigFile($o->{config});

	my $meta = propertyFromString($o->{meta});
	my $outputDir = firstDef($o->{output},
		tempFileName("gwas_$o->{type}", '', { mkDir => 0, dontDelete => 1 }));
	Mkpath($outputDir) if (! -e $outputDir);

	my @files = @ARGV;
	foreach $file (@files) {
		$o = {%$o, inputDir => firstDef(splitPathDict($inp)->{dir}, '.'), outputDir => $outputDir };
		my @ofs = convertFile($file, $o, $meta);
	}
exit(0);
