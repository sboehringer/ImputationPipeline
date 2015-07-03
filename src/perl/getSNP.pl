#!/usr/bin/perl

# Usage:
# perl getSNP.pl gtypeFile files.spec, files2.spec.....

use TempFileNames;
use PropertyList;
use Set;
use Getopt::Long;
use Pod::Usage;
use IO::File;

my $options = {
	'outputDir' => '.',
	'inputDir' => '.',
};

#main @ARGV %ENV
	GetOptions($options,
		'help|?', 
		'man', 
		#'endpoint=s',
		'outputDir=s',
		'inputDir=s',
		'genotypefile=s'
	);
	pod2usage(0) if $options->{'help'};
	pod2usage(-exitstatus => 0, -verbose => 2) if $options->{'man'};

	if(!(-e $options->{outputDir})){
		system("mkdir " . $options->{outputDir});
	}
	# <p> file names
	my $name = shift;
	my $output = $options->{outputDir};
	my $prefix = splitPathDict($name)->{base};
	my $infoFile = "$output/$prefix.info";

	# <p> SNPs
	my @snps = (readFile("$name.snps") =~ m{(\d+)}sog);
	#print "SNPs to be searched:\n", join(' ', @snps), "\n";

	# <p> search file
	my $gt = IO::File->new();
	$gt->open($options->{genotypefile});
	my @found;
	while (my $line = <$gt>) {
		my ($snp, $gts) = ($line =~ m/^\S+ rs(\d+) \d+ \w \w (.*)/so);
		#print "$snp ". substr($gts, 0, 20), "\n";
		if (defined(my $i = which($snp, \@snps))) {
			writeFile("$output/$prefix.$snp", $gts);
			push(@found, splice(@snps, $i, 1));
		}
	}
	$gt->close();
	Log("Couldn't append SNP list to $infoFile", 4)
		if (!defined(writeFile($infoFile, join("\n", (@found, '')), { append => 'YES' })));

	print "Found SNPs:\n", join(' ', @found), "\n";
exit(0);
