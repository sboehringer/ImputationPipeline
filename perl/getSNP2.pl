#!/usr/bin/perl

# Usage:
# perl getSNP.pl --genotypefile=*.spec files.spec, files2.spec.....

use TempFileNames;
use PropertyList;
use Getopt::Long;
use Pod::Usage;


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

my $options = {
	'outputDir' => '.',
	'inputDir' => '.',
	'genotypeExtension' => 'gens'
	};

#main @ARGV %ENV
	if(!(-e $options->{outputDir})){
		system("mkdir " . $options->{outputDir});
	}

	GetOptions($options,
		'help|?', 
		'man', 
		#'endpoint=s',
		'outputDir=s',
		'inputDir=s',
		'genotypefile=s',
		'genotypeExtension=s'
	);

	pod2usage(0) if $options->{'help'};
	pod2usage(-exitstatus => 0, -verbose => 2) if $options->{'man'};

	my $gtypeFile = $options->{'genotypefile'};

	my $res = {};
	$res->{files} = [];
	foreach(@ARGV){
		my $files = propertyFromString(readFile($_));
		my $snpFiles = propertyFromString(readFile($options->{'genotypefile'}));
		my $snpDir = splitPathDict($options->{'genotypefile'})->{dir};

		qsubActivate($files->{jids}, tempFileName('/tmp/gwasconvertbatchJids'));
		
		foreach $file (@{$files->{files}}){
			push(@{$res->{files}}, {
				name => "$options->{'outputDir'}/". splitPathDict($file->{name})->{base}
			});
			foreach $snpFile (@{$snpFiles->{files}}) {
				#$file->{name}=~m/(.*)\/(.*)/;
				#$options->{'inputDir'} = $1;
				#my $name = $2;
				QSystem("getSNP.pl $file->{name} "
					."--outputDir $options->{outputDir} "
					."--genotypefile $snpDir/". splitPathDict($snpFile->{name})->{file}
					.".$options->{genotypeExtension}");
			}
		}
	}
	$res->{jids} = [split("\n", readFile($QSub->{jidFile}))];
	writeFile($options->{outputDir} . "/files.spec", stringFromProperty($res));
exit(0);
