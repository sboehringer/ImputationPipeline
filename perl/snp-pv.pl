#!/usr/bin/perl

# Usage:
# perl callR.pl --permNum=10 -phenotypefile=FILE

use TempFileNames;
use PropertyList;
use Getopt::Long;
use Pod::Usage;



my $options = {
	'outputDir' => '.',
	'inputDir' => '.',
	'permNum' => 9,
	};

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
                $cmd = "qsub.pl $wait $storeJids --unquote -- '$cmd'";
        }
        System($cmd, $logLevel, @opts);
}

if(!(-e $options->{outputDir})){
	system("mkdir " . $options->{outputDir});
}


GetOptions($options,
	'help|?', 
	'man', 
	#'endpoint=s',
	'outputDir=s',
	'inputDir=s',
	'permNum=i',
	'phenotypefile=s' 
	);

pod2usage(0) if $options->{'help'};
pod2usage(-exitstatus => 0, -verbose => 2) if $options->{'man'};

my $execDir = splitPathDict($0)->{dir};
my $res = {};
$res->{files} = [];
foreach(@ARGV){

	my $files = propertyFromString(readFile($_));
	qsubActivate($files->{jids}, tempFileName('/tmp/gwasconvertbatchJids'));
	
	foreach $file (@{$files->{files}}){

		$file->{name}=~m/(.*)\/(.*)/;
		$options->{'inputDir'}=$1;
		my $name = $2;
		push(@{$res->{files}}, {name => $options->{'outputDir'} . "/". $name});
		
		QSystem("R < $execDir/snp-pv.R --vanilla --args " . 
			$options->{inputDir} . " " . 
			$options->{'outputDir'} . "/" . $name . ".pv " .
			$options->{phenotypefile} . " " .
			$name . " " .
			$options->{permNum});	
	}
}

$res->{jids} = [split("\n", readFile($QSub->{jidFile}))];
writeFile($options->{outputDir} . "/files.spec", stringFromProperty($res));

exit(0);
