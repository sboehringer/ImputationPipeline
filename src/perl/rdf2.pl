#!/usr/bin/perl
#use strict;

use Data::Dumper;
use TempFileNames;
use PropertyList;
use Getopt::Long;
use Pod::Usage;

my $man = 0;
my $help = 0;

my $options = {
	'endpoint' => 'http://bind.bio2rdf.org/sparql',
	'outputDir' => '.',
	'inputDir' => '.'
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
                $cmd = "qsub.pl $wait $storeJids -- $cmd";
        }
        System($cmd, $logLevel, @opts);
}

sub check
{		
		my $file=shift;
		if (!$file->{name} && !$file->{database}) {
		        print "No name or database \n";
			return 0;
		}

		if ($file->{database} != "gid_linkage" && (!$file->{word} || !$file->{ID}))
		{
			print "No word or ID identification\n";
			return 0;
		}

		if (! (-e $options->{inputDir} . "/" . $file->{name} . ".list"))
		{
			print "List file does not exist\n";
			return 0;
		}
	return 1;
}


#main $#ARGV @ARGV %ENV

GetOptions($options,
	'help|?', 
	'man', 
	#'endpoint=s',
	'outputDir=s',
	'inputDir=s'
	);

pod2usage(0) if $options->{'help'};
pod2usage(-exitstatus => 0, -verbose => 2) if $options->{'man'};


if(!(-e $options->{outputDir})){
	system("mkdir " . $options->{outputDir});
}

my $res = {};
$res->{files} = [];
my $i = 0;

foreach(@ARGV){
	my $queries = propertyFromString(readFile($_));
	qsubActivate($queries->{jids}, tempFileName('/tmp/gwasconvertbatchJids'));
    
	foreach(@{$queries->{queries}}){
		$_->{name} =~ m/.*\/(.*)/;
		my $outfilename = $options->{outputDir} . "/" . $1;
		system("perl rdf.pl" . 
			" -file=" .  $_->{name} . ".qur" .
			" -endpoint=" . $_->{endpoint} .
			" -outfile=" . $outfilename);
		$res->{files}[$i] = {name => $outfilename};
		$i++;
	}
	
	foreach my $file (@{$queries->{databases}}){			
				
		next if (!&check($file));
		$file->{name} =~ m/.*\/(.*)/;
		my $outfilename = $options->{outputDir} . "/" . $1;
		
		my $par="";
		$par=" -word" if ($file->{word});				
		$par=" -ID" if ($file->{ID});

		QSystem("rdf.pl" . 
			" -database=" . $file->{database} .
			" -list=". $file->{name} . ".list" .
			$par .
			" -outfile=" . $outfilename);
		$res->{files}[$i] = {name => $outfilename};
		$i++;
	}
	
	
}

$res->{jids} = [split("\n", readFile($QSub->{jidFile}))];
writeFile($options->{outputDir} . "/files.spec", stringFromProperty($res));

                             
exit(0);

__END__

=head1 NAME

B<RDF2 something>
   
=head1 SYNOPSIS

perl rdf2.pl B<-endpoint=>'http://example.com/sparql' B<-outputDir=>'output' 
B<inputDir=>'input' B<name-of-input-file(s)>

 Options:
   -help            brief help message
   -man             full documentation
   -inputDir	    directory with input file(s)
   -outputDir       directory for results


=head1 OPTIONS

=over 14

=item B<-help>

Print a brief help message and exits.

=item B<-man>

Prints the manual page and exits.

=item B<-inputDir>

The input information is in the directory.

=item B<-outputDir>

The results are written in the directory.

=back

=head1 DESCRIPTION

B<rdf2.pl> will read the given B<input file(s)> from B<inputDir> which should looks like:

{databases=
       	({
       		name="example_1";
       		database="go";
       		word="1";
       		ID="0";
       	});	

 queries=
        ({
               	name="biogrid";
               	endpoint="http://geneid.bio2rdf.org/sparql";
        })	
}

And run the rdf.pl script. Results are 
written in B<output> directory.


=cut
