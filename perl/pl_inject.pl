#!/usr/bin/perl
#
#Thu Mar 17 17:37:11 CET 2011

use TempFileNames;
use PropertyList;
use Data::Dumper;
use Set;

# inject files from directory into files.spec pipeline format by
#	listing a directory and filtering by pattern
#	meta can be used to create meta-information from the file-name

# default options
$main::d = { unique => 1, pattern => '.*', 'grep-match' => 1,
	fileSpecName => 'files.spec', fileType => 'generic',
	'inject-files-spec' => \&injectFilesSpecs,
 };
# options
$main::o = [
	'meta|m=s', 'pattern|p=s', 'outputDir|o=s', 'unique!', 'grep-match!',
	'fileSpecName=s', 'fileType=s', 'absolute-pathes!'
];
$main::usage = '';
$main::helpText = <<'HELP_TEXT'.$TempFileNames::GeneralHelp;
	Options:
	--unique	make files unique, e.g. list only one file for
			different extensions; on by default
	--grep-match	only record the matched portion of the
			file name; on by default
	--inject-files-spec	inject output from a different pipeline project
			recodes the files.spec

	Examples:
	# match files from pattern and put the number captured by the first
	#	pair of parentheses into the 'chr' meta field
	#	print to stdout
	pl_inject.pl imputation_04 \
		--pattern "LLS_Offspring_Partnerschr21_chr(\\d+)_imputed-\\d+" \
		--meta 'chromosome = $1;' --fileType tped3col

HELP_TEXT

sub injectFilesFromDir { my ($c, $dir) = @_;
	# <p> grep
	my @files = grep { $_ =~ m{$c->{pattern}} } sort { $a cmp $b } dirList($dir);
	@files =  map { $_ =~ m{$c->{pattern}}; $& } @files if ($c->{'grep-match'});
	@files = map { "$dir/$_" } @files if ($c->{'absolute-pathes'});
	@files =  unique(@files) if ($c->{unique});

	# <p> make spec
	my @fileSpecs = map { { name => $_ } } @files;

	# <p> annotate meta-information
	if (defined($c->{meta})) {
		@fileSpecs = map { my $s = $_;
			my @matches = ($s->{name} =~ m{$c->{pattern}});
			my $interpDict = makeHash([map { "\$$_" } 1..(@matches)], [@matches]);
			my $meta = propertyFromString(mergeDictToString($interpDict, "{ $c->{meta} }"));
			$_ = { %$s, %$meta }
		} @fileSpecs;
	}
	return @fileSpecs;
}
sub injectFiles { my ($c, @pathes) = @_;
	my @fileSpecs = map { injectFilesFromDir($c, $_) } @pathes;
	my $fileSpec = { jids => [], files => [@fileSpecs], type => $c->{fileType} };
	if (defined($c->{outputDir})) {
		writeFile("$c->{outputDir}/$c->{fileSpecName}", stringFromProperty($fileSpec));
	} else {
		print(stringFromProperty($fileSpec));
	}
}

sub injectFilesSpec { my ($c, $dir) = @_;
	my $externalDir = splitPathDict($dir)->{dir};
	my $spec = propertyFromString(readFile("$dir/files.spec"));
	my $files = [map { {%$_, name => "$externalDir/$_->{name}" } } @{$spec->{files}}];
	return { files => $files, type => $spec->{type} };
}
# injects a dirs by assuming a files.spec in each
# 	take type from first spec
sub injectFilesSpecs { my ($c, @pathes) = @_;
	Log('Injecting specs '. join(' ', @pathes), 5);
	my @fileSpecs = map { injectFilesSpec($c, $_) } @pathes;
	my $fileSpec = { jids => [], files => [map { @{$_->{files}} } @fileSpecs], type => $fileSpecs[0]->{type} };
	if (defined($c->{outputDir})) {
		writeFile("$c->{outputDir}/$c->{fileSpecName}", stringFromProperty($fileSpec));
	} else {
		print(stringFromProperty($fileSpec));
	}
	return 0;
}

#main $#ARGV @ARGV %ENV
	#initLog(2);
	my $c = StartStandardScript($main::d, $main::o);
	injectFiles($c, @ARGV);
exit(0);
