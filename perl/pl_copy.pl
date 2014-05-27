#!/usr/bin/perl
#
#Fri Nov 11 16:28:13 CET 2011

use TempFileNames;
use PropertyList;
use Data::Dumper;
use Set;

# copy pipeline output

# default options
$main::d = { fileSpecName => 'files.spec', appendSourceDir => 1 };
# options
$main::o = [
	'outputDir|o=s', 'fileSpecName=s', 'fileType=s', 'absolute-pathes!', 'inputs=s', 'relocate'
];
$main::usage = '[options] input1 ...';
$main::helpText = <<'HELP_TEXT'.$TempFileNames::GeneralHelp;
	Options:
	--outputDir	where to place output
	--fileType	which file type to declare
	--fileSpecName	name of the output file (default: files.spec) 
	--absolute-pathes	convert references to absolute pathes <i>
	--inputs	file containing input specifictations separated by new-lines
	--appendSourceDir	append the source dir to files in the new file.spec to garuantee
			unambiguous file names
	--linkAllSuffices	soft link all files with the given prefix <i>

	Examples:
	pl_copy.pl --fileType fineFiles --absolute-pathes input_00/files.spec --outputDir input_01

HELP_TEXT

sub copyFiles { my ($c, @files) = @_;
	# <p> initialization
	my $outputDir = $c->{outputDir};
	# <p> all input locations
	push(@files, grep {$_ ne '' } split(/\n/, readFile($c->{inputs}))) if (defined($c->{inputs}));

	# <p> aggregate all files
	#my $spec0 = propertyFromString(readFile($files[0]));
	my @specs = map { propertyFromString(readFile($_)) } @files;
	my @parts = map { @{$_->{files}} } @specs;
	my @partsAbs = map { my $i = $_;
		my @sp = map {
			my %f = %{$_};
			my $sp = splitPathDict($f{name});
			# <A> components of file.spec and name path overlap by conention by one dir component
			my $r = { %f,
				name =>  $sp->{isRelative}
					? join('/', splice(splitPathDict($files[$i])->{dirComponents}, 0, -1)). "/$f{name}"
					: $f{name}
			};
			$r
		} @{$specs[$i]->{files}};
		@sp;
	} 0..$#specs;	my @jids = map { @{$_->{jids}} } @specs;	# propagate dependencies
	# <N><i> check for uniform file type -> change to generic if not the case

	# <p> relocate
	#	only to be used for within-pipeline copying
	if ($c->{relocate}) {
		@parts = map { my $f = $_;
			my $sp = splitPathDict($f->{name});
			my $pathDest = "$outputDir/". $sp->{file}
			. ($c->{appendSourceDir}? '_'. $sp->{dirComponents}[$#{$sp->{dirComponents}}]: '');
			my $pathRel = relativePath($pathDest, $f->{name});
			symlink($pathRel, $pathDest);
			Log("symlinking: $pathRel --> $pathDest", 3);
			# files.spec pathes are relative to project directory: one level up
			$pathRel =~ s{\A\.\./+}{}so;
			Log("files.spec: $pathRel", 5);
			#my $part = { %$f, name => $pathRel };
			my $part = { %$f, name => $pathDest, orig => $pathRel };
			$part
		} @parts;
	}

	# <p> absolute-pathes
	#	importing from external pipeline structures
	if ($c->{'absolute-pathes'}) {
		@parts = map { my $f = $_;
			Log("Registering (absolute): $f->{name}", 3);
			# files.spec pathes are relative to project directory: one level up
			#my $part = { %$f, name => $pathRel };
			my $part = { %$f };
			$part
		} @partsAbs;
	}

	# <p> output file-spec
	my $fileSpec = { jids => [@jids], files => [@parts],
		type => firstDef($c->{fileType}, $fileSpecs[0]->{type}, 'generic')
	};
	if (defined($c->{outputDir})) {
		writeFile("$c->{outputDir}/$c->{fileSpecName}", stringFromProperty($fileSpec));
	} else {
		print(stringFromProperty($fileSpec));
	}
}

#main $#ARGV @ARGV %ENV
	#initLog(2);
	my $c = StartStandardScript($main::d, $main::o);
	copyFiles($c, @ARGV);
exit(0);
