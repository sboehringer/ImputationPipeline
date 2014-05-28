#!/usr/bin/perl
#
#Thu Dec  2 16:07:59 CET 2010

use TempFileNames;
use Data::Dumper;

my $helpText = <<HELP_TEXT;
	Options:
	--header=s	do files have headers
	--extensions=s	ext1;ext2;...: extensions of files to summarize
	--headers=s	[+-]+: indicate files for which extensions do have headers

	Example:
	GWASsummarize.pl ~/tmp/gwas_test/input.pipe --qsub --outputDir gwas_05
	# files do not have header lines
	GWASsummarize.pl ~/tmp/gwas_test/input.pipe --no-header --qsub --outputDir gwas_05
	# debug
	GWASsummarize.pl imputation_01/files.spec --outputDir gwas_05 --doLogOnly --no-qsub --header
	GWASsummarize.pl imputation_01/files.spec --headers '-+' --extensions ';_info' --outputDir gwas_05 --doLogOnly --no-qsub --header

$TempFileNames::GeneralHelp
HELP_TEXT

{
	package PipelineSummarizer;
	use Moose;
	use PipelineFileset;
	use TempFileNames;

	extends 'PipelineFileset';
	has 'headers', is => 'ro';
	has 'extensions', is => 'ro';

	sub system { my ($self, $cmd, $logLevel, %opts) = @_;
		$self->SUPER::system(join("\n", @$cmd), $logLevel,
			(%opts, cmdAsFile => $self->outputDir.'/pipelineCommand.sh'));
	}
	sub batchCommands { my ($self, $stratum) = @_;
		my $f = $self->spec->{files}[0];
		my $sp = splitPathDict($f->{name});
		my $outputPath = $self->outputDir. '/'. $sp->{base};

		my @files = map {
			my $extension = $self->extensions->[$_];
			my $header = $self->headers->[$_];
			my $output = "$outputPath$extension";
			my @cmds = map {
				sprintf('tail -n +%d %s%s %s %s',
					($self->{header}? !!$_ + 1: 1),
					$self->spec->{files}[$_]{name}, $extension,
					$_? '>>': '>', $output)
			} 0..$#{$self->spec->{files}};
			my $file = {
				cmd => [@cmds],
				file => { %$f, name => $output }
			};
			$file
		} 0..$#{$self->extensions};
		return @files;
	}
	no Moose;
}

#main $#ARGV @ARGV %ENV
#	initLog(2);
	my $o = { strata => '', filespec => 1 };
	my $result = GetOptionsStandard($o,
		# System
		'qsub!',
		# intput options
		'strata=s',
		'extension=s', 'extensions=s',
		# other output options
		'header!', 'headers=s',
		'filespec!',
		'outputPrefix|outputDir|output|o=s'
	);
	if ($o->{help} || !$result) {
		printf("USAGE: %s [--output outputDir] \\\n"
			."\t[--header|--no-header]"
			."\tinput.spec\n"
		. "\n$helpText", ($0 =~ m{/?([^/]*)$}o));
		exit(!$result);
	}
	#$c = readConfigFile($o->{config});

	my @files = @ARGV;
	Log(Dumper($o));
	my $extensions = defined($o->{extensions})
		? [split(/;/, $o->{extensions})]
		: (defined($o->{extension})? [$o->{extension}]: ['']);
	my $headers = defined($o->{headers})
		? [map { $_ ne '-' } split(//, $o->{headers})]
		: (defined($o->{header})? [$o->{header}]: [0]);
	foreach my $inp (@files) {
		my $p = PipelineSummarizer->new(
			specPath => $inp, %$o, useQsub => $o->{qsub},
			headers => $headers, extensions => $extensions, writefilespec => $o->{filespec}
		);
		$p->run();
	}
exit(0);
