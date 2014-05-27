#!/usr/bin/perl
#
#Thu Dec  2 16:07:59 CET 2010

use TempFileNames;
use Data::Dumper;

my $helpText = <<HELP_TEXT;
	Options:

	Example:
	GWASsummarize.pl ~/tmp/gwas_test/input.pipe --qsub --outputDir gwas_05
	# files do not have header lines
	GWASsummarize.pl ~/tmp/gwas_test/input.pipe --no-header --qsub --outputDir gwas_05
	# debug
	GWASsummarize.pl imputation_01/files.spec --outputDir gwas_05 --doLogOnly --no-qsub --header

$TempFileNames::GeneralHelp
HELP_TEXT

{
	package PipelineSummarizer;
	use Moose;
	use PipelineFileset;
	use TempFileNames;

	extends 'PipelineFileset';
	has 'header', is => 'ro', isa => 'Int', default => 1;

	sub system { my ($self, $cmd, $logLevel, %opts) = @_;
		$self->SUPER::system(join("\n", @$cmd), $logLevel,
			(%opts, cmdAsFile => $self->outputDir.'/pipelineCommand.sh'));
	}
	sub batchCommands { my ($self, $stratum) = @_;
		my $f = $self->spec->{files}[0];
		my $sp = splitPathDict($f->{name});
		my $outputPath = $self->outputDir. '/'. $sp->{base};

		my @cmds = map {
			sprintf('tail -n +%d %s %s %s',
				($self->{header}? !!$_ + 1: 1),
				$self->spec->{files}[$_]{name}, $_? '>>': '>', $outputPath)
		} 0..$#{$self->spec->{files}};

		return ({
			cmd => [@cmds],
			file => { %$f, name => $self->outputDir.'/'.$sp->{base} }
		});
	}
	no Moose;
}

#main $#ARGV @ARGV %ENV
#	initLog(2);
	my $o = { strata => '' };
	my $result = GetOptionsStandard($o,
		# System
		'qsub!',
		# intput options
		'strata=s',
		# other output options
		'header!',
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
	foreach my $inp (@files) {
		my $p = PipelineSummarizer->new(
			specPath => $inp, %$o, useQsub => $o->{qsub}, header => int($o->{header})
		);
		$p->run();
	}
exit(0);
