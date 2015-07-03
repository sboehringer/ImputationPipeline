#!/usr/bin/perl
#
#Thu Dec  2 16:07:59 CET 2010

use TempFileNames;
use Data::Dumper;

my $helpText = <<HELP_TEXT;
	Options:

	Example:
	GWASsummarize.pl ~/tmp/gwas_test/input.pipe --qsub --outputDir gwas_05

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
		my $sp = splitPathDict($self->spec->{files}[0]->{name});
		my $outputPath = $self->outputDir. '/'. $sp->{base}. "_$stratum";

		my @cmds = map {
			my @namesplit = split(/\./,$self->spec->{files}[$_]{name});
			my @dirsplit = split(/\//,$namesplit[0]);
			my $number =  @dirsplit;
			splice @dirsplit, $number-1, 0, $stratum;
			sprintf('tail -n +%d %s %s %s',
				!!$_ + 1, join('/', @dirsplit).'-'.$stratum.'.'.$namesplit[1], $_? '>>': '>', $outputPath)
		} 0..$#{$self->spec->{files}};

		return ({
			cmd => [@cmds],
			file => { name => $self->outputDir.'/'.$sp->{base}. "_$stratum" }
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
		'phenos=s',
		# other output options
		'header!',
		'outputPrefix|outputDir|output|o=s'
	);
	if ($o->{help} || !$result) {
		printf("USAGE: %s [--output outputDir] \\\n"
			."\t--phenos phenotypes (comma separated)\\\n"
			."\t--type typeOfTest \\\n"
			."\t--phenotypes phe1,... --covariates cov1,... \\\n"
			."\t--varFile covarPhenotypes [--pedFile pedFile] \\\n"
			."\t--variableFileOutput convertedFile \\\n"
			."\t[--varNameMap fid=FIDname,iid=IIDname] \\\n"
			."\tinput.spec\n"
		. "\n$helpText", ($0 =~ m{/?([^/]*)$}o));
		exit(!$result);
	}
	#$c = readConfigFile($o->{config});

	my @files = @ARGV;
	#my @phenos = split(',', $o->{phenos});	
	Log(Dumper($o));
	#foreach my $pheno (@phenos) {
	foreach my $inp (@files) {
		my $p = PipelineSummarizer->new(specPath => $inp, %$o, useQsub => $o->{qsub} );
		$p->run();
	}#}
exit(0);
