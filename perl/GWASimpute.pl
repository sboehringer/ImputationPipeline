#!/usr/bin/perl
#
#Tue Nov  9 18:57:13 CET 2010
#	derived from impute.pl

use TempFileNames;
use Set;
use POSIX;
use Data::Dumper;
use PropertyList;

$helpText = <<HELP_TEXT;
	Options:
	--cmdtemplate impute|prephase|impute_prephased|chrX|chrX_prephase|chrX_impute_prephased

$TempFileNames::GeneralHelp
HELP_TEXT

#
#	<p> configuration
#

my $IMPUTATION_CMD21 =
'impute212 IMPUTE_CMDLINE -pgs_miss -h HAP_FILE -l LEGEND_FILE -m MAP_FILE -g GENOTYPE_FILE -o OUTPUT_DIR/OUTPUT_FILE -Ne NUMBER_NE -buffer NUMBER_BUFFER -k NUMBER_K -iter NUMBER_ITERATIONS -burnin NUMBER_BURNIN -fix_strand_g -int NUMBER_START NUMBER_STOP';

my $IMPUTATION_CMD_PREPHASE21 =
'impute212 IMPUTE_CMDLINE -m MAP_FILE -g GENOTYPE_FILE -o OUTPUT_DIR/OUTPUT_FILE_prephase -Ne NUMBER_NE -buffer NUMBER_BUFFER -k NUMBER_K -iter NUMBER_ITERATIONS -burnin NUMBER_BURNIN -fix_strand_g -int NUMBER_START NUMBER_STOP -phase -include_buffer_in_output -stage_one -hap_samp_dir OUTPUT_DIR';

my $IMPUTATION_CMD_IMPUTE_FROM_PREPHASE21 =
'impute212 IMPUTE_CMDLINE -pgs_miss -h HAP_FILE -l LEGEND_FILE -m MAP_FILE -g GENOTYPE_FILE -o OUTPUT_DIR/OUTPUT_FILE -Ne NUMBER_NE -buffer NUMBER_BUFFER -k NUMBER_K -iter NUMBER_ITERATIONS -burnin NUMBER_BURNIN -fix_strand_g -int NUMBER_START NUMBER_STOP -stage_two -hap_samp_dir PREPHASED_DIR';

my $IMPUTATION_CMD22 =
'impute22 IMPUTE_CMDLINE -pgs_miss -h HAP_FILE -l LEGEND_FILE -m MAP_FILE -g GENOTYPE_FILE -o OUTPUT_DIR/OUTPUT_FILE -Ne NUMBER_NE -buffer NUMBER_BUFFER -k NUMBER_K -iter NUMBER_ITERATIONS -burnin NUMBER_BURNIN -align_by_maf_g -int NUMBER_START NUMBER_STOP';

my $IMPUTATION_CMD_PREPHASE22 =
'impute22 IMPUTE_CMDLINE -m MAP_FILE -g GENOTYPE_FILE -o OUTPUT_DIR/OUTPUT_FILE_prephase -Ne NUMBER_NE -buffer NUMBER_BUFFER -k NUMBER_K -iter NUMBER_ITERATIONS -burnin NUMBER_BURNIN -align_by_maf_g -int NUMBER_START NUMBER_STOP -phase -include_buffer_in_output -stage_one -hap_samp_dir OUTPUT_DIR';

my $IMPUTATION_CMD_IMPUTE_FROM_PREPHASE22 =
'impute22 IMPUTE_CMDLINE -pgs_miss -h HAP_FILE -l LEGEND_FILE -m MAP_FILE -g GENOTYPE_FILE -o OUTPUT_DIR/OUTPUT_FILE -Ne NUMBER_NE -buffer NUMBER_BUFFER -k NUMBER_K -iter NUMBER_ITERATIONS -burnin NUMBER_BURNIN -align_by_maf_g -int NUMBER_START NUMBER_STOP -stage_two -hap_samp_dir PREPHASED_DIR';

my $IMPUTATION_CMD_CHRX = 
'impute22 -pgs_miss IMPUTE_CMDLINE -chrX -h HAP_FILE -l LEGEND_FILE -m MAP_FILE -g GENOTYPE_FILE -o OUTPUT_DIR/OUTPUT_FILE -sample_g OUTPUT_DIR/sex.sample -Ne NUMBER_NE -buffer NUMBER_BUFFER -k NUMBER_K -iter NUMBER_ITERATIONS -burnin NUMBER_BURNIN -align_by_maf_g -int NUMBER_START NUMBER_STOP';

my $IMPUTATION_CMD_PREPHASE_CHRX =
'impute22 IMPUTE_CMDLINE -chrX -m MAP_FILE -g GENOTYPE_FILE -o OUTPUT_DIR/OUTPUT_FILE_prephase -Ne NUMBER_NE -buffer NUMBER_BUFFER -k NUMBER_K -iter NUMBER_ITERATIONS -burnin NUMBER_BURNIN -align_by_maf_g -int NUMBER_START NUMBER_STOP -phase -include_buffer_in_output -stage_one -hap_samp_dir OUTPUT_DIR';

my $IMPUTATION_CMD_IMPUTE_FROM_PREPHASE_CHRX =
'impute22 IMPUTE_CMDLINE -pgs_miss -chrX -h HAP_FILE -l LEGEND_FILE -m MAP_FILE -g GENOTYPE_FILE -o OUTPUT_DIR/OUTPUT_FILE -Ne NUMBER_NE -buffer NUMBER_BUFFER -k NUMBER_K -iter NUMBER_ITERATIONS -burnin NUMBER_BURNIN -align_by_maf_g -int NUMBER_START NUMBER_STOP -stage_two -hap_samp_dir PREPHASED_DIR';

my %cmdTemplates21 = (
	'impute' => $IMPUTATION_CMD21,
	'prephase' => $IMPUTATION_CMD_PREPHASE21,
	'impute_prephased' => $IMPUTATION_CMD_IMPUTE_FROM_PREPHASE21
);
my %cmdTemplates = (
	'impute' => $IMPUTATION_CMD22,
	'prephase' => $IMPUTATION_CMD_PREPHASE22,
	'impute_prephased' => $IMPUTATION_CMD_IMPUTE_FROM_PREPHASE22,
	'chrX' => $IMPUTATION_CMD_CHRX,
	'chrX_prephase' => $IMPUTATION_CMD_PREPHASE_CHRX,
	'chrX_impute_prephased' => $IMPUTATION_CMD_IMPUTE_FROM_PREPHASE_CHRX
);



my %defaultParameters = (
	NUMBER_NE =>  11418,
	NUMBER_BUFFER => 50,	# 250 <!>
	NUMBER_K => 42,			# "copying states"
	NUMBER_ITERATIONS => 30,
	NUMBER_BURNIN => 10,
	chunkSize => 4e6
);
my %referencePanelFiles = (
	hapmap3 => {
		REFDIR => firstDef($ENV{'referencepanel_hapmap3'}, '/data/data0/hapmap3/phased'),
		HAP_FILE => "HAPMAP_DIR/CEU+TSI.chrNUMBER_CHR.hap",	# haplotypes
		LEGEND_FILE => "HAPMAP_DIR/hapmap3.r2.b36.chrNUMBER_CHR.legend",	# snp names
		MAP_FILE => "HAPMAP_DIR/genetic_map_chrNUMBER_CHR_combined_b36.txt"	# recombination map
	},
	hapmap2b21 => {
		REFDIR => firstDef($ENV{'referencepanel_hapmap2b21'}, '/data/data0/hapmap2/phased'),
		# haplotypes
		HAP_FILE => "HAPMAP_DIR/genotypes_chrNUMBER_CHR_CEU_r21_nr_fwd_phased_all_by_snp_no_mono",
		LEGEND_FILE => "HAPMAP_DIR/genotypes_chrNUMBER_CHR_CEU_r21_nr_fwd_legend_all_no_mono",	# snp names
		MAP_FILE => "HAPMAP_DIR/genetic_map_CEU_chrNUMBER_CHR.txt"	# recombination map
	},
	hapmap2b22 => {
		REFDIR => firstDef($ENV{'referencepanel_hapmap2b22'}),
		# haplotypes
		HAP_FILE => "HAPMAP_DIR/genotypes_chrNUMBER_CHR_CEU_r22_nr.b36_fwd_phased_by_snp",
		LEGEND_FILE => "HAPMAP_DIR/genotypes_chrNUMBER_CHR_CEU_r22_nr.b36_fwd_legend_by_snp",	# snp names
		MAP_FILE => "HAPMAP_DIR/genetic_map_chrNUMBER_CHR_CEU_b36.txt"	# recombination map
	},
	hapmap2_chrX => {
		dir => '/data/data0/hapmap2/phased',
		# haplotypes
		HAP_FILE => "HAPMAP_DIR/genotypes_chrX_CEU_r21_nr_fwd_non-par_phased_by_snp_no_mono",
		LEGEND_FILE => "HAPMAP_DIR/genotypes_chrX_CEU_r21_nr_fwd_non-par_legend.txt",	# snp names
		MAP_FILE => "HAPMAP_DIR/genetic_map_chrX_non-par.txt"	# recombination map
	},
	hapmap2_par => { # pseudo-autosomal
		dir => '/data/data0/hapmap2/phased',
		# haplotypes
		HAP_FILE => "HAPMAP_DIR/genotypes_chrX_CEU_r21_nr_fwd_par1_phased_by_snp_no_mono",
		LEGEND_FILE => "HAPMAP_DIR/genotypes_chrX_CEU_r21_nr_fwd_par1_legend.txt",	# snp names
		MAP_FILE => "HAPMAP_DIR/genetic_map_chrX_par1.txt"	# recombination map
	}
);

sub referencePanelFiles { my ($refPanel) = @_;
	return {
		REFDIR => $ENV{"referencepanel_$refPanel"},
		# haplotypes
		HAP_FILE => 'HAPMAP_DIR/'. $ENV{"${refPanel}_genotypes"},	# genotypes
		LEGEND_FILE => 'HAPMAP_DIR/'. $ENV{"${refPanel}_legend"},	# physical map
		MAP_FILE => 'HAPMAP_DIR/'. $ENV{"${refPanel}_map"},			# genetic map
	}
}

$BATCH_TEMPLATE = "#!/bin/sh\nCMDS\necho Analysis done\n";
$CMD_TEMPLATE = "nohup sh -c 'NODE_CMDS' &\n";

$QSUB_BATCH_TEMPLATE = "#!/bin/sh\nCMDS\necho Jobs submitted\n";
$QSUB_CMD_TEMPLATE = "qsub.pl 'CMD'";

#
#	<p> qsub
#

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

#
# <p> helper functions
#

sub fileTail { my ($path) = @_;
	my $sp = splitPathDict($path);
	return $sp->{extension} eq 'gz'? `zcat $path | tail -n 1`: `tail -n 1 $path`;
}

sub specificationFromPath { my ($a) = @_;
	return -e $a? propertyFromString(readFile($a)): undef;
}

sub chromosomeLength { my ($i, $o) = @_;
	my $refFiles = $o->{refFiles};
	my $mapFile = mergeDictToString({ %$refFiles, NUMBER_CHR => $i,
		HAPMAP_DIR => firstDef($o->{referencePanelDir}, $refFiles->{REFDIR}) },
		'LEGEND_FILE', {iterate => 'yes'});
	my $size = (split(/\s+/, fileTail($mapFile)))[1];
	return $size;
}

sub createImputationBatch { my ($i, $o) = @_;
	# <p> create cmd list
	my $chunkSize = firstDef($o->{chunkSize}, $defaultParameters{chunkSize});
	my ($f, $t) = ($o->{createImputationBatch} =~ m{(\d+):(\d+)}so);
	my $refFiles = $o->{refFiles};
	my $prp = specificationFromPath($o->{'prephased-files'});
	# <N> index in prephased files; assume same order for prephasing as imputation
	my $prpI = 0;

	my @cmds = map {
		my $f = $i->{files}[$_];
		my $sp = splitPathDict($f->{name});
		my $chrSize = chromosomeLength($f->{chromosome}, $o);
		my $noChunks = POSIX::ceil($chrSize / $chunkSize);
		Log("chromosomeLength [$f->{chromosome}]: $chrSize, chunks $noChunks", 4);
		my %pars = (%defaultParameters, %$refFiles,
			CMD => $cmdTemplates{$o->{cmdtemplate}},
			GENOTYPE_FILE => "$f->{name}.gens",
			OUTPUT_DIR => $o->{output},
			OUTPUT_FILE => "$sp->{base}_imputed-NUMBER_CHUNK.gens",
			HAPMAP_DIR => firstDef($o->{referencePanelDir}, $refFiles->{REFDIR}),
			NUMBER_CHR => $f->{chromosome}
		);
#		Log($cmd);
		my @cmds = map {
			my $chunk = $_;
			my $prephased = splitPathDict(defined($prp)? $prp->{files}[$prpI++]{name}: '');
			my $cmd = mergeDictToString({%pars,
				NUMBER_START => ($chunk - 1) * $chunkSize + 1,
				NUMBER_STOP => $chunk * $chunkSize,	NUMBER_CHUNK => $chunk,
				PREPHASED_DIR => $prephased->{dir}, PREPHASED_FILE => $prephased->{path}
				}, $pars{CMD}, {iterate => 'yes'});
			{
				cmd => $cmd,
				file => { %$f,
					name => "$o->{output}/$sp->{base}_imputed-$chunk",
					orig => $f->{name}
				}
			}
		} (1 .. $noChunks);
		@cmds
	} 0 .. $#{$i->{files}};

	return @cmds;
}

# $i: input
# $o: options
sub imputeFiles { my ($i, $o) = @_;
	my $f = 'convert2'.$o->{type};

	my @cmds = createImputationBatch($i, $o);
	Log(Dumper([@cmds]), 5);
	QSystem($_->{cmd}, 2) foreach (@cmds);

	my $spec = { type => 'tped3col', files => [map { $_->{file} } @cmds] };
	$spec->{jids} = [split("\n", readFile($QSub->{jidFile}))] if ($o->{qsub});
	return $spec;
}

#main $#ARGV @ARGV %ENV
#	initLog(2);
	$o = { qsub => 0, referencePanel => 'hapmap2b22', imputeParameters => '', cmdtemplate => 'impute' };
	$result = GetOptionsStandard($o,
		'qsub!',
		'cmdtemplate=s',	# which type of analysis to run
		'cmdPrepare=s',
		# input options
		'prephased-files=s',
		'genotypes=s',	# as alternative to a plain argument

		# output operations (type == filetype)
		'type=s',
		# other output options
		'outputDir|output|o=s',
		# output specification path
		'outputSpec=s',

		# imputation arguments
		'referencePanel=s',
		# imputation arguments
		'imputeParameters=s',
		'chunkSize=s'
	);
	if ($o->{help} || !$result) {
		printf("USAGE: %s [--referencePanel hapmap2b21|hapmap2b22|hapmap3] [--output|o outputDir] input.spec\n"
		."$helpText", ($0 =~ m{/?([^/]*)$}o));
		exit(!$result);
	}
	# <p> refine options
	#	imputation
	my $iOptions = propertyFromString('{'. $o->{imputeParameters}. '}');
	%defaultParameters = (%defaultParameters, %$iOptions);
	#	reference panel files
	$o = {%$o, refFiles => firstDef(
		referencePanelFiles($o->{referencePanel}),	# patterns defined through environment variables
		$referencePanelFiles{$o->{referencePanel}}	# pre-defined patterns
	)};

	my @files = grep { defined } (@ARGV, $o->{genotypes});
	foreach $inp (@files) {
		my $s = specificationFromPath($inp);
		my @waitForJids = @{$s->{jids}};
		my $sprp = specificationFromPath($o->{'prephased-files'});
		# <p> defaults
		my $outputDir = firstDef($o->{outputDir},
			tempFileName("gwas_$o->{type}", '', { mkDir => 0, dontDelete => 1 }));
		Mkpath($outputDir) if (! -e $outputDir);
		my $outputSpec = "$outputDir/files.spec" if (!defined($o->{outputSpec}));
		my $to = { %$o, output => $outputDir, outputSpec => $outputSpec };

		qsubActivate([grep {defined} @waitForJids, @{$sprp->{jids}}],
			tempFileName('/tmp/gwasconvertbatchJids')) if ($to->{qsub});

		if (defined($o->{cmdPrepare})) {
 			QSystem($o->{cmdPrepare}, 2);
			@waitForJids = split("\n", readFile($QSub->{jidFile})) if ($o->{qsub});
			qsubActivate([@waitForJids], tempFileName('/tmp/gwasconvertbatchJids')) if ($to->{qsub});
		}

		my $sn = imputeFiles($s, $to);
		if (defined($to->{outputSpec})) {
			writeFile($to->{outputSpec}, stringFromProperty($sn));
		} else {
			print(stringFromProperty($to->{outputSpec}));
		}
	}
exit(0);
