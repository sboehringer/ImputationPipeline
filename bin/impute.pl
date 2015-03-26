#!/usr/bin/perl
#
#Wed Jun  2 14:45:02 CEST 2010

use TempFileNames;
use Set;
use POSIX;
use Data::Dumper;

$helpText = <<HELP_TEXT;
	there is no specific help.

$TempFileNames::GeneralHelp
HELP_TEXT

#
#	<p> configuration
#

$IMPUTATION_CMD = 'impute2 -h HAP_FILE -l LEGEND_FILE -m MAP_FILE -g GENOTYPE_FILE -o OUTPUT_DIR/OUTPUT_FILE -Ne NUMBER_NE -buffer NUMBER_BUFFER -k NUMBER_K -iter NUMBER_ITERATIONS -burnin NUMBER_BURNIN -fix_strand_g -int NUMBER_START NUMBER_STOP > OUTPUT_DIR/OUTPUT_FILE.stdout.txt 2> OUTPUT_DIR/OUTPUT_FILE.stderr.txt';

%defaultParameters = (
	NUMBER_NE =>  11418,
	NUMBER_BUFFER => 50,	# 250 <!>
	NUMBER_K => 42,			# "copying states"
	NUMBER_ITERATIONS => 30,
	NUMBER_BURNIN => 10,
	chunkSize => 1e7
);
%referencePanelFiles = (
	hapmap3 => {
		dir => '/data/data0/hapmap3/phased',
		HAP_FILE => "HAPMAP_DIR/CEU+TSI.chrNUMBER_CHR.hap",	# haplotypes
		LEGEND_FILE => "HAPMAP_DIR/hapmap3.r2.b36.chrNUMBER_CHR.legend",	# snp names
		MAP_FILE => "HAPMAP_DIR/genetic_map_chrNUMBER_CHR_combined_b36.txt"	# recombination map
	},
	hapmap2 => {
		dir => '/data/data0/hapmap2/phased',
		# haplotypes
		HAP_FILE => "HAPMAP_DIR/genotypes_chrNUMBER_CHR_CEU_r21_nr_fwd_phased_all_by_snp_no_mono",
		LEGEND_FILE => "HAPMAP_DIR/genotypes_chrNUMBER_CHR_CEU_r21_nr_fwd_legend_all_no_mono",	# snp names
		MAP_FILE => "HAPMAP_DIR/genetic_map_CEU_chrNUMBER_CHR.txt"	# recombination map
	}
);

$BATCH_TEMPLATE = "#!/bin/sh\nCMDS\necho Analysis done\n";
$CMD_TEMPLATE = "nohup sh -c 'NODE_CMDS' &\n";

$QSUB_BATCH_TEMPLATE = "#!/bin/sh\nCMDS\necho Jobs submitted\n";
$QSUB_CMD_TEMPLATE = "qsub.pl 'CMD'";

#
# <p> helper functions
#

sub writeBatchFiles { my ($o, $cmds, $outputPrefix) = @_;
	my @cmds = @$cmds;
	# <p> create batch files
	my $cmdsPerHost = POSIX::ceil(int(@cmds) / $o->{hosts});
	Log("Cmds per host: $cmdsPerHost");
	Log("Total cmds: ". int(@cmds));
	my @batches = map { mergeDictToString({ CMDS => $_ }, $BATCH_TEMPLATE) }
	map {
		my @cmdsOnHost = grep { defined } @cmds[(($_ - 1) * $cmdsPerHost) .. ($_ * $cmdsPerHost - 1)];
		my $cmdsPerNode = POSIX::ceil(int(@cmdsOnHost) / $o->{nodes});
		my $cmdsOnHost = join("\n", map {
			my @cmdsOnNode = grep { defined }
				@cmdsOnHost[(($_ - 1) * $cmdsPerNode) .. ($_ * $cmdsPerNode - 1)];
			my $nodeCmds = join(' ; ', @cmdsOnNode);
			mergeDictToString({ NODE_CMDS => $nodeCmds }, $CMD_TEMPLATE);
		} (1 .. $o->{nodes}));
		$cmdsOnHost
	} (1 .. $o->{hosts});
	my $outputPrefix = firstDef($o->{outputPrefix}, $outputPrefix);
	for $i (0 ... ($o->{hosts} - 1)) {
		my $path = "$outputPrefix-".($i + 1). '.sh';
		Log("Writing batch file: $path");
		writeFile($path, $batches[$i]);
	}
}

sub writeQsubFile { my ($o, $cmds, $outputPrefix) = @_;
	my $outputPrefix = firstDef($o->{outputPrefix}, $outputPrefix);

	my @batches = map { mergeDictToString({ CMD => $_ }, $QSUB_CMD_TEMPLATE) } @$cmds;
	my $batch = mergeDictToString({ CMDS => join("\n", @batches) }, $QSUB_BATCH_TEMPLATE);

	writeFile("$outputPrefix.sh", $batch);
}

sub fileTail { my ($path) = @_;
	my $sp = splitPathDict($path);
	return $sp->{extension} eq 'gz'? `zcat $path | tail -n 1`: `tail -n 1 $path`;
}

sub chromosomeLength { my ($i, $o) = @_;
	my $refFiles = $referencePanelFiles{$o->{referencePanel}};
	my $mapFile = mergeDictToString({ %$refFiles, NUMBER_CHR => $i,
		HAPMAP_DIR => firstDef($o->{referencePanelDir}, $refFiles->{dir}) },
		'LEGEND_FILE', {iterate => 'yes'});
	my $size = (split(/\s+/, fileTail($mapFile)))[1];
	return $size;
}

sub createImputationBatch { my ($o, $base, $files, $prefixDir) = @_;
	# <p> make sure output dir exists
	Mkpath($o->{outputDir});

	# <p> create cmd list
	my $chunkSize = $defaultParameters{chunkSize};
	my ($f, $t) = ($o->{createImputationBatch} =~ m{(\d+):(\d+)}so);
	my $refFiles = $referencePanelFiles{$o->{referencePanel}};
	my @cmds = map {
		my $i = $_;
		my $chrSize = chromosomeLength($i, $o);
		my $noChunks = POSIX::ceil($chrSize / $chunkSize);
		Log("chromosomeLength [$i]: $chrSize, chunks $noChunks");
		my ($file) = (grep { ($_ =~ m{(?:^|[^d])(\d+)}o)[0] == $i } @$files);
		my %pars = (%defaultParameters, %$refFiles,
			CMD => $IMPUTATION_CMD,
			GENOTYPE_FILE => "$file.gens",
			OUTPUT_DIR => $o->{outputDir},
			OUTPUT_FILE => "${file}_imputed-NUMBER_CHUNK.gens",
			HAPMAP_DIR => firstDef($o->{referencePanelDir}, $refFiles->{dir}),
			NUMBER_CHR => $i
		);
#			Log($cmd);
		map {  mergeDictToString({%pars,
			NUMBER_START => ($_ - 1) * $chunkSize + 1,
			NUMBER_STOP => $_ * $chunkSize,	NUMBER_CHUNK => $_},
			$IMPUTATION_CMD, {iterate => 'yes'}) } (1 .. $noChunks);
	} ($f .. $t);

	# <p> create batch files
	if (defined($o->{qsub})) {
		writeQsubFile($o, [@cmds], './qsub-'. splitPathDict($base)->{base});
	} else {
		writeBatchFiles($o, [@cmds], './test-'. splitPathDict($base)->{base});
	}
}

# fetch phenotypes from atrocious impute file format
sub getPhenotypes { my ($path) = @_;
	# <p> extract phenotypes from impute format
	open($path, $path);
	my @hd1 = split(/\s+/, <$path>);
	my @hd2 = split(/\s+/, <$path>);
	close($path);
	my @phenotypes = grep { defined } map { @hd2[$_] eq "P"? @hd1[$_]: undef } 0 .. (int(@hd2) - 1);
	return @phenotypes;
}

#
#	revert hwe hack: remove _hwe
#

sub testImputedGenotypes { my ($o, $base, $prefixDir) = @_;
	my @phenotypes = getPhenotypes($o->{phenotypes});
	Log("Phenotypes: ". join(", ", @phenotypes), 4);

	# <p> test each phenotype
	my @files = fileList("$o->{outputDir}/$base", 'gens');
	my @cmds;
	foreach $phenotype (@phenotypes) {
		foreach $file (@files) {
			my $sp = splitPathDict($file);
			my $cmd = "snptest -data '$o->{outputDir}/$file' $o->{phenotypes} "
				."-o $o->{outputDir}/$sp->{base}_${phenotype}.test -pheno $phenotype "
				# <p> tests
				."-method expected -frequentist 1 -hwe "
				# <p> covariables
				."-cov_all "
				# <p> stdout
				."> $o->{outputDir}/$sp->{base}_${phenotype}_test.stdout";
			push(@cmds, $cmd);
			#System($cmd, 2);
		}
	}
	if (defined($o->{qsub})) {
		writeQsubFiles($o, [@cmds], './qsub-'. splitPathDict($base)->{base});
	} else {
		writeBatchFiles($o, [@cmds], './test-'. splitPathDict($base)->{base});
	}
}

# we have chromosomes given
# gather all files from chromosomePattern
sub summarizeTests { my ($o, $base, $prefixDir) = @_;
	# <p> phenotypes involved
	my @phenotypes = getPhenotypes($o->{phenotypes});
	# <p> chromosome range involved
	my ($f, $t) = ($o->{summarizeTests} =~ m{(\d+):(\d+)}so);
	#garpsnpset_chr4_imputed-16_Zr_ex1_TSH.test
	# raw list of files in outputDirectory
	my @filesRaw = fileList("$o->{outputDir}/$base", "test");
	# <p> create file per phenotype
	foreach $phenotype (@phenotypes) {
		my $outputFile = "$o->{outputDir}/${base}_tests_$phenotype.txt";
		Log("Summarizing phenotype: $phenotype to '$outputFile'");
		my $k = 0;
		for (my $i = $f; $i <= $t; $i++) {
			my @files = grep { /(?<=[^\d])$i[^\d]*imputed-\d+_$phenotype.test$/ } @filesRaw;
			# sort according to chromosome part
			@files = sort {
				my ($an) = ($a =~ m/(?<=[^\d])$i[^\d]*imputed-(\d+)_$phenotype.test$/);
				my ($bn) = ($b =~ m/(?<=[^\d])$i[^\d]*imputed-(\d+)_$phenotype.test$/);
				$an <=> $bn
			} @files;
#print join("\n", @files)."\n";
			for (my $j = 0; $j < @files; $j++, $k++) {
				System(sprintf("tail -n10000000 %s $o->{outputDir}/%s >%s $outputFile",
					$k? '-n+2': '', $files[$j], $k? '>': ''), 5);
			}
		}
	}
}

sub makeUnique { my ($o, $base, $prefixDir) = @_;
	# <p> phenotypes involved
	my @phenotypes = getPhenotypes($o->{phenotypes});

	foreach $phenotype (@phenotypes) {
		my ($in, $out) = (
			"$o->{outputDir}/${base}_tests_$o->{infix}${phenotype}.txt",
			"$o->{outputDir}/${base}_tests_$o->{infix}${phenotype}_unique.txt"
		);
		my $cmd = "csv.pl -s --groupBy rsid::select_max::info --header $in > $out";
		System($cmd, 3);
	}
}

#main $#ARGV @ARGV %ENV
#	initLog(2);
	$o = { config => 'config.cfg', nodes => 12, hosts => 2, referencePanel => 'hapmap2' };
	$result = GetOptionsStandard($o,
		'createImputationBatch=s',
		'testImputedGenotypes',
		'summarizeTests=s',
		'phenotypes=s',
		'nodes=s', 'hosts=s',
		'outputPrefix|o=s', 'outputDir=s',
		'referencePanelDir=s', 'referencePanel=s',
		'makeUnique',
		'covariates',
		# scheduling
		'qsub',
		# hack to modify file names
		'infix=s'
	);
	if ($o->{help} || !$result) {
		printf("USAGE: %s [--createImputationBatch=from:to] [--testImputedGenotypes] "
		."[--nodes=n] [--makeUnique] basePath\n"
		."$helpText", ($0 =~ m{/?([^/]*)$}o));
		exit(!$result);
	}
	my @trunk = fileList($ARGV[0]);
	my @files = unique(map { splitPathDict($_)->{base} } @trunk);
	my $prefixDir = firstDef(splitPathDict($ARGV[0])->{dir}, '.');
	$o->{outputDir} = "imputed_$ARGV[0]" if (!defined($o->{outputDir}));

	createImputationBatch($o, $ARGV[0], [@files], $prefixDir) if (defined($o->{createImputationBatch}));
	testImputedGenotypes($o, $ARGV[0], $prefixDir) if (defined($o->{testImputedGenotypes}));
	summarizeTests($o, $ARGV[0], $prefixDir) if (defined($o->{summarizeTests}));
	makeUnique($o, $ARGV[0], $prefixDir) if (defined($o->{makeUnique}));
exit(0);
