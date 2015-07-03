#!/usr/bin/perl
#
#Tue Nov 30 17:41:36 CET 2010
# derived from GWASconvert.pl

use TempFileNames;
use Set;
use Statistics::R;
use PropertyList;
use Data::Dumper;

$helpText = <<HELP_TEXT;
	Options:
	--pedFile pedFile : optional pedFile with no header row
		Columns are named 'fid','iid','pid','mid','sex'
		These row names are imposed and the file is merged with the phenotype file with other vars
		The order of the ped file is preserved such that the pedFile can be used to impose a certain order
		on the phenotype file

	Example:
	GWAStest.pl --type qtassoc --phenotypes tsh,richness --covariates sex,age input.pipe

$TempFileNames::GeneralHelp
HELP_TEXT

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

sub inverseDictFromString { my ($str) = @_;
	my %dict = map { split(/\s*=\s*/) } split(/s*,\s*/, $str);
	return inverseMap(\%dict);
}

sub testAssocFamily { my ($i, $o, $phenotype) = @_;
	# <p> create variable file
	my $varFileCvtd = "$o->{outputDir}/$o->{varFileOutput}";
	my $tempFileVarFile = tempFileName("/tmp/pipeline_$ENV{USER}_vfile");
	my $varPrefix = ($o->{varFile} =~ m{^\[}sog)? '': '[header=T]:';
	my $pedPrefix = ($o->{pedFile} =~ m{^\[}sog)? '': '[header=F]:';
	my $pedHeaderMap = unparseRdata(inverseDictFromString($o->{varNameMap}));
	my $cmd;
	if (defined($o->{pedFile})) {
		my $setPedHeader = ($o->{pedFile} =~ m{header=F}sog)? '--op setHeader=fid,iid,pid,mid,sex ': '';
		$cmd = "csv2.pl --o /dev/null --logLevel ". firstDef($TempFileNames::__verbosity, 4). " -- "
			."$varPrefix$o->{varFile} "
			."--opr 'expr=names(TTOP) = vector.replace(names(TTOP), $pedHeaderMap)' "
			."$pedPrefix$o->{pedFile} "
			. $setPedHeader. ' '
			."--opr 'expr=names(TTOP) = vector.replace(names(TTOP), $pedHeaderMap)' "
			."--opr takeCol=fid,iid,pid,mid,sex "
			."--op 'writeTable=[header=F,sep=S,file=$o->{outputDir}/$o->{pedFileOutput}]' "
			."--op joinOuterLeft=fid,iid "
			."--opr colsBringToFront=fid,iid "
			."--op 'writeTable=[header=F,sep=S,file=$varFileCvtd]' "
			."--op takeHeader "
			."--op 'writeTable=[header=F,sep=S,file=$varFileCvtd.cols]' ";
	} else {
		$cmd = "csv2.pl --o /dev/null --logLevel $TempFileNames::__verbosity -- "
			."$varPrefix$o->{varFile} "
			."--opr 'expr=names(TTOP) = vector.replace(names(TTOP), $pedHeaderMap)' "
			."--op 'writeTable=[header=F,sep=S,cols=fid:iid:pid:mid:sex,"
				."file=$o->{outputDir}/$o->{pedFileOutput}]' "
			."--opr colsBringToFront=fid,iid "
			."--op 'writeTable=[header=F,sep=S,file=$varFileCvtd]' "
			."--op takeHeader "
			."--op 'writeTable=[header=F,sep=S,file=$varFileCvtd.cols]' ";
	}
	System($cmd, 4);
	my @cols = split(/\n/, readFile("$varFileCvtd.cols"));
	Log(join(',', @cols), 5);
	my @phenotypeI = which_indeces([$phenotype], \@cols);
	die "Phenotype $phenotype does not exist in variable file" if (!defined($phenotypeI[0]));
	@phenotypeI = map { $_ - 1 } @phenotypeI;
	Log('Indeces for phenotypes '.join(',', $phenotype). ': '. join(',', @phenotypeI), 5);
	my @covariates = split(/\s*,\s*/, $o->{covariates});
	my @covariatesI =  map { $_ - 1 } grep { defined($_) } which_indeces(\@covariates, \@cols);
	die "Some covariate among ".join(',', @covariates). " does not exist in variable file [columns: "
		. join(',', @cols). "]"
		if (@covariatesI < @covariates);
	Log('Indeces for covariates '.join(',', @covariates). ': '. join(',', @covariatesI), 5);

	my $covTemplate = @covariates? '-c '. join(',', @covariatesI). ' ': '';
	my $cmdName = firstDef($o->{commandName}, 'qt_assoc_trans');
	my $cmdTemplate = "$cmdName -3 -n $phenotypeI[0] $covTemplate "
		."-p $varFileCvtd "
		."-i $o->{outputDir}/$o->{pedFileOutput} "
		."-j INPUT_FILE "
		."-o OUTPUT_FILE ";
	my @cmds = map {
		my $f = $_;
		my $sp = splitPathDict($f->{name});
		my $outFile = "$o->{outputDir}/$sp->{base}_qtassoc";
		my $cmd = mergeDictToString({INPUT_FILE => "$f->{name}.gens", OUTPUT_FILE => $outFile},
			$cmdTemplate, {iterate => 'yes'});
		{
			cmd => $cmd,
			file => { %$f, name => $outFile }
		}
	} @{$i->{files}};
Log(Dumper([@cmds]));

	return @cmds;
}

sub testQtassoc { my ($i, $o, $phenotype) = @_;
	testAssocFamily($i, {%$o, commandName => 'qt_assoc_trans'}, $phenotype);
}

sub testCcassoc { my ($i, $o, $phenotype) = @_;
	testAssocFamily($i, {%$o, commandName => 'cc_assoc_trans'}, $phenotype);
}

# $i: input
# $o: options
sub testFiles { my ($i, $o, $phenotype) = @_;
	my $f = 'test'.ucfirst($o->{type});

	my @cmds = $f->($i, $o, $phenotype);
	for $cmd (@cmds) {
		QSystem($cmd->{cmd}, 2);
	}

	my $spec = { type => "test_phenotype_$o->{type}", files => [map { $_->{file} } @cmds] };
	$spec->{jids} = [split("\n", readFile($QSub->{jidFile}))] if ($o->{qsub});
	return $spec;
}

sub specificationFromArgument { my ($a, $o) = @_;
	return propertyFromString(readFile($a));
}

#main $#ARGV @ARGV %ENV
	#initLog(4);
	$o = { varFile => 'variables.txt', varFileOutput => 'variables.txt',
		pedFileOutput => 'pedfile',
		mapPedHeader => 'fid=fid,iid=iid'
	};
	$result = GetOptionsStandard($o,
		# System
		'qsub!',
		# specification
		'type=s',
		'covariates|covs=s',
		'phenotypes|phenos=s',
		# input options
		'varFile|variableFile=s',
		'pedFile|pedigreeFile=s',
		'varNameMap=s',
		# other output options
		'outputDir|output|o=s',
		'varFileOutput|variableFileOutput=s',
		'pedFileOutput|pedigreeFileOutput=s'
	);
	if ($o->{help} || !$result) {
		printf("USAGE: %s [--output outputDir] \\\n"
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
	my @phenotypes = split(/,/, $o->{phenotypes});
	foreach $inp (@files) {
		my $s = specificationFromArgument($inp, $o);
		# <p> defaults
		foreach $phenotype (@phenotypes) {
			my $outputDir = firstDef($o->{outputDir},
				tempFileName("gwas_$o->{type}", '', { mkDir => 0, dontDelete => 1 }));
			$outputDir = "$outputDir_$phenotype" if (@phenotypes > 1);
			Mkpath($outputDir) if (! -e $outputDir);
			my $outputSpec = "$outputDir/files.spec" if (!defined($o->{outputSpec}));
			my $to = { %$o, output => $outputDir, outputSpec => $outputSpec };

			qsubActivate($s->{jids}, tempFileName('/tmp/gwasconvertbatchJids')) if ($to->{qsub});

			my $sn = testFiles($s, $to, $phenotype);
			if (defined($to->{outputSpec})) {
				writeFile($to->{outputSpec}, stringFromProperty($sn));
			} else {
				print(stringFromProperty($to->{outputSpec}));
			}
		}
	}
exit(0);
