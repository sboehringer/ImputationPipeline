#!/usr/bin/perl
#
#Mon Feb 14 16:57:30 CET 2011

#use perl5i::2;
use TempFileNames;
use Set;
use Statistics::R;
use Data::Dumper;

# default options
$main::d = { };
# options
$main::o = [
	'+createSnptestPhenotypeFile=s', '+createXXAssocPhenotypeFile=s',
	'pedFile=s', 'phenotypeFile=s', 'phenotypes=s', 'covariates=s',
	'variableFile|varFile=s', 'headerMap=s'
];
$main::usage = '';
$main::helpText = <<HELP_TEXT.$TempFileNames::GeneralHelp;
	Example:
	pedfile.pl --createSnptestPhenotypeFile test.pheno \
		--phenotypes Adiponectin:Q --covariates Sex:F,Age,BMI \
		--headerMap fid=FID,iid=IID \
		--variableFile [header=T,sep=T]:Phenofile.txt \
		--pedFile imputation_00/pedfile

HELP_TEXT

sub dictFromString { my ($str) = @_;
	my %dict = map { split(/\s*=\s*/) } split(/s*,\s*/, $str);
	return { %dict };
}
sub inverseDictFromString { my ($str) = @_;
	return inverseMap(dictFromString($str));
}

sub typedList { my ($str, $defaultType) = @_;
	my @vars = map { my ($v, $t) = ($_ =~ m{(.*?)(?::([A-Z]))?$}o);
		{ type => firstDef($t, $defaultType), name => $v }
	} split(/s*,\s*|\n/, $str);
	return @vars;
}

# covariate types
%main::covTypeMap = ( 'C' => 'C', 'F' => 'D' );
# response types
%main::resTypeMap = ( 'Q' => 'P', 'B' => 'B' );
sub doCreateSnptestPhenotypeFile { my ($c, @files) = @_;
	# <p> create 'sample file' header
	my @covariates = typedList($c->{covariates}, 'C');
	my @phenotypes = typedList($c->{phenotypes}, 'B');
	Log(Dumper([@covariates]));
	Log(Dumper([@phenotypes]));
	my @cols = ('ID_1', 'ID_2', 'missing',
		(map { $_->{name} } @covariates), (map { $_->{name} } @phenotypes));
	my @types = (('0') x 3,
		(map { $main::covTypeMap{$_->{type}} } @covariates),
		(map { $main::resTypeMap{$_->{type}} } @phenotypes));
	Log(join(' ', @cols));
	Log(join(' ', @types));
	# <p> write header
	my $fh = IO::File->new($c->{createSnptestPhenotypeFile}, 'w');
#		say $fh join(' ', @cols);
#		say $fh join(' ', @types);
		print $fh join(' ', @cols), "\n";
		print $fh join(' ', @types), "\n";
	$fh->close();

	# <p> join files together to produce 'sample file' content
	my @outputCols = ('fid', 'iid', 'missing',
		(map { $_->{name} } @covariates), (map { $_->{name} } @phenotypes));
	my $varPrefix = ($c->{variableFile} =~ m{^\[}sog)? '': '[header=T]:';
	#my $headerMap = unparseRdata(inverseDictFromString($c->{headerMap}));
	my $headerMap = unparseRdata(dictFromString($c->{headerMap}));
	my $cmd = "csv2.pl --o /dev/null --logLevel ". firstDef($TempFileNames::__verbosity, 4). ' --'
		." $varPrefix$c->{variableFile}"
		." --opr 'expr=names(TTOP) = vector.replace(names(TTOP), $headerMap)'"
		." $c->{pedFile}"
		.' --op setHeader=fid,iid,pid,mid,sex'
		.' --op joinOuterLeft=fid,iid'
		.' --op addCol=missing=0'
		.' --opr takeCol='.join(',', @outputCols)
		." --op 'writeTable=[header=F,sep=S,append=T,file=$c->{createPhenotypeFile}]' ";
	System($cmd, 4);
}

sub doCreateXXAssocPhenotypeFile { my ($c, @files) = @_;
	my $outputDir = $c->{createXXAssocPhenotypeFile};
	# <p> create variable file
	my $varFileCvtd = "$outputDir/variables";
	my $tempFileVarFile = tempFileName("/tmp/pipeline_$ENV{USER}_vfile");
	my $varPrefix = ($c->{variableFile} =~ m{^\[}sog)? '': '[header=T]:';
	#my $headerMap = unparseRdata(inverseDictFromString($c->{headerMap}));
	my $headerMap = unparseRdata(dictFromString($c->{headerMap}));
	my $setPedHeader = '--op setHeader=fid,iid,pid,mid,sex ';
	my $cmd;
	$cmd = "csv2.pl --o /dev/null --logLevel ". firstDef($TempFileNames::__verbosity, 4). " -- "
		."$varPrefix$c->{variableFile} "
		."--opr 'expr=names(TTOP) = vector.replace(names(TTOP), $headerMap)' "
		."$c->{pedFile} ". $setPedHeader. ' '
		."--opr 'expr=names(TTOP) = vector.replace(names(TTOP), $headerMap)' "
		."--opr takeCol=fid,iid,pid,mid,sex "
		."--op 'writeTable=[header=F,sep=S,file=$outputDir/pedfile]' "
		."--op joinOuterLeft=fid,iid "
		."--opr colsBringToFront=fid,iid "
		."--op 'writeTable=[header=F,sep=S,file=$varFileCvtd]' "
		."--op takeHeader "
		."--op 'writeTable=[header=F,sep=S,file=$varFileCvtd.cols]' ";
	System($cmd, 4);
}

#main $#ARGV @ARGV %ENV
	#initLog(2);
	my $c = StartStandardScript($main::d, $main::o);
exit(0);
