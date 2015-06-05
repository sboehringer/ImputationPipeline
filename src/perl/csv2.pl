#!/usr/bin/perl
#
#Mon Aug 23 18:07:57 CEST 2010
# use R to do table operations

use TempFileNames;
use Set;

$helpText = <<'HELP_TEXT';
	csv2.pl [options] -- --op operation1 file1 ... --op operation2 --op operation3 [[filter]:]fileN ...
	# --opr instead of --op replaces the current table instead of pushing a new table on the stack
	# Operations
		takeRow=ARG	select rows based on R-expression ARG
		takeCol=ARG	select cols based on R-expression ARG
		takeHeader	create new table only consisting of table names of top table
		transpose	transpose the top table

	# Filters
	# Filters are optional specificiation for reading individual files
	# A filter takes the form [op1=arg1,op2=arg2,...]:
	# Supported options are:
		header=T|F
		sep=S|T|,|;

	# Examples:

	# Transpose a space separated table
	csv2.pl -s --logLevel 5 -- file1 --op transpose
	# left outer join (merge),
	#	the 'left' table is the last table read (inverse notation)
	#	in this example /tmp/test1.csv
	#	the order of the left table is preserved
	csv2.pl --logLevel 5 -- [sep=S,header=T]:/tmp/test2.csv \
		[sep=S,header=F]:/tmp/test1.csv \
		--op setHeader=C0,C1 --op joinOuterLeft

	# Replace (genetic) map file physical positions,
	# retain old position if map-replace is not available
	csv2.pl --no-outputHeader --sepo T --o gift_chr22.map.new -- \
		[sep=S,header=T]:/hapmap2/phased/genotypes_CEU_r21_nr_fwd_legend \
		[sep=T,header=F]:gift_chr22.map \
		--op setHeader=chr,idRs,map,pos0 --op joinOuterLeft \
		--op 'expr=TTOP$pos[is.na(TTOP$pos)] = TTOP$pos0[is.na(TTOP$pos)]' \
		--op project=chr,idRs,map,pos 

	# Align two files according to some field
	# get order of lines for both files (writeTable) in order to intersect and
	#	align files
	csv2.pl --no-outputHeader -- [header=F]:map1 [header=F]:map2 \
		--op intersectWithOrder \
		--op writeTable=[header=F,sep=S,cols=O1,file=map1o] \
		--op writeTable=[header=F,sep=S,cols=O2,file=map2o]
	# for big files the final alignment can be helped by a table that together with
	# the current index gives the minium index that will be encountered later. This
	# allows to reduce the amount of lines held in memory at a given time
	csv2.pl --no-outputHeader -- [header=F]:map2o \
	--op 'expr=TTOP = cbind(TTOP, sapply(1:dim(TTOP)[1], function(i)min(TTOP[(i):dim(TTOP)[1], 1])))'
	# add constant column
	csv2.pl -- [header=F]:map1 --op addCol=new=0

HELP_TEXT
$helpText .= $TempFileNames::GeneralHelp;

%separators = ( '\\t' => "\t", ' ' => ' ', ',' => ',', ';' => ';' );
%repForSep = %{inverseMap({%separators})};
%separatorsA = ( %separators, 'S' => ' ', 'T' => "\t", C => ',' );

$RCMDTemplate = <<TEMPLATE;
source("$ENV{RPRIVATE}/RgenericAll.R");
TEMPLATE

%ops = (
	'takeRow' => { template => 'TNEW = TTOP[TARG, , drop = F];' },
	'takeCol' => { template => 'TTOP = TTOP[, COLS, drop = F];' },
	'addCol' => { template => 'TTOP = data.frame(TTOP, TARG);' },
	'colsBringToFront' => { template => 'colIndeces = which.indeces(COLS, names(TTOP));'
		.'TNEW = TTOP[, c(colIndeces, setdiff(1:dim(TTOP)[2], colIndeces)), drop = F];' },
	'takeHeader' => { template => 'TNEW = data.frame(names = names(TTOP));' },
	'setHeader' => { template => 'names(TTOP) = splitString(\',\', \'TARG\');' },
	'transpose' => { template => 'TTOP = t(TTOP);' },
	'cbind' => { template => 't00 = cbind(TALL);' },
	'select' => { template => 'TNEW = with(TTOP, TTOP[TARG, ]);' },
	'project' => { template => 'TTOP = TTOP[, COLS, drop = F];' },
	'project2cols' => { template => 'TTOPm1 = TTOPm1[, which.indices(TTOP[, 1], names(TTOPm1), drop = F];' },
	'project2rows' => { template => 'TTOPm1 = TTOPm1[which.indices(TTOP[, 1], TTOPm1[, COLS], drop = F];' },
	'intersect' => { template => 'TTOPm1 = data.frame(X=intersect(TTOPm1[, 1], TTOP[, 1]));' },
	'intersectWithOrder' => { template =>'TTOP = Merge('
		.'data.frame(X=TTOPm1[, 1], O1=1:dim(TTOPm1)[1]), '
		.'data.frame(X=TTOP[, 1], O2=1:dim(TTOP)[1]), safemerge = SAFEMERGE);' },
	'joinOuterLeft' => { template => 'ns = names(TTOP); '
		.'TTOP = data.frame(TTOP, order.orig.TTOP = 1:dim(TTOP)[1]); '
		.'mergeVars = if ("TARG" == "") intersect(names(TTOP), names(TTOPm1)) else COLS; '
		.'TNEW = Merge(TTOP, TTOPm1, by = mergeVars, all.x = T, sort = F, safemerge = SAFEMERGE); '
		.'TNEW = .dfm(TNEW[order(TNEW$order.orig.TTOP), ], "order.orig.TTOP");' },
	'joinOuter' => { template => 'TNEW = Merge(TTOP, TTOPm1, all = T, sort = F, safemerge = SAFEMERGE);' },
	'expr' => { template => 'TARG;' },
	'writeTable' => { complexArgs => 1,
		defaults => { APPEND => 'F', HEADER => 'T', SEP => 'T' },
		template => 'cols = if ("COLS" == "c()") 1:dim(TTOP)[2] else COLS; '
		.'write.table(TTOP[, cols], file = "FILE", col.names = HEADER, quote = F, row.names = F, sep = "SEP", append = APPEND);' },
);

sub colSelectorFromArg { my ($o, $arg) = @_;
	my $sep = defined($o->{colSeparator})? $o->{colSeparator}: ',';
	if ($arg =~ m{[a-z]}soig) {
		return sprintf('c(%s)', join(',', map { "\"$_\"" } split(/\s*$sep\s*/, $arg)));
	} else {
		return sprintf('c(%s)', join(',', split(/\s*$sep\s*/, $arg)));
	}
}

%FOMappers = (	# file option mappers
	'sep' => sub { ( separator => $separatorsA{$_[0]} ) },
	'header' => sub { ( header => $_[0] eq 'T'? 1: 0 ) }
);

sub fileOptions { my ($oStr) = @_;
	my %options = map { my ($k, $v) = split(/=/, $_);
		($FOMappers{$k}->($v))
	} split(/,/, $oStr);
	return %options;
}

# complex args are comma separated key-value pairs
%CONameMap = ( header => 'HEADER', cols => 'COLS', file => 'FILE',
	sep => 'SEP', append => 'APPEND'
);
sub complexOptions { my ($oStr, $defaults) = @_;
	my %options = map { my ($k, $v) = split(/=/, $_);
		($CONameMap{$k}, $v)
	} split(/,/, join('', ($oStr =~ m{^\s*\[(.*)\]\s*$}so)));
	# <p> special options
	$options{COLS} = colSelectorFromArg({ colSeparator => ':' }, $options{COLS});
	$options{SEP} = $separatorsA{$options{SEP}};
use Data::Dumper; Log(Dumper(\%options));
	return (%$defaults, %options);
}

sub produceRcmds { my ($o, $args) = @_;
	my $cmd = $RCMDTemplate;
	my $tables = 0;
	for (my $i = 0; $i < @$args; $i++) {
		if ($args->[$i] eq '--op' || $args->[$i] eq '--opr') {
			# <p> prepare special variables
			my ($ttopm1, $ttop, $tnew) =
				(sprintf("t%02d", $tables - 2), sprintf("t%02d", $tables - 1), sprintf("t%02d", $tables));
			# <p> analyze options
			my ($op, $opArg) = ($args->[++$i] =~ m{^(\w+)(?:\s*=\s*(.*))?}sog);
			$tnew = $ttop if ($args->[$i] eq '--opr');
			my %optionDict = (
				SAFEMERGE => $o->{safemerge},
				TTOPm1 => $ttopm1, TTOP => $ttop, TNEW => $tnew, TARG => firstDef($opArg, ''),
				TALL => join(', ', map { sprintf("t%02d", $_) } 0..($tables - 1)),
				COLS => colSelectorFromArg($o, $opArg)
			);
			# <p> handle complex args
			if ($ops{$op}{complexArgs}) {
				%optionDict = (%optionDict, complexOptions($opArg, $ops{$op}{defaults}));
			}

			my $rop = mergeDictToString(\%optionDict, $ops{$op}{template},
				{ sortKeys => 'YES', iterate => 'YES' } );
			$tables++ if ($ops{$op}{template} =~ /TNEW/o);
			$tables = 1 if ($ops{$op}{template} =~ /t00/o);	# collapsing operation
			$tables-- if ($ops{$op}{template} =~ /^TTOPm1/o);
			$cmd .= $rop. "\n";
			Log("Performing operation $op with arg '$opArg'", 4);
		} else {	# read a file
			my ($options, $file) = ($args->[$i] =~ m{(?:\[(.*?)\]:)?(.*)}sog);
			my %fo = (%$o, fileOptions($options));
			$file = $file eq '-'? 'stdin()': (($file =~ /gz$/so)? "gzfile('$file')": "'$file'");
			Log("Reading file: $file", 4);

			$cmd .= sprintf("t%02d = read.table(file = %s, sep = \"%s\", header = %s, "
				 ."as.is = T, comment.char = '');\n",
				$tables++, $file, $repForSep{$fo{separator}}, $fo{header}? 'T': 'F');
		}
	}
	$cmd .= sprintf(
		"write.table(t%02d, file = %s, quote = F, row.names = F, sep = \"%s\", col.names = %s);\n",
		$tables - 1, $o->{output} eq ''? 'stdout()': "'$o->{output}'",
			$repForSep{$separatorsA{$o->{outputSeparator}}},
		firstDef($o->{outputHeader}, $o->{header}, 1)? 'T': 'F');

	Log($cmd, 5);
	return $cmd;
}

#main $#ARGV @ARGV %ENV
#	initLog(2);
	$o = { config => 'config.cfg', header => 1, output => '', separator => ',', safemerge => 1 };
#	$o = { output => '/tmp/default.txt' };
#	$result = GetOptionsStandard($o, 'simple', 'filter|f=s', 'int=i', 'onOff!', 'output|o=s');
#	$result = GetOptionsStandard($o, 'credentials');
	$result = GetOptionsStandard($o, '-s', '-t', 'outputSeparator|sepo=s',
		'header!', 'output|o=s', 'outputHeader!', 'safemerge!');
	$o->{separator} = "\t" if ($o->{t});
	$o->{separator} = ' ' if ($o->{s});
	$o->{outputSeparator} = firstDef($o->{outputSeparator}, $o->{separator}, ',');

	if ($o->{help} || !$result)
	{	printf("USAGE: %s\n$helpText", ($0 =~ m{/?([^/]*)$}o));
		exit(!$result);
	}
	#$c = readConfigFile($o->{config});
	#$cred = KeyRing->new()->handleCredentials($o->{credentials}, '.this_cookie') || exit(0);
	my $Rcmds = produceRcmds($o, [@ARGV]);
	my $tempFile = tempFileName('/tmp/csv2transf');
	writeFile($tempFile, $Rcmds);
	System("R --vanilla --slave --no-readline -f $tempFile", 4);

exit(0);
