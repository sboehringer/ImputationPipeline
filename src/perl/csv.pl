#!/usr/bin/perl
#
#Wed Sep  2 15:15:15 CEST 2009

use TempFileNames;
use Data::Dumper;
use Set;
use DBI;
use FileHandle;

$TEST_CSV = <<TEST_CSV_TEXT;
a	b	c	d
a	c	1	X
b	d	2	X
aa	g	3	X
a	a	3	X
b	a	3	Y
c	d	4	Y
b	a	1.5	Z
TEST_CSV_TEXT

$helpText = <<HELP_TEXT;
	Options:
	-t	set input and output separator to "\\t" (default is ",")
	--transposeChunk=N	[tranpose]: how many columns are to be transposed of a time
			has to be a multiple of --transposeBy
	--transposeBy=N	[transpose]: how many columns are to be kept together in the tranposition
			if original is (R x C) produces (C/N, R*N)

	Examples:
	csv.pl -t /tmp/test.csv
	csv.pl -t /tmp/test.csv --map b=k:v --mapFile /tmp/test-map.csv
	# map column 1 in test.csv mapping values through pairs key:value from col 0:1 from test-map.csv
	csv.pl -t /tmp/test.csv --map 1=0:1 --mapFile /tmp/test-map.csv --no-mapHeader --no-header
	# map column 1 in test.csv mapping using the dict specification
	csv.pl /tmp/test.csv --map '1=DICT(c=v1;g=0)'
	# select columns
	csv.pl /tmp/test.csv --select 1
	# delete columns
	csv.pl /tmp/test.csv --delete 1
	# add constant columns
	csv.pl /tmp/test.csv --constant '0=FAM_RA,2=0,3=0,4=0,5=-9' --separator "\t"
	# add constant columns
	csv.pl /tmp/test.csv --reshuffle 2,3=3,2
	# map fields
	csv.pl /tmp/test.csv --filter '1+= { s/_/ /g }' --filter '1+= { s/a/ATCG/g }'
	csv.pl narac.csv --outputSeparator " " --no-outputHeader --delete 3,4,5,6,7,8 --reshuffle 1,2=2,1 --constant 0=NAR1,2=0,3=0 --filter '6+=s/_/ /g' --filter '6+=s/\\\?/0/g' 
	# combine files, RE-variables are substituted into other arguments
	csv.pl --combineFiles './.*chr(\\d+).legend' --constant '1=\$1' \
		-s --outputSeparator=, --setHeader 0=idRs,1=chr,2=pos
	# combine files as above, sort numerically by captured RE
	csv.pl --combineFiles './.*chr(\\d+).*legend' --constant '1=\$1' \
		-s --outputSeparator=' ' --setHeader 0=idRs,1=chr,2=pos,3=a1,4=a2 --sortNumerically | less
	# filter rows based on id-file and log the selected line numbers
	csv.pl -t /tmp/test.csv --selectRowsBy=c  --selectRowsIds=/tmp/ids.txt --logRowNumbers=/tmp/rowNumbers.txt
	# filter rows based on a file containing row indeces (as derivable from selectRowsBy)
	csv.pl -t /tmp/test.csv --selectRowsByRowNumbers=/tmp/rowNumbers.txt
	# transpose table
	csv.pl -t /tmp/test.csv --transpose --no-header
	# groupBy
	csv.pl  --useTestData -t --groupBy a::select_max::c --header
	# high performance transpose: transpose in steps by two columns each
	csv.pl -t --useTestData --no-header --transpose --transposeBy=2 --transposeChunks=2 --logLevel 4 --os=,
	# filter rows by expression
	csv.pl -t --useTestData --selectRowsByExpression 'c > 1.2'
	csv.pl -t --useTestData --selectRowsByExpression 'c > 1.2 && d eq "Y"'

	# order of operator precedence
	selectRowsFor, select, delete, reshuffle, constant, setHeader, filter

$TempFileNames::GeneralHelp
HELP_TEXT

%separators = ( '\\t' => "\t" );

# reset global fields
sub resetGlobalVariables {
	@globalTable = ();
	@globalHeader = ();
	@transposedHeader = ();
	$globalRow = 0;
}
resetGlobalVariables();

sub colsFromColSpec { my ($s, $header) = @_;
	my (@cols) = map { /(\d+):(\d+)/? ($1)..($2): $_ } split(/,/, $s);
	@cols = map { is_integer($_)? $_: indexOf($header, $_) } @cols;
	return @cols;
}

# <!><i> use sophisticated parsing/quoting from Set.pm
# <!> allow several processors

sub is_integer { $_[0] =~ m{^[0-9]+$}o; }
sub processor { my ($o, $header) = @_;
	my $rowNumber = 0;	# row number counter
	my @prcs;
	# <p> row selection
	if ($o->{selectRowsBy} ne '') {
		my $col = $o->{selectRowsBy};
		my $colI = is_integer($col)? int($col): indexOf($header, $col);
		my $d = dictWithKeys([split(/\n/, readFile($o->{selectRowsIds}))]);
		push(@prcs, sub {
			return undef if (!defined($d->{@_[$colI]}));
			return @_;
		});
	}
	if ($o->{selectRowsByExpression} ne '') {
		my $cols = makeHash($header, [map { "\$_[$_]" } 0 .. $#$header]);
		$cols->{''} = undef;
		Log("Columns: ". join(', ', keys %$cols), 5);
		my $code = 'sub { $_ = $_[0]; '. mergeDictToString($cols, $o->{selectRowsByExpression}). ' }';
		Log("row filter: '$code'", 5);
		my $sub = eval($code);
		push(@prcs, sub {
			return undef if (!$sub->(@_));
			return @_;
		});
	}
	if ($o->{selectRowsByRowNumbers} ne '') {
		my $d = dictWithKeys([(($o->{selectRowsByRowNumbers} =~ m{^\d[\d,:]+$}sog)
		? colsFromColSpec($o->{selectRowsByRowNumbers})
		: split(/\n/, readFile($o->{selectRowsByRowNumbers})))]);
		push(@prcs, sub {
			return undef if (!defined($d->{$rowNumber}));
			return @_;
		});
	}
	if ($o->{selectRowsByFile} ne '') {
		my ($col, $path) = split(/,/, $o->{selectRowsByFile});
		my $colI = is_integer($col)? int($col): indexOf($header, $col);
		my $sel = dictWithKeys([map { $_->[0] } @{readCsv($path, {header => 0})->{data}}]);
		push(@prcs, sub {
			return undef if (!defined($sel->{@_[$colI]}));
			return @_;
		});
	}

	# <p> catch entries to form a new, transposed header
	if (defined($o->{transposedHeaderFrom})) {
		push(@prcs, sub {
			push(@transposedHeader,  @_[$o->{transposedHeaderFrom}]);
			return @_;
		});
	}

	if ($o->{map} ne '') {
		my ($map, $col, $colI,   $key, $val, $dictS);
		if ($o->{map} =~ m{=\s*DICT}) {
			($col, $dictS) = ($o->{map} =~ m{^([^=]*)=\s*DICT\s*\((.*)\)$}so);
			$map = { map { split(/\s*=\s*/) } split(/\s*;\s*/, $dictS) };
		} else {
			my $mapCsv = readCsv($o->{mapFile}, { sep => firstDef($o->{mapSeparator}, $o->{separator}),
				autoColumns => !$o->{mapHeader}});
			($col, $key, $val) = ($o->{map} =~ m{^([^=]*)=(.*):(.*)$}so);
			my $keyI = is_integer($key)? int($key): indexOf($mapCsv->{factors}, $key);
			my $valI = is_integer($val)? int($val): indexOf($mapCsv->{factors}, $val);
			my @keys = map { $_->[$keyI] } @{$mapCsv->{data}};
			my @vals = map { $_->[$valI] } @{$mapCsv->{data}};
			$map = makeHash([@keys], [@vals]);
		}
		$colI = is_integer($col)? int($col): indexOf($header, $col);
		push(@prcs, sub {
			@_[$colI] = $map->{@_[$colI]} if (defined($map->{@_[$colI]}));
			return @_;
		});
	}
	if ($o->{select} ne '') {
		my @cols = colsFromColSpec($o->{select}, $header);
		push(@prcs, sub { return @_[@cols];	});
	}
	if ($o->{delete} ne '') {
		my @cols = colsFromColSpec($o->{delete}, $header);	#split(/,/, $o->{delete});
		push(@prcs, sub {@_[@cols] = undef; return grep { defined($_) } @_; } );
	}
	if ($o->{deleteBeforeSelect}
		&& $o->{select} ne '' && $o->{delete} ne '') {
		my $select = $prcs[$#prcs - 1];
		splice(@prcs, -2, 1);
		push(@prcs, $select);
	}
	if ($o->{reshuffle} ne '') {
		my ($to, $from) = (split(/=/, $o->{reshuffle}));
		my @colsTo = split(/,/, $to);
		my @colsFrom = split(/,/, $from);
		push(@prcs, sub { my @new = @_;
			for $i (0..$#colsTo) { $new[$colsTo[$i]] = @_[$colsFrom[$i]]; }
			return @new;
		});
	}
	# <!><i> proper quoting support
	# <!><i> header support
	if ($o->{constant} ne '') {
		# insertion of constant columns
		my @cc = split(/,/, $o->{constant});
		my @idcs = map { /^\s*(\d+)/; int($1) } @cc;
		my @values = map { /^\s*\d+\s*=\s*(.*)/; $1 } @cc;
		push(@prcs, sub {
			for $i (0..$#idcs) { splice(@_, $idcs[$i], 0, $values[$i]); }
			return @_;
		});
	}
	if ($o->{insertOrderByFile} ne '') {
		my ($colTo, $colFrom, $path) = split(/,/, $o->{insertOrderByFile});
		my @ordered = map { $_->[0] } @{readCsv($path, {header => 0})->{data}};	# ordered list of values
		my $oh = makeHash(\@ordered, [0..$#ordered]);
		push(@prcs, sub {
			my $o = $oh->{$_[$colFrom]};
			Log("No order defined for value $_[$colFrom].", 1) if (!defined($o));
			splice(@_, $colTo, 0, sprintf("%06d", $o));
			return @_;
		});
	}

	if ($o->{filter} ne '') {
		foreach $filter (@{$o->{filter}}) {
			my ($cols, $filter) = ($filter =~ m{(.*?)\=(.*)}s);
			my @cols = split(/,/, $cols);
			my $code = 'sub { $_ = $_[0]; '. $filter. '; $_[0] = $_; }';
			my $sub = eval($code);
			push(@prcs, sub { my @new = @_;
				my @ccols = @cols;
				splice(@ccols, -1, 1, int($ccols[$#ccols])..($#_)) if (substr($ccols[$#ccols], -1) eq '+');
				for $i (@ccols) { $sub->($new[$i]); }
				return @new;
			});
		}
	}
	# <p> output processors (unconditional processors)
	if (defined($o->{transpose}) || defined($o->{transposeBy})
	||  defined($o->{groupBy}) || defined($o->{cbind}) || defined($o->{probeColumns})) {
		# store
		push(@prcs, sub {
			push(@{$globalTable[$globalRow++]}, @_);
			return 0;
		});
	} else {
		#print
		push(@prcs, sub {
			my $sep = mergeDictToString(\%separators, firstDef($o->{outputSeparator}, $o->{separator}, ','));
			print { $o->{fhOut} } join($sep, @_), "\n";
			return 0;
		});
	}
	# <p> log row numbers
	if ($o->{logRowNumbers} ne '') {
		my $f = $o->{logRowNumbers};
		open($f, ">$f");
		# <N> rely on automatic closing of file upon exit
		push(@prcs, sub {
			print $f "$rowNumber\n"; return undef;
		});
	}

	# <p> return meta closure
	return sub {
		for $p (@prcs) {
			@_ = $p->(@_);
			last if (!defined($_[0]));	#<!> zero column tables
		}
		$rowNumber++;
	};
}
sub headerProcessor { my ($o, $header) = @_;
	my @prcs;
	if ($o->{select} ne '') {
		my @cols = colsFromColSpec($o->{select}, $header);
		#my @cols = split(/,/, $o->{select});
		push(@prcs, sub { return @_[@cols]; });
	}
	if ($o->{delete} ne '') {
		my @cols = split(/,/, $o->{delete});
		push(@prcs, sub { @_[@cols] = $magic; return grep { $_ ne $o->{magic} } @_; });
	}
	if ($o->{reshuffle} ne '') {
		my ($to, $from) = (split(/=/, $o->{reshuffle}));
		my @colsTo = split(/,/, $to);
		my @colsFrom = split(/,/, $from);
		push(@prcs, sub { my @new = @_;
			for $i (0..$#colsTo) { $new[$colsTo[$i]] = @_[$colsFrom[$i]]; }
			return @new;
		});
	}
	if ($o->{constant} ne '') {
		# insertion of constant columns
		my @cc = split(/,/, $o->{constant});
		my @idcs = map { /^\s*(\d+)/; int($1) } @cc;
		my @values = map { 'C_'.($_ + 1) } @idcs;
		push(@prcs, sub {
			for $i (0..$#idcs) { splice(@_, $idcs[$i], 0, $values[$i]); }
			return @_;
		});
	}
	if ($o->{setHeader} ne '') {
		# insertion of constant columns
		my @cc = split(/,/, $o->{setHeader});
		my @idcs = map { /^\s*(\d+)/; int($1) } @cc;
		my @values = map { /^\s*\d+\s*=\s*(.*)/; $1 } @cc;
		push(@prcs, sub {
			for $i (0..$#idcs) { @_[$idcs[$i]] = $values[$i]; }
			return @_;
		});
	}
	if (defined($o->{groupBy}) || defined($o->{cbind})) {
		# store
		push(@prcs, sub { push(@globalHeader, @_);
			return @_;
		});
	}
	if (firstDef($o->{outputHeader}, 1) && !$o->{cbind} && !$o->{probeColumns}) {
		my $sep = mergeDictToString(\%separators, firstDef($o->{outputSeparator}, $o->{separator}, ','));
		push(@prcs, sub { print { $o->{fhOut} } join($sep, @_), "\n"; return @_; });
	}
	return sub { for $p (@prcs) { @_ = $p->(@_); } return @_; };
}

sub parser { my ($o) = @_;
	return sub { split(/$o->{separator}/, substr(<>, 0, -1)); }
}

sub parserFile { my ($o, $path) = @_;
	open($path, $path);
	return sub { split(/$o->{separator}/, substr(<$path>, 0, -1)); }
}

# only parse one line (for probing)
sub parserFile1 { my ($o, $path) = @_;
	my $line = 0;
	open($path, $path);
	return sub {
		return () if ($line++ > defined($o->{header}));
		split(/$o->{separator}/, substr(<$path>, 0, -1));
	}
}

# does not work efficiently enough: Thu Oct 15 17:02:23 CEST 2009
sub parser_dbi { my ($o, $f) = @_;
	my $fd = splitPathDict($f);
	$dbh = DBI->connect("DBI:CSV:f_dir=$fd->{dir}") or die "Cannot connect: " . $DBI::errstr;
	#$sth = $dbh->prepare("CREATE TABLE a (id INTEGER, name CHAR(10))")
	#	or die "Cannot prepare: " . $dbh->errstr();
	$sth = $dbh->prepare("SELECT * FROM narac.csv");
	$sth->execute() or die "Cannot execute: " . $sth->errstr();
	$sth->finish();
	$dbh->disconnect();
}

sub iterateInput { my ($pa, $o) = @_;
	my $header = undef;
	if ($o->{header}) {
		$header = [map { uqs($_) } $pa->()];
		my $hpr = headerProcessor($o, $header);
		my @headerNew = $hpr->(@$header);
	}
	$pr = processor($o, $header);
	while (@fields = $pa->()) {
		$pr->(@fields);
	}
}

sub combineFiles { my ($o) = @_;
	my @keys = keys %$o;
	my $sp = splitPathDict($o->{combineFiles});
	my @files = grep { /$sp->{file}/ } dirList($sp->{dir});
	if ($o->{sortNumerically}) {
		@files = sort {
			my ($an) = ($a =~ /$sp->{file}/);
			my ($bn) = ($b =~ /$sp->{file}/);
			$an <=> $bn
		} @files;
	}
	my $i;
	for ($i = 0; $i < @files; $i++) {
		@ARGV = $files[$i];	# <!> make use of <> mechanism in parser
		# substitute in patterns from filename
		my @pat = ($files[$i] =~ m/$sp->{file}/s);	# catch matches
		my $on = {%$o, outputHeader => !$i};
		$on->{$_} =~ s/\$(\d+)/$pat[$1 - 1]/se for (@keys);

		iterateInput(parser($on), $on);
	}
}

sub select_max { my ($rows, $values) = @_;
	#Log('Rows: '. join(' ', @$rows). ' Values: '. join(' ', @$values), 2);
	return [$rows->[whichMax($values)]];
}

sub groupBy { my ($o) = @_;
	iterateInput(parser($o), $o);
	my ($colN, $funcN, $colFN) = split(/\s*::\s*/, $o->{groupBy});
	my $col = is_integer($colN)? int($colN): indexOf(\@globalHeader, $colN);
	my $colF = is_integer($colFN)? int($colFN): indexOf(\@globalHeader, $colFN);
	Log("Group by variable $colN [Col:$col] reducing by $funcN on $colFN [Col:$colF].", 5);

	# <p> capture groups
	my ($n, $m, $dict) = (int(@globalTable), int(@{$globalTable[0]}));
	for (my $i = 0; $i < $n; $i++) { push(@{$dict->{$globalTable[$i]->[$col]}}, $i); }
	# <p> reduce groups
	foreach $k (keys %$dict) {
		$dict->{$k} = $funcN->($dict->{$k}, [map { $globalTable[$_]->[$colF] } @{$dict->{$k}}]);
	}
	my @rows = sort map { @$_ } values %$dict;
	# <p> output
	my $sep = mergeDictToString(\%separators, firstDef($o->{outputSeparator}, $o->{separator}, ','));
	for (my $i = 0; $i < @rows; $i++) {
		print { $o->{fhOut} } join($sep, @{$globalTable[$rows[$i]]}), "\n";
	}
}

sub transpose { my ($o, $file) = @_;
	# <p> preparation
	# tranpose by groups of columns
	my $by = firstDef($o->{transposeBy}, 1);

	# <p> read input
	iterateInput(defined($file)? parserFile($o, $file): parser($o), $o);
	my ($n, $m) = (int(@globalTable), int(@{$globalTable[0]}));
	Log("Table layout: $n x $m", 5);
	Log('Transposing to: '. ($m/$by).' x '. $n*$by, 5);

	# <p> prepare output
	my $sep = mergeDictToString(\%separators, firstDef($o->{outputSeparator}, $o->{separator}, ','));
	# <p> output header
	if (defined($o->{transposedHeaderFrom})) {
		# repeat headers as dictated by the transposeBy argument
		my @hd = map { my $h = $_; map { $h.$_ } 1..$by } @transposedHeader;
		print { $o->{fhOut} } join($sep, @hd). "\n";
	}
	# <p> create transposed rows
	my $i;
	for ($i = 0; $i < $m/$by; $i++) {
		print { $o->{fhOut} } join($sep, map { @{$_}[($by*$i)..($by*($i+1) - 1)] } @globalTable), "\n";
	}
}

# allow partial transposition in case of very large files
sub transposeBlocked { my ($o, $file) = @_;
	if (defined($o->{transposeChunks})) {
		die "Option --transpose with transposeChunks argument does not work with stdin-input."
			if (!defined($file));
		iterateInput(parserFile1($o, $file), $o);
		close($file);	#<!> better resource handling
		my $cols = int(@{$globalTable[0]});
		my $chunks = $o->{transposeChunks};
		for (my $i = 0; $i * $chunks < $cols; $i++) {
			my $tmpo = {%$o, select => sprintf("%d:%d", $i * $chunks, min(($i + 1) * $chunks - 1, $cols))};
			Log("Transposing chunk: $tmpo->{select}", 5);
			resetGlobalVariables();
			transpose($tmpo, $file);
		}
	} else {
		transpose($o);
	}
}

sub cbind { my ($o, $files) = @_;
	# <p> preparation

	# <p> read input
	foreach $path (@$files) {
		$globalRow = 0;
		iterateInput(parserFile($o, $path), $o);
	}

	# <p> prepare output
	my $sep = mergeDictToString(\%separators, firstDef($o->{outputSeparator}, $o->{separator}, ','));
	# <p> output header
	if (!!$o->{outputHeader}) {
		print { $o->{fhOut} } join($sep, @globalHeader). "\n";
	}
	# <p> create transposed rows
	for (my $i = 0; $i < @globalTable; $i++) { print { $o->{fhOut} } join($sep, @{$globalTable[$i]}), "\n" }
}

sub probeColumns { my ($o, $files) = @_;
	foreach $file (@$files) {
		resetGlobalVariables();
		iterateInput(parserFile1($o, $file), $o);
		close($file);	#<!> better resource handling
		my $cols = int(@{$globalTable[0]});
		Log("Cols $cols for file $file");
	}
}

#main $#ARGV @ARGV %ENV
	initLog(2);
	$o = { output => 'STDOUT',
		separator => ',', quoteChar => '"', header => 1, mapHeader => 1, mapSeparator => "\t",
		magic => '<!>magic-placeholder<!>'
	};
	$result = GetOptionsStandard($o, 'separator=s', 'outputSeparator|os=s', 'quoteChar=s', 'magic=s',
		'header!', 'outputHeader!', 'setHeader=s',
		'map=s', 'mapHeader!', 'mapFile=s', 'mapSeparator=s',
		'select=s', 'constant=s', 'delete=s', 'reshuffle=s', 'filter=s@', 'insertOrderByFile=s',
		'selectRowsBy=s', 'selectRowsIds=s', 'selectRowsByFile=s', 'selectRowsByRowNumbers=s',
			'selectRowsByExpression=s',
		'logRowNumbers=s',
		'deleteBeforeSelect',

		# short hands
		't', 's',	#initialize separators to tab, space resp.

		# aggregators
		'combineFiles=s', 'sortNumerically',
		'groupBy=s',

		# special
		'transpose', 'transposeChunks=s', 'transposeBy=s', 'transposedHeaderFrom=s',
		'cbind',
		'probeColumns',

		# testing
		'useTestData',
		
		# output
		'output|o=s'
	);
#	$result = GetOptionsStandard($o, 'credentials');
	$result = GetOptionsStandard($o);
	$o->{separator} = "\t" if ($o->{t});
	$o->{separator} = ' ' if ($o->{s});
	$o->{fhOut} = $o->{output} eq 'STDOUT'? \*STDOUT: FileHandle->new($o->{output}, 'w');
	if ($o->{help} || !$result)
	{	printf("USAGE: %s\n$helpText", ($0 =~ m{/?([^/]*)$}o));
		exit(!$result);
	}
	#$c = readConfigFile($o->{config});
	if (0) {
	foreach $f (@ARGV) {
		my $p = parser_dbi($o, $f);
	}
	} else {
	# <p> activate test data
	if ($o->{useTestData}) {
		my $tf = tempFileName('/tmp/cvsTest');
		writeFile($tf, $TEST_CSV);
		push(@ARGV, $tf);
	}

	# <p> perform actions
	if (defined($o->{combineFiles})) {
		combineFiles($o);
	} elsif (defined($o->{transpose}) || defined($o->{transposeBy})) {
		transposeBlocked($o, $ARGV[0]);
	} elsif (defined($o->{groupBy})) {
		groupBy($o);
	} elsif (defined($o->{cbind})) {
		cbind($o, [@ARGV]);
	} elsif (defined($o->{probeColumns})) {
		probeColumns($o, [@ARGV]);
	} else {
		iterateInput(parser($o), $o);
	}
	}
	$o->{fhOut}->close();
exit(0);
