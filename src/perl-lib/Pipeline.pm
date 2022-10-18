#
#	Pipeline.pm
#Tue Aug 16 14:51:26 CEST 2011
#

#
#	<p> global variables
#

#	identifier: /[a-z_][a-z0-9_]*/i
my $pgrammar = q{
	pipeline: pipeline_component(s? /\|/)
	pipeline_component: sub_pipeline | pipe
	sub_pipeline: '(' pipeline(s /,/) ')'
		{ $return = $item[2]; }
	pipe: identifier pipe_tag(?)
		{ $return = { name => $item[1], tag => $item[2][0] }; }
	pipe_tag: ':' identifier
		{ $return = $item[2] }
	identifier: /[a-z0-9_]+/i
	string: /STRING_RE/
};

#
#	<p> functions
#

# $ls:	lines
sub readPipelineConfig { my ($ls, $c) = @_;
	# <p> check nesting of includes
	die "Config file inclusions nested too deeply for $f" if ($c->{level} > $c->{maxConfigNesting});
	my @c = ();
	my @lines = @$ls;
	while (@lines > 0) {
		my $l = shift(@lines);
		next if ($l =~ m{^\s*$|^#}so);
		my ($k, $v) = ($l =~ m{(\S+)\s*(.*)$}sog);
		# <p> line coninuation
		for (; $v =~ m{\\$}so; $v .= shift(@lines)) { chop($v); }
		if ($k eq '@include') {
			# as a special case, allow interporlation of hostname in includes <!>
			$v = mergeDictToString({ 'ENV:HOST' => $ENV{HOST}, '$HOST' => $ENV{HOST} }, $v);
			push(@c, readPipelineConfigFile($v, { %$c, level => $c->{level} + 1} ));
		} else { push(@c, $k, $v); }
	}
	return @c;
}
sub readPipelineConfigFile { my ($f, $c) = @_;
	# <p> read file into lines
	my @c = readPipelineConfig([split(/\n/, readFile(firstFile($f, $c->{pathes})))], $c);
	return @c;
}

# allow regexes of the following form:
# ~~split~~{regex}~~sep~~value
# split with regex "split", match regex globally on value and join with "sep"
sub parseConfigRegexes { my (%c) = @_;
	my @vs = map {
		my ($spl, $regex, $sep, $v) = ($_ =~ m{^~~(.*)~~[{](.*)[}]~~(.*)~~(.*)$}so);
		if ($regex ne '') {
			$v = mergeDictToString(\%c, $v, { sortKeys => 'YES', iterate => 'YES' });
			join($sep, map { $_ =~ m{$regex}s } split(/$spl/, $v));
		} else { $_ }
	} values %c;
	%c = %{makeHash([keys %c], \@vs)};
	return %c;
}

# read pipeline configs, handling includes and regexes
# no substitution is performed
sub readPipelineConfigsRaw { my ($fs, $pathes, $literals) = @_;
	# <p> raw read
	my $rpC = { level => 0, maxConfigNesting => $main::d->{maxConfigNesting}, pathes => $pathes };
	my %c = 		readPipelineConfig($literals, $rpC);
	%c = (%c, map { readPipelineConfigFile($_, $rpC ) } @$fs);
	# <p> regex matching
	%c = parseConfigRegexes(%c);
	return {%c};
}

# perform recursive substitutions in config values
sub pipelineConfigSubstitute { my ($c) = @_;
	my %c = %$c;
	# <p> deep replacement
	my %r = (%c, map { ("\$$_", $ENV{$_}) } keys %ENV);	# augement %c with environment variables
	my @vs = map { mergeDictToString(\%r, $_, { sortKeys => 'YES', iterate => 'YES'}) } values %c;
	%c = %{makeHash([keys %c], \@vs)};
	# <p> sub options for pipeline components
	my @comps = unique(map { (split(/:/))[0] } keys %c);
	# <p> build dicts for components
	%c = map { my $comp = $_;
		my @es = grep { (split(/:/))[0] eq $comp } keys %c;
		my %cc = map { my $k = (split(/:/))[1]; ($k, $c{$_}) } @es;
		( $comp => { %cc } )
	} @comps;
	return {%c};
}

# Iterate nested structure: HASH: pipe, ARRAY: sub-pipeline, or
#	HASH.isSubpipeline: sub-pipeline, HASH:pipe
# $p: pipeline, $i: nesting level
# %o: pipe: pipeline component handler, subPipe: sub-pipeline handler 
#	catchDependencies: which dependency leaves a given pipe? (<=> nextDepends)
sub iteratePipelineRaw { my ($p, $i, $depends, %o) = @_;
	# iterate components
	my @r = map { my ($k, $pc) = ($_, $p->[$_]);
		# <p> which ids does this pipe depend on
		my $depIds = $k
			? (ref($p->[$k - 1]) eq 'HASH'? $p->[$k - 1]{id}: undef)
			: $depends;
		if (ref($pc) eq 'ARRAY' || (ref($pc) eq 'HASH' && $pc->{isSubpipeline})) {
			$pc = $pc->{pipeline} if (ref($pc) eq 'HASH');
			my $sp = [map {
					[iteratePipelineRaw($pc->[$_], $i + 1, $depIds, %o)]
				} 0 .. $#$pc];
			# <p> call handler
			$sp = $o{subPipe}->($sp, $i,
				pipeIndex => $k, dependsOn => $depIds, %o) if (defined($o{subPipe}));
			$sp
		} elsif (ref($pc) eq 'HASH') {
			my $r = defined($o{pipe})? $o{pipe}->($pc, $i,
				pipeIndex => $k, dependsOn => $depIds, %o): $pc;
			$r
		}
	} 0 .. $#$p;
	return @r;
}

# $p: pipeline, $i: intendation
# %o: pipe: pipeline component handler, subPipe: sub-pipeline handler 
sub iteratePipeline { my ($p, $i, %o) = @_;
	return iteratePipelineRaw($p, $i, undef, %o);
}

sub expandStrataRaw { my ($pl, $pars, %o) = @_;
	my @p = ();
	for (my $i = 0; $i < @$pl; $i++) {
		my $pipe = $pl->[$i];
		if (ref($pipe) eq 'HASH') {
			# <A> autovivification
			my $strata = firstDef($pars->{$pipe->{tag}}{strata},
				$pars->{$pipe->{name}}{strata});
			if (defined($strata)) {
				my @strata = split(/\s*,\s*/, $strata);
				Log("Strata expansion $pipe->{name}:$pipe->{tag}: $strata", 5);
				my @sp = map {
					my $head = { %$pipe, stratum => $_ };
					my @tail = @{expandStrataRaw([@{$pl}[($i + 1) .. $#$pl]], $pars,
						forceSubpipe => 1)};
					[$head, @tail]
				} @strata;
				push(@p, [ @sp ]);
				return [@p];
			} else { push(@p, $pipe); }
		} else {
			push(@p, expandStrataRaw($pipe, $pars));
		}
	}
	return [@p];
}

# automatically copy pipeline components
#	add stratum component to pipes as appropriate
sub expandStrata { my ($pl, $pars) = @_;
	return expandStrataRaw($pl, {%$pars});
}

sub recordDependencies { my ($p) = @_;
	# <p> record dependencies-
	my $id = 0;		# id counter for pipes
	my @p0 = iteratePipeline($p, 0,
		subPipe => sub { my ($sp) = @_;
			$sp = { isSubpipeline => 1, pipeline => $sp, id =>
				[map { my @a = @$_; @{$a[$#a]->{id}} } @$sp] } },
		pipe => sub { my ($p, $i, %o) = @_;
			{ pipeline => $p, id => [${$o{id}}++] } },
		id => \$id
	);
	# <p> re-hook dependencies
	my @p1 = iteratePipeline([@p0], 0,
		subPipe => sub { my ($sp) = @_; $sp },
		pipe => sub { my ($pc, $i, %o) = @_;
			return { %{$pc->{pipeline}}, dependsOn => [@{$o{dependsOn}}],
				id => $pc->{id}[0], level => $i };
	});
	return [@p1];
}

# pipeline instances are specializations of generic pipes
# a pipe specified in the pipeline as generic:instance is duplicated with vars instance:var
# overwriting vars generic:var
sub pipelineConfigCopyInstances { my ($pipeline, $p) = @_;
	# <p> new version parameters
	my $pN = {%$p};
	# <p> seriazlize pipeline
	$pipeline = [unlist(@$pipeline)];
	# copy config and presubstitute to get parameter dict-structure
	my $pC = pipelineConfigSubstitute({%$p});
	# <p> handle pipe-instances
	for (my $i = $from; $i <= $to; $i++) {
		my $pipe = $pipeline->[$i];
		if ($pipe->{tag} ne '') {
			my @n = keys(%{$pC->{$pipe->{name}}});	# variables defined by the super-pipe
			my @sn = map { "$pipe->{name}:$_" } @n;	# variable qualifiers super-pipe
			my @in = map { "$pipe->{tag}:$_" } @n;	# variable qualifiers instance-pipe
			my $d = makeHash([@sn], [@in]);			# variable substitution dict
			my @v = map { firstDef($p->{"$pipe->{tag}:$_"},
				mergeDictToString($d, $p->{"$pipe->{name}:$_"}, { sortKeys => 'YES' })) } @n;
			$pN = { %$pN, %{makeHash([@in], [@v])} };
		}
	}
	return $pN;
}

sub readPipeline { my ($fs, $pathes) = @_;
	# <p> raw parsing of config
	my $p = readPipelineConfigsRaw($fs, $pathes, ['G:WD '.substr(`pwd`, 0, -1)]);
	# copy config and presubstitute to be able to construct the pipeline
	my $pC = pipelineConfigSubstitute({%$p});
	# <p> parse pipeline definition
	my $pparser = Parse::RecDescent->new($pgrammar);
	my $pipelineName = firstDef($pC->{G}{Name}, 'Pipeline');
	my $pipeline = $pparser->pipeline($pC->{G}{$pipelineName});
	# <p> copy configs based on pipeline definition
	$p = pipelineConfigCopyInstances($pipeline, $p);
	# <p> subistitute values
	$p = pipelineConfigSubstitute($p);

	$pipeline = expandStrata($pipeline, $p);
	Log('Pipeline: '. Dumper($pipeline), 5);
	$pipeline = recordDependencies($pipeline);
	Log('Pipeline: '. Dumper($pipeline), 5);
	return {parameters => $p, pipeline => $pipeline};
}

sub pipeFullName { my ($p) = @_;
	return $p->{name}. ($p->{tag} ne ''? ':'. $p->{tag}: '');
}

%main::PipelineTableDesc = ( parameters => { width => 79 },
	columns => {
		id => { width => 2, format => '%0*d' },
		component => { width => -30, format => '%*s' },
		stratum => { width => -15, format => '%*s' },
		'#' => { width => 4, format => '%*d' },
		perc => { width => 4, format => 'percent' },
		progress => { width => -18, format => '%*s' }
	}
);

sub pipeDesc { my ($pc, $i, %o) = @_;
	{	id => $pc->{id},
		component => (' ' x (2*$i)). ($o{pipeIndex}? '-': '+'). pipeFullName($pc),
		stratum => $pc->{stratum}
	}
}

sub pipeDirForPpars { my ($pp, $id) = @_;
	# <p> fetch pars
	my $prefix = firstDef($pp->{G}{prefix}, 'pipeline');
	my $prefixDigits = firstDef($pp->{G}{digits}, 2);
	return sprintf('%s_%0*d', $prefix, $prefixDigits, $id);
}

sub pipeDescProgress { my ($pc, $i, %o) = @_;
	my $progressError = { %{pipeDesc($pc, $i, %o)}, perc => 'nan', '#' => 'nan', progress => '!error!' };
	# read spec file
	my $spec = eval { propertyFromString(readFile(
		pipeDirForPpars($o{parameters}, $pc->{id}). '/files.spec'))};
	return $progressError if ($@);
	my $Njobs = int(@{$spec->{jids}});
	my $progress = 1;
	if ($Njobs) {
		my $Nrunning = int(grep { defined } which_indeces($spec->{jids}, $o{jids}, 1));
		$progress = 1 - ($Nrunning / $Njobs);
	}

	return {
		%{pipeDesc($pc, $i, %o)},
		perc => $progress, '#' => $Njobs, progress => progressPrint($progress,
			width => abs($main::PipelineTableDesc{columns}{progress}{width}))
	};
}

sub printPipeline { my ($p, $f, $cols, %o) = @_;
	# <p> default paramters
	$cols = ['id', 'component', 'stratum'] if (!defined($cols));
	$f = \&pipeDesc if (!defined($f));

	say(sprintf("Pipeline %s running in directory '%s'\n",
		firstDef($p->{parameters}{G}{Name}, '[Generic Pipeline]'), substr(`pwd`, 0, -1)));
	say(formatTable(\%main::PipelineTableDesc,
		[unlist([iteratePipeline($p->{pipeline}, 0,  pipe => $f, %o)])],
		$cols
	));
}

sub printPipelineProgress { my ($p, $c) = @_;
	my $ids = [$c->{schedulerClass}->new()->queuedJobs()];
	printPipeline($p, \&pipeDescProgress, 
		['id', 'component', 'stratum', '#', 'progress', 'perc'],
		parameters => $p->{parameters}, jids => $ids
	);
}

sub prepareRunningDir { my ($c, $p) = @_;
	# directories containing runs
	my $runPrefix = firstDef($p->{parameters}{G}{RunningPrefix}, 'R_');
	my $runPrefixDigits = firstDef($p->{parameters}{G}{RunningPrefixDigits}, 2);

	# determine files and dirs
	my $prefix = firstDef($p->{parameters}{G}{prefix}, 'pipeline');
	my $prefixDigits = firstDef($p->{parameters}{G}{digits}, 2);
	my $pipeDir = sub { sprintf('%s_%0*d', $prefix, $prefixDigits, $_[0]) };
	my @outputDirs = map { $pipeDir->($_->{id}) } unlist(@{$p->{pipeline}});
	my ($from, $to) = (firstDef($c->{from}, 0), firstDef($c->{to}, int(@outputDirs)));
	@outputDirs = @outputDirs[$from .. $to];

	# further directories
	my @otherDirs = ('diag');

	# <p> remove symbolic links
	Unlink([@outputDirs, @otherDirs], 5);

	# <p> what was run before?
	my @dirs = map { ($_ =~ m{(\d+)$})[0] } dirListPattern($runPrefix);
	# <p> make new running dir
	my $runningDir = sprintf('%s%0*d', $runPrefix, $runPrefixDigits, !@dirs? 1: (max(@dirs) + 1));
	Mkdir($runningDir, 3);
	# <p> create dirs and links
	foreach $o (@outputDirs, @otherDirs) {
		Mkdir("$runningDir/$o", 5);
		Symlink("$runningDir/$o", $o, 5);
	}
	return $runningDir;
}

sub LogPipeline { my ($p) = @_;
	my $logDir = $main::ENV{PIPELINE_LOG_DIR};
	return if ($logDir eq '');
	my $d = (sort { $b cmp $a } dirListPattern("$logDir/\\d+"))[0];
	writeFile(sprintf("$logDir/%05d_pipelineRun", defined($d)? $d + 1: 0), stringFromProperty($p));
	#writeFile('diag/pipeline.plist', stringFromProperty($p));
}

sub submitPipeline { my ($c, $p) = @_;
	my $runningDir = prepareRunningDir($c, $p);
	# <p> redirect stderr
	open STDERR, ">diag/pipeline_diagnostics_log_$p->{parameters}{G}{Name}"
		or die "Can't redirect STDERR: $!"
		if (!defined($c->{'print-diagnostics'})); #'

	# <p> set environment variables
	%ENV = (%ENV, %{$p->{parameters}{ENV}});
	# <p> fetch pars
	my $prefix = firstDef($p->{parameters}{G}{prefix}, 'pipeline');
	my $prefixDigits = firstDef($p->{parameters}{G}{digits}, 2);
	my $pipeDir = sub { sprintf('%s_%0*d', $prefix, $prefixDigits, $_[0]) };
	my $specFile = firstDef($p->{parameters}{G}{specFile}, 'files.spec');
	my $cmdPostfix = firstDef($p->{parameters}{G}{cmdPostfix}, '.pl');
	my $ppars = $p->{parameters};
	LogPipeline($p);

	# <p> assign id based properties, collect parameters
	my $dpars = {};	# dynamic parameters
	my @p2 = map { my $p = $_;
		my $outputDir = $pipeDir->($p->{id});
		$dpars->{pipeFullName($p). '_OUTPUT_DIR'} = $outputDir;
		({ %$p, outputDir => $outputDir })
	} unlist(@{$p->{pipeline}});
	#print(Dumper($dpars));
	#print(Dumper([@p2]));

	my ($from, $to) = (firstDef($c->{from}, 0), firstDef($c->{to}, int(@p2) - 1));
	for (my $i = $from; $i <= $to; $i++) {
		my $pipe = $p2[$i];
		# pipe options
		my %po = (%{$ppars->{$pipe->{name}}}, %{$ppars->{$pipe->{tag}}});
		my $stageMsg = sprintf('Stage %0*d: %s', $prefixDigits, $i, pipeFullName($pipe));
		Log($stageMsg, 2);
		say $stageMsg;
		my @inputs = !@{$pipe->{dependsOn}}
			? $ppars->{G}{PipelineInput}					# beginning of pipeline
			: map { $pipeDir->($_) } @{$pipe->{dependsOn}};	# inner node of the pipeline
		my $outputDir = $pipeDir->($pipe->{id});
		my $cmd = firstDef($po{cmd},
			"$pipe->{name}$cmdPostfix OPTIONS --outputDir OUTPUT_DIR INPUTS");
		# parameters for pipe
		my %pp = (%{$ppars->{'G'}}, %po);
		# interpolate cmd
		# <i> allow to refer to all starta by PipeName:STRATUM
		# @strata = @strata[0 .. (2*$pipe->{level} + 1)];	# truncate stratum stack to current level
		# push(@strata, "$pipe->{name}:STRATUM", $pipe->{stratum});
		# $interp = { %$interp, @strata };
		my $interp = { %$dpars,
			'OUTPUT_DIR' => $outputDir,
			'INPUTS' => $pp{InputAsIs}
				? join(' ', @inputs)
				: join(' ', map { qs($_. '/'. $specFile) } @inputs),
			'PIPE_PARAMETERS' => join(' ', map { "--$_ ". qs($pp{$_}) } keys %pp),
			'OPTIONS' => firstDef($po{Options}, ''),
			'STRATUM' => $pipe->{stratum}
		};
		$cmd = mergeDictToString($interp, $cmd, { sortKeys => 'YES', iterate => 'YES' });
		# prepare output directory
		#Mkpath($outputDir) if (!-e $outputDir && !$main::__doLogOnly);
		# simple one-by-one transform
		#print(Dumper({%po}));
		# <!> $c->{pipelineClass} no longer used
		if ($po{cmd_isTransform} || $po{cmd_isSummary}) {
			my $pipelineClass = $po{cmd_isTransform}? 'PipelineTransform': 'PipelineSummary';
			my $s = $c->{schedulerClass}->new();
			my $pt = $pipelineClass->new(
				outputDir => $outputDir,
				outputExtension => $po{outputExtension},
				commandTemplate => $cmd,
				commandPrepare => mergeDictToString($interp, $po{cmd_prepare},
					{ sortKeys => 'YES', iterate => 'YES' }),
				scheduler => $s,
				# <!> hack to allow debugging/working around server configuration bugs
				o => { synchroneous_prepare => $po{synchroneous_prepare} }
			);
			$pt->run(@inputs);
			Log("cmd inlined as $pipelineClass: $pipe->{name}", 4);
		} else {
			System($cmd, 3);
		}
	}
	writeFile($c->{pipelineStateFile}, stringFromProperty({ pipeline => $p, config => $c }));
}

#
#	<p> Pipeline class
#

use MooseX::Declare;

class Pipeline {
	has 'parameters' => ( isa => 'Maybe[HashRef]', is => 'rw', default => undef );
	has 'pipeline' => ( isa => 'Maybe[HashRef]', is => 'rw', default => undef );
	has 'c' => ( isa => 'Maybe[HashRef]', is => 'rw', default => undef );

	method fromString(Str $path) {
	}
	method fromPath(Str $path) {
	}

	method run() {
	}
}


1;
