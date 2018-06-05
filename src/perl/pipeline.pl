#!/usr/bin/env perl
#
#Tue Dec 21 20:40:43 CET 2010

#use perl5i::2;
#no strict 'refs';
use v5.10;
use TempFileNames;
use PropertyList;
use Parse::RecDescent;
use Data::Dumper;
use Set;
use PipelineFileset;

# default options
$main::d = {
	pipelineClass => 'PipelineTransform',
	schedulerClass => 'BatchQueueSGE',
	pipelineStateFile => 'diag/pipeline.spec',
	pipelineParameterFile => 'diag/pipeline_parameters',
	callTriggers => 0, triggerPrefix => '', maxConfigNesting => 10,
	helpOnEmptyCall => 1
};
# options
$main::o = ['+print-example', '+print-pipeline', '+print-progress', '+print-parameters', '+dump',
	'rerun', 'input|i=s', 'from=i', 'to=i', 'range=s', 'subrange=s',
	'pipelineClass=s', 'print-diagnostics',
	'maxConfigNesting=i'
];
$main::usage = 'pipeline-spec.pipe ...';
$main::helpText = <<HELP_TEXT;
	(c) 2010-2016 by Stefan Boehringer, Statistical Genetics, LUMC

	Options:
	--print-pipeline	print the pipeline as specified in the spec file
	--print-progress	print the pipeline progress for the pipeline execution living in the current directory
	--print-example	print an example pipeline specification
	--print-diagnostics	print generated commands; otherwise they get stored in
			pipeline-diagnostics-[pipeline-name]
	--print-parameters	print all pipeline parameters as defined by the input files
	--dump	print internal pipeline configuration

	Examples:
	pipeline.pl my_pipeline.pipe
	pipeline.pl my_pipeline.pipe my_overwrites --input myFile
	pipeline.pl my_pipeline.pipe --print-parameters | less -S
	pipeline.pl --from 5 --to 7 my_pipeline.pipe
	pipeline.pl --range 5:7 my_pipeline.pipe
	# subrange is only effective for transforming steps, runs specified jobs from the subrange
	pipeline.pl --range 5 --subrange 10:10 my_pipeline.pipe

$TempFileNames::GeneralHelp
HELP_TEXT

$main::examplePipeline = <<PIPELINE;
#Pipeline  option file
#Options are specified as Component:Optionname
# 'G' is the global component
#
# G:Pipeline : specify the pipeline
#	run jobs seperated by '|' sequentially and
#	jobs separated by ',' concurrently
#	group jobs by parentheses '(', ..., ')'
G:Pipeline	GWASconvert:tped | GWASimpute |  ( GWAStest:snptest | GWASsummary, GWAStest:qtassoc | GWASsummary )

# pipeline component options
tped:type	ped2tped
PIPELINE

@main::pipelinePathes = ( '.', $ENV{PIPELINEPATHES}, "$ENV{HOME}/MyLibrary/Pipelines" );

# triggers
sub dump { print Dumper($_[0]) }
sub print_example { print $main::examplePipeline, "\n"; }
sub print_pipeline { printPipeline($_[0]) }
sub print_progress { my ($c) = @_;
	my $state = propertyFromString(readFile($c->{pipelineStateFile}));
	printPipelineProgress($state->{pipeline}, $state->{config})
}
$main::printKeyWidth = 30;
sub parameters_as_text { my ($c, %pars) = @_;
	my %p = %{$c->{parameters}};
	# iterate outer and inner keys
	my @pars = map { my $ko = $_;
		map { my $ki = $_;
			my $k = $ko.':'.$ki;
			my $v = $p{$ko}{$ki};
			# output formatting
			$v =~ s{\s+}{ }sog;
			my $t =  $k. (' ' x max($main::printKeyWidth - length($k), 1)). $v;
			$t
		} (sort keys %{$p{$ko}})
	} (sort keys %p);
	return @pars;
}

sub print_parameters { my ($c) = @_;
	my @pars = parameters_as_text($c);
	foreach my $p (@pars) { say $p; }
}

#	identifier: /[a-z_][a-z0-9_]*/i
my $pgrammar = q{
	# start
	pipeline: pipeline_component(s? /\|/)

	# atoms
	identifier: /[a-z0-9_]+/i
	string: /STRING_RE/
	number: /\d+/

	# other rules
	pipeline_component: sub_pipeline | pipe
	sub_pipeline: '(' pipeline(s /,/) ')'
		{ $return = $item[2]; }
	pipe: pipe_identifier pipe_input(?)
		{ $return = { %{$item[1]}, input => $item[2] }; }
	pipe_identifier: identifier pipe_tag(?)
		{ $return = { name => $item[1], tag => $item[2][0] }; }
	pipe_tag: ':' identifier
		{ $return = $item[2]; }
	pipe_input: '[' pipe_input_selector ']'
		{ $return = $item[2]; }
	pipe_input_identifier: pipe_identifier | number
	pipe_input_selector: pipe_input_identifier(s /,/)
};
# <%> recover syntax highlighting in kate | /

# $ls:	lines
sub readPipelineConfig { my ($ls, $c) = @_;
	# <p> check nesting of includes
	die "Config file inclusions nested too deeply for $f" if ($c->{level} > $c->{maxConfigNesting});
	my @c = ();
	my @lines = @$ls;
	my $host = firstDef($ENV{HOST_PIPELINE}, $ENV{HOST});
	while (@lines > 0) {
		my $l = shift(@lines);
		next if ($l =~ m{^\s*$|^#}so);
		my ($k, $v) = ($l =~ m{(\S+)\s*(.*)$}sog);
		# <p> line coninuation
		for (; $v =~ m{\\$}so; $v .= shift(@lines)) { chop($v); }
		if ($k eq '@include') {
			# as a special case, allow interporlation of hostname in includes <!>
			$v = mergeDictToString({ 'ENV:HOST' => $host, '$HOST' => $host }, $v, { sortKeys => 'YES' });
			push(@c, readPipelineConfigFile($v, { %$c, level => $c->{level} + 1} ));
		} else { push(@c, $k, $v); }
	}
	return @c;
}
sub readPipelineConfigFile { my ($f, $c) = @_;
	my $path = firstFile($f, $c->{pathes});
	die "Config file $f not found" if (! -e $path);
	# <p> read file into lines
	Log("Including pipeline configuration:$path", 5);
	my @c = readPipelineConfig([split(/\n/, readFile($path))], $c);
	return @c;
}

# allow regexes of the following form:
# ~~split~~{regex}~~sep~~value
# split with regex "split", match regex globally on value and join with "sep"
sub parseConfigRegexes { my (%c) = @_;
	my @vs = map {
		my ($spl, $regex, $sep, $v) = ($_ =~ m{^[~][~](.*)[~][~][{](.*)[}][~][~](.*)[~][~](.*)$}so);
		
		if ($regex ne '') {
			$v = mergeDictToString(\%c, $v, { iterate => 'YES' });
			join($sep, map { $_ =~ m{$regex}s } split(/$spl/, $v));
		} else { $_ }
	} values %c;
	%c = %{makeHash([keys %c], \@vs)};
	return %c;
}

# allow regexes of the following form:
# ~~split~~{regex}~~sep~~value
# split with regex "split", match regex globally on value and join with "sep"
sub pipelineInlineCalls { my ($c) = @_;
	my %c = %$c;
	my @vs = map {
		my ($cmd) = ($_ =~ m{^`(.*)`$}so);
		if ($cmd ne '') {
			System($cmd, 5, undef, { returnStdout => {'yes'}})
		} else { $_ }
	} values %c;
	my %c = %{makeHash([keys %c], \@vs)};
	return {%c};
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
sub pipelineConfigSubstituteRaw { my ($c) = @_;
	my %c = %$c;
	# <p> deep replacement
	my $envkeys = minus([keys %ENV], ['_']);	# protect special perl variables
	my %r = (%c, map { ("\$$_", $ENV{$_}) } @$envkeys);	# augement %c with environment variables
	my @vs = map { mergeDictToString(\%r, $_, { iterate => 'YES' }) } values %c;
	%c = %{makeHash([keys %c], \@vs)};
	return {%c};
}

# get fields from super-namespace defined by 'isa' key
sub namespaceSuper { my ($c, $ns) = @_;
	my $isa = $c->{$ns}{isa};
	# <p> copy field from parent
	my %ns = ((defined($isa)? namespaceSuper($c, $isa): ()), %{$c->{$ns}});
	# <p> promote substitutions <!>
	if (defined($isa)) {
		%ns = map { my ($k, $v) = ($_, $ns{$_});
			$v =~ s{$isa:}{$ns:}sg;
			($k => $v)
		} keys %ns;
	}
	return %ns;
}

sub parameterDicts { my ($c) = @_;
	my %c = %$c;
	# <p> sub options for pipeline components
	my @namespaces = unique(map { (split(/:/))[0] } keys %c);
	# <p> build dicts for components
	%c = map { my $comp = $_;
		my @es = grep { (split(/:/))[0] eq $comp } keys %c;
		my %cc = map { my $k = (split(/:/))[1]; ($k, $c{$_}) } @es;
		( $comp => { %cc } )
	} @namespaces;
 	my %nsvs = map { ($_ => { (namespaceSuper(\%c, $_)) } ) } @namespaces;
 	return {%nsvs};
}

sub parameterDict2raw { my ($p, $name) = @_;
	return map { ( "$name:$_" => $p->{$_} ) } keys %$p;
}
sub parameterDicts2raw { my ($c) = @_;
# 	my %c = map { my $k = $_;
# 		map { ( "$k:$_" => $c->{$k}{$_} ) } keys %{$c->{$k}}
# 	} keys %$c;
	my %c = map { parameterDict2raw($c->{$_}, $_) } keys %$c;
	return {%c};
}

sub pipelineConfigSubstitute { my ($c) = @_;
	my $c = parameterDicts($c);
	$c = pipelineConfigSubstituteRaw(parameterDicts2raw($c));
	return parameterDicts($c);
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

# allow to redefine pipeinstances based on the namesapce stratumSubstitution parameter
# (see gwas examples). allows to stratify by subpipleines instead of only changing parameters
sub expandStrataPipeSubstitution { my ($pipe, $stratum, $pars) = @_;
	return $pipe if (!defined($pars->{stratumSubstitution}));
	# instantiate substitution
	my $pS = mergeDictToString({STRATUM => $stratum}, $pars->{stratumSubstitution});
	my $pN = makeHash(['name', 'tag'], [split(':', $pS)]);
	my %r = (%$pipe, %$pN);
	return {%r};
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
				my @strata = split(/\s*[,\n]\s*/, $strata);
				Log("Strata expansion $pipe->{name}:$pipe->{tag}: $strata", 5);
				my @sp = map { my $stratum = $_;
					my $head = expandStrataPipeSubstitution({ %$pipe, stratum => $stratum },
						$_, $pars->{$pipe->{name}});
					my @subPipeline = map {
						expandStrataPipeSubstitution($_, $stratum, $pars->{$_->{name}})
					} @{$pl}[($i + 1) .. $#$pl];
					my @tail = @{expandStrataRaw([@subPipeline], $pars,
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
	#my $pC = pipelineConfigSubstitute({%$p});
	# copy pipeline parameters in pristine stage
	my $pC = parameterDicts($p);
	# <p> handle pipe-instances
	for (my $i = 0; $i <= @$pipeline; $i++) {
		my $pipe = $pipeline->[$i];
		if ($pipe->{tag} ne '') {
			# use inheritance mechanism instead
			if (0) {
			# pipe to be inherited
			my %pi = %{$pC->{$pipe->{name}}};
			# <p> map reference in values
 			my @n = keys(%pi);	# variables defined by the super-pipe
			# <p> map inherited keys to keys in new namespace
 			my $d = makeHash([map { "$pipe->{name}:$_" } @n], [map { "$pipe->{tag}:$_" } @n]);
			# <p> %pi with mapped values
 			%pi = map { ( $_ => mergeDictToString($d, $pi{$_}, { iterate => 'YES' } ) ) } @n;
			my %pc = parameterDict2raw(\%pi, $pipe->{tag});	# make copy
			my %po = parameterDict2raw($pC->{$pipe->{tag}}, $pipe->{tag});	# overwrites
 			$pN = { ( %$pN, %pc, %po, "$pipe->{tag}:name" => $pipe->{tag} ) };
			}
			$pN = {(%$pN, parameterDict2raw({
				(%{$pC->{$pipe->{tag}}}, name => $pipe->{tag}, isa => $pipe->{name})
			}, $pipe->{tag}))};
 		}
	}
	return $pN;
}

sub checkPipelineConfiguration { my ($pipeline, $pars) = @_;
	my $check = 0;
	foreach $pipe (@$pipeline) {
		if (ref($pipe) eq 'ARRAY') {
			$check |= checkPipelineConfiguration($pipe, $pars);
		} else {
			my $namespace = firstDef($pipe->{tag}, $pipe->{name});
			for $key (keys %{$pars->{$namespace}}) {
				if ($pars->{$namespace}{$key} eq '__must_be_specified__') {
					my $ev = $pars->{$namespace}{"${key}_example"};	# example value
					my $example = !defined($ev)? '': "\n\tExample value: $ev";
					Log("Key $namespace:$key undefined but specification required.$example", 1);
					$check = 1;
				}
			}
		}
	}
	return $check;
}

sub readPipeline { my ($fs, $pathes) = @_;
	# <p> raw parsing of config
	my $p = readPipelineConfigsRaw($fs, $pathes, [	# add literal definitions
		'G:WD '. substr(`pwd`, 0, -1),
		'G:pipeline_running_dir '. substr(`pwd`, 0, -1),
		'G:pipeline_time_start '.localtime(),
		'G:pipeline_files '. join(':', @$fs),	# <!> cave no ':' pathes
		'G:pipeline_operator '. $ENV{USER}
	]);
	# copy config and presubstitute to be able to construct the pipeline
	my $pC = pipelineConfigSubstitute({%$p});
	Log('Log 1st substitution: '. Dumper($pC), 5);
	# <p> parse pipeline definition
	my $pparser = Parse::RecDescent->new($pgrammar);
	my $pipelineName = firstDef($pC->{G}{Name}, 'Pipeline');
	my $pipeline = $pparser->pipeline($pC->{G}{$pipelineName});
	# <p> copy configs based on pipeline definition
	$p = pipelineConfigCopyInstances($pipeline, $p);
	# <p> subistitute values
	$p = pipelineInlineCalls($p);
	$p = pipelineConfigSubstitute($p);
	Log('Log 2nd substitution: '. Dumper($p), 5);

	$pipeline = expandStrata($pipeline, $p);
	Log('Pipeline: '. Dumper($pipeline), 5);
	$pipeline = recordDependencies($pipeline);
	Log('Pipeline: '. Dumper($pipeline), 5);
	my $check = checkPipelineConfiguration($pipeline, $p);
	die 'undefined pipeline parameters' if ($check);
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
	# my $Njobs = int(@{$spec->{jids}});
	my $Njobs = int(@{$spec->{jobs}{allIds}});	# assume pl_sentinel.pl use
	my $progress = 1;
	if ($Njobs) {
		my $Nrunning = int(grep { defined } which_indeces($spec->{jobs}{allIds}, $o{jids}, 1));
		$progress = 1 - ($Nrunning / $Njobs);
	}

	return {
		%{pipeDesc($pc, $i, %o)},
		perc => $progress, '#' => $Njobs, progress => progressPrint($progress,
			width => abs($main::PipelineTableDesc{columns}{progress}{width}))
	};
}

sub printPipeline { my ($p, $f, $cols, %o) = @_;
	# <p> default parameters
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
	@outputDirs = @outputDirs[@{$c->{range}}];

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
	my $pipeDirRaw = sub { sprintf('%s_%0*d', $prefix, $prefixDigits, $_[0]) };
	my $pipeDir = sub { readlink($pipeDirRaw->($_[0])) };
	my $specFile = firstDef($p->{parameters}{G}{specFile}, 'files.spec');
	my $cmdPostfix = firstDef($p->{parameters}{G}{cmdPostfix}, '.pl');
	my $ppars = $p->{parameters};
	LogPipeline($p);

	# <p> assign id based properties, collect parameters
	my $dpars = {};	# dynamic parameters
	my @p2 = map { my $p = $_;
		my $outputDir = $pipeDir->($p->{id});		# might come from different runs
		$dpars->{pipeFullName($p). '_OUTPUT_DIR'} = $outputDir;
		({ %$p, outputDir => $outputDir })
	} unlist(@{$p->{pipeline}});
	#print(Dumper($dpars));
	#print(Dumper([@p2]));

	writeFile($c->{pipelineStateFile}, stringFromProperty({ pipeline => $p, config => $c }));
	writeFile($c->{pipelineParameterFile}, join("\n", parameters_as_text($p)));
	for my $i (@{$c->{pipeRange}}) {
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
		# <p> cmd interpolation
		my $cmd = firstDef($po{cmd},
			"$pipe->{name}$cmdPostfix OPTIONS --outputDir OUTPUT_DIR INPUTS");
		# pipe parameters
		my %pp = (%{$ppars->{'G'}}, %po);
		# set environment
		my %old_env = %ENV;
		my %pipe_env = map {
			my ($k) = (/^ENV_ADD_(.*)/);
			$k => "$ENV{$k}$pp{$_}"
		} grep { /^ENV_ADD_/ } keys %pp;
		%ENV = (%ENV, %pipe_env);

		# <i> allow to refer to all strata by PipeName:STRATUM
		# @strata = @strata[0 .. (2*$pipe->{level} + 1)];	# truncate stratum stack to current level
		# push(@strata, "$pipe->{name}:STRATUM", $pipe->{stratum});
		# $interp = { %$interp, @strata };
		# magic options: InputAsIs, InputAsOptions, InputAsFile Options
		my @options = split(/\s*,\s*/, $pp{InputAsOptions});
		my @inputsCmdline = $pp{InputAsIs}? @inputs: (map { $_. '/'. $specFile } @inputs);
		if ($pp{InputsAsFile}) {
			writeFile("$outputDir/inputsAsFile", join("\n", @inputsCmdline));
			@inputsCmdline = ( "$outputDir/inputsAsFile" );
		} else {
			@inputsCmdline = map { qs($_) } @inputsCmdline;
		}
		@inputsCmdline = map { "$options[$_] $inputsCmdline[$_]" } 0..$#inputsCmdline;
		my $interp = { %$dpars,
			'OUTPUT_DIR' => $outputDir,
			'INPUTS' => join(' ', @inputsCmdline),
			'PIPE_PARAMETERS' => join(' ', map { "--$_ ". qs($pp{$_}) } keys %pp),
			'OPTIONS' => firstDef($po{Options}, ''),
			'STRATUM' => $pipe->{stratum}
		};
		$cmd = mergeDictToString($interp, $cmd, { iterate => 'YES' });
		# prepare output directory
		#Mkpath($outputDir) if (!-e $outputDir && !$main::__doLogOnly);
		# simple one-by-one transform
		#print(Dumper({%po}));
		# <!> $c->{pipelineClass} no longer used
		my $cmdPrepare = mergeDictToString($interp, $po{cmd_prepare}, { iterate => 'YES' });
		if ($po{cmd_isTransform} || $po{cmd_isSummary}) {
			my $pipelineClass = $po{cmd_isTransform}? 'PipelineTransform': 'PipelineSummary';
			my $s = $c->{schedulerClass}->new();
			my $pt = $pipelineClass->new(
				outputDir => $outputDir,
				outputExtension => $po{outputExtension},
				commandTemplate => $cmd,
				commandPrepare => $cmdPrepare,
				scheduler => $s,
				strata => [$pipe->{stratum}],	#<!> stack of strata from super-pipelines
				fileMeta => defined($po{meta})? { %{propertyFromString($po{meta})} }: {},
				# <!> hack to allow debugging/working around server configuration bugs
				o => { synchroneous_prepare => $po{synchroneous_prepare} },
				range => $c->{subRange}
			);
			$pt->run(@inputs);
			Log("Cmd inlined as $pipelineClass: $pipe->{name}", 4);
		} else {
			System($cmd, 3);
		}
		# run the sentinel wrapper; <!> modifies files.spec
		my @jids = @{propertyFromString(readFile("$outputDir/files.spec"))->{jids}};
		System(sprintf("pl_sentinel.pl --waitForJids %s --wrap $outputDir", join(',', @jids)), 3);
		# <p> restore environment
		%ENV = %old_env;
	}
}

sub splitRange { my ($s, $N) = @_;
	my @els = split(/:/, $s);
	$from = $els[0];
	$to = (int(@els) == 2)? $els[1]: (defined($N)? $N: $from);
	return ($from .. $to);
}

sub refineConfig { my ($c, $p) = @_;
	my $Npipeline = int(unlist(@{$p->{pipeline}}));
	my ($from, $to) = (firstDef($c->{from}, 0), firstDef($c->{to}, $Npipeline - 1));
	my @pipeRange = ($from .. $to);
	@pipeRange = splitRange($c->{range}, $Npipeline) if (defined($c->{range}));
	my @subRange = defined($c->{subrange})? splitRange($c->{subrange}): ();
	return {%$c, pipeRange => [@pipeRange], subRange => [@subRange]};
}

#main $#ARGV @ARGV %ENV
	#initLog(2);
	my $c = StartStandardScript($main::d, $main::o);
	# <p> read pipeline
	my $p = readPipeline([@ARGV], [@main::pipelinePathes]);
#print(Dumper($p->{parameters}));
	# <p> refine configuration
	$c = refineConfig($c, $p);
	# <p> triggers
	callTriggersFromOptions({%$c, %$p});
	# <p> pipeline scheduling
	submitPipeline($c, $p);
exit(0);
