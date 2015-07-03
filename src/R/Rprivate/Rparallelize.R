#
#	Rparallel.back.R
#Sun Jul 15 10:48:17 UTC 2012

#
#	<p> general documentation
#

# online vs offline mode: online means that rampUps are computed in one go, whereas offline backends compute one rampUp for a single invocation
# delegating backends are backends that forward execution to another offline backend
#	example: OGSremote -> OGS

#
#	<p> generic interface
#

setGeneric("isSynchroneous", function(self) standardGeneric("isSynchroneous"));
setGeneric("lapply_dispatch", function(self, l, f, ...) standardGeneric("lapply_dispatch"));
setGeneric("lapply_dispatchFinalize", function(self) standardGeneric("lapply_dispatchFinalize"));
setGeneric("lapply_results", function(self, r) standardGeneric("lapply_results"));
# parallelize function as customized by the backend
setGeneric('parallelize_backend', function(self, call_) standardGeneric('parallelize_backend'));
#	scheduling
setGeneric('initScheduling',
	function(self, call_) standardGeneric('initScheduling'));
setGeneric('performParallelizationStep',
	function(self, call_, Lapply_config) standardGeneric('performParallelizationStep'));
setGeneric('finalizeParallelization',
	function(self, r) standardGeneric('finalizeParallelization'));

setGeneric('saveParallelizationState',
	function(self) standardGeneric('saveParallelizationState'));
setGeneric('restoreParallelizationState',
	function(self) standardGeneric('restoreParallelizationState'));
setGeneric('scheduleNextParallelization',
	function(self, call_) standardGeneric('scheduleNextParallelization'));
setGeneric('pollParallelization',
	function(self, options) standardGeneric('pollParallelization'));
setGeneric('getResult',
	function(self) standardGeneric('getResult'));

#
#	<p> default class
#

#' Class \code{"ParallelizeBackend"}
#' 
#' Base class for parallelization backends. Please refer to documentation of the methods
#' individually for more complete documentation.
#' 
#' 
#' @name ParallelizeBackend-class
#' @aliases ParallelizeBackend-class
#' finalizeParallelization,ParallelizeBackend-method
#' getResult,ParallelizeBackend-method initialize,ParallelizeBackend-method
#' initScheduling,ParallelizeBackend-method
#' isSynchroneous,ParallelizeBackend-method
#' lapply_dispatchFinalize,ParallelizeBackend-method
#' lapply_dispatch,ParallelizeBackend-method
#' lapply_results,ParallelizeBackend-method
#' parallelize_backend,ParallelizeBackend-method
#' performParallelizationStep,ParallelizeBackend-method
#' pollParallelization,ParallelizeBackend-method
#' restoreParallelizationState,ParallelizeBackend-method
#' saveParallelizationState,ParallelizeBackend-method
#' scheduleNextParallelization,ParallelizeBackend-method
#' finalizeParallelization getResult initialize initScheduling isSynchroneous
#' lapply_dispatchFinalize lapply_dispatch lapply_results parallelize_backend
#' performParallelizationStep pollParallelization restoreParallelizationState
#' saveParallelizationState scheduleNextParallelization
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ParallelizeBackend", config, signature)}. %% ~~ describe objects
#' here ~~ Config is a list containing parameters and signature is a character
#' string that uniquely identifies the computation that is to be parallelized.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso %% ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ %% ~~or
#' \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
#' \code{\linkS4class{ParallelizeBackendLocal}},
#' \code{\linkS4class{ParallelizeBackendSnow}},
#' \code{\linkS4class{ParallelizeBackendOGSremote}}
#' @keywords classes
#' @examples
#' 
#' showClass("ParallelizeBackend")
#' 
setClass('ParallelizeBackend',
	representation = list(
		config = 'list', offline = 'logical', signature = 'character'
	),
	prototype = list(config = list(), offline = F, signature = '')
);
setMethod('initialize', 'ParallelizeBackend', function(.Object, config = list(), signature = '') {
	.Object@config = config;
	.Object@signature = signature;
	if (!is.null(config$offline)) .Object@offline = config$offline;
	.Object
});

#
#	<p> default class implementation
#

setMethod('isSynchroneous', 'ParallelizeBackend', function(self) { return(T); });
# use envir__ to evaluate ...
setMethod('lapply_dispatch', 'ParallelizeBackend', function(self, l, f, ..., envir__ = parent.frame()) { 
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply__ = get('Lapply__', envir = parallelize_env);
	freezer = Lapply_executionState__$currentFreezer();
	args = eval(list(...), envir = envir__);
	Log(sprintf('Pushing @ depth %d', Lapply__$getDepth()), 6);
	freezer$push(Lapply__$sequence, f, l, args);
	NULL
});
setMethod('lapply_dispatchFinalize', 'ParallelizeBackend', function(self) { 
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	freezer = Lapply_executionState__$currentFreezer();
	parallelize_setEnable(F);
	r = lapply(1:freezer$Ncalls(), function(i) {
		call = freezer$call(i);
		#call = callEvalArgs(call);
		r = Do.call(call$f, call$args, envir = call$envir);
	});
	freezer$finalizeResults();
	parallelize_setEnable(T);
	r
});
setMethod('lapply_results', 'ParallelizeBackend', function(self, r) { 
	stop('ParallelizeBackend: result retrieval only supported for asynchroneous backends.');
});
setMethod('parallelize_backend', 'ParallelizeBackend', function(self, call_) {
	with(Lapply_getConfig(), if (self@offline) {
		parallelizeOfflineStep(call_, Lapply_config = Lapply_getConfig());
	} else {
		Lapply_initialze_probing();
		for (i in 1:parallel_stack) {
			r = performParallelizationStep(self, call_, Lapply_config = Lapply_getConfig());
			if (all(class(r) != 'Lapply_error')) break;
		}
		r
	});
});
setMethod('performParallelizationStep', 'ParallelizeBackend', function(self, call_, Lapply_config) {
	parallelizeStep(call_, Lapply_config = Lapply_config);
});
setMethod('finalizeParallelization', 'ParallelizeBackend', function(self, r)r);
setMethod('pollParallelization', 'ParallelizeBackend',
	function(self, options = list())list(continue = F, message = '')
);

#
#		<p> parallelization state
#

# <A> running in '.' will not create sub-directory
#	used by remoting computations and already changing to remote stateDir
parallelizationStatePath = function(self, tag = '', ..., ext = '.RData') {
	tagStr = sprintf(tag, ...);
	path = if (self@config$stateDir == '.')
		sprintf('./%s%s', tagStr, ext) else
		sprintf('%s/parallelization_%s/%s%s', self@config$stateDir, self@signature, tagStr, ext);
	Log(sprintf('parallelization path: %s', path), 7);
	path
}
parallelizationStateObjects = c(
	'Lapply_globalConfig__', 'Lapply__', 'Lapply_executionState__', 'Lapply_backend__'
);
saveParallelizationStatePath = function(self, path = NULL) {
	if (is.null(path)) path = parallelizationStatePath(self, 'state');
	Log(sprintf('Saving state to %s', path), 5);
	parallelizationStateObjects = names(as.list(parallelize_env));
	Save(parallelizationStateObjects, file = path, symbolsAsVectors = T, envir = parallelize_env);
}
restoreParallelizationStatePath = function(self, path = NULL) {
	if (is.null(path)) path = parallelizationStatePath(self, 'state');
	Load(file = path, envir = parallelize_env);
}

setMethod('initScheduling', 'ParallelizeBackend', function(self, call_) {
	stateDir = parallelizationStatePath(self, '', ext = '');
	Log(sprintf('State dir: %s', stateDir), 5);
	Dir.create(stateDir, recursive = T);
	saveParallelizationStatePath(self);
});
setMethod('saveParallelizationState', 'ParallelizeBackend', function(self) {
	saveParallelizationStatePath(self);
});
setMethod('restoreParallelizationState', 'ParallelizeBackend', function(self) {
	restoreParallelizationStatePath(self);
});
setMethod('scheduleNextParallelization', 'ParallelizeBackend', function(self, call_) {
	NULL
});
setMethod('getResult', 'ParallelizeBackend', function(self) {
	if (self@config$doSaveResult)
		r = get(Load(file = parallelizationStatePath(self, 'result'))[1]) else
		stop(sprintf('result was not saved for signature %s', self@signature));
});

#
#	Rparallel.back.local.R
#Tue Jun 23 14:14:40 2015

#
#	<p> local backend
#

#' Class \code{"ParallelizeBackendLocal"}
#'
#' Backend class implementing local execution.
#'
#' @name ParallelizeBackendLocal
#' @rdname ParallelizeBackendLocal-class
#' @aliases ParallelizeBackendLocal-class
#' initialize,ParallelizeBackendLocal-method
#' lapply_dispatchFinalize,ParallelizeBackendLocal-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ParallelizeBackendLocal", config, ...)}.
#' During normal operation you do not have to create objects of this class yourself. Instead, \code{parallelize_initialize} will create such instances for you. The class can be configured with the following field in the \code{Lapply_config} argument of \code{parallelize_initialize}.
#' \itemize{
#'   \item freezerClass: defaults to \code{LapplyPersistentFreezer}
#'   \item stateDir: directory to store results from computations. This location is passed to \code{LapplyPersistentFreezer}. If temporary behavior is desired it can be set to: \code{sprintf('\%s/tmp/remote', tempdir())}.
#'    \item sourceFiles: a vector of files to be sourced prior to parallel execution
#' }
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\linkS4class{ParallelizeBackend}},
#'   \code{\linkS4class{ParallelizeBackendSnow}},
#'   \code{\linkS4class{ParallelizeBackendOGSremote}}
#' @keywords classes
#' @examples
#' 
#' showClass("ParallelizeBackendLocal")
#' 
setClass('ParallelizeBackendLocal',
	contains = 'ParallelizeBackend',
	representation = list(),
	prototype = list()
);
setMethod('initialize', 'ParallelizeBackendLocal', function(.Object, config, ...) {
	.Object = callNextMethod(.Object, config = config, ...);
	Dir.create(config$stateDir, recursive = T);
	# 24.7.2013 -> use stateDir instead
	.Object
});
setMethod('lapply_dispatchFinalize', 'ParallelizeBackendLocal', function(self) { 
	Log(sprintf('Local dispatch, tmp: %s', self@config$stateDir), 5);
	parallelize_setEnable(F);
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply__ = get('Lapply__', envir = parallelize_env);
	freezer = Lapply_executionState__$currentFreezer();
	r = lapply(1:freezer$Ncalls(), function(i) {
		mycall = freezer$call(i);
		mycall = callEvalArgs(mycall);
		r = Do.call(mycall$f, mycall$args, envir = mycall$envir);
		freezer$pushResults(r);
		r
	});
	freezer$finalizeResults();
	save(r, file = sprintf('%s/sequence-%d.RData', self@config$stateDir, Lapply__$sequence));
	parallelize_setEnable(T);
	NULL
});

#
#	Rparallel.back.snow.R
#Tue Jun 23 14:16:39 2015

#
#	<p> SNOW execution
#

#' Class \code{"ParallelizeBackendSnow"}
#' 
#' Backend class for parallelization on SNOW clusters
#' 
#' 
#' @name ParallelizeBackendSnow-class
#' @rdname ParallelizeBackendSnow-class
#' @aliases ParallelizeBackendSnow-class
#' initialize,ParallelizeBackendSnow-method
#' lapply_dispatchFinalize,ParallelizeBackendSnow-method
#' @docType class
#'
#' @section Objects from the Class: Objects can be created by calls of the form
#'	\code{new("ParallelizeBackendSnow", config, ...)}.
#' During normal operation you do not have to create objects of this class yourself. Instead, \code{parallelize_initialize} will create such instances for you. The class can be configured with the following field in the \code{Lapply_config} argument of \code{parallelize_initialize}.
#' \itemize{
#'   \item freezerClass: defaults to \code{LapplyPersistentFreezer}
#'   \item stateDir: directory to store results from computations. This location is passed to \code{LapplyPersistentFreezer}. If temporary behavior is desired it can be set to: \code{sprintf('\%s/tmp/remote', tempdir())}.
#'    \item sourceFiles: a vector of files to be sourced prior to parallel execution
#'    \item libraries: a vector of package names to be loaded prior to parallel execution
#'    \item localNodes: an integer number of how many parallel snow jobs are to be created. This should not be larger than the number of (logical) cores available as a general rule. A snow cluster is created using the \code{makePSOCKcluster}
#' }
#' You should be able to run a so-called \code{PSOCKS} cluster to use this package. See the \code{parallel} package for details (see also).
#'
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{makePSOCKcluster}},
#' \code{\linkS4class{ParallelizeBackend}},
#' \code{\linkS4class{ParallelizeBackendLocal}},
#' \code{\linkS4class{ParallelizeBackendSnow}},
#' \code{\linkS4class{ParallelizeBackendOGSremote}}
#' @keywords classes
#' @examples
#' 
#' showClass("ParallelizeBackendSnow")
#' 
#' Lapply_config = list(parallel_count = 24, backends = list(
#'     snow = list(
#'       localNodes = 8, sourceFiles = c('myScript.R'), libraries = c('boot')
#'     )
#' );
setClass('ParallelizeBackendSnow',
	contains = 'ParallelizeBackend',
	representation = list(),
	prototype = list()
);
setMethod('initialize', 'ParallelizeBackendSnow', function(.Object, config, ...) {
	.Object = callNextMethod(.Object, config = config, ...);
	args = List_(config[c('sourceFiles', 'localNodes', 'splitN', 'libraries')], rm.null = T);
	args$libraries = c(args$libraries, 'parallelize.dynamic');
	#args = c(args, list(evalEnvironment = T));
	do.call('specifyCluster', args);
	.Object
});
setMethod('lapply_dispatchFinalize', 'ParallelizeBackendSnow', function(self) { 
	Log(sprintf('Snow dispatch, tmp: %s', self@config$stateDir), 5);
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	freezer = Lapply_executionState__$currentFreezer();
	calls = freezer$getCalls();
	Log(sprintf('Snow dispatch: %d calls', length(calls)), 5);
# 	calls = lapply(calls, function(call) {
# 		call$fct = environment_eval(call$fct, functions = T);
# 		call
# 	});
	r = clapply(calls, function(call) {
		parallelize_setEnable(F);
 		#sink('/tmp/debug', append = T);print(Lapply);sink();
		#call = callEvalArgs(call);
# 		sink('/tmp/debug', append = T);print(join(names(as.list(environment(call$fct)))));print(as.list(environment(as.list(environment(call$fct))$f)));print(str(call));sink();
		Do.call(call$fct, call$args)
	});
	freezer$pushResults(r);
	freezer$unlistResults();
	freezer$finalizeResults();
	NULL
});
#
#	Rparallel.back.R
#Tue Jun 23 14:18:33 2015

#
#	<p> OGS execution
#

#
#	ParallelizeBackendOGS S4 class
#

# steps: jid associated with calls to parallelizationStep
# chunks: jids associated with *Apply calls
# sometimes steps is one more than chunks (during computations)
.ParallelizeBackendOGSstateClass = setRefClass('ParallelizeBackendOGSstate',
	fields = list( steps = 'list', chunks = 'list', logPath = 'character' ),
	methods = list(
	initialize = function(...) {
		steps <<- list();
		chunks <<- list();
		logPath <<- '';
		.self
	},
	log = function() { if (logPath != '') save(.self, file = logPath); },
	pushStep = function(jid) {
		steps[[length(steps) + 1]] <<- jid;
		.self$log();
	},
	pushChunks = function(jids) {
		chunks[[length(chunks) + 1]] <<- jids;
		.self$log();
	},
	chunksJids = function() { if (!length(chunks)) c() else chunks[[length(chunks)]]; },
	setLogPath = function(path) {
		logPath <<-path;
		if (file.exists(logPath)) file.remove(logPath);
	}
	)
);
.ParallelizeBackendOGSstateClass$accessors(names(.ParallelizeBackendOGSstateClass$fields()));

#
#	class ParallelizeBackendOGS is expected to work in the current directory
#	if files are to be setup, ParallelizeBackendOGSremote should be used
#

#' Class \code{"ParallelizeBackendOGS"}
#' 
#' %% ~~ A concise (1-5 lines) description of what the class is. ~~ Backend
#' class implmenting Open Grid Scheduler support
#' 
#' 
#' @name ParallelizeBackendOGS-class
#' @aliases ParallelizeBackendOGS-class
#' finalizeParallelization,ParallelizeBackendOGS-method
#' initialize,ParallelizeBackendOGS-method
#' initScheduling,ParallelizeBackendOGS-method
#' lapply_dispatchFinalize,ParallelizeBackendOGS-method
#' pollParallelization,ParallelizeBackendOGS-method
#' restoreParallelizationState,ParallelizeBackendOGS-method
#' scheduleNextParallelization,ParallelizeBackendOGS-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{}. %% ~~ describe objects here ~~
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso %% ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ %% ~~or
#' \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
#' \code{\linkS4class{ParallelizeBackend}},
#' \code{\linkS4class{ParallelizeBackendLocal}},
#' \code{\linkS4class{ParallelizeBackendSnow}},
#' \code{\linkS4class{ParallelizeBackendOGSremote}}
#' @keywords classes
#' @examples
#' 
#' showClass("ParallelizeBackendOGS")
#' 
setClass('ParallelizeBackendOGS',
	contains = 'ParallelizeBackend',
	representation = list(jids = 'ParallelizeBackendOGSstate'),
	prototype = list(jids = .ParallelizeBackendOGSstateClass$new())
);
.ParallelizeBackendOGSDefaultConfig = list(
	qsubOptions = '--queue all.q'
);

setMethod('initialize', 'ParallelizeBackendOGS', function(.Object, config, ...) {
	# <p> super-class
	config = merge.lists(.ParallelizeBackendOGSDefaultConfig, config);
	.Object = callNextMethod(.Object, config = config, ...);
	# <p> OGS initialization
	Log('initializing OGS', 6);
	.Object@offline = T;
	# <p> RNG
	RNGkind("L'Ecuyer-CMRG");
	set.seed(as.integer(Sys.time()));

	# <p> setup environment
	setupLocalEnv();

	# <p> jid state
	.Object@jids$setLogPath(parallelizationStatePath(.Object, 'jids'));
	.Object
});

setMethod('initScheduling', 'ParallelizeBackendOGS', function(self, call_) {
	callNextMethod(self);
	# <p> dir initialization
	dir = parallelizationStatePath(self, tag = '', ext = '');
	Dir.create(dir, recursive = T);
	# <p> initialize files
	sentinelPath = parallelizationStatePath(self, 'sentinel');
	if (file.exists(sentinelPath)) file.remove(sentinelPath);
});

.parallelizationStepOGS = function(call_, pathHandover) {
	# <!> potential race condition with scheduleNextParallelization
	r0 = get(Load(file = pathHandover, Load_sleep = 5)[1]);
	Lapply_backend__ = get('Lapply_backend__', envir = parallelize_env);
	Lapply_backend__@jids$pushStep(r0$jid);
	parallelize_setEnable(T);	# default is off
	parallelizeOfflineStep(call_, Lapply_config = Lapply_getConfig());
}

shellEnvString = function(env, sep = '+++', prefix = '') {
	join(kvlapply(env, function(k, v)Sprintf('%{prefix}s%{k}s=%{v}s')), sep)
}
qsubEnvOptions = function(env) {
	qsubOptions = join(c('--setenv', shellEnvString(env, '+++'), '--setenvsep=+++'), ' ');
	#Logs('QsubEnvOptions: %{qsubOptions}s', level = 6);
	qsubOptions
}
remoteEnvAdd = function(vars = list(
	PATH = "echo `echo 'cat(system.file(package = \"parallelize.dynamic\"))' | Rscript -`/Perl",
	PERL5LIB = "echo `echo 'cat(system.file(package = \"parallelize.dynamic\"))' | Rscript -`/Perl"),
	userhost = 'localhost') {
	env = kvlapply(vars, function(name, cmd) {
		valueNew = trimString(System(cmd,
			return.output = T, patterns = 'ssh', ssh_host = userhost)$output);
		valueOld = trimString(System(Sprintf("echo $%{name}s"),
			return.output = T, patterns = 'ssh', ssh_host = userhost)$output);
		Sprintf('%{valueNew}s:%{valueOld}s')
	});
	env
}
remoteEnvSetup = function(remoteDir) {
	sp = splitPath(remoteDir, ssh = T);
	env = remoteEnvAdd(userhost = sp$userhost);
	remoteEnvProfile = Sprintf('%{remoteDir}s/remoteProfile.sh');
	envAsString = shellEnvString(env, "\n", prefix = 'export ');
	writeFile(remoteEnvProfile, envAsString, ssh = T);
	Logs("Remote env: %{envAsString}s", level = 6);
	splitPath(remoteEnvProfile, ssh = T)$path
}

freezeCallOGS = function(self, ..f, ...,
	freeze_file = tempfile(), freeze_control = list(), waitForJids = c(),
	patterns = 'qsub', cwd = NULL, ssh_host = 'localhost', ssh_source_file = NULL,
	qsubPath = parallelizationStatePath(self, 'qsub', ext = ''),
	qsubMemory = '4G', qsubOptionsAdd = '',
	envir = NULL, thaw_transformation = identity, freeze_env_eval = F) {

	path = freezeCall(freeze_f = ..f, ...,
		freeze_file = freeze_file, freeze_save_output = T, freeze_control = freeze_control,
		freeze_envir = NULL, freeze_env_eval = freeze_env_eval,
		freeze_objects = 'parallelize_env', thaw_transformation = thaw_transformation);
	wrap = frozenCallWrap(path, freeze_control);
	wait = if (!length(waitForJids)) '' else sprintf('--waitForJids %s', paste(waitForJids, collapse = ','))
	qsubOptions = Sprintf('%{options}s --outputDir %{qsubPath}Q %{wait}s %{qsubOptionsAdd}s',
		options = self@config$qsubOptions
	);
	qsubOptions = mergeDictToString(list(`QSUB_MEMORY` = qsubMemory), qsubOptions);
	Logs("qsubOptions: %{qsubOptions}s", level = 5)
	r = System(wrap, 5, patterns = patterns, qsubOptions = qsubOptions, cwd = cwd,
		ssh_host = ssh_host, ssh_source_file = ssh_source_file, return.cmd = T);
	r
}

# we use the freeze/thaw mechanism and a handover such that restoring the state would
#	destroy handover changes, the saving still occurs for tracking purposes
setMethod('restoreParallelizationState', 'ParallelizeBackendOGS', function(self) {
	NULL
});

setMethod('scheduleNextParallelization', 'ParallelizeBackendOGS', function(self, call_) {
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	c = Lapply_getConfig();
	freeze_control = list(
		sourceFiles = self@config$sourceFiles,
		libraries = unique(c('parallelize.dynamic', self@config$libraries)),
		objects = parallelizationStateObjects,
		logLevel = Log.level(),
		rng = RNGuniqueSeed(self@signature)
	);
	path = parallelizationStatePath(self, 'rampUp:%03d', Lapply_executionState__$rampUp);
	pathHandover = parallelizationStatePath(self, 'rampUp:%03d_handover', Lapply_executionState__$rampUp);
	# <i> gather information from previous step
	#qacct -j 257
	# new path for each rampUp due to potential race condition
	r0 = freezeCallOGS(self, ..f = .parallelizationStepOGS, call_,
		# .parallelizationStepOGS
		pathHandover = pathHandover,
		# freeze
		freeze_file = path, freeze_control = freeze_control, qsubMemory = self@config$qsubRampUpMemory,
		waitForJids = self@jids$chunksJids()
	)
	save(r0, file = pathHandover);
	r0
});

setMethod('lapply_dispatchFinalize', 'ParallelizeBackendOGS', function(self) { 
	Log(sprintf('OGS Dispatching, tmp: %s', self@config$stateDir), 5);
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply__ = get('Lapply__', envir = parallelize_env);
	freezer = Lapply_executionState__$currentFreezer();

	# <p> setup
	c = Lapply_getConfig();
	freeze_control = list(
		sourceFiles = self@config$sourceFiles,
		libraries = unique(c('parallelize.dynamic', self@config$libraries)),
		objects = parallelizationStateObjects,
		logLevel = Log.level()
	);
	# <p> split up calls into 'parallel_count' no of slots
	idcs = splitListIndcs(freezer$Ncalls(), c$parallel_count);

	ogs_frozen_call__ = function(listcalls) {
		parallelize_setEnable(F);
		lapply(listcalls, function(lc) {
			lapply(lc$elements, function(e)
				try(do.call(lc$fct, c(list(e), lc$arguments)))
			)
		})
	}
	r = lapply(1:dim(idcs)[1], function(job_index__) {
		path = parallelizationStatePath(self, 'sequence:%03d_chunk:%05d', Lapply__$sequence, job_index__);
		mycalls = freezer$callRange(idcs[job_index__, 1], idcs[job_index__, 2]);
		# force evaluation/restriction of environment
		mycalls = lapply(mycalls, function(lc) {
			# < 24.6.2015
			#lc$fct = environment_eval(lc$fct, functions = self@config$copy_environments);
			if (self@config$copy_environments) {
				lc$fct = environment_eval(lc$fct, functions = FALSE, recursive = FALSE);
			}
			lc
		});
		freeze_control_chunk = c(freeze_control, list(rng = RNGuniqueSeed(c(self@signature, job_index__))));
		Log(sprintf("Unique seed for job %d: %d", job_index__, freeze_control_chunk$rng$seed), 5);
		r = freezeCallOGS(self, ogs_frozen_call__, listcalls = mycalls,
			freeze_file = path, freeze_control = freeze_control_chunk,
			cwd = getwd(),
			qsubMemory = self@config$qsubParallelMemory,
			thaw_transformation = thaw_object
		);
		r = c(r, list(file = path, from = idcs[job_index__, 1], to = idcs[job_index__, 2]));
		r
	});
	self@jids$pushChunks(list.kp(r, 'jid', do.unlist = T));
	freezer$pushResults(r);
	#freezer$unlistResults();
	freezer$finalizeResults();
	NULL
});

setMethod('finalizeParallelization', 'ParallelizeBackendOGS', function(self, r) {
	Log(sprintf('OGS finalizing parallelization %s', self@signature), 5);
	if (self@config$doSaveResult)
		save(r, file = parallelizationStatePath(self, 'result'));
	sentinel = list(signature = self@signature);
	save(sentinel, file = parallelizationStatePath(self, 'sentinel'));
	r
});

progressStatJids = function(jids, jidsRunning) {
	jidsPending = intersect(jids, jidsRunning);
	N = length(jids);
	Npending = length(jidsPending);
	r = list(N = N, Npending = Npending, Ncomplete = N - Npending, complete = 1 - Npending / N);
	r
}
.progressStat = function(jidsTasks, i, jidsRunning) {
	jidsTask = if (nif(length(jidsTasks) < i)) NULL else jidsTasks[[i]];
	progressStatJids(jidsTask, jidsRunning)
}

.stdProgressFormat = list(
	title = '%-20s ', Ncomplete = '%4d/', N = '%d', progress = ' [%25s] ', Perc = '%3.0f%%'
);
progressString = function(stat, title = 'Task', format = .stdProgressFormat, NanString = '----') {
	format = merge.lists(.stdProgressFormat, format);
	L = nchar(sprintf(format$progress, '-'));	# length progress bar
	progressBar = if (is.nan(stat$complete)) sprintf('%-*s', L, 'count pending') else
		paste(c(rep('#', round(stat$complete * L, 0)),
			rep('.', round((1 - stat$complete) * L, 0))), collapse = '');
		values = list(title = title,
			Perc = floor(100 * stat$complete),
			Ncomplete = stat$Ncomplete, N = stat$N, progress = progressBar);
		r = unlist(nlapply(format, function(n) {
			if (is.nan(values[[n]])) NanString else sprintf(format[[n]], values[[n]])
	}));
	r = paste(r, collapse = '');
	r
		
}

.pollJids = function(...) {
	qstat = System("qstat -u \\* -xml | xml sel -t -m '//JB_job_number' -v 'text()' -o ' '",
		logLevel = 6, ..., return.output = T);
	jids = fetchRegexpr('(\\d+)', qstat$output, captures = T);
	jids
}

.pollMessageRaw_table = function(jids, qstat_jids) {
	N = max(length(jids$steps), length(jids$chunks));
	msg = as.vector(sapply(1:N, function(i) {
		psc = .progressStat(jids$chunks, i, qstat_jids);
		pss = .progressStat(jids$steps, i, qstat_jids);
		c(
			progressString(psc, title = sprintf('  Parallelization %d', i)),
			progressString(pss, title = sprintf('Rampdown %d', i))
		)
	}));
	msg
}
.pollMessage_table = function(msg, continue) {
	header = paste(rep('-', 79), collapse = '');
	conclusion = if (continue) 'Further scheduling pending' else 'Computation complete';
	#messageRaw = paste(msg, collapse = "\n");
	#message = paste(c(header, messageRaw, header, conclusion, '', ''), collapse = "\n");
	message = c(header, msg, header, conclusion);
	message
}

.pollMessageRaw = function(jids, qstat_jids) {
	Nsteps = length(jids$steps);
	Nchunks = length(jids$chunks);
	jids = if (Nsteps >= Nchunks) jids$steps[[Nsteps]] else jids$chunks[[Nchunks]];
	ps = progressStatJids(jids, qstat_jids);
	progressString(ps, title = if (Nsteps >= Nchunks)
		Sprintf('Rampdown %{Nsteps}d') else Sprintf('Parallelization %{Nchunks}d'));
}

.pollMessage = function(msg, continue) {
	Sprintf('%{msg}s | [%{cont}s]', cont = if (continue) 'continued' else 'done');
}

setMethod('pollParallelization', 'ParallelizeBackendOGS', function(self, options = list()) {
	continue = !file.exists(parallelizationStatePath(self, 'sentinel'));
	# <p> fetch jids
	qstat_jids = .pollJids();
	# <p> restore state locally
	Load(file = parallelizationStatePath(self, 'state'));
	# <p> raw message
	Lapply_backend__ = get('Lapply_backend__', envir = parallelize_env);
	message = .pollMessageRaw(Lapply_backend__@jids, qstat_jids);
	# <p> refine
	message = .pollMessage(message, continue);
	#			=~ m{(\d+)}sog)
	r = list(continue = continue, message = message);
	r
});
#
#	Rparallel.back.ogsremote.R
#Tue Jun 23 14:20:45 2015

#
#	ParallelizeBackendOGSremote S4 class
#

.ParallelizeBackendOGSremoteDefaultConfig = list(
	remote = 'localhost:parallelize_projects'
);

#' Class \code{"ParallelizeBackendOGSremote"}
#' 
#' Backend class supporting Open Grid Scheduler use from remote machines
#' 
#' 
#' @name ParallelizeBackendOGSremote-class
#' @aliases ParallelizeBackendOGSremote-class
#' getResult,ParallelizeBackendOGSremote-method
#' initialize,ParallelizeBackendOGSremote-method
#' initScheduling,ParallelizeBackendOGSremote-method
#' lapply_dispatchFinalize,ParallelizeBackendOGSremote-method
#' performParallelizationStep,ParallelizeBackendOGSremote-method
#' pollParallelization,ParallelizeBackendOGSremote-method
#' @docType class
#'
#' @section Objects from the Class:
#' Objects can be created by calls of the form
#'	\code{new("ParallelizeBackendOGSremote", config, ...)}.
#' During normal operation you do not have to create objects of this class yourself. Instead, \code{parallelize_initialize} will create such instances for you. The class can be configured with the following field in the \code{Lapply_config} argument of \code{parallelize_initialize}.
#' \itemize{
#'   \item freezerClass: defaults to \code{LapplyPersistentFreezer}. It is recommended to use \code{LapplyGroupingFreezer} for this backend as it is the most efficient freezer. Currently, \code{LapplyGroupingFreezer} is only supported for this backend.
#'   \item stateDir: directory to store results from computations. This location is passed to \code{LapplyPersistentFreezer}. If temporary behavior is desired it can be set to: \code{sprintf('\%s/tmp/remote', tempdir())}.
#'    \item sourceFiles: a vector of files to be sourced prior to parallel execution
#'    \item libraries: a vector of package names to be loaded prior to parallel execution
#'    \item remote: a scp path to a folder on the server that can be used to store temporary files, e.g. 'user@@localhost:tmp/remote/test'. A unique subfolder per computation is created within this folder to store files (unique tempfolder).
#'     \item qsubOptions: extra options that are passed to the \code{qsub.pl} utility included in the package that is used to submit jobs. Execute \code{./qsub.pl --help} in the \code{inst/Perl} folder of the package to see all options and examples. Important options include \code{--queue} to specify the queue, \code{--memory} to set an upper bound for the needed memory (.e.g. \code{--memory 4G}) and \code{--logLevel} to set verbosity of output (level 5 produces detailed output).
#' }
#' To use this backend you have to have access password-less ssh access to a linux server running the Open Grid Scheduler (OGS) or the Sun Grid engine (SGE). You can install OGS locally (see \link{http://gridscheduler.sourceforge.net/CompileGridEngineSource.html}). \code{ssh} and \code{scp} have to be installed on the local machine.
#' Job output (stdout, stderr) as well as \code{qsub.pl} output is stored in subfolder of the unique tempfolder starting with 'qsubOutput'.
#'
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso %% ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ %% ~~or
#' \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
#' \code{\linkS4class{ParallelizeBackend}},
#' \code{\linkS4class{ParallelizeBackendLocal}},
#' \code{\linkS4class{ParallelizeBackendSnow}},
#' \code{\linkS4class{ParallelizeBackendOGSremote}}
#' @keywords classes
#' @examples
#' 
#' showClass("ParallelizeBackendOGSremote")
#' 
setClass('ParallelizeBackendOGSremote',
	contains = 'ParallelizeBackend',
	representation = list(jids = 'ParallelizeBackendOGSstate'),
	prototype = list(jids = .ParallelizeBackendOGSstateClass$new())
);
setMethod('initialize', 'ParallelizeBackendOGSremote', function(.Object, config, ...) {
	# <p> super-class
	config = merge.lists(.ParallelizeBackendOGSDefaultConfig, config);
	.Object = callNextMethod(.Object, config = config, ...);
	# <p> OGS initialization
	Log('initializing OGS for remote execution', 6);
	.Object@offline = T;
	# restart on other host
	.Object
});

.remoteConfigForOGSremote = function(stateDir = '.') {
	Lapply_remote_config = Lapply_getConfig();
	backendConfig = merge.lists(
		Lapply_remote_config$backendConfig,
		list(backend = 'OGS', stateDir = stateDir, logLevel = Log.level())
	);
	Lapply_remote_config$backends[[Lapply_remote_config$backend]] = 
		Lapply_remote_config$backendConfig = backendConfig;
	Lapply_remote_config
}
.OGSremoteFile = function(self, tag = '', ext = '.RData') {
	Lapply_remote_config = .remoteConfigForOGSremote(stateDir = self@config$remote);
	remoteDummy = new('ParallelizeBackendOGS', config =
		Lapply_remote_config$backendConfig, signature = self@signature);
	remoteDir = parallelizationStatePath(remoteDummy, tag = tag, ext = ext);
	remoteDir
}
.OGSremoteWorkingDir = function(self).OGSremoteFile(self, tag = '', ext = '')

setMethod('initScheduling', 'ParallelizeBackendOGSremote', function(self, call_) {
	callNextMethod(self);
	Log('ParallelizeBackendOGSremote:initScheduling', 6);
	r = with(self@config, {
	# <p> check starting sentinel
	sentinelPath = parallelizationStatePath(self, 'OGSremote_sentinel');
	if (file.exists(sentinelPath) && !self@config$force_rerun) {
		Log(sprintf('Signature %s already scheduled.', self@signature), 5);
		return(NULL);
	}
# 	# prevent further parallelize calls from re-initializing
# 	c = Lapply_getConfig();
# 	c$backendConfig$force_rerun = F;
# 	Lapply_setConfig(c);

	# <p> establish start sentinel
	sentinel = list(signature = self@signature);
	save(sentinel, file = sentinelPath);

	# <p> setup remote environment
	#remoteDir = sprintf('%s/%s', remote, self@signature);
	remoteDir = .OGSremoteWorkingDir(self);
	sp = splitPath(remoteDir, ssh = T);
	Log(sprintf('setting up parallelization step in dir %s', remoteDir), 5);
	ignore.shell = Log.level() < 5;
	Dir.create(remoteDir, recursive = T, ignore.shell = ignore.shell);
	# either copy source files or explicitely spcified copyFiles (important for dirs);
	if (is.null(self@config$sourceFiles)) sourceFiles = c();
	if (is.null(self@config$copyFiles)) copyFiles = c();
	copyFiles =  unique(union(copyFiles, sourceFiles));
	Log(sprintf('Copying files: %s', join(copyFiles, ', ')), 5);
	File.copy(copyFiles, remoteDir, ignore.shell = ignore.shell, recursive = T, symbolicLinkIfLocal = T);
	# clear jids
	File.remove(.OGSremoteFile(self, 'jids'));
	# <p> remote environment: environment variables
	remoteProfile = remoteEnvSetup(remoteDir);

	# <p> create remote wrappers
	parallelize_remote = function(call_, Lapply_config) {
		parallelize_initialize(Lapply_config = Lapply_config,
			backend = Lapply_config$backend, copy_environments = Lapply_config$copy_environments);
		r = parallelize_internal(call_, parallelize_wait = F);
	};
	# <p> start rampup on remote host
	freeze_control = list(
		sourceFiles = self@config$sourceFiles,
		libraries = self@config$libraries,
		logLevel = Log.level(),
		freeze_relative = T
	);
	remoteConfig = .remoteConfigForOGSremote(stateDir = '.');
	Log('ParallelizeBackendOGSremote:initScheduling:callEvalArgs', 7);
	call_ = callEvalArgs(call_, env_eval = self@config$copy_environments);
	Log('ParallelizeBackendOGSremote:initScheduling:freezeCallOGS', 7);
	r = freezeCallOGS(self, parallelize_remote,
		# parallelize_remote
		call_, Lapply_config = remoteConfig,
		# freeze
		freeze_control = freeze_control,
		freeze_file = sprintf('%s/rampUp:000.RData', remoteDir),
		# System
		patterns = c('cwd', 'qsub', 'ssh'),
		cwd = sp$path, ssh_host = sp$userhost,
		qsubPath = sprintf('%s/qsub', sp$path), qsubMemory = self@config$qsubRampUpMemory,
		ssh_source_file = c(self@config$ssh_source_file, remoteProfile), qsubOptionsAdd = '--exports=-'
#		ssh_source_file = c(self@config$ssh_source_file, remoteProfile)
	);
	# end with
	});
	Log('ParallelizeBackendOGSremote:initScheduling:freezeCallOGS:after', 7);
	self@jids$pushStep(r$jid);
	r
});

# instead of doing something here, we poll the remote backend
setMethod('performParallelizationStep', 'ParallelizeBackendOGSremote',
	function(self, call_, Lapply_config) {
	# prevent from completing computation, result has to be gathered by polling
	Lapply_error();

	if (0) {
	stop('ParallelizeBackendOGSremote backend is a delegating backend and does not perform parallelization itself. Use the following to monitor this backend in a loop:
		r = NULL;
		p = pollParallelization(self);
		if (!p$continue) r = getResult(Lapply_backend__);
		r
	');
	}
});

.catVectorAsLine = function(message, width = options('width')$width) {
	messagePadded = sapply(message, function(line) sprintf('%s%*s', line, width - nchar(line) - 1, ' '));
	messageInALine = paste(messagePadded, collapse = '');
	cat(messageInALine);
	flush.console();
	cat("\r");
}

.catVector = function(message, width = options('width')$width, clear = T, padLines = 40) {
	if (clear) cat(paste(rep("\n", 100), collapse = ''));
	cat(paste(c(message, ''), collapse = "\n"));
	if (padLines > 0) cat(paste(rep("\n", padLines), collapse = ''));
}
catCr = function(message) {
	cat("\r");
	cat(message);
}


setMethod('pollParallelization', 'ParallelizeBackendOGSremote', function(self,
	options = list(printProgress = T)) {
	# <p> overwrite backend configuration
	remote_config = .remoteConfigForOGSremote();
	jidFile = .OGSremoteFile(self, 'jids');
	jids = get(Load(file = jidFile, Load_sleep = 30, Load_retries = 60)[[1]]);
	qstat_jids = .pollJids(patterns = 'ssh',
		ssh_host = splitPath(jidFile, ssh = T)$userhost, ssh_source_file = self@config$ssh_source_file);
	#print(jids); print(qstat_jids);
	message = .pollMessageRaw(jids, qstat_jids);
	# <p> check for completion
	continue = !File.exists(.OGSremoteFile(self, 'sentinel'));
# 	# <p> add rampup
# 	message = c(
# 		progressString(.progressStat(self@jids$steps, 1, qstat_jids), title = 'Rampup 1'),
# 		message
# 	);
	# <p> refine
	message = .pollMessage(message, continue);
	if (!continue) cat("\n");
	#.catVector(message);
	catCr(message);
	r = list(message = message, continue = continue);
	r
});

setMethod('lapply_dispatchFinalize', 'ParallelizeBackendOGSremote',
	function(self) { NULL });

setMethod('getResult', 'ParallelizeBackendOGSremote', function(self) {
	r = get(Load(file = .OGSremoteFile(self, 'result'))[1]);
	r
});
#
#	Rparallel.R
#Fri Jun 15 12:29:14 CEST 2012
#source('Rparallel.back.R');
library('tools');

#' Automate parallelization of function calls by means of dynamic code analysis
#' 
#' Passing a given function name or a call to the parallelize/parallelize_call
#' functions analyses and executes the code, if possible in parallel. Parallel
#' code execution can be performed locally or on remote batch queuing systems.
#' 
#' \tabular{ll}{ Package: \tab parallelize.dynamic\cr Type: \tab Package\cr
#' Version: \tab 0.9\cr Date: \tab 2012-12-12\cr License: \tab LGPL\cr Depends:
#' \tab methods, tools, parallel\cr }
#' 
#' Use \code{parallelize_initialize} to set up a configuration for performing
#' parallel computations. After that, you can use \code{parallelize} and
#' \code{parallelize_call} to run a dynamic analysis on given functions or
#' function calls and execute parallel jobs resulting from this analysis. For
#' the remote backend OGSremote, the current implmentation is expected to break
#' on machines running operating systems from the Windows family on account of
#' dependencies on system calls. The local backend snow should work on Windows.
#' Patches are welcome to solve any Windows issues.
#' 
#' @name parallelize.dynamic-package
#' @aliases parallelize.dynamic-package parallelize.dynamic
#' @docType package
#' @author Stefan Böhringer
#' 
#' Maintainer: Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link[parallel:parallel-package]{parallel}}
#' @references R Journal article "Dynamic parallelization of R functions",
#' submitted
#' @keywords package

#' @export parallelize parallelize_call parallelize_initialize parallelize_declare parallelize_setEnable parallelize_internal tempcodefile readFile
#' @exportMethod finalizeParallelization
#' @exportMethod getResult
#' @exportMethod initialize
#' @exportMethod initScheduling
#' @exportMethod isSynchroneous
#' @exportMethod lapply_dispatchFinalize
#' @exportMethod lapply_dispatch
#' @exportMethod lapply_results
#' @exportMethod parallelize_backend
#' @exportMethod performParallelizationStep
#' @exportMethod pollParallelization
#' @exportMethod restoreParallelizationState
#' @exportMethod saveParallelizationState
#' @exportMethod scheduleNextParallelization
#' @exportClass LapplyExecutionState
#' @exportClass LapplyFreezer
#' @exportClass ParallelizeBackend
#' @exportClass ParallelizeBackendLocal
#' @exportClass ParallelizeBackendOGSremote
#' @exportClass ParallelizeBackendSnow
#' @exportClass LapplyRNGseedCapsule

#
#	<p> Lapply state reference classes
#

#' @title Class \code{"LapplyState"}
#' 
#' This class is the base class for classes reflecting different stages of the
#' parallelization process: probing and running.
#' 
#' 
#' @name LapplyState-class
#' @docType class
#' @section Extends:
#' 
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{LapplyExecutionState-class}},
#' \code{\link{LapplyRunState-class}} %% ~~or \code{\linkS4class{CLASSNAME}}
#' for links to other classes ~~~
#' @keywords classes
#' @examples
#' 
#' showClass("LapplyState")
#' 
LapplyStateClass = setRefClass('LapplyState',
	fields = list( sequence = 'numeric', depth = 'numeric',
	runMode = 'logical', probeMode = 'logical', max_depth = 'numeric'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		sequence <<- 0;
		depth <<- 0;
		runMode <<- F;
		probeMode <<- F;
		.self$initFields(...);
		.self
	},
	# depth is defined by the number of times Lapply is recursively called
	depthInc = function() { depth <<- depth + 1; },
	depthDec = function() { depth <<- depth - 1; },
	# sequence is defined by the number of Lapplys that were started no matter how deeply nested
	sequenceInc = function() { sequence <<- sequence + 1; },
	sequenceDec = function() { sequence <<- sequence - 1; },
	isEqualTo = function(s) { depth == s$depth && sequence == s$sequence }
	#
	#	</p> methods
	#
	)
);
LapplyStateClass$accessors(names(LapplyStateClass$fields()));

#' Class \code{"LapplyProbeState"}
#' 
#' This subclass of \code{"\linkS4class{LapplyState}"} tracks probing runs of
#' the parallelization process.
#' 
#' 
#' @name LapplyProbeState-class
#' @docType class
#' @note See documentation of \code{"\linkS4class{LapplyState}"}.
#' @section Extends: Class \code{"\linkS4class{LapplyState}"}, directly.
#' 
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{"\linkS4class{LapplyState}"}
#' @keywords classes
#' @examples
#' 
#' showClass("LapplyProbeState")
#' 
LapplyProbeStateClass = setRefClass('LapplyProbeState',
	fields = list( elements = 'list' ),
	contains = 'LapplyState',
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		# initialize super class
		callSuper(...);
		# defaults
		elements <<- list();
		# overwrites
		.self$setProbeMode(T);
		.self
	},
	pushElements = function(es) {
		elements[[depth]] <<- if (length(elements) < depth) es else c(elements[[depth]], es);
		NULL
	},
	elementsCount = function(atDepth) {
		count = sum(if (atDepth > length(elements)) elements[[length(elements)]] else elements[[atDepth]]);
		count
	}
	#
	#	</p> methods
	#
	)
);
LapplyProbeStateClass$accessors(names(LapplyProbeStateClass$fields()));

#' Class \code{"LapplyRunState"}
#' 
#' This subclass of \code{"\linkS4class{LapplyState}"} tracks running of code
#' of the parallelization process.
#' 
#' 
#' @name LapplyRunState-class
#' @docType class
#' @section Extends: Class \code{"\linkS4class{LapplyState}"}, directly.
#' 
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\linkS4class{LapplyState}}
#' @keywords classes
#' @examples
#' 
#' showClass("LapplyRunState")
#' 
LapplyRunStateClass = setRefClass('LapplyRunState',
	fields = list( chunkSize = 'numeric' ),
	contains = 'LapplyState',
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		# initialize super class
		callSuper(...);
		# overwrites
		.self$setRunMode(T);
		.self
	}
	#
	#	</p> methods
	#
	)
);
LapplyRunStateClass$accessors(names(LapplyRunStateClass$fields()));

#
#	<p> random number generation related classes
#

assignRandomSeed = function(v).Random.seed <<- v
LapplyRNGseedCapsuleClass = setRefClass('LapplyRNGseedCapsule',
	fields = list(
		type = 'character',
		seed = 'integer'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		.self$initFields(...);
		.self
	},
	# <!>
	# use .RandomSeed interface
	#
	store = function() {
		seed <<- get('.Random.seed', envir = .GlobalEnv);
		type <<- RNGkind();
		NULL
	},
	restore = function() {
		assignRandomSeed(c(NULL, seed))
		NULL
	}
	#	</p> methods
	#
	)
);
LapplyRNGseedCapsuleClass$accessors(names(LapplyRNGseedCapsuleClass$fields()));

#
#	<p> freezer classes
#


# Freezer class stores individual calls for list elements iterated overwrites
# Also the structure of Lapply calls is stored to be able to re-associate results
#	with Lapply calls
# The isolation of individual calls allows for re-shuffeling of bundling calls for final
#	execution

#' Class \code{"LapplyFreezer"}
#' 
#' This class encapsulates storage of calls and their results. Interaction with
#' this is done from backends and subclassing is only required if a new storage
#' mechanism of unevaluated calls or results thereof is needed. The end user
#' does not interact with this class.
#' 
#' 
#' @name LapplyFreezer-class
#' @docType class
#' @section Extends:
#' 
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{LapplyPersistentFreezer-class}} %% ~~or
#' \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
#' @keywords classes
#' @examples
#' 
#' showClass("LapplyFreezer")
#' 
LapplyFreezerClass = setRefClass('LapplyFreezer',
	fields = list( slots = 'list', calls = 'list', results = 'list', copy_env = 'logical' ),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		slots <<- list();
		calls <<- list();
		copy_env <<- FALSE;
		.self$initFields(...);
		.self
	},
	# depth is defined by the number of times Lapply is recursively called
	clear = function() {
		slots <<- list();
		calls <<- list();
		gc();
	},
	push = function(sequence, f, l, args) {
		# store by seqeunce id from LapplyState object
		Log(sprintf('Freezing %d invocations @ seq %d.', length(l), sequence), 5);
		slots[[as.character(sequence)]] <<- list(
			# definition of function called
			f = f,
			# number of list elements iterated
			N = length(l),
			# start index of result list in sequential order of calls
			start = sum(list.key(slots, 'N')) + 1
		);
		Log(sprintf('LapplyFreezer: copy environment: %s', copy_env), 5);
		calls <<- c(calls, lapply(l, function(e) {
			callWithFunctionArgs(f, c(list(e), args), env_eval = copy_env)
		}));
		NULL
	},
	Ncalls = function()length(calls),
	call = function(i)calls[[i]],
	callRange = function(from, to)stop('callRange not implemented in LapplyFreezer'),

	# <p> results
	# for efficiency use nested structure
	pushResults = function(r){
		results[[length(results) + 1]] <<- r;
	},
	# in case results were stored in chunks (as lists) this method flattens the structure
	unlistResults = function() {
		results <<- unlist(results, recursive = F);
		NULL
	},
	finalizeResults = function() { NULL },
	# only to be called after finalizeResults
	resultsForSequence = function(s) {
		slot = slots[[as.character(s)]];
		results[slot$start : (slot$start + slot$N - 1)];
	}

	#
	#	</p> methods
	#
	)
);
LapplyFreezerClass$accessors(names(LapplyFreezerClass$fields()));

#' Class \code{"LapplyPersistentFreezer"}
#' 
#' Subclass of \code{LapplyFreezer} that stores results on disk. See
#' \code{LapplyFreezer} for more documentation.
#' 
#' 
#' @name LapplyPersistentFreezer-class
#' @docType class
#' @section Extends: Class \code{"\linkS4class{LapplyFreezer}"}, directly.
#' 
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{LapplyFreezer-class}}
#' @keywords classes
#' @examples
#' 
#' showClass("LapplyPersistentFreezer")
#' 
LapplyPersistentFreezerClass = setRefClass('LapplyPersistentFreezer',
	contains = 'LapplyFreezer',
	fields = list(),
	methods = list(
	finalizeResults = function() {
		callSuper();
	},
	resultsForSequence = function(s) {
		slot = slots[[as.character(s)]];
		seqCalls = slot$start : (slot$start + slot$N - 1);
		# iterate all chunks in current rampUp (== length(results))
		r = lapply(results[[length(results)]], function(r) {
			i = intersect(r$from:r$to, seqCalls);
			r1 = if (length(i) > 0) {
				r0 = frozenCallResults(r$file);
				# do we have to skip portions of current list?
				from = max(slot$start - r$from + 1, 1);
				# how much is left to grab?
				to = from + min(r$to - r$from + 1 - (from - 1), slot$N - max(r$from - slot$start, 0)) - 1;
				r0 = subListFromRaggedLists(r0, from = from, to = to);
				r0
			} else NULL;
			r1
		});
		r = r[!sapply(r, is.null)];
		r = unlist.n(r, 1);
		r
	}

	)
);

LapplyGroupingFreezerClass = setRefClass('LapplyGroupingFreezer',
	contains = 'LapplyPersistentFreezer',
	methods = list(
	#
	#	<p> methods
	#
	push = function(sequence, f, l, args) {
		# store by seqeunce id from LapplyState object
		Log(sprintf('Freezing %d invocations @ seq %d.', length(l), sequence), 5);
		slots[[as.character(sequence)]] <<- list(
			# definition of function called
			f = f,
			# number of list elements iterated
			N = length(l),
			# start index of result list in sequential order of calls
			start = sum(list.key(slots, 'N')) + 1
		);
		calls <<- c(calls, list(list(elements = l, fct = f, arguments = args)));
		NULL
	},
	call = function(i) {
		# length of groups lapply calls
		Nsegments = sapply(calls, function(e)length(e$elements));
		NsegmentsCS = c(0, cumsum(Nsegments));
		segment = rev(which(i > NsegmentsCS))[1];
		mycall = calls[[segment]];

		callWithFunctionArgs(mycall$fct,
			c(mycall$elements[i - NsegmentsCS[segment]], mycall$arguments, env_eval = copy_env)
		)
	},
	callRange = function(from, to){
		# length of groups lapply calls
		Nsegments = sapply(calls, function(e)length(e$elements));
		sl = subListFromRaggedIdcs(Nsegments, from, to);
		r = lapply(sl, function(e){
			lc = calls[[e$segment]];		# list-call
			r = list(
				elements = lc$elements[e$range$from: e$range$to], fct = lc$fct, arguments = lc$arguments
			);
			r
		});
		r
	},
	Ncalls = function() {
		sum(sapply(calls, function(e)length(e$elements)))
	},
	getCalls = function() {
		r = lapply(1:self$Ncalls(), function(i)self$call(i));
		r
	}
 	, resultsForSequence = function(s) {
 		#r = unlist.n(callSuper(s), 1);
 		r = callSuper(s);
		r
 	}
	#
	#	</p> methods
	#
	)
)
# The sentinel stack records entry points into parallelization for the different sequential rampUps
#	occuring during parallelization the ramp down of a given Lapply is equivalent to the rampUp of
#	the ensueing parallelization
# As probing occurs for successively deeper levels, the first sequence number for a given level is stored
#	write-once and will represent the first lapply-loop to parallelize
#	As the depth of the rampDown is unclear, sequenceStop will be updated to represent the most current
#	sequence number of ongoing Lapply loops
# Recovering will than happen between sequence and sequenceStop at the recorded depth

#' Class \code{"LapplyExecutionState"}
#' 
#' An instance of this class reflects the entire lifetime of a dynamic
#' parallelization.
#' 
#' 
#' @name LapplyExecutionState-class
#' @docType class
#' @section Extends:
#' 
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Stefan Böhringer
#' 
#' Maintainer: Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{LapplyFreezer-class}}, \code{\link{LapplyState-class}}
#' @keywords classes
#' @examples
#' 
#' showClass("LapplyExecutionState")
#' 
LapplyExecutionStateClass = setRefClass('LapplyExecutionState',
	fields = list(
		# execution state
		sequenceNos = 'list', rampUp = 'numeric',
		# results
		freezerClass = 'character', freezers = 'list', copy_environments = 'logical',
		# random numbers
		randomSeed = 'list'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(freezerClass = 'LapplyFreezer', ...) {
		# initialize super class
		copy_environments <<- TRUE;
		callSuper(freezerClass = freezerClass, ...);
		# defaults
		sequenceNos <<- list();
		rampUp <<- 1;
		# copy current random seed
		.self$storeRandomSeed();	#randomSeed <<- list();
		# overwrites
		.self
	},
	# add a new sentinel in the parallelization stack
	addSentinel = function() {
		sequenceNos[[length(sequenceNos) + 1]] <<- list(depth = -1, sequence = -1, sequenceStop = -1);
	},
	# update only last element in stack (previous sentinels are fixed)
	# only record first sequence no for given rampUp and depth
	pushSequenceForRampUp = function(sequenceNo, depth) {
		N = length(sequenceNos);
		# if we probe for bigger depthes we re-record the starting sequence
		#	as parallelization will happen at that deeper level
		if (sequenceNos[[N]]$depth < depth) {
			sequenceNos[[N]] <<- list(depth = depth, sequence = sequenceNo);
		}
		# a new sequence number at the current maximal depth is recored as a potential stop of
		#	the current parallelization
		if (sequenceNos[[N]]$depth <= depth) {
#			sequenceNos[[rampUp + 1]] <<- merge.lists(sequenceNos[[rampUp + 1]],
#				list(sequenceStop = sequenceNo));
			sequenceNos[[N]]$sequenceStop <<- sequenceNo;
			Log(sprintf('new sentinel stop: %d', sequenceNo), 6);
		}
		NULL
	},
	# the currentSentinel is the one to skip, which comes from the previous cursor position
	currentSentinel = function() {
		sequenceNos[[rampUp]]
#		if (rampUp == 1 || rampUp - 1 > length(sequenceNos))
#			list(depth = -1, sequence = -1, sequenceStop = -1) else
#			sequenceNos[[rampUp - 1]]
	},
	incCursor = function() {
		rampUp <<- rampUp + 1;
		.self$currentSentinel()
	},
	resetCursor = function() {
		rampUp <<- 1;
		.self$restoreRandomSeed();
	},

	#
	#	functional methods
	#

	rampUpForeFront = function()length(sequenceNos),
	# detect range where results need to be recovered (thawed from the freezer)
	# the latest state (stack position N) is nascent and ignored
	#	there is currently probing or parallelization going on
	checkAgainstState = function(state) {
		#N = length(sequenceNos);
		N = .self$rampUpForeFront();
		sentinel = .self$currentSentinel();
		r = (N > rampUp &&
			state$sequence >= sentinel$sequence &&
			state$sequence <= sentinel$sequenceStop &&
			state$depth == sentinel$depth);
		#Log(sprintf('sentinel check %d [rampup %d]', r,  Lapply_executionState__$rampUp));
		r
	},
	skipToRampDown = function(state) {
		N = length(sequenceNos);
		sentinel = .self$currentSentinel();
		r = (N > rampUp && state$sequence <= sentinel$sequenceStop);
		#Log(sprintf('sentinel check %d [rampup %d]', r,  Lapply_executionState__$rampUp));
		r
	},
	# <N> after probing length of sequenceNos is increased by one
	#	when reaching this point we want to parallelize
	isLastRampUp = function() {
		rampUp == length(sequenceNos)
	},
	# <N> tb called after the state has been processed
	#	if the last sequence was processed the cursor is advanded to proceed to the next rampUp
	adjustCursor = function(state) {
		sentinel = .self$currentSentinel();
		if (.self$checkAgainstState(state) && state$sequence == sentinel$sequenceStop)
			.self$incCursor();
	},
	#
	#	freezer methods
	#
	currentFreezer = function() {
		if (rampUp > length(freezers)) {
			freezers[[rampUp]] <<- getRefClass(freezerClass)$new(copy_env = copy_environments);
		}
		freezers[[rampUp]]
	},

	#
	#	random numbers
	#
	storeRandomSeed = function() {
		# force first number to be generated if non was so far
		if (!exists('.Random.seed', .GlobalEnv)) runif(1);
		randomSeed <<- list(kind = RNGkind(), seed = get('.Random.seed', envir = .GlobalEnv));
		NULL
	},
	restoreRandomSeed = function() {
		RNGkind(randomSeed$kind[1], randomSeed$kind[2]);
		# assume .Random.seed not to be masked
		# explicit assignment in .GlobalEnv not possible due to R package policies
		.Random.seed <- randomSeed$seed;
	}
	

	#
	#	</p> methods
	#
	)
);
LapplyExecutionStateClass$accessors(names(LapplyExecutionStateClass$fields()));


#
#	<p> core parallize functions
#

#if (!exists('parallelize_env')) parallelize_env <- new.env();

# force_rerun instructs backends to ignore state-retaining files and re-run all computations

#  parallelize_initialize
#' Initialize dynamic parallelization of ensuing parallelize calls
#' 
#' Initialzes the parallelization process. The config argument describes all
#' parameters for as many backends as are available. Remaining arguments select
#' a configuration for the ensuing parallelization from that description.
#' 
#' \code{Lapply_config} is a list with the following elements
#'	\itemize{
#'		\item max_depth: maximal depth to investigate during probing
#'		\item parallel_count: provide default for the number of parallel jobs to generate, overwritten by
#'			the function argument
#'		\item offline: this option determines whether parallelize returns before performing the ramp-down. This is relevant for backends running on remote machines. See especially the \code{OGSremote} backend.
#'		\item backends: a list that contains parameters specific for backends. The name of each element should be the name of a backend without the prefix \code{ParallelizeBackend}. Each of the elements is itself a list with the paramters. If several configurations are required for a certain backend - e.g. a batch-queuing system for which different queues are to be used - the name can be chosen arbitrarily. Then the element must contain an element with name \code{backend} that specifies the backend as above (example below). For the backend specific parameters see the class documentation of the backends.
#'  }
#' 
#' @aliases parallelize_initialize Lapply_initialize
#' @param Lapply_config A list describing possible configurations of the
#' parallelization process. See Details.
#' @param stateClass A class name representing parallelization states. Needs
#' only be supplied if custom extensions have been made to the package.
#' @param backend The name of the backend used. See Details and Examples.
#' @param freezerClass The freezerClass used to store unevaluated calls that
#' are to be executed in parallel. Needs only be supplied if custom extensions
#' have been made to the package.
#' @param \dots Extra arguments passed to the initializer of the stateClass.
#' @param force_rerun So called offline computations are stateful. If a given
#' rampUp has been completed an ensuing call - even a rerun of the script in a
#' new R interpreter - reuses previous result. If set to TRUE force_rerun
#' ignores previous results and recomputes the whole computation.
#' @param sourceFiles Overwrite the \code{sourceFiles} entry in
#' \code{Lapply_config}.
#' @param parallel_count Overwrite the \code{parallel_count} entry in
#' \code{Lapply_config}.
#' @param declare_reset if \code{FALSE}, add sourcefile/libraries to what was declared before
#' @param copy_environments defaults to \code{TRUE} and copies all unbound variables if the
#' backend requires so. \code{...} need to be specified as arguments to \code{Lapply} like so:
#' \code{Lapply(values, function(v, ...)(1 + compute(v, ...)), ...)}. Otherwise an error messages
#' results on 'non-local' backends (everything but \code{local}) at the moment.
#' @return Value \code{NULL} is returned.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{parallelize}}, \code{\link{parallelize_call}}, 
#'	 \code{\linkS4class{ParallelizeBackend}},
#'	 \code{\linkS4class{ParallelizeBackendLocal}},
#'   \code{\linkS4class{ParallelizeBackendSnow}},
#'   \code{\linkS4class{ParallelizeBackendOGSremote}}
#' @examples
#' 
#'   config = list(max_depth = 5, parallel_count = 24, offline = TRUE, backends = list(
#'     snow = list(
#'       localNodes = 1, sourceFiles = c('RgenericAll.R', 'Rgenetics.R', 'RlabParallel.R')
#'     ),
#'     local = list(
#'       path = sprintf('%s/tmp/parallelize', tempdir())
#'     ),
#'     `ogs-1` = list(
#'       backend = 'OGS',
#'       sourceFiles = c('RgenericAll.R', 'RlabParallel.R'),
#'       stateDir = sprintf('%s/tmp/remote', tempdir()),
#'       qsubOptions = sprintf('--queue all.q --logLevel %d', 2),
#'       doNotReschedulde = TRUE
#'     ),
#'     `ogs-2` = list(
#'       backend = 'OGS',
#'       sourceFiles = c('RgenericAll.R', 'RlabParallel.R'),
#'       stateDir = sprintf('%s/tmp/remote', tempdir()),
#'       qsubOptions = sprintf('--queue subordinate.q --logLevel %d', 2),
#'       doSaveResult = TRUE
#'     ),
#'     `ogs-3` = list(
#'       backend = 'OGSremote',
#'       remote = 'user@@localhost:tmp/remote/test',
#'       sourceFiles = c('RgenericAll.R', 'RlabParallel.R'),
#'       stateDir = sprintf('%s/tmp/remote/test_local', tempdir()),
#'       qsubOptions = sprintf('--queue all.q --logLevel %d', 2),
#'       doSaveResult = TRUE
#'     )
#'   ));
#'   # run ensuing parallelizations locally, ignore result produced earlier
#'   parallelize_initialize(config, backend = "local", force_rerun = FALSE);
#'   # run ensuing parallelizations on the snow cluster defined in the snow backend section
#'   parallelize_initialize(config, backend = "local");
#'   # run ensuing parallelizations on a local Open Grid Scheduler
#'   parallelize_initialize(config, backend = "ogs-1");
#'   # run same analysis as above with different scheduling options
#'   parallelize_initialize(config, backend = "ogs-2");
#'   # run same analysis on a remote Opend Grid Scheduler
#'   # user 'user' on machine 'localhost' is used
#'   parallelize_initialize(config, backend = "ogs-3");
#' 
# <!> Lapply_config_default -> get('Parallelize_config__')
parallelize_initialize = Lapply_initialize = function(Lapply_config = get('Parallelize_config__'), 
	stateClass = 'LapplyState', backend = 'local', freezerClass = 'LapplyFreezer', ...,
	force_rerun = FALSE, sourceFiles = NULL, libraries = NULL, parallel_count = NULL,
	copy_environments = TRUE, declare_reset = FALSE, rngSeedCapsules = 'LapplyRNGseedCapsule') {
	# <p> check for turning off
	if (backend == 'off') {
		parallelize_setEnable(F);
		return(NULL);
	} else parallelize_setEnable(T);
	# <p> misc setup
	Log.setLevel(firstDef(Lapply_config$logLevel, Log.level(), 4));
	parallelize_setEnable(T);
	# <p> config
	configPre = Lapply_createConfig();
	sourceFiles = c(
		if (declare_reset) c() else configPre$sourceFiles,
		Lapply_config$sourceFiles, Lapply_config$backends[[backend]]$sourceFiles, sourceFiles
	);
	libraries = unique(c('parallelize.dynamic',
		if (declare_reset) c() else configPre$libraries,
		libraries
	));
	copyFiles = if (declare_reset) c() else configPre$copyFiles;
	backendClass = firstDef(Lapply_config$backends[[backend]]$backend, backend);
	backendConfig = merge.lists(
		Lapply_backendConfig_default,
		Lapply_backendConfigSpecific_default[[backendClass]],	# <i> --> class method
		Lapply_config$backends[[backend]],
		list(force_rerun = force_rerun,
			sourceFiles = sourceFiles, libraries = libraries, copyFiles = copyFiles,
			copy_environments = copy_environments)
	);
	Lapply_config = merge.lists(
		Lapply_config_default,
		Lapply_config,
		list(backend = backend, backendConfig = backendConfig, parallel_count = parallel_count,
			sourceFiles = sourceFiles, libraries = libraries,
			copy_environments = copy_environments,
			seedCapsules = rngSeedCapsules)
	);
	Lapply_setConfig(Lapply_config);
	# <p> backend
	if (exists('Lapply_backend__',  envir = parallelize_env)) rm('Lapply_backend__', envir = parallelize_env);
	# <p> iteration states
	Lapply_initializeState(stateClass, ...);
	freezerClass = firstDef(backendConfig$freezerClass, freezerClass);
	assign('Lapply_executionState__', LapplyExecutionStateClass$new(
		freezerClass = freezerClass, copy_environments = Lapply_config$copy_environments),
		envir = parallelize_env);
	NULL
}

#  parallelize_declare
#' Declare static parallelization environment for parallel calls.
#'
#' This includes files to be 
#' sourced, packages to be loaded and files to be copied. This function performs a subset of
#' \code{parallelize_initialize} and its intended use is to factor out certain parameters from
#' \code{parallelize_initialize} calls.
#'
#' 
#' @param source Overwrite the \code{sourceFiles} entry in
#' \code{Lapply_config}.
#' @param packages Overwrite the \code{library} entry in
#' \code{Lapply_config}.
#' @param copy Vector of pathes that is recursively copied if needed.
#' @param reset If true (the default), values are replaced otherwise values are appended
#' @return Value \code{NULL} is returned.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{parallelize}}, \code{\link{parallelize_call}}, 
#'	 \code{\linkS4class{ParallelizeBackend}},
#'	 \code{\linkS4class{ParallelizeBackendLocal}},
#'   \code{\linkS4class{ParallelizeBackendSnow}},
#'   \code{\linkS4class{ParallelizeBackendOGSremote}}
#' @examples
#'
#'   ## Not run:
#'   # run ensuing parallelizations locally, ignore result produced earlier
#'   parallelize_declare(source = 'mySourceFile.R', packages = 'glmnet');
#' 
parallelize_declare = function(source = NULL, packages = NULL, copy = NULL, reset = TRUE) {
	Lapply_config = Lapply_createConfig();
	# <p> config
	sourceFiles = unique(c(if (!reset) Lapply_config$sourceFiles else c(), source));
	libraries = unique(c(if (!reset) Lapply_config$libraries else c(), packages));
	copyFiles = unique(c(if (!reset) Lapply_config$copyFiles else c(), copy));
	# <!><i> copyFiles

	Lapply_setConfig(merge.lists(
		Lapply_config_default,
		Lapply_config,
		list(sourceFiles = sourceFiles, libraries = libraries, copyFiles = copyFiles)
	));
	NULL
}

parallelize_initializeBackendWithCall = function(call_, Lapply_config) with(Lapply_config, {
	# heuristic to get original function name if not supplied
	#functionName = firstDef(call_$name, deparse(sys.call(-6)[[2]][[1]]));
	functionName = firstDef(call_$name, deparse(sys.call(-6)[[2]]));
	#signature = md5sumString(sprintf('%s%s', tag, deparse(.f)));
	signature = md5sumString(sprintf('%s%s', functionName, backend));
	Log(sprintf('parallelize signature %s', signature), 5);

	backendClass = sprintf('ParallelizeBackend%s', uc.first(firstDef(backendConfig$backend, backend)));
	#backendConfig = rget(sprintf('%s_config__', backendClass), default = list());
	assign('Lapply_backend__', new(backendClass, config = backendConfig, signature = signature),
		envir = parallelize_env);
})

Lapply_initializeState = function(stateClass = 'LapplyState', ...) {
	state = getRefClass(stateClass)$new(...);
	assign('Lapply__', state, envir = parallelize_env);
}
Lapply_initialze_probing = function() {
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply_executionState__$resetCursor();
}

Lapply_setConfig = function(config) {
	assign('Lapply_globalConfig__', config, envir = parallelize_env);
}
Lapply_getConfig = function() {
	get('Lapply_globalConfig__', envir = parallelize_env);
	#Lapply_globalConfig__
}
Lapply_createConfig = function() {
	if (!exists('Lapply_globalConfig__', envir = parallelize_env))
		Lapply_setConfig(Lapply_config_default);
	Lapply_getConfig()
}
Lapply_storeSeeds = function() {
	# make work standalone and from package
	getConfig = if (exists('Lapply_getConfig')) Lapply_getConfig else
		parallelize.dynamic:::Lapply_getConfig;
	o = getConfig();
	capsules = lapply(o$seedCapsules, function(capsule) {
		cap = new(capsule);
		cap$store();
		cap
	})
	assign('seedCapsules', capsules, envir = parallelize_env);
}
Lapply_restoreSeeds = function() {
	capsules = get('seedCapsules', envir = parallelize_env);
	lapply(capsules, function(capsule)capsule$restore());
}

#
#	</p> Lapply state reference classes
#


#
#	<p> S3 classes
#

Lapply_error = function() {
	Lapply__ = get('Lapply__', envir = parallelize_env);
	e = structure(list(state = Lapply__$copy()), class =  c('Lapply_error', 'simpleError'));
	e
}
Lapply_error.as.character = function(e) {
	msg = structure(sprintf('Lapply stopped at sequence %d, depth %d', e$state$sequence, e$state$depth),
		error = e);
	msg
}

#
#	</p> S3 classes
#

#
#	<p> error handling
#

Throw = function(msg, state) {
	assign('Global_error_state__', state, envir = parallelize_env);
	stop(msg);
}

Catch = function(result, errorClass, handler) {
	doCallHandler = class(result) == 'try-error' &&
		any(class(get('Global_error_state__', envir = parallelize_env)) == errorClass);
	r = if (doCallHandler) {
		errorState = get('Global_error_state__', envir = parallelize_env);
		handler(errorState);
	} else NULL;
	r = list(result = r, didCall = doCallHandler);
	r
}

Try = function(expr, catch = list(), silent = T, setClass = F) {
	r = try(expr, silent = silent);
	didCall = F;
	if (exists('Global_error_state__', envir = parallelize_env)) {
		for (i in 1:length(catch)) {
			errorClass = names(catch)[i];
			r0 = Catch(r, errorClass, catch[[i]]);
			if (r0$didCall) {
				r = r0$result;
				if (setClass) {
					if (is.null(r)) r = integer(0);
					class(r) = errorClass;
				}
			}
			didCall = didCall || r0$didCall;
		}
		remove('Global_error_state__', envir = parallelize_env);
	}
	if (!didCall && class(r) == 'try-error') stop(r[1]);
	r
}

#
#	</p> error handling
#

Lapply_config_default = list(
	max_depth = 2, parallel_count = 32, parallel_stack = 10,
	provideChunkArgument = F, offline = F, stateDir = '.',
	wait_interval = 30,
	copy_environments = TRUE
);
Lapply_backendConfig_default = list(
	doNotReschedule = F, doSaveResult = F
);
Lapply_backendConfigSpecific_default = list(
	local = list(freezerClass = 'LapplyFreezer'),
	snow = list(freezerClass = 'LapplyFreezer'),
	OGS = list(freezerClass = 'LapplyGroupingFreezer'),
	OGSremote = list(freezerClass = 'LapplyGroupingFreezer')
);

Lapply_do = function(l, .f, ..., Lapply_config, Lapply_chunk = 1, envir__) {
	#f_ = function(e, ...)do.call(.f, c(list(e), list(...)), envir = envir__);
	f_ = function(e, ...)do.call(.f, list(e, ...), envir = envir__);
	r = if (Lapply_config$provideChunkArgument)
		lapply(l, f_, Lapply_chunk = Lapply_chunk, ...) else
		lapply(l, f_, ...);
	r
}

Lapply_probeDepth = function(l, .f, ..., Lapply_config, Lapply_chunk = 1, envir__) {
	# <p> probe deeper levels if Lapply_depth not yet reached
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply__ = get('Lapply__', envir = parallelize_env);
	Lapply__$pushElements(length(l));
	Log(sprintf('Lapply: Adding %d elements @depth %d.', length(l), Lapply__$depth), 5);
	r = if (Lapply__$max_depth > Lapply__$depth) {
		probeWrapper = function(e, ...) {
			Try(
				.f(e, ...), catch = list(Lapply_error = function(e) {
					Log(sprintf('caught escape from depth %d', e$depth));
					# <N> the escape left the Lapply state stale
					Lapply__$depthDec();
					e
				})
			)
		};
		Lapply_do(l, probeWrapper, ...,
			Lapply_config = Lapply_config, Lapply_chunk = Lapply_chunk, envir__ = envir__);
	} else list(Lapply_error());
	# pushSequenceForRampUp records depending of its current state
	#	see implementation thereof
	Lapply_executionState__$pushSequenceForRampUp(Lapply__$sequence, Lapply__$depth);
	# generate escape from ramp-down
	#Throw('lapply probe escape', Lapply_error());
	if (any(as.vector(sapply(r, class)) == 'Lapply_error')) Throw('lapply probe escape', Lapply_error());
	# reached when no parallelization happened
	r
}

# from determines the starting point of probing
Lapply_probe = function(call_, Lapply_config) with(Lapply_config,  {
 	depths = 1:max_depth;
	Log(sprintf("Lapply_probe: depths to probe: %s", join(depths)), 5);
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	# probing will determine a new sentinel
	Lapply_executionState__$addSentinel();
	r = NULL;
	for (i in depths) {
		Lapply_restoreSeeds();
		# reset state: first rampUp, cursor to beginning
		Lapply_executionState__$resetCursor();
		Lapply_initializeState('LapplyProbeState', max_depth = i);
		Lapply__ = get('Lapply__', envir = parallelize_env);
		# probing function
		Log(sprintf('Lapply: probing depth %d.', i), 5);
		r = Try(Do.call(call_$fct, call_$args, envir = call_$envir),
			catch = list(Lapply_error = function(e) {
				Log('final catch', 6);
				e
			}));
		# no parallelization found, real result already computed
		if (all(class(r) != 'Lapply_error')) break;
		# compute registered parallelization 
		count = Lapply__$elementsCount(i);
		Log(sprintf('Lapply: registered %d parallel jobs @depth %d.', count, i), 5);
		# <p> determine stopping condition
		rampUp = Lapply_executionState__$getRampUp();
		# specific counts per rampUp
		this_parallel_count =
			if (rampUp > length(parallel_count)) parallel_count[1] else parallel_count[rampUp];
		if (count >= this_parallel_count) {
			Log(sprintf('Lapply_probe: %d jobs @depth %d >= %d. Switching to run-mode.',
				count, i, this_parallel_count), 5);
			break;
		}
		# break if no parallelism was found
	}
	# in case of parallelization Lapply_errors were thrown. these are re-thrown at higher levels such
	#	that return values are nested lists of Lapply_errors. We detect this case by probing for a return
	#	list and checking classes of members
	if (any(as.vector(sapply(r, class)) == 'Lapply_error')) {
		r = Lapply_error();
	}
	r
})

Lapply_parallelize = function(l, .f, ..., Lapply_config, envir__) {
	Lapply_backend__ = get('Lapply_backend__', envir = parallelize_env);
	Lapply__ = get('Lapply__', envir = parallelize_env);
	r = if (Lapply__$depth == Lapply__$max_depth) {
		lapply_dispatch(Lapply_backend__, l, .f, ..., envir__ = envir__);
	} else {
		Log(sprintf('entering parallelization at depth %d', Lapply__$depth), 6);
		Lapply_do(l, function(e, ...)Try(.f(e, ...),
			catch = list(Lapply_error = function(e){
				Log('Lapply_do catch', 6);
				Lapply__$depthDec();	# <A> balance depth
				e
			})),
		..., Lapply_config = Lapply_config, envir__ = envir__);
	}
	# <i><N> raise exception only if asynchroneous
	# synchroneous computations are only possible when parallelizing at level 1
	#	or the "program-counter" can be manipulated
	# re-throw
	Throw('lapply run escape', Lapply_error());
	NULL
}

# excute code for the given rampUp
#	Lapply_depth: depth at which to parallelize
Lapply_run = function(call_, Lapply_depth, Lapply_config) {
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply_executionState__$resetCursor();
	# reset state cursor
	Lapply_initializeState('LapplyRunState', max_depth = Lapply_depth);
	Log(sprintf('Lapply_run: running at depth %d.', Lapply_depth), 5);
	
	r = Try(Do.call(call_$fct, call_$args, envir = call_$envir),
		catch = list(Lapply_error = function(e)Log('final run catch', 5)));
	Lapply_backend__ = get('Lapply_backend__', envir = parallelize_env);
	lapply_dispatchFinalize(Lapply_backend__);
	r
}

Lapply_recoverState = function(sequence) {
	Lapply__ = get('Lapply__', envir = parallelize_env);
	Log(sprintf('Recovering state for sequence %d, depth %d.', Lapply__$sequence, Lapply__$depth), 5);
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	freezer = Lapply_executionState__$currentFreezer();
	r = freezer$resultsForSequence(sequence);
	r
}

# Lapply should not applied here as it lives in the parallelize.dynamic package and will
#	not be replaced by source
# Lapply will be set to Lapply_backup on activation of parallelization
#Lapply = lapply;
.lapply = Lapply_backup = function(l, .f, ...,
	Lapply_config = Lapply_getConfig(),
	#Lapply_local = rget('Lapply_local', envir = parallelize_env, default = T),
	Lapply_local = Lapply_config$local,
	Lapply_chunk = 1) {

	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply__ = get('Lapply__', envir = parallelize_env);
	# <p> Lapply__ state
	Lapply__$depthInc();
	envir__ = parent.frame();
	#	starting new Lapply
	Lapply__$sequenceInc();
	# <p> handle cases
	Log(sprintf("Sequence %d.", Lapply__$sequence), 6);
	r = if (Lapply_executionState__$checkAgainstState(Lapply__)) {
		r = Lapply_recoverState(Lapply__$sequence);
		Lapply_executionState__$adjustCursor(Lapply__);
		r
	#	<p> Lapply within range of sentinel but not at max_depth
	} else if (Lapply_executionState__$skipToRampDown(Lapply__)) {
		Log(sprintf("Skipping sequence %d.", Lapply__$sequence), 5);
		# <p> either do regular lapply or skip parallelisation to required rampUp
		Lapply_do(l, .f, ..., Lapply_config = Lapply_config, Lapply_chunk = Lapply_chunk, envir__ = envir__);
	#	<p> probe for degree of parallelism
	} else if (Lapply__$probeMode) {
		Lapply_probeDepth(l, .f, ...,
			Lapply_config = Lapply_config, Lapply_chunk = Lapply_chunk, envir__ = envir__);
	#	<p> parallelization
	} else if (Lapply__$runMode) {
		Lapply_parallelize(l, .f, ..., Lapply_config = Lapply_config, envir__ = envir__);
	#	<p> local mode
	} else {
		Lapply_do(l, .f, ..., Lapply_config = Lapply_config, Lapply_chunk = Lapply_chunk, envir__ = envir__);
	};
	# <p> Lapply__ state
	Lapply__$depthDec();

	r
}

#Sapply = sapply;
Sapply_backup = function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
	r = Lapply(X, FUN, ...);
	r0 = sapply(r, identity, simplify = simplify, USE.NAMES = USE.NAMES);
	r0
}

#' Expose an apply-loop to parallelization
#' 
#' Replacing and apply/lapply/sapply call with a Apply/Lapply/Sapply call makes
#' it amenable to analysis by the parallelize function that can determine
#' dynamic parallelism in running code.
#' 
#' Please refer
#' to the documentation of apply/lapply/sapply for further documenation. The
#' semantics of Apply/Lapply/Sapply are identical to apply/lapply/sapply. Using
#' these functions implies that you want the parallelization mechanism to be
#' applied to these loops.
#' 
#' @aliases Apply Lapply Sapply
#' @param X See documentation for \code{apply}.
#' @param MARGIN See documentation for \code{apply}.
#' @param FUN See documentation for \code{sapply}.
#' @param simplify See documentation for \code{sapply}.
#' @param USE.NAMES See documentation for \code{sapply}.
#' @param .f See documentation for \code{lapply}.
#' @param l See documentation for \code{lapply}.
#' @param Lapply_config See documentation for \code{parallelize_intialize}.
#'   Normally, this argument should be ignored.
#' @param Lapply_local Force local execution. Normally, this argument should be
#'   ignored.
#' @param Lapply_chunk Normally, this argument should be ignored.
#' @param \dots See documentation for \code{apply}.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{parallelize}}
#' @keywords programming iteration parallel programming
#' @examples
#' 
#' 	r0 = sapply(1:10, function(x)x^2);
#' 	r1 = Sapply(1:10, function(x)x^2);
#' 	print(all(r0 == r1));
#' 
#Apply = function(X, MARGIN, FUN, ...) {}
Apply_margin_error = 'wrong MARGIN argument supplied to Apply';
#Apply = apply;
Apply_backup = function(X, MARGIN, FUN, ...) {
	r = if (length(MARGIN) == 1) {
		extractor = if (MARGIN == 1) function(X, i)X[i, ] else
			if (MARGIN == 2) function(X, i)X[, i] else stop(Apply_margin_error);
		r0 = Lapply(1:dim(X)[MARGIN], function(i, ..., Apply_object__, Apply_FUN__, Apply_extractor__) {
			Apply_FUN__(Apply_extractor__(Apply_object__, i), ...)
		} , ..., Apply_object__ = X, Apply_FUN__ = FUN, Apply_extractor__ = extractor);
		r = sapply(r0, function(e)e);
		r
	} else if (length(MARGIN) == 2 && all(MARGIN == 1:2)) {
		extractor = function(X, tuple)X[tuple[1], tuple[2]];
		els = apply(merge(data.frame(row = 1:dim(X)[1]), data.frame(col = 1:dim(X)[2])), 1, as.list);
		r0 = Lapply(els, function(i, ..., Apply_object__, Apply_FUN__, Apply_extractor__) {
			Apply_FUN__(Apply_extractor__(Apply_object__, unlist(i)), ...)
		} , ..., Apply_object__ = X, Apply_FUN__ = FUN, Apply_extractor__ = extractor);
		r = sapply(r0, identity);
		if (is.vector(r)) r = matrix(r, ncol = dim(X)[1]);
		r
	} else {
		stop(Apply_margin_error);
	}
	r
}

parallelizeStep = function(call_, Lapply_config) {
	Lapply_storeSeeds();
	# probe parallelism
	r = Lapply_probe(call_, Lapply_config = Lapply_config);
	# no parallelization was possible
	if (all(class(r) != 'Lapply_error')) return(r);
	# run computation for this rampUp sequence
	Lapply__ = get('Lapply__', envir = parallelize_env);
	Lapply_restoreSeeds();
	Lapply_run(call_, Lapply_depth = Lapply__$max_depth, Lapply_config = Lapply_config);
	Lapply_error();
}

# tag: allow to uniquify the signature for multiple calls to the same function
parallelizeOfflineStep = function(call_, Lapply_config) with(Lapply_config, {
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply_backend__ = get('Lapply_backend__', envir = parallelize_env);
	Log(sprintf('parallelize rampUp %d', Lapply_executionState__$rampUpForeFront()), 5);
	#statePath = sprintf('%s/.parallelize%s.RData', stateDir, signature);
	if (Lapply_executionState__$rampUpForeFront() == 0) {
		initScheduling(Lapply_backend__, call_);
	} else {
		restoreParallelizationState(Lapply_backend__);
	}
	#r = parallelizeStep(.f, ..., Lapply_config = Lapply_config);
	r = performParallelizationStep(Lapply_backend__, call_, Lapply_config = Lapply_config);
	saveParallelizationState(Lapply_backend__);
	if (any(class(r) == 'Lapply_error')) {
		if (!backendConfig$doNotReschedule)
			scheduleNextParallelization(Lapply_backend__, call_);
	} else {
		r = finalizeParallelization(Lapply_backend__, r);
	}
	r
})

parallelize_dummy = function(.f, ..., Lapply_config = NULL, envir__ = NULL) {
	.f(...)
}
parallelize_call_dummy = function(.call, Lapply_config = NULL, envir__ = parent.frame(n = 2)) {
	base:::eval(.call, envir = envir__)
}

parallelize_internal = function(call_, Lapply_local = rget('Lapply_local', default = F),
	parallelize_wait = T) {
	Lapply_config = merge.lists(Lapply_getConfig(), list(local = Lapply_local));
	r = if (Lapply_local) {
		# <!><i> setEnable(F), re-enable afterwards
		do.call(call_$fct, call_$args, envir = call_$envir);
	} else {
		Lapply_setConfig(Lapply_config);
		parallelize_initializeBackendWithCall(call_, Lapply_config = Lapply_config);
		Lapply_backend__ = get('Lapply_backend__', envir = parallelize_env);
		r = parallelize_backend(Lapply_backend__, call_);
		# this is a delegating backend (only supported in offline mode)
		if (parallelize_wait && Lapply_backend__@offline) {
			while (pollParallelization(Lapply_backend__)$continue) Sys.sleep(Lapply_config$wait_interval);
			r = getResult(Lapply_backend__);
		}
		r
	}
	r
}

#' Subject a function to dynamic parallelization
#' 
#' This function executes all necessary steps to perform a dynamic analysis of
#' parallelism of a given function, create objects that encapsulate code that
#' can be executed in parallel, transfer this to a execution backend which
#' potentially is a remote target, recollect results and resume execution in a
#' transparent way.
#' 
#' Function parallelize and parallelize_call both perform a dynamic
#' parallelization of the computation of .f, i.e. parallelism is determined at
#' run-time. Points of potential parallelism have to be indicated by the use of
#' Apply/Sapply/Lapply (collectively Apply functions) instead of
#' apply/sapply/lapply in existing code. The semantics of the new function is
#' exactly the same as that of the original functions such that a simple
#' upper-casing of these function calls makes existing programs amenable to
#' parallelization. Parallelize will execute the function .f, recording the
#' number of elements that are passed to Apply functions. Once a given
#' threshold (degree of parallelization) is reached computation is stopped and
#' remaining executions are done in parallel. A call parallelize_initialize
#' function determines the precise mechanism of parallel execution and can be
#' used to flexibly switch between different resources.
#' 
#' @aliases parallelize parallelize_call
#' @param .f Function to be parallelized given as a function object.
#' @param \dots Arguments passed to .f.
#' @param Lapply_local Force local execution.
#' @param parallelize_wait Force to poll completion of computation if backend
#' returns asynchroneously
#' @param .call Unevaluated call to be parallelized
#' @return The value returned is the result of the computation of function .f
#' @section Important details: \itemize{
#' \item The package creates files in a
#' given folder. This folder is not temporary as it might be needed across R
#' sessions. md5-fingerprints of textual representations of function calls are
#' used to create subfolders therein per \code{parallelize} call. Conflicts can
#' be avoided by choosing different function names per \code{parallelize} call
#' for parallelizing the same function several times. For example: \code{f0 =
#' f1 = function(){42}; parallelize(f0); parallelize(f1);}
#' \item In view of
#' efficiency the parallize.dynamic package does not copy the whole workspace
#' to all parallel jobs. Instead, a hopefully minimal environment is set up for
#' each parallel job. This includes all parameters passed to the function in
#' question together with variables defined in the closure. This leaves all
#' functions undefined and it is therefore expected that all necessary function
#' defintions are given in separate R-files or libraries which have to be
#' specified in the \code{config} variable. These are then sourced/loaded into
#' the parallel job. A way to dynamically create source files from function
#' definitions is given in the Examples section.
#' }
#' @author Stefan Böhringer, \email{r-packages@@s-boehringer.org}
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' \code{\link{Apply}}, \code{\link{Sapply}}, \code{\link{Lapply}},
#' \code{\link{parallelize_initialize}}
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#'   # code to be parallelized
#'   parallel8 = function(e) log(1:e) %*% log(1:e);
#'   parallel2 = function(e) rep(e, e) %*% 1:e * 1:e;
#'   parallel1 = function(e) Lapply(rep(e, 15), parallel2);
#'   parallel0 = function() {
#'     r = sapply(Lapply(1:50, parallel1),
#'       function(e)sum(as.vector(unlist(e))));
#'     r0 = Lapply(1:49, parallel8);
#'     r
#'   }
#' 
#'   # create file that can be sourced containing function definitions
#'   # best practice is to define all needed functions in files that
#'   # can be sourced. The function tempcodefile allows to create a
#'   # temporary file with the defintion of given functions
#'   codeFile = tempcodefile(c(parallel0, parallel1, parallel2, parallel8));
#' 
#'   # definitions of clusters
#'   Parallelize_config = list(max_depth = 5, parallel_count = 24, offline = FALSE,
#'   backends = list(
#'     snow = list(localNodes = 2, splitN = 1, sourceFiles = codeFile),
#'     local = list(
#'       path = sprintf('%s/tmp/parallelize', tempdir())
#'     )
#'   ));
#' 
#'   # initialize
#'   parallelize_initialize(Parallelize_config, backend = 'local');
#' 
#'   # perform parallelization
#'   r0 = parallelize(parallel0);
#'   print(r0);
#' 
#'   # same
#'   r1 = parallelize_call(parallel0());
#'   print(r1);
#' 
#'   # compare with native execution
#'   parallelize_initialize(backend = 'off');
#'   r2 = parallelize(parallel0);
#'   print(r2);
#' 
#'   # put on SNOW cluster
#'   parallelize_initialize(Parallelize_config, backend = 'snow');
#'   r3 = parallelize(parallel0);
#'   print(r3);
#' 
#'   # analyse parallelization
#'   parallelize_initialize(Parallelize_config, backend = 'local');
#'   Log.setLevel(5);
#'   r4 = parallelize(parallel0);
#'   Log.setLevel(6);
#'   r5 = parallelize(parallel0);
#' 
#'   print(sprintf('All results are the same is %s', as.character(
#'     all(sapply(list(r0, r1, r2, r3, r4, r5), function(l)all(l == r0)))
#'   )));
#' 
parallelize = parallelize_backup = function(.f, ..., Lapply_local = rget('Lapply_local', default = FALSE),
	parallelize_wait = TRUE, envir__ = parent.frame()) {
	envir_evaled = envir__;
	call_ = list(fct = .f, args = list(...), envir = envir_evaled, name = as.character(sys.call()[[2]]));
	parallelize_internal(call_, Lapply_local = Lapply_local, parallelize_wait = parallelize_wait);
}


parallelize_call = parallelize_call_backup = function(.call, ...,
	parallelize_wait = TRUE, envir__ = parent.frame()) {
	envir_evaled = envir__;
	call_  = encapsulateCall(sys.call()[[2]], ..., envir__ = envir_evaled);
	parallelize_internal(call_, ..., parallelize_wait = parallelize_wait);
}

#parallelize_mention = function(...)NULL

#
#	<p> utility functions
#

#' @title Create a temporary file that contains the function definition of the
#' argument.
#' 
#' Create a temporary file that contains the function definition of the
#' argument so that this file can be sourced to re-instantiate the function.
#' 
#' Create a temporary file that contains the function definition of the
#' argument so that this file can be sourced to re-instantiate the function.
#' The temporary file is written to the temporary folder of the current R
#' session.
#' 
#' @param fcts function object the code of which is to be written to file
#' @return Returns the path to the file written.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @examples
#' 
#'   # code to be parallelized
#'   parallel8 = function(e) log(1:e) %*% log(1:e);
#'   parallel2 = function(e) rep(e, e) %*% 1:e * 1:e;
#'   parallel1 = function(e) Lapply(rep(e, 15), parallel2);
#'   parallel0 = function() {
#'     r = sapply(Lapply(1:50, parallel1),
#'       function(e)sum(as.vector(unlist(e))));
#'     r0 = Lapply(1:49, parallel8);
#'     r
#'   }
#' 
#'   codeFile = tempcodefile(c(parallel0, parallel1, parallel2, parallel8));
#'   cat(readFile(codeFile));
#' 
tempcodefile = function(fcts) {
	fctNames = as.character(as.list(sys.call()[[2]])[-1]);
	# create source file with code
	code = join(sapply(fctNames,
		function(name) {
			def = join(deparse(get(name)), sep = "\n");
			code = sprintf('%s = %s', name, def);
			code
		}), sep = "\n");
	codeFile = tempfile();
	# windows specific code
	codeFile = gsub('([\\])', '/', codeFile);
	writeFile(codeFile, code);
	codeFile
}
