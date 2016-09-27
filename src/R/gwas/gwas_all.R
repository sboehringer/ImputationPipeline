#
#	gwas_all.R
#Mon May  2 15:53:34 CEST 2011

source('gwas_qc.R');
source('gwas_analysis.R');

gwasParameterDefaults = list(
	# flags controlling execution of the pipeline
	readVariablesCache = F,
	readVariablesDeduplicateFam = FALSE,	#<!> not implemented
	readVariablesDeduplicateIds = FALSE,
	readVariablesEnforceUniqueIds = TRUE,
	qcParPathesExludedSNPs = NULL,
	qcPerformDistRun = T,	# run long running jobs
	# shell environment
	pipelineEnvironment = list(QSUB_QUEUE = 'all.q'),

	# parameters
	rep.keys = list(
		`G:Title` = 'GWAS, generic',
		`G:Analyst` = 'Stefan Boehringer',
		`G:Department` = 'unknown',
		`G:Email` = 'no@email'
	),
	readVariables.file = c(),
	readVariables.headermap = list(sex.gt = 'sex'),
	qsubOptions = '--queue all.q',
	qsubOptionsBigMem = '--queue all.q --memory 8G',

	qcParMissingLcutoff = 0.02,

	qcParIMissCutOff = 0.02,
	qcIgnoreSexCheck = F,

	qcParHWEzoom = 1e3,
	qcParHWEzoomQuantile = .05,
	qcParInbreedingCIlevel = .95,
	qcParTechnicalDuplicatesListCount = 30,
	# <p> integrative analysis
	qcParPruning = list(windowSize = 50, windowShift = 5, thresholdVIF = 2),
	qcParMdsUsePrunedSnps = T,
	qcParMdsRule = '',
	qcParMdsForce = '',
	qcParHClustTypes = c('ibs', 'ibd'),
	qcParMdsColorBy = 'mds_all',
	qcParMdsFigureCols = 2,
	qcParMdsNsplit = 6,
	qcParHClustColorBy = 'dist_all',

	qcParIBSruleRE = '',
	qcParIBSruleIBS01 = '',
	qcParPruning = list(windowSize = 50, windowShift =  5, thresholdVIF = 2),

	assParMaf = 0.01,
	assParCountMDScomponents = c(),
	assParFlipToRare = T,	# recode genotypes to calculate effect according to allele w/ MAF < 0.5
	assParTopN = 70,
	assParMafTest = 0.00,	# maf for complete data to be tested (0 for compatibility as default)
							# reasonably 0.01 or 0.05 depending on sample size
	assParGtfsTest = 0.005,	# if two genotypes have frequency less than assParGtfsTest,
							# exclude SNP; relevant when only heterozygotes are observed
							# should be filtered by earlier steps
	assManhattenCutoff = 5e-8,

	# as per default, take sex from ped file
	filesFamFormat = '[HEADER=F,SEP=S,NAMES=fid.gt;id;pid.gt;mid.gt;sex.gt;affected.gt,CONST=genotyped:1]',
	filesMergeBy = 'id',

	outputFileTemplate = '%{outputDir}s/reportGwas-%{runName}s.pdf',

	# <p> debugging parameters
	assAnalyzeNsnpsPerChunk = 0,	# if > 0, only analzyze that many SNPs per chunk (splitting step
	assPerfModels = NULL			# perform all models by default, otherwise model indeces are given
);

gwasParameterEraser = list(
	qcParMdsRule = '',
	qcParIBSruleRE = '',
	qcParIBSruleIBS01 = '',
	qcParIinbreedingRule = '',
	qcParPruning = list(),
	assParCountMDScomponents = c()
);

gwasReadVariablesStd = function(o, readVariables.headermap = list()) with(o, {
	if (readVariablesDeduplicateFam) {
		fam = readTable(sprintf('%s:%s.fam', filesFamFormat, o$input));
		stop("readVariablesDeduplicateFam option not implemented");
	} else {
		files = c(
			readVariables.file,
			sprintf('%s:%s.fam', filesFamFormat, o$input)
		);
	}
	d = NULL;
	for (file in files) {
		Logs('gwasReadVariablesStd: reading %{file}s', logLevel = 5);
		d0 = readTable(file, headerMap = readVariables.headermap);
		if (readVariablesDeduplicateIds) {
			d0 = d0[!duplicated(d0$id), , drop = FALSE];
		}
		Logs('gwasReadVariablesStd: dim %{nr}d:%{nc}d', nr = dim(d0)[1], nc = dim(d0)[2], logLevel = 5);
		Logs('gwasReadVariablesStd: names = %{names}s', join(names(d0), ', '), logLevel = 6);
		#d = if (is.null(d)) d0 else merge(d, d0, all.x = T, by = c('fid', 'id'));
		byVars = intersect(intersect(filesMergeBy, names(d)), names(d0));
		if (!is.null(d) && length(byVars) == 0) stop("Could not merge data sets");
		d = if (is.null(d)) d0 else merge(d, d0, all.x = T, all.y = T, by = byVars);
	}
	d = data.frame.types(d, factor = c('genotyped'), names = readVariables.headermap);
	d
})

gwasReadVariables = function(o, genotyped = TRUE) with(o, {
	#d = if (!is.null(o$readVariables)) {	#<!> does not work due to prefixing
	dataCache = Sprintf('%{output}s/data_cache.Rdata', output = files_this$outputDir);
	d = if (nif(o$readVariablesCache) && file.exists(dataCache) && !nif(o$readVariablesInvalidateCache)) {
		get(load(dataCache)[1])
	} else if (!is.null(o[['readVariables']])) {
		# this function should start by calling gwasReadVariablesStd if no special customization is needed
		get(readVariables)(o)
	} else {
		gwasReadVariablesStd(o)
	}
	if (nif(o$readVariablesCache)) save(d, file = dataCache);
# 	if (!is.null(o$readVariables.file)) {
# 		d = readTable(readVariables.file)
# 		if (!is.null(o$readVariables.headermap)) {
# 			names(d) = vector.replace(names(d), o$readVariables.headermap);
# 		}
# 		d
# 	}
	if (genotyped) d = d[!is.na(d$genotyped), ];
	d
})

gwasFileParameters = function(o) with (o, {
	inputFileBase = splitPath(input)$file;
	# <p> derive file parameters
	fp = list(
		runIndex = runIndex,
		outputPrefix = outputDir,
		outputPrefixGlobal = Sprintf("%{outputDir}s/%{inputFileBase}"),
		outputBase = outputDir,
		outputDir = sprintf('%s/%s', outputDir, runName)
	);
	fp = merge.lists(fp, list(
		outputDirQc = sprintf('%s/qc', fp$outputDir),
		outputDirAnalysis = sprintf('%s/analysis', fp$outputDir),
		outputDirExport = sprintf('%s/export', fp$outputDir)
	));
	fp = merge.lists(fp, list(
		outputPrefixQc = sprintf('%s/%s', fp$outputDirQc, inputFileBase),
		outputPrefixAnalysis = sprintf('%s/%s', fp$outputDirAnalysis, inputFileBase),
		outputPrefixExport = sprintf('%s/%s', fp$outputDirExport, inputFileBase)
	));
	fp
})

gwasReadOptionsFile = function(optionsFile, run = NULL) {
	os = eval(parse(text = sprintf('list(%s)', readFile(optionsFile))));
	
	# <p> merge options
	# <i> dependency graphs
	ns = names(os);
	#if (is.null(run)) run = rev(ns)[1];
	if (is.null(run)) run = ns[1];
	runI = which(ns == run);
	# <p> coalesce configurations <!> dependency tree not honored
	# parameters in gwasParameterEraser are not inherited
	o = if (runI == 1) {
		Merge.lists(c(list(default = gwasParameterDefaults), os[1]),
			listOfLists = T, recursive = T, keyPathes = 'rep.keys')
	} else {
		Merge.lists(c(
			list(default = gwasParameterDefaults),
			os[1:(runI - 1)],
			list(eraser = gwasParameterEraser),
			os[runI]
		), listOfLists = T, recursive = T, keyPathes = 'rep.keys');
	}
	# <p> set derived parameters
	o = merge.lists(o, list(
		runName = run,
		runIndex = runI,
		optionsFile = optionsFile
	));
	# <p> files
	files = gwasFileParameters(o);
	o = merge.lists(o, files, list(files_this = files));
	# <p> backreferences to previous run
	o = merge.lists(o, (if (runI > 1) {
		if (is.null(o$previousRun)) o$previousRun = ns[runI - 1];
		previousOutputDir = sprintf('%s/%s', o$outputPrefix, o$previousRun);
		oPre = propertyFromString(readFile(sprintf('%s/options', previousOutputDir)));
		list(previousOutputDir = previousOutputDir, files_pre = oPre$files_this);
	} else list(files_pre = files)));
	o
}

gwasReadVariableFromOptionsFile = function(optionsFile, run = NULL, genotyped = TRUE) {
	gwasReadVariables(gwasReadOptionsFile(optionsFile, run), genotyped = genotyped)
}

# find most recent run based on key
gwasMostRecentRunKey = function(o, key,
	rule = function(key, o)((!is.null(o[[key]]) && o[[key]] != '') || as.integer(o$runIndex) == 1),
	skip_this = F) {
	skip = skip_this;
	repeat {
		condition = rule(key, o);
		if ((condition && !skip) || as.integer(o$runIndex) == 1) break;
		o = propertyFromString(readFile(sprintf('%s/options', o$files_pre$outputDir)));
		skip = F;
	}
	r = if (condition) o else NULL;
	r
}

gwasMostRecentRun = function(o, keys,
	rule = function(key, o)((!is.null(o[[key]]) && o[[key]] != '') || as.integer(o$runIndex) == 1),
	skip_this = F) {
	r = lapply(keys, function(key)gwasMostRecentRunKey(o, key, rule, skip_this));
	i = which.max(as.integer(list.kp(r, 'runIndex', template = -1, do.unlist = T)));
	r0 = if (length(i) > 0) r[[i]] else NULL;
	r0
}
gwasMostRecentRunOptions = function(o, keys,
	rule = function(key, o)((!is.null(o[[key]]) && o[[key]] != '') || as.integer(o$runIndex) == 1)) {
	oMr = gwasMostRecentRun(o, keys, rule);
	o = merge.lists(o, list(files_pre = oMr$files_this));
	o
}

# create caches, temporary databases
# <i> this should go into a ReferenceClasses object
gwasInitializeGlobalData = function(o) with(o, {
	# <p> bim database
	bimDb = Sprintf('%{outputPrefixGlobal}s_bim.sqlite');
	bimPath = Sprintf('%{input}s.bim');
	if (!file.exists(bimDb) || file.info(bimDb)$mtime < file.info(bimPath)$mtime) {
		csv2sqlite(bimPath, bimDb, columnsNames = qquote('chr id mapGen mapPhy a1 a2'),
			index = 'id', inputSep = 'T', inputHeader = F,
			types = list(chr = 'integer', pos_gen = 'real', pos_phy = 'integer'));
	}
	o
})

gwasEstablishEnvironment = function(o) {
	sapply(1:length(o$pipelineEnvironment), function(i)do.call(Sys.setenv, o$pipelineEnvironment[i]));
}

gwasInitialize = function(optionsFile, run = NULL, doReset = F) {
	# <p> load options file
	o = gwasReadOptionsFile(optionsFile, run);
	gwasEstablishEnvironment(o);

	# <p> sowReap
	SowReapInit(
		ensembleClass = 'SowCatcherEnsemblePersistent',
		path = Sprintf('%{o}s/sowreap', o = o$outputDir)
	);
	SowReapCreateField(c('excl_ind', 'excl_marker'),
		sowCatcherClass = 'SowCatcherPersistent', doReset = doReset
	);

	# <p> variables
	runI = o$runIndex;
	outputPrev = Sprintf('%{output}s/sowreap', output = o$previousOutputDir);

	# <p> prepare directory, document parameters
	if (!file.exists(o$outputDir)) dir.create(o$outputDir, recursive = T);
	writeFile(sprintf('%s/options', o$outputDir), stringFromProperty(o));
	Dir.create(o$outputDirExport);

	# <p> exclusions
	d = gwasReadVariables(o);
	exclInds = c();
	if (runI == 1) {
		if (!is.null(o$qcParIexlusionCol)) {
			exclInds = union(exclInds, d$id[d[[o$qcParIexlusionCol]]]);
		}
		if (!is.null(o$qcParIexclusionList)) {
			exclInds = union(exclInds, o$qcParIexclusionList);
		}
		column = firstDef(o$qcParIexclusionFilesColumn, 1);
		fileExclusions = if (length(o$qcParIexclusionFiles) > 0)
			avu(sapply(o$qcParIexclusionFiles, function(file) {
			is = readTable(file)[, column];
			Log(sprintf('Exclusion [%s] : %d', file, length(is)), 3);
			is
		})) else c();
		exclInds = union(exclInds, fileExclusions);
		exclInds = exclInds[!is.na(exclInds)];
	} else {
		exclInds = ReapFromDisk(outputPrev, sow_field = 'excl_ind', fields = 'all')$all;
	}
	Log(sprintf('Externally excluded indivduals: %d', length(exclInds)), 3);
	exclMarkers = if (runI == 1) {
		if (!is.null(o$qcParPathesExludedSNPs)) {
			ms = readTable(o$qcParPathesExludedSNPs)[, 1];
			ms
		} else c();
	} else {
		ReapFromDisk(outputPrev, sow_field = 'excl_marker', fields = 'all')$all;
	}
	Sow(external = exclInds, sow_field = 'excl_ind');
	Sow(external = exclMarkers, sow_field = 'excl_marker');
	Sow(not_genotyped = d$id[is.na(d$genotyped)], sow_field = 'excl_ind');
	#
	# <p> from here on objects may be added to o <A>
	#
	gwasInitializeGlobalData(o);

	r = list(options = o);
	r
}

gwasCleanUp = function(o) {
	if (exists('globalBimDb', envir = .GlobalEnv)) rm('globalBimDb', envir = .GlobalEnv);
}

gwasConclude = function(o) {
	#Log.setLevel(4);
	gwasCopyResults(o);
	# <p> sowReap
	SowReapConclude();
	# <p>
	gwasCleanUp(o);
}

# <!> Sow called in this function <%>
gwasInitializeReporting = function(o) {
	# <p> admin reporting
	REP.tex('G:DESCRIPTION', firstDef(o$studyDescription, ''));
	REP.tex('G:ROUNDNAME', firstDef(o$runName, 'unnamed'));

	# <p> auto report keys
	nlapply (o$rep.keys, function(n)REP.tex(n, o$rep.keys[[n]]));
	d0 = gwasReadVariables(o, genotyped = F);

	# <p> base data characteristics
	REP.tex('G:Count_inidividuals', nrow(d0));
	d1 = gwasReadVariables(o, genotyped = T);
	REP.tex('G:Count_genotyped', nrow(d1));
	#REP.tex

	# <p> formatted parameters
	REP.tex('ASS:assParMaf_perc', o$assParMafTest, fmt = 'percent');
	#%{parAssMaf, f, .1}
	NULL
}

#
#	<p> handling exclusions
#

excl_ind_types = list(
	all =		c('external', 'not_genotyped', 'missing', 'sex', 'inbreeding', 'technical_dupl',
		'mds', 'ibs_IBS01', 'ibs_IBS02', 'ibs_IBS12', 'ibs_RE'),
	pruned =	c('external', 'not_genotyped', 'missing', 'sex', 'inbreeding', 'technical_dupl',
		'mds', 'ibs_IBS01', 'ibs_IBS02', 'ibs_IBS12', 'ibs_RE'),
	marginal =	c('external', 'not_genotyped', 'missing', 'sex', 'inbreeding', 'technical_dupl'),
	baseline =	c('external', 'not_genotyped'),	# subtract before comparing missingness
	external =	c('external')
);
excl_marker_types = list(
	all =		c('external', 'missing', 'hwe', 'maf'),
	pruned =	c('external', 'missing', 'hwe', 'maf', 'pruning'),
	marginal =	c('external', 'missing', 'hwe', 'maf'),
	baseline =	c('external'),	# subtract before comparing missingness
	external =	c('external')
);
readExclusionsRaw = function(o, sow_field, type = 'all', do_union = T) {
	typeMap = get(Sprintf('%{sow_field}s_types'));
	fields = typeMap[[type]];
	#if (is.null(fields)) fields = type;
	r = Reap(sow_field = sow_field, fields = fields, vivify = T);
	if (do_union) r = Union(unlist(r));
	r
}

readExclusionsInds = function(o, type = 'all', do.union = T) {
	readExclusionsRaw(o, 'excl_ind', type, do.union)
}
readExclusionsMarkers = function(o, type = 'all', do.union = T) {
	readExclusionsRaw(o, 'excl_marker', type, do.union)
}

readExclusions = function(o, type = 'all', do_union = T) {
	r = list(
		individuals = readExclusionsInds(o, type, do.union = do_union),
		markers = readExclusionsMarkers(o, type, do.union = do_union)
	);
	r
}

#
#	copy results to tagged file names in order to avoid confusions on re-runs
#
gwasCopyResults = function(o) with(o, {
	tag = format(Sys.time(), "%Y%m%d-%H:%M");
	file.copy(
		sprintf('%s/reportGwas-%s.pdf', outputDir, runName),
		sprintf('%s/reportGwas-%s-%s.pdf', outputDir, runName, tag)
	);
	file.copy(
		sprintf('%s/options', outputDir, runName),
		sprintf('%s/options-%s-%s', outputDir, runName, tag)
	);
})

gwasCheckInput = function(o, d) {
	if (nrow(d) <= 1) {
		Log('Data set too small.', 1);
		stop('Data set too small.');
	}
	ns = names(d);
	nsMiss = setdiff(c('id', 'sex', o$removeDuplicatesColumn), ns);
	if (length(nsMiss) > 0) {
		Logs('Essential column(s) "%{cols}s" missing from data', cols = join(nsMiss, ', '), level = 1);
		stop('essential column missing from data');
	}
	if (o$readVariablesEnforceUniqueIds && sum(duplicated(d$id)) > 0) {
		Logs('Id column not unique: "%{ids}s" duplicated.',
			ids = join(d$id[duplicated(d$id)], ', '), level = 1);
		stop('Id column not unique');
	}

	sexF = as.factor(d$sex);
	if (!all(sort(levels(sexF)) == 1:2)) {
		Logs('Sex must be coded as 1:male, 2:female', level = 1);
		stop('Sex column wrongly coded');
	}
	# check variables of association model
	varsResponses = list.kp(o$assParModels, 'responses', do.unlist = TRUE);
	varsU = setdiff(unique(c(
		unlist(lapply(list.kp(o$assParModels, 'f1'), function(e)all.vars(as.formula(e)))),
		varsResponses
	)), c('MARKER', 'RESPONSE'));
	Logs('Variables used in analysis: %{vars}s', vars = join(varsU, ', '), level = 4);
	if (!all(all(varsU %in% names(d)))) {
		varMiss = join(varsU[!(varsU %in% names(d))], ', ');
		Logs('Variables used in formulas not present in data: %{varMiss}s', level = 1);
		stop('Variables missing from data');
	}
	Ncomplete = sum(!apply(d[, setdiff(varsU, varsResponses), drop = F], 1, function(r)any(is.na(r))));
	Logs('Number of complete cases: %{Ncomplete}s', level = 4);
	if (Ncomplete == 0) {
		stop('No non-missing data (except genotypes, response)');
	}

	# <p> check data types
	classes = lapply(d, class);
	if (any(classes %in% c('list', 'matrix'))) {
		print(classes);
		stop('Forbidden column classes found in covariate data.');
	}
	NULL
}

gwasRun = function(optionsFile, run = NULL, opts = NULL, resetCache = F) {
	gc();
	Log('GWAS analysis: start', 3);

	# <p> initialize options and files
	r = gwasInitialize(optionsFile, run, doReset = resetCache);
	o = r$options;

	# <p> prepare reporting
	# set variable global to the ensuing analysis
	.globalOutput = list(prefix = con(o$outputDir, '/'));
	REP.new(
		'gwas/reportGwas.tex',
		cache = sprintf('%s/reportGWAS_cache', o$outputDir),
		setup = 'setup-report-lua.tex', latex = 'lualatex',
		resetCache = resetCache
	);
	gwasInitializeReporting(o);

	input = o$input;
	outputDir = o$outputDir;
	# opts = list(qcIndividuals = F)
	if (!is.null(opts)) o = merge.lists(o, opts);
	d = gwasReadVariables(o);
	gwasCheckInput(o, d);

	# <p> generated options
	o = merge.lists(o, list(exclusions = r$exclusions));
	# <p> QC
	# <!> $ accepts infixes
	if (nit(o[['qcPerform']]))
		qcAll(input, outputDir, d, o);
	gc();

	# <p> run association
	if (nit(o$assPerform)) {
		if (nit(o$assPerformDataSplit)) {
			plinkSplitData(input, sprintf('%s/splits', outputDir), Nchunks = o$assParNchunks);
		}
		if (nit(o$assPerformAnalysis)) {
			associationAnalysis(input, outputDir, d, o);
		}
		if (nit(o$assSummary)) {
			associationAnalysisSummary(input, outputDir, d, o);
		}
	}
	gc();

	# <p> logging
	sink(sprintf('%s/warnings.txt', outputDir));
		warnings();
	sink();
	# <p> create report
	#Log.setLevel(5);
	#REP.finalize(verbose = T, output = sprintf('%s/reportGwas-%s.pdf', outputDir, o$runName), cycles = 3);
	#REP.finalize(verbose = T, output = with(o, Sprintf(outputFileTemplate)), cycles = 3);
	REP.finalize(verbose = FALSE, output = with(o, Sprintf(outputFileTemplate)), cycles = 3);
	gwasConclude(o);
	Log('GWAS analysis: finished', 3);
}

gwasPublish = function(optionsFile, runs = c('R01', 'R02', 'R03')) {
	sapply(runs, function(runName) with(gwasInitialize(optionsFile, run = runName)$options, {
		publishFile(Sprintf(outputFileTemplate), into = Sprintf('%{runName}s'));
	}))
}

#
#	<p> post-GWAS
#

#	exportPath = 'results/export/destination';
# 	exportCleanedData(exportPath, optionsFile = optionsFile, run = 'R03');
# 	writePipeFile(exportPath, optionsFile = optionsFile, run = 'R03',
# 		referencePanel = c('gonl4', '1000genomesv3'));
# 	transferPipeline(exportPath, optionsFile = optionsFile, run = 'R03', doCopy = T);
# 	startPipeline(exportPath, optionsFile = optionsFile, run = 'R03');

# referencePanel %in% c('gonl4', '1000genomesv3')
postGWASimputation = function(exportPath = 'results/export/clean', optionsFile = optionsFile, run = 'R03',
	referencePanel = '1000genomesv3') {
	exportCleanedData(exportPath, optionsFile = optionsFile, run = run);
	extraInterpolation = list(REFPANEL = referencePanel);
	createPipeline(exportPath, optionsFile, run = run, templatePath = 'gwas/gwasImputationTemplate.pipe',
		extraInterpolation = extraInterpolation);
}

postGWASassociation = function(exportPath = 'results/export/clean', optionsFile = optionsFile, run = 'R03') {
	o = gwasInitialize(optionsFile, run = run)$options;
	if (is.null(o$remotePath)) stop('No remote path specified in config file.');
	spR = splitPath(o$remotePath, ssh = T);

	# <p> create pipeline depending on previous imputation run
	exportPathOrig = exportPath;
	prefixOrig = splitPath(exportPath)$path;
	exportPath = Sprintf('%{exportPath}s-assoc-%{run}s');
	prefix = splitPath(exportPath)$path;
	if (T) exportCleanedData(exportPath, optionsFile = optionsFile, run = run);

	# <p> files
	files = Sprintf('%{prefix}s%{file}s', file = c('.fam', '.pipe', '-variables.csv'));
	filePed = splitPath(files[1])$file;

	# <p> models
	expandedModels = expandModels(o$assParModels, input = NULL, o, d = NULL, noMDS = TRUE)$models;
	models = pipelineModels(expandedModels);
	# <!> hard-coded pipeline stage
	input = list(EXTERNAL_PED_FILE = filePed, IMPUTATION_PIPELINE =
		with(spR, Sprintf('%{path}s/imputation_06')));

	# <p> write and create pipeline file
	remotePath = with(o, Sprintf('%{remotePath}s-assoc-%{run}s'));
	createPipeline(exportPath, optionsFile, run = run, templatePath = 'gwas/gwasAssociationTemplate.pipe',
		files = files, extraInterpolation = c(models, input), remotePath = remotePath);
}

postGWASpublish = function(exportPath = 'results/export/clean', optionsFile = optionsFile, run = 'R03',
	pipeline_pattern = 'imputation_%{stage}02d',
	postfix = Sprintf('-assoc-%{run}s'), postfixPublish = '-publish') {
	rd = pipelineGetResultDir(splitPath(exportPath)$path, optionsFile, run, pipeline_pattern,
		postfix = postfix);
	#print(remoteDir);
	#pipelineGetReports = function(prefix = 'results/data-cleaned-%D', optionsFile = optionsFile, run = 'R03',
	#pipeline_pattern = 'imputation_%{stage}02d') {

	# destination sub-folder
	destDir = Sprintf('%{exportPath}s%{postfixPublish}s');
	descDocPath = Sprintf('%{destDir}s/description.txt');
	writeFile(descDocPath, descDoc, mkpath = T);
	path = publishFile(descDocPath, 'imputation-reports', 'description.txt');
	rPdf = with(rd, System(Sprintf('scp "%{remote_path}s/*.pdf" %{destDir}q')), 2);
	rJpeg = with(rd, System(Sprintf('scp %{remote_path}q/*.jpeg %{remote_path}q/*.jpg %{destDir}q')), 2);
	System(Sprintf('chmod ug+rwX %{destDir}q'), 2)

}

postGWASdoAll = function(exportPath = 'results/export/clean', optionsFile = optionsFile, run = 'R03',
	referencePanel = '1000genomesv3', doFetchResults = FALSE) {
	exportCleanedData(exportPath, optionsFile = optionsFile, run = run);
	writePipeFile(exportPath, optionsFile = optionsFile, run = run, referencePanel = referencePanel);
	transferPipeline(exportPath, optionsFile = optionsFile, run = run, doCopy = T);
	startPipeline(exportPath, optionsFile = optionsFile, run = run);

	if (doFetchResults) imputationFetchResult(exportPath, optionsFile, run);
}

# <p> helper functions

createPipeline = function(exportPath = 'results/export/clean', optionsFile = optionsFile, run = 'R03',
	extraInterpolation = list(),
	files = Sprintf('%{prefix}s%{file}s', file = c('.bim', '.bed', '.fam', '.pipe')),
	templatePath = 'gwas/gwasImputationTemplate.pipe', remotePath = NULL,
	doRunPipeline = FALSE, doTransferPipeline = TRUE) {
	writePipeFile(exportPath, optionsFile = optionsFile, run = run,
		extraInterpolation = extraInterpolation, templatePath = templatePath);

	if (doTransferPipeline)
		transferPipeline(exportPath, remotePath = remotePath,
			optionsFile = optionsFile, run = run, files = files, doCopy = T);
	if (doRunPipeline) startPipeline(exportPath, optionsFile = optionsFile, run = run);
}
imputationFetchResult = function(exportPath, optionsFile, run = 'R03') {
	# assume initPublishing having been called
	#initPublishing('gwasHuidtoxiciteit201103', '201407');
	pipelineGetReports(exportPath, optionsFile = optionsFile, run = run);
	pipelineResultFiles(exportPath, optionsFile = optionsFile, run = run);
}


exportCleanedData = function(prefix = 'results/data-cleaned-%D', optionsFile = optionsFile, run = 'R03',
	noMDS = FALSE) {
	# <p> initialize
	Dir.create(prefix, treatPathAsFile = T);
	o = gwasInitialize(optionsFile, run = run)$options;

	# <p> plink file
	e = readExclusions(o, type = 'all');
	plinkExportPrunedFile(o$input, prefix, list(exclusions = e));

	# <p> data
	d = gwasReadVariableFromOptionsFile(optionsFile, run = run);
	if (!noMDS) {
		mds = readMDS(o$input, o);
		d = merge(d, mds, all.x = T);
	}

	# <p> write output
	write.csv(data.frame(id = e$individuals), Sprintf('%{prefix}s-individuals-excluded.csv'));
	included = setdiff(d$id, e$individuals);
	d0 = d[which.indeces(included, d$id), , drop = F];
	write.csv(d0[, 'id', drop = F], Sprintf('%{prefix}s-individuals-included.csv'));
	write.csv(d0, Sprintf('%{prefix}s-variables.csv'));

	# <p> logging
	Logs('Individuals excluded: %{Ne}d, included: %{Ni}d, Rows in data: %{Nr}d',
		Ne = length(e$individuals), Ni = length(included), nrow(d0),
		logLevel = 1
	);
}

testTypes = list(glmBin = 'applyLogisticPerSnp', glmSurv = 'applyCoxPerSnp', glmOrd = NULL);

pipelineModels = function(models) {
	r = ilapply(models, function(m, i) {
		if (is.null(testTypes[[m$stat]]))
			stop(Sprintf('Pipeline function for test statistic: %{stat}s undefined', stat = m$stat));
		r = list(name = Sprintf('%{testType}s:model%{i}d', testType = testTypes[[m$stat]]),
			# null2na (Nina) due to listKeyValue
			formulas = listKeyValue(
				c(	Sprintf('model%{i}d:formula1'), Sprintf('model%{i}d:formula0'),
					Sprintf('model%{i}d:select')),
				c(	mergeDictToString(list(MARKER = 'MARKER_dosage'), m$f1),
					mergeDictToString(list(MARKER = 'MARKER_dosage'), m$f0),
					ifelse(is.null(m$subset), NA, Deparse(m$subset)))
				)
		);
		List_(r, rm.na = T)
	});
	#r = r[sapply(r, length) > 0];
	formulas = join(c(sapply(r, function(e) {
		ns = names(e$formulas);
		join(nelapply(e$formulas, function(n, v)Sprintf('%{n}s\t%{v}s')), sep = '\n')
	}), ''), sep = "\n\n");

	pipeline = join(sapply(list.kp(r, 'name', do.unlist = T), function(p)
		Sprintf('%{p}s | GWASsummarize')), ', ');
	r0 = list(TESTS_PIPELINE = pipeline, TESTS_FORMULAS = formulas);
	r0
}

# 	# assume data to be cleaned, MDS variables will be fed through data export
# 	expandedModels = expandModels(o$assParModels, input = NULL, o, d = NULL, noMDS = TRUE)$models;
# 	models = pipelineModels(expandedModels);
writePipeFile = function(prefix = 'results/data-cleaned-%D', optionsFile = optionsFile, run = 'R03',
	referencePanel = 'hapmap2b22', headerMap = 'id:iid', templatePath = 'gwas/gwasImputationTemplate.pipe',
	extraInterpolation = list()) {
	# <p> initialize
	Dir.create(prefix, treatPathAsFile = T);
	o = gwasInitialize(optionsFile, run = run)$options;
	sp = splitPath(prefix);

	# <p> template interpolation
	template = readFile(templatePath);
	substDict = c(list(PREFIX = sp$file, `__HEADERMAP__` = headerMap),
		extraInterpolation
	);
	pipe = mergeDictToString(substDict, template);
	cat(pipe);
	writeFile(Sprintf('%{prefix}s.pipe'), pipe);
	pipe
}

transferPipeline = function(prefix = 'results/data-cleaned-%D', optionsFile = optionsFile, run = 'R03',
	doCopy = T, doCopyPipeFile = F,
	files = Sprintf('%{prefix}s%{file}s', file = c('.bim', '.bed', '.fam', '.pipe')),
	remotePath = NULL) {
	sp = splitPath(prefix);
	if (is.null(remotePath)) remotePath = gwasInitialize(optionsFile, run = run)$options$remotePath;
	Dir.create(remotePath, recursive = T);
	print(files);
	if (doCopy) File.copy(files, remotePath);
	if (doCopyPipeFile) File.copy(Sprintf('%{prefix}s%{file}s', file = '.pipe'), remotePath);
	spR = splitPath(remotePath, ssh = T);
	cmd = Sprintf('pipeline.pl --print-pipeline %{base}s.pipe', base = sp$file);
	System(cmd, patterns = c('cwd', 'ssh'), 1, doLogOnly = T, ssh_host = spR$userhost, cwd = spR$path);
}

startPipeline = function(prefix = 'results/data-cleaned-%D', optionsFile = optionsFile, run = 'R03') {
	o = gwasInitialize(optionsFile, run = run)$options;
	sp = splitPath(prefix);
	spR = splitPath(o$remotePath, ssh = T);
	cmd = Sprintf('pipeline.pl %{base}s.pipe', base = sp$file);
	System(cmd, patterns = c('cwd', 'ssh'), 1, doLogOnly = F, ssh_host = spR$userhost, cwd = spR$path);
}

pipelineGetResultDir = function(prefix, optionsFile, run, pipeline_pattern, postfix = '') {
	o = gwasInitialize(optionsFile, run = run)$options;
	sp = splitPath(prefix);
	spR = splitPath(o$remotePath, ssh = T);
	cmd = Sprintf('pipeline.pl --print-pipeline %{base}s%{postfix}s.pipe', base = sp$file);
	r = System(cmd, patterns = c('cwd', 'ssh'), 1, return.output = T, doLogOnly = F,
		ssh_host = spR$userhost, cwd = Sprintf('%{base}s%{postfix}s', base = spR$path));
	stage = as.integer(fetchRegexpr('(\\d+)\\s+-gwasReport', r$output, captures = T));
	pipeline_dir = Sprintf(pipeline_pattern);
	r = list(pipeline_dir = pipeline_dir, stage = stage,
		remote_path = Sprintf('%{remote}s%{postfix}s/%{pipeline_dir}s', remote = o$remotePath));
	r
}

descDoc = 'This folder contains reports copied from a compute server. Naming of files is related to the workflow process on the server.';

pipelineGetReports = function(prefix = 'results/data-cleaned-%D', optionsFile = optionsFile, run = 'R03',
	pipeline_pattern = 'imputation_%{stage}02d') {

	rd = pipelineGetResultDir(prefix, optionsFile, run, pipeline_pattern);
	pipeline_dir = rd$pipeline_dir;

	# destination sub-folder
	descDocPath = tempfile();
	writeFile(descDocPath, descDoc);
	path = publishFile(descDocPath, 'imputation-reports', 'description.txt');
	outputDir = splitPath(path)$dir;
	r = System(Sprintf('umask 766 ; scp "%{remote}s/%{pipeline_dir}s/*.pdf" "%{outputDir}s"', remote = rd$o$remotePath), 2);

}

descDocFiles = 'This folder contains a verbatim copy of files from the compute server which are summarized in the pdf-files. Naming of files is related to the workflow process on the server and might seem arbitrary. Tables and pictures are included in the pdf and are stored here to facilitate extraction for publication.';

pipelineResultFiles = function(prefix = 'results/data-cleaned-%D', optionsFile = optionsFile, run = 'R03',
	pipeline_pattern = 'imputation_%{stage}02d') {

	rd = pipelineGetResultDir(prefix, optionsFile, run, pipeline_pattern);
	pipeline_dir = rd$pipeline_dir;

	# destination sub-folder
	descDocPath = tempfile();
	writeFile(descDocPath, descDocFiles);
	path = publishFile(descDocPath, 'imputation-files', 'description.txt');
	outputDir = splitPath(path)$dir;
	r = System(Sprintf('umask -S u=rwx,g=r,o=r ; scp "%{remote}s/%{pipeline_dir}s/*" "%{outputDir}s"',
		remote = rd$o$remotePath), 2);
	r
}


