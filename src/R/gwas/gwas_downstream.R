#
#	gwas_downstream.R
#
#Wed Nov 23 16:28:14 CET 2016


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
	createPipelineRaw(exportPath, optionsFile, run = run, templatePath = 'gwas/gwasImputationTemplate.pipe',
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
	#remotePath = with(o, Sprintf('%{remotePath}s-assoc-%{run}s'));
	remotePath = remotePathForOptionsFileAssoc(optionsFile, run);
	createPipelineRaw(exportPath, optionsFile, run = run, templatePath = 'gwas/gwasAssociationTemplate.pipe',
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
	destDir = Sprintf('%{exportPath}q%{postfix}q%{postfixPublish}q');
	descDocPath = Sprintf('%{destDir}s/description.txt');
	writeFile(descDocPath, descDoc, mkpath = T);
	path = publishFile(descDocPath, 'imputation-reports', 'description.txt');
	rCp = with(rd, System(Sprintf(con('scp "%{remote_path}s/*.pdf" ',
		'%{remote_path}q/*.jpeg %{remote_path}q/*.jpg ',
		'%{remote_path}q/*.xls %{remote_path}q/*.csv ',
		'%{destDir}q'))), 2);
	System(Sprintf('cp %{exportPath}q%{postfix}q*.csv %{destDir}q'), 2);
	System(Sprintf('chmod ug+rwX %{destDir}q'), 2)
	publishDir(destDir, asSubdir = T);
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

createPipelineRaw = function(exportPath = 'results/export/clean', optionsFile = optionsFile, run = 'R03',
	extraInterpolation = list(),
	files = Sprintf('%{exportPath}s%{file}s', file = c('.bim', '.bed', '.fam', '.pipe')),
	templatePath = 'gwas/gwasImputationTemplate.pipe', remotePath = NULL,
	doRunPipeline = FALSE, doTransferPipeline = TRUE) {
	writePipeFile(exportPath, optionsFile = optionsFile, run = run,
		extraInterpolation = extraInterpolation, templatePath = templatePath);

	if (doTransferPipeline)
		remotePath = transferPipeline(exportPath, remotePath = remotePath,
			optionsFile = optionsFile, run = run, files = files, doCopy = T);
	if (doRunPipeline) startPipeline(exportPath, optionsFile = optionsFile, run = run);
	remotePath
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
	Dir.create(prefix, treatPathAsFile = T, recursive = T);
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

exportIndividualsForModel = function(model, output = 'results/export/individuals-') {
	ids = model$data$id;
	tag = analysisTag(model$model);
	path = Sprintf('%{output}s%{tag}q-individuals-included.csv');
	Logs('Writing individuals for model %{tag}s.', logLevel = 4);
	write.csv(data.frame(id = ids), path);
	path
}

# models is output of expandedModelsData
exportIndividualsForModels = function(models, output = 'results/export/individuals-') {
	lapply(models, exportIndividualsForModel, output = output)
}

testTypes = list(
	glmBin = 'applyLogisticPerSnp',
	glmSurv = 'applyCoxPerSnp',
	glmLm = 'applyLmPerSnp',
	glmOrd = NULL
);

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

remotePathForOptionsFile = function(remotePath = NULL, optionsFile, run = 'R03') {
	if (is.null(remotePath)) remotePath = gwasInitialize(optionsFile, run = run)$options$remotePath;
	remotePath
}
remotePathForOptionsFileAssoc = function(optionsFile, run = 'R03') {
	o = gwasInitialize(optionsFile, run = run)$options;
	remotePath = remotePathForOptionsFile(o$remotePath, optionsFile, run);
	with(o, Sprintf('%{remotePath}s-assoc-%{run}s'))
}

# <A> names pipeline file as dir with extension .pipe
transferPipeline = function(prefix = 'results/data-cleaned-%D', optionsFile = optionsFile, run = 'R03',
	doCopy = T, doCopyPipeFile = T,
	files = Sprintf('%{prefix}s%{file}s', file = c('.bim', '.bed', '.fam', '.pipe')),
	remotePath = NULL) {
	sp = splitPath(prefix);
	#if (is.null(remotePath)) remotePath = gwasInitialize(optionsFile, run = run)$options$remotePath;
	# equivalent to the line above
	remotePath = remotePathForOptionsFile(remotePath, optionsFile, run);
	Dir.create(remotePath, recursive = T);
	print(files);
	if (doCopy) File.copy(files, remotePath);
	if (doCopyPipeFile) File.copy(Sprintf('%{prefix}s%{file}s', file = '.pipe'), remotePath);
	spR = splitPath(remotePath, ssh = T);
	cmd = Sprintf('pipeline.pl --print-pipeline %{base}s.pipe', base = sp$file);
	System(cmd, patterns = c('cwd', 'ssh'), 1, doLogOnly = T, ssh_host = spR$userhost, cwd = spR$path);
	remotePath
}

startPipeline = function(prefix = 'results/data-cleaned-%D', optionsFile = optionsFile, run = 'R03') {
	o = gwasInitialize(optionsFile, run = run)$options;
	sp = splitPath(prefix);
	spR = splitPath(o$remotePath, ssh = T);
	cmd = Sprintf('pipeline.pl %{base}s.pipe', base = sp$file);
	System(cmd, patterns = c('cwd', 'ssh'), 1, doLogOnly = F, ssh_host = spR$userhost, cwd = spR$path);
}

pipelineGetResultDir = function(prefix, optionsFile, run,
	pipeline_pattern = 'imputation_%{stage}02d', postfix = '', resultRe = '(\\d+)\\s+-gwasReport',
	remotePath = NULL, pipelineFile = NULL) {
	o = gwasInitialize(optionsFile, run = run)$options;
	sp = splitPath(prefix);
	spR = splitPath(firstDef(remotePath, o$remotePath), ssh = T);
	#cmd = Sprintf('pipeline.pl --print-pipeline %{base}s%{postfix}s.pipe', base = sp$file);
	if (is.null(pipelineFile)) pipelineFile = Sprintf('%{base}s%{postfix}s.pipe', base = sp$file)
	cmd = Sprintf('pipeline.pl --print-pipeline %{pipelineFile}s');
	r = System(cmd, patterns = c('cwd', 'ssh'), 1, return.output = T, doLogOnly = F,
		ssh_host = spR$userhost, cwd = Sprintf('%{base}s%{postfix}s', base = spR$path));
	stage = as.integer(fetchRegexpr(resultRe, r$output, captures = T));
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

postGWAStopSNPs = function(exportPath = 'results/export/clean', optionsFile = optionsFile, run = 'R03',
	pipeline_pattern = 'imputation_%{stage}02d',
	postfix = Sprintf('-assoc-%{run}s'),
	postfixPublish = '-publish', postfixTopList = '-topSnps-raw-100.csv') {

	rd = pipelineGetResultDir(splitPath(exportPath)$path, optionsFile, run, pipeline_pattern,
		postfix = postfix);
	pl = propertyFromString(readFile(Sprintf('%{dir}s/files.spec', dir = rd$remote_path), ssh = T));
	snps = lapply(pl$files, function(file) with(file, {
		top = readTable(Sprintf('%{dir}s/%{name}s%{postfixTopList}s',
			dir = rd$remote_path, name = splitPath(name)$file), ssh = T);
		list(table = top, desc = file)
	}));
}

#
#	<p> select SNPs from imputation pipeline
#

pipelineRemotePath = function(remotePath, postfix, run)
	with(o, Sprintf('%{remotePath}s-%{postfix}s-%{run}s'));
# use remote information + local export path which includes file prefix
pipelineRemotePrefix = function(remotePath, postfix, run, exportPath) {
	prefix = splitPath(exportPath)$file;
	pathWprefix = Sprintf('%{remote}s/%{prefix}s', remote = pipelineRemotePath(remotePath, postfix, run));
	pathWprefix
}

# <i> refactor postGWASassociationt to use createPipeline
createPipeline = function(
	exportPath = 'results/export/clean', optionsFile, run = 'R03',
	files = c(), templatePath, interpolationExtra = list(),
	postfix = 'selection') {

	o = gwasInitialize(optionsFile, run = run)$options;
	if (is.null(o$remotePath)) stop('No remote path specified in config file.');
	spR = splitPath(o$remotePath, ssh = T);

	# <p> create pipeline depending on previous imputation run
	exportPathOrig = exportPath;
	prefixOrig = splitPath(exportPath)$path;
	exportPath = Sprintf('%{exportPath}s-%{postfix}s-%{run}s');
	prefix = splitPath(exportPath)$path;

	interStd = list(IMPUTATION_PIPELINE = with(spR, Sprintf('%{path}s/imputation_06')));

	# <p> write and create pipeline file
	remotePath = pipelineRemotePath(remotePath, postfix, run);
	# returns remotePath
	createPipelineRaw(exportPath, optionsFile, run = run, templatePath = templatePath,
		files = files, extraInterpolation = c(interStd, interpolationExtra), remotePath = remotePath);
}

writeSNPcsv = function(snps, exportPath = 'results/export/clean', postfix = 'selection') {
	snpPath = Sprintf('%{exportPath}s-%{postfix}s-snps.csv');
	writeTable(as.data.frame(snps), path = Sprintf('[HEADER=F,QUOTE=F]:%{snpPath}s'));
	snpPath
}

postGWASsnpSelectionRaw = function(snps, exportPath = 'results/export/clean', optionsFile, run = 'R03',
	postfix = 'selection') {

	snpPath = writeSNPcsv(snps, exportPath, postfix);
	interpolation = list(SNP_FILE = splitPath(snpPath)$file);
	createPipeline(exportPath = exportPath, optionsFile = optionsFile,
		files = snpPath, interpolationExtra = interpolation,
		postfix = postfix, templatePath = 'gwas/gwasSnpSelectionTemplate.pipe');
}

# snps: output from postGWAStopSNPs: file description + table with snps
# snps can be modified to contain an element marker which is then used instead of snps[[i]]$table$marker
#	use to complemente with external snp names
postGWASsnpSelection = function(snps, exportPath = 'results/export/clean', optionsFile, run = 'R03',
	postfix = 'selection-top') {

	if (notE(exportPath)) {
		o = gwasReadOptionsFile(optionsFile, run = run);
		famFile = splitPath(exportPath)$file;
		# assume the imputation to live@ remotepath/file(exportPath) (see createPipeline), IMPUTATION_PIPELINE
		famPath = splitPath(famFilePathFromRemotePath(with(o, Sprintf('%{remotePath}s')), famFile))$path;
		dest = Sprintf('%{remote}s/%{famFile}s.fam',
			remote = pipelineRemotePath(o$remotePath, postfix, run))
		File.copy(famPath, dest, symbolicLinkIfLocal = F);
	}

	snpFiles = sapply(snps, function(snp) {
		postfix = Sprintf('selection-%{postf}s', postf = splitPath(snp$desc$name)$file);
		path = writeSNPcsv(firstDef(snp$marker, snp$table$marker), exportPath, postfix);
		path
	});

	snpFileNames = sapply(snpFiles, function(p)splitPath(p)$file);
	interpolation = list(SNP_FILE = join(snpFileNames, ','));
	createPipeline(exportPath = exportPath, optionsFile = optionsFile,
		files = snpFiles, interpolationExtra = interpolation,
		postfix = postfix, templatePath = 'gwas/gwasSnpSelectionTemplate.pipe');
}

# fam: data frame with ped (plink) file; otherwise a try to read from local file is made
postGWASsnpSelectionRead = function(exportPath = 'results/export/clean', optionsFile, run = 'R03',
	postfix = 'selection-top', imputationFileIdColumns = c('snpId', 'snp', 'posPhy', 'A1', 'A2'), fam = NULL,
	# overwriting the pipe-file as generated by previous steps for debuggin/worarounds <A><N>
	pipelineFile = NULL) {

	rd = pipelineGetResultDir(splitPath(exportPath)$path, optionsFile, run,
		postfix = Sprintf('-%{postfix}s-%{run}s'),
		resultRe = '(\\d+)\\s+-Copy:Union', pipelineFile = pipelineFile);
	# <!><%> tb removed
	#rd$remote_path = 'shark:/exports/molepi/users/sboehringer/eurotarget-201609-selection-top-R03/R_04/imputation_17';
	pl = propertyFromString(readFile(Sprintf('%{dir}s/files.spec', dir = rd$remote_path), ssh = T));
	# <!> pl contains separate entries for gt and info files
	# -> form pairs
	pl1 = apply(matrix(1:length(pl$files), byrow = T, ncol = 2), 1, function(idcs) {
		list(genotypes = pl$files[[idcs[1]]], info = pl$files[[idcs[2]]])
	});
	# <p> fam file
	if (is.null(fam))
		fam = readTable(Sprintf('[SEP=S,HEADER=F,NAMES=fid;iid;pid;fid;y]:%{exportPath}s.fam'),
			autodetect = F);
	# <p> read remote pathes
	dataSnps = lapply(pl1, function(file) {
		fileRemote = splitPath(file$genotypes$name)$file;

		# <p> genotypes
		path = Sprintf('[SEP=S,HEADER=F]:%{dir}s/%{file}s', dir = rd$remote_path, file = fileRemote);
		gtsRaw = readTable(path, ssh = T, autodetect = F);
		snps = Df_(gtsRaw[, 1:length(imputationFileIdColumns)], names = imputationFileIdColumns);
		Nsnps = length(snps$snp);

		# <p> reshape genotype matrix (snp x ind) -> (ind x snp(3col))
		gtsN = gtsRaw[, -(1:length(imputationFileIdColumns))];
		gtsStacked = matrix(as.vector(t(gtsN)), byrow = T, ncol = 3);
		# all genotypes 0 of all snps, then genotypes 1, genotypes 2
		gtsByCall = matrix(as.vector(gtsStacked), ncol = 3*Nsnps);
		gtsByGt = lapply(1:3, function(i)gtsByCall[, rangeBlock(i, Nsnps)]);
		gtsBySnps = matrix.intercalate(gtsByGt, listOfMatrices = T, direction = 2);
		dimnames(gtsBySnps) = list(as.character(fam$iid), pastem(snps$snp, 0:2, sep = ':'));

		# <p> dosage
		dosage = matrix(gtsStacked %*% 0:2, ncol = Nsnps);
		dimnames(dosage) = list(as.character(fam$iid), snps$snp);

		# <p> info file
		pathInfo = Sprintf('[SEP=S,HEADER=T]:%{dir}s/%{file}s',
			dir = rd$remote_path, file = splitPath(file$info$name)$file);
		dataInfo = readTable(pathInfo, ssh = T, autodetect = F);

		r = list(snps = snps, genotypes = gtsBySnps, dosage = dosage, fam = fam,
			file = fileRemote, info = dataInfo);
		r
	});
	dataSnps
}

postGWASwriteSnpSelectionData = function(dataSnps, exportPath, optionsFile, run = 'R03',
	postfix = '-fulldata-top-%{model}s', all.cov = FALSE) {
	o = gwasReadOptionsFile(optionsFile, run = run);
	d = gwasReadVariables(o, genotyped = TRUE);
	r = lapply(dataSnps, function(snp) {
		# <p> genotypes
		d0 = merge(d, Df(id = dimnames(snp$genotypes)[[1]], snp$genotypes), all.x = all.cov);
		path0 = Sprintf('%{exportPath}s%{pf}s-genotypes', pf = Sprintf(postfix, model = snp$file));
		#pathes0 = paste(path0, ifelse(ncol(d0) > 256, '.csv', c('.csv', '.xls'), sep = '');
		pathes0 = paste(path0, c('.csv', '.sav', '.csv2'), sep = '');
		writeTable(d0, pathes0);

		# <p> dosage
		d1 = merge(d, Df(id = dimnames(snp$dosage)[[1]], snp$dosage), all.x = all.cov);
		path1 = Sprintf('%{exportPath}s%{pf}s-dosage', pf = Sprintf(postfix, model = snp$file));
		#pathes1 = paste(path1, ifelse(ncol(d1) > 256, '.csv', c('.csv', '.xls'), sep = '');
		pathes1 = paste(path1, c('.csv', '.sav', '.csv2'), sep = '');
		writeTable(d1, pathes1);

		# <p> info
		pathI = Sprintf('%{exportPath}s%{pf}s-info', pf = Sprintf(postfix, model = snp$file));
		#pathes1 = paste(path1, ifelse(ncol(d1) > 256, '.csv', c('.csv', '.xls'), sep = '');
		pathesI = paste(pathI, c('.csv', '.sav', '.csv2'), sep = '');
		writeTable(snp$info, pathesI);
		r = list(genotypes = list(data = d0, path = pathes0), dosage = list(data = d1, path = pathes1),
			info = list(data = snp$info, path = pathesI))
		r
	});
	r
}

#
#	<p> other helpers
#

famFilePathFromRemotePath = function(remotePath, pipeFile = NULL) {
	# <p> get input prefix
	sp = splitPath(remotePath, ssh = T);
	pipeFile = if (notE(pipeFile)) splitPath(pipeFile, ssh = T)$file else sp$file;
	command = with(sp, Sprintf(con(
		"pipeline.pl --print-parameters %{pipeFile}s.pipe 2>&1 | ",
		"perl -ne 'print $1 if (m{^G:PipeInput\\s+(.*)}m)'"
	)));
	famPrefix = System(command, 1, patterns = c('cwd', 'ssh'),
		ssh_host = sp$userhost, cwd = sp$path, return.output = T)$output;
	# <p> path to fam-file
	famPath = Sprintf(con(
		'[HEADER=F,SEP=S,NAMES=fid;id;pid;mid;sex;affected]:',
		'%{remotePath}s/%{famPrefix}s.fam'));
	print(famPath);
	famPath
}

# read fam file from imputation dir
# assume that the pipe file has the same name as the directory <!>
readFamFileFromRemotePath = function(remotePath) {
	famPath = famFilePathFromRemotePath(remotePath);
	# <p> read file
	fam = readTable(famPath, ssh = T);
}


#
#	<p> export data [non-imputed]
#

# model: mds[[model]]$model
snpsTopHitsSingle = function(o, model, Ntop = 1, Novershoot = 30, Nregion = 3e4, sizeExtract = 5e4) {
	# <p> read associations
	assFile = exportFileName(o, analysisTag(model), 'snpAssociations.csv', mkdir = F);
	ass = readTable(assFile);

	# <p> extract top regions
	assTop = ass[order(ass$P)[1:(Ntop*Novershoot)], ];
	for (i in 1:Ntop) {
		if (nrow(assTop) <= i) break;
		phyDiff = abs(assTop$posPhy - assTop$posPhy[i]);
		sel = (assTop$chr == assTop$chr[i] & phyDiff > 0 & phyDiff < Nregion);
		assTop = assTop[!sel, , drop = F];
	}
	if (nrow(assTop) > Ntop) assTop = assTop[1:Ntop, , drop = F];

	# <p> ranges around top regions
	ranges = lapply(1:nrow(assTop), function(i) with(assTop[i, ],
		Df(region = i, plinkRangeAroundSnp(o, marker, sizeExtract))
	));
	ranges = unique(do.call(rbind, ranges)[, c('region', 'chr', 'id', 'mapPhy')]);

	# <p> exclusions
	e = readExclusions(o);
	rangesClean = ranges[!(ranges$id %in% e$markers), , drop = F];

	rangesClean
}

# model: mds[[model]]$model
snpDataTopHitsSingle = function(o, model, Ntop = 1, Novershoot = 30, Nregion = 3e4, sizeExtract = 5e4) {
	rangesClean = snpsTopHitsSingle(o, model, Ntop, Novershoot, Nregion, sizeExtract);
	# <p> read genotypes
	gts = readPlinkBed(o$input, rangesClean$id, usePedIid = TRUE);
	gtsClean = gts[!(row.names(gts) %in% readExclusions(o)$individuals), , drop = F];
	gtsClean
}

# Novershoot: maximal expected number of SNPs in region
# Nregion: def of physical size of region
# sizeExtract: physical distance how much to export
snpDataTopHits = function(optionsFile, models = NULL, run = 'R03', Ntop = 1, Novershoot = 30,
	Nregion = 3e4, sizeExtract = 5e4, snpNamesOnly = F) {
	# <p> initialization
	#snps = postGWAStopSNPs(imputationPrefix, optionsFile, run = 'R03');
	i = gwasInitialize(optionsFile, run = run);
	o = gwasReadOptionsFile(optionsFile, run = run);
	d = gwasReadVariables(o, genotyped = TRUE);
	mds = expandedModelsData(d, o);
	if (is.null(models)) models = 1:length(mds);

	extractor = if (snpNamesOnly) snpsTopHitsSingle else snpDataTopHitsSingle;
	r = lapply(models, function(i) {
		extractor(o, mds[[i]]$model, Ntop, Novershoot, Nregion, sizeExtract)
	});
	r
}
