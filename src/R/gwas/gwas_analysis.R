#
#	gwas_analysis.R
#Tue Apr 19 12:03:15 2011

library('gap');
library('ggplot2');
library('snpStats');
library('survival');
library('MASS');
library('lme4');
source('Rgenetics.R');
source('gwas_manhattan.R');
source('gwas_models.R');

#
#	<p> analysis with estimated IBD
#


# assume one-parameter genetic model
# assume intercept in the model
lhSpecificationFromModelFrameNull = function(X) {
	df0 = dim(X)[2];

	spec_glmBinIBD = list(
		ll = "glmIbd_optim", lhInterface = 'inline',
		alt = list(
			start = c(-1, rep(0, df0 - 1), 1, 0),	# also specifies number of parameters
			pars = list(
				list(name = "beta0", entries = 1:df0, type = 'real'),
				list(name = "sigma", entries = df0 + 1, type = 'positive'),
				list(name = "betaA", entries = df0 + 2, type = 'real')
			)
		),
		null = list(
			parsFree = 2	# alternative: list free parameters or specify tail from alt
		)
	);
	spec_glmBinIBD
}

glmBinIBDprepare = function(data, f1, f0, o) {
	# <p> read ibd matrix, entries according to fam-file
	oMds = gwasMostRecentRun(o, 'qcParMdsRule');
	# <!><%> '_1' hack for current analysis 1/2013
	matrixPath = sprintf('%s/%s.genome.ibd_1', oMds$outputDirQc, splitPath(o$input)$file);
	# <!> hardcoded stuff
	#matrixPath = 'results/R01/qc/ghana-4.genome.ibd_1';
	Log(sprintf('Reading ibd matrix: %s', matrixPath), 4);
	ibd = 1 - as.matrix(read.csv(matrixPath, sep = ',', header = F));

	# <p> condense list of individuals
	e = readExclusions(o);
	fam  = readTable(sprintf(
		'[HEADER=F,SEP=S,NAMES=fid.1;id;pid;mid;sex.1;affected,CONST=genotyped:1]:%s.fam', o$input));
	finalIds = setdiff(fam$id, avu(e$individuals));
	finalIds = intersect(finalIds, data$id);
	# reduce by incomplete data
	f1wom = formulaWithoutMarker(f1);
	row.names(data) = 1:(dim(data)[1]);
	X = model.matrix(model.frame(f1wom, data = data), data = data);
	complete = as.integer(row.names(X));
	completeIds = data$id[complete];
	finalIds = intersect(finalIds, completeIds);

	# condense 
	finalIdcs = which.indeces(finalIds, fam$id);
	ibd = ibd[finalIdcs, finalIdcs];
	ibdC = ibd;
	for (i in 1:3) {
		ibdC = ibdC %*% diag(1/diag(ibdC));
		ibdES = eigen(ibdC);
		#imCond = abs(Im(ibd1es$values)) < 1e-10;
		#ibd1es$values[imCond] = Re(ibd1es$values)[imCond];
		if (all(abs(Im(ibdES$values)) < 1e-10)) {
			ibdES$values = Re(ibdES$values);
			ibdES$values[ibdES$values < 0] = 0;
		} else {
			Log("Warning: introducing strong bias");
		}
		ibdC = Re(ibdES$vectors %*% diag(ibdES$values) %*% t(ibdES$vectors));
	}
	Save(ibdC, file = sprintf('%s/ibdC-%s.Rdata', o$outputPrefixAnalysis, gwasFormula2tag(f1)));
	Nint = 1e3;	# <i> parameter
	#ibd2 = .1 * ibd1 + .9 * diag(rep(1, dim(ibd1)[1]));

	# <p> lh specification
	spec = lhSpecificationFromModelFrameNull(X);

	# <p> generate random effect integration mass
	Z = mvrnorm(Nint, rep(0, dim(ibdC)[1]), ibdC);
	y = as.integer(data[[formula.response(f1)]][as.integer(row.names(X))]) - 1;

	r = list(finalIds = finalIds, lhZ = Z, lhY = y, lhSpec = spec);
	Save(r, file = sprintf('%s/ibdC-%s-pars.Rdata', o$outputPrefixAnalysis, gwasFormula2tag(f1)));
	gc();
	r
}


glmIbd_optim = function(beta0, sigma, betaA, y, X, Z) {
	# <p> fixed effect linear predictor
	lpred = X %*% c(beta0, betaA);
	# <p> integrate out random effect
	ps0 = expit(t(sqrt(sigma) * Z) + as.vector(lpred));
	ps1 = apply(ps0, 2, function(c) ifelse(y, c, 1 - c));
	ps2 = apply(ps1, 1, sum);
	ll = sum(log(ps2));	# taking mean is insubstantial: ll = sum(log(ps / dim(Z)[1]));
	ll
	# <p> normal distribution
	#resid = as.vector(y - yPred0);
	#ll = sum(dnorm(resid, 0, sqrt(sigma[1]), log = T));
}

glmBinIBD = function(data, f1, f0, o, finalIds, lhY, lhZ, lhSpec) {
	X = model.matrix(model.frame(f1, data = data), data = data)
	# tailor frames
	idcs = which.indeces(data$id, finalIds);
	#print(f1);
	lr = lhTestLR(lhSpec, y = lhY[idcs], X = X, Z = lhZ[, idcs]);
	r = list(effects0 = lr$par.null[1:(length(lr$par.null) - lr$df)], effects1 = lr$par.alt, p.value = lr$p);
	r
}

#
#	<p> helping functions
#

gwasFormula2tag = function(f1) {
	#mds = fetchRegexpr('C(\\d+)', f1, captures = T);
	tag = mergeDictToString(list(
		`\\s+` = '',
		`_` = '-',
		`Surv\\(.*?\\)` = 'surv',
		MARKER = 'snp',
		# <p> MDS covariates
		# less obfuscated version
		#`C(\\d+)(.C(\\d+))*` = sprintf('MDS%s', join(mds, ':')),
		# first component
		`C(\\d+)((.C(\\d+))*)` = 'MDS\\1:\\2',
		# other components
		`\\+C` = ''
		#`C([0-9]+)` = ":\\1"
	), f1, re = T, doApplyValueMap = F, doOrderKeys = F);
	tag
}

#
#	<p> genotype input
#

# treat as raw as conversion to data.frame is slow
readPlinkBinary = function(prefix) {
	p = read.plink(sprintf("%s.bed", prefix), sprintf("%s.bim", prefix), sprintf("%s.fam", prefix));
	#p@.Data	#snpMatrix
	#snpStats
	gts = p$genotypes;
	dimnames(gts)[[1]] = p$fam$member;
	gts
}
# genotypes are counts of alleles
# for flipToMinor = T genotypes count the minor allele
# plink counts 2nd allele by default, return list of counted alleles
#	(alleles whose risk is assessed in the statistical analysis)
gtsFromPlinkAlleles = function(gts, flipToMinor = T, alleles = NULL) {
	gts = apply(gts, 2, as.integer);
	gts = ifelse(gts == 0, NA, gts - 1);

	as = NULL;
	if (flipToMinor) {
		gtsRaw = apply(gts, 2, gtsFlipToMinorInd);
		gts = sapply(gtsRaw, function(e)e$gts);
		didFlip = list.key(gtsRaw, 'didFlip');
		if (!is.null(alleles)) as = as.matrix(alleles)[, 2 - didFlip];
	}
	list(gts = gts, alleles = as)
}

# <i> call gtsFromPlinkAlleles
gtsFromPlink = function(gts, flipToMinor = T, alleles = 0:1) {
	gts = apply(gts, 2, as.integer);
	gts = ifelse(gts == 0, NA, gts - 1);
	if (flipToMinor) gts = apply(gts, 2, gtsFlipToMinor);
	gts
}


# snp may be a vector
dataForSnpsRaw = dataForSnpRaw = function(d, snp, input, indExcl, markerExcl,
	silentOnExcluded = F, returnSnpNames = F, flipToMinor = T) {
	# <p> snp exclusion
	I = intersect(snp, markerExcl);
	if (length(I) > 0 && !silentOnExcluded) {
		stop(sprintf('tried to retrieve data set for excluded marker %s', join(I)));
	} else {
		snp = setdiff(snp, markerExcl);
	}
	
	# <p> read genotypes
	dG = readPlinkBinary(input);
	ns = dimnames(dG);
	#gts = as.integer(dG[, which(ns[[2]] == snp)]);
	# <p> lookup SNPs
	is = which.indeces(snp, ns[[2]], ret.na = T);
	if (sum(is.na(is)) > 0) {
		Log(sprintf('dataForSnpsRaw: %d requested SNPs not in dataset.', sum(is.na(is))), 3);
	}
	# <p> remove missing
	snp = snp[!is.na(is)];
	is = na.omit(is);
	gts = gtsFromPlink(dG[, is, drop = F], flipToMinor = flipToMinor);

	# <p> exclude individuals
	samples = setdiff(d$id, indExcl);
	dP = d[which.indeces(samples, d$id), ];

	# <p> merge data
	dGts =  data.frame.types(data.frame(gts, id = ns[[1]]), names = snp);
	d1 = merge(dP, dGts);
	r = if (returnSnpNames) {
		r = list(data = d1, snps = snp);
	} else d1;
	r
}

dataForSnps = dataForSnp = function(o, snp, silentOnExcluded = F, returnSnpNames = F,
	ignoreIndividualExclusion = F, ignoreMarkerExclusion = F, flipToMinor = T) {
	d = gwasReadVariables(o);
	e = readExclusions(o);
	if (ignoreIndividualExclusion) {
		e$individuals = c();
	}
	if (ignoreMarkerExclusion) e$markers = c();
	r = dataForSnpRaw(d, snp, o$input, e$individuals, e$markers,
		silentOnExcluded = silentOnExcluded, returnSnpNames = returnSnpNames, flipToMinor = flipToMinor);

	# add column with exclusion status
	if (ignoreIndividualExclusion) {
		e = readExclusions(o);
		excluded =  which.indeces(e$individuals, r$id, ret.na = T);
		included.logical = vector.spread(F, excluded, length(r$id), T);
		r$is_included_sample = included.logical;
	}
	r
}

# dataReadSnps = dataForSnp = function(o) {
# 	d = gwasReadVariables(o);
# 	e = readExclusions(o);
# 	dG = readPlinkBinary(o$input);
# 	snps = setdiff(dimnames(dG)[[2]], e$markers);
# 	snps
# }

#
#	<p> run R-model on plink-chunk
#

# tag2filename
tag2fn = function(tag)gsub("[~*:+\"]", '_', tag);
# tag2headiing
tag2tex = function(tag) {
	s = gsub("[~]", '$\\\\sim$', tag);
	s = gsub("[_]", '\\\\_', tag);
	s
}

resultsFileRaw = function(o, tag, i, extension = 'RData') {
	tag = tag2fn(tag);
	fn = Sprintf('%{prefix}s-%{tag}s-%{i}03d.%{extension}s', prefix = o$outputPrefixAnalysis);
	Log("Chunk filename: %{fn}s", 7);
	fn
}

analyzeChunk = function(i, input, outputDir, d, o, indExcl, markerExcl,
	tempDir = plinkTempDir, tag = NULL, f1, f0,stat = 'glmBin', ...) {
	# <p> preparation
	statF = get(stat);
	if (is.null(tag)) tag = formula.response(f1);

	# <p> read genotypes
	dG = readPlinkBinary(sprintf('%s/splits/%s%03d', outputDir, splitPath(input)$base, i));
	ns = dimnames(dG);

	# <p> exclude individuals
	samples = setdiff(d$id, indExcl);
	dP = d[which.indeces(samples, d$id), ];

	# <p> iterate SNPs heeding exclusions
	snps = setdiff(ns[[2]], markerExcl);
	# <!><%> shorten analysis
	#snps = snps[1:min(10, length(snps))]
	#snps = gsub('[-?]', '.', snps);	# replace special characters
	r = lapply(snps, function(snp) {
		snpInf = plinkGetSnpInfo(o, snp);
		gtsRaw = gtsFromPlinkAlleles(dG[, which(ns[[2]] == snp), drop = F],
			flipToMinor = o$assParFlipToRare, alleles = snpInf[, c('a1', 'a2'), drop = F]);
		gts = gtsRaw$gts;
		# <p> create data frame
		dGS = Df(gts, id = ns[[1]], names = snp);	# data frame with one SNP only (first column)
		#d1 = merge(Df_(dP, as_character = 'id'), dGS, by = 'id');
		d1 = merge(dP, dGS, by = 'id');
		#f0 = sprintf('Surv(time, event) ~ sex + age + %s + arm + %s*arm', snp, snp);
		f1 = as.formula(mergeDictToString(list(MARKER = sprintf("`%s`", snp)), f1));
		f0 = as.formula(mergeDictToString(list(MARKER = sprintf("`%s`", snp)), f0));
		# <p> align data set (see regressionCompareModels)
		# no longer works as of R 2.15.2
		#rows = as.integer(row.names(model.frame(f1, data = d1)));
		rows = apply(d1[, all.vars(f1)], 1, function(e)all(!is.na(e)));
		d2 = d1[rows, ];
		if (nrow(d2) == 0) {
			stop('No unmissing data in association analysis');
		}

		# <p> recheck maf
		af = afForGts(d2[[snp]]);
# 		if (snp %in% snps[as.integer(seq(1, length(snps), length.out = 50))]) {
# 			print(list(snp = snp, af = af, snpInf, minor = gtsRaw$allele));
# 		}
		if (min(af, 1 - af) < o$assParMafTest) return(list(marker = snp, mafExcl = T));

		# <p> perform statistical testing
		print(snp);
		r = statF(d2, f1, f0, o, ...);
		#r = do.call(stat, c(list(data = d1, f1 = f1, f0 = f0, o = o), list(...)));
		r = list(marker = snp, mafExcl = F, af = af, allele = gtsRaw$allele, r = r);
		r
	});
	save(r, file = resultsFileRaw(o, tag, i));
	NULL
}

formulaWithoutMarker = function(f1) {
	f1 = gsub('\\s*[+*]\\s*MARKER', '', f1);
	f1 = gsub('MARKER\\s*[+*]\\s*', '', f1);
	f1
}

describeDistribution = function(v) {
	s = summary(v, maxsum = 20);
	s0 = join(sapply(1:length(s), function(i) {
		if (is.integer(s[i]))
			sprintf('%s: %d', names(s)[i], s[i]) else
		if (is.numeric(s[i]))
			sprintf('%s: %.3f', names(s)[i], s[i]) else
			sprintf('%s: %s', names(s)[i], s[i])
	}), sep = '; ');
	s0
}

describeVariablesRaw = function(input, outputDir, d, o, f1, f0, tableName = 'ASS:VARS') {
	# <p> variables
	t0 = data.frame.types(sapply(c(setdiff(all.vars(as.formula(f1)), 'MARKER'), 'genotyped'), function(e) {
		list(e, fraction(is.na(d[[e]])), describeDistribution(d[[e]]))
	}), do.transpose = T, do.unlist = T, names = c('Name', 'Miss.', 'Distribution'));
	varTab = report.data.frame.toString(
		t0,
		digits = c(NA, '%1', 'p{6cm}'),
		quoteHeader = F, ignoreRowNames = T,
		caption = 'ASS:VARS:caption'
	);
	REP.tex(tableName, varTab, fmt = 'tiny');
	NULL
}

describeVariables = function(input, outputDir, d, o, qcIndExcl = NULL, f1, f0, tag = NULL) {
	# <p> remove excluded individuals
	if (length(qcIndExcl) > 0) {
		excl = which.indeces(qcIndExcl, d$id);
		# <A> changed from stop -> Log due to possible data filtering
		if (length(excl) < length(qcIndExcl)) {
			Log(Sprintf('Assocation: describeVariable: could not find all indiviuals to be excluded (%{countE}d < %{countTbE}d).', countE = length(excl), countTbE = length(qcIndExcl)), 1);
		}
		d = d[-excl, ];
	}
	row.names(d) = NULL;	# reset row names <A> to later be able to refer to row.names

	# <p> before missingness exclusion
	describeVariablesRaw(input, outputDir, d, o, f1, f0, tableName = 'ASS:VARS');

	# <p> total
	d = d[!is.na(d$genotyped), ];
	nas = apply(d[, setdiff(all.vars(as.formula(f1)), 'MARKER'), drop = F], 1, function(r)any(is.na(r)));
	#rns = row.names(model.frame(formulaWithoutMarker(f1), data = d));
	#N = length(rns);
	N = sum(!nas);
	REP.tex('ASS:N', N);
	# <p> after missingness exclusion
	#d = d[as.integer(rns), ];
	d = d[!nas, ];
	describeVariablesRaw(input, outputDir, d, o, f1, f0, tableName = 'ASS:VARSpost');
	if (!is.null(tag)) 
		write.csv(Df(id = d$id), file = exportFileName(o, tag, 'individuals-included.csv'));
}

analyzeData = function(input, outputDir, d, o,
	f1 = 'Surv(time, event) ~ arm + sex + age + MARKER',
	f0 = 'Surv(time, event) ~ arm + sex + age',
	tag = '', stat = 'glmBin') {
	# <p> prepare folder
	outputAnalysis =  sprintf('%s/analysis', outputDir);
	if (!file.exists(outputAnalysis)) dir.create(outputAnalysis);

	prepare = sprintf('%sprepare', stat);
	parsPrepare = if (exists(prepare)) {
		parsPrecomputed = sprintf('%s/ibdC-%s-pars.Rdata', o$outputPrefixAnalysis, gwasFormula2tag(f1));
		if (file.exists(parsPrecomputed) && T) get(load(parsPrecomputed)[1]) else
			get(prepare)(d, f1, f0, o);
	} else list();
	# <p> analyse chunks
	#analyzeChunk(1, pathGenotypes, tempDir);
	analysisFct = function(i, input, outputDir, d, o, indExcl, markerExcl, f1, f0, tag, stat, ...){
		analyzeChunk(i, input, outputDir,
			d, o, indExcl, markerExcl, f1 = f1, f0 = f0, tag = tag, stat = stat, ...)
	};
	pars = list(l = 1:o$assParNchunks, .f = analysisFct,
		input = input, outputDir = outputDir, d = d, o = o,
		indExcl = readExclusionsInds(o, 'all'), markerExcl = readExclusionsMarkers(o, 'all'),
		f1 = f1, f0 = f0, tag = tag, stat = stat,
		.clRunLocal = rget(".clRunLocal", envir = .GlobalEnv)
	);

	r = do.call('clapply', c(pars, parsPrepare));
	NULL
}

#
#	<p> report analysis
#

collectResults = function(chunks, o, tag) {
	pb = txtProgressBar(label = 'Results: ');
	r = lapply(chunks, function(i) {
#		path = sprintf('%s-%03d.RData', prefix, i);
		#sprintf('%s/analysis/%s-%s', outputDir, splitPath(input)$file, tag2fn(tag))
		path = resultsFileRaw(o, tag, i);
		r = if (file.exists(path)) get(load(file = path)[1]) else list();
		Log(sprintf("Result chunk %2d @ '.../%s': #%d", i, splitPath(path)$file, length(r)), 7);
		setTxtProgressBar(pb, i/length(chunks));
		r
	});
	close(pb);
	r = unlist.n(r, 1);
	Logs("Read %{Nsnps}d results for model %{tag}s", Nsnps = length(r), logLevel = 2);
	#r = unlist.n(r, 2);
	r
}

writeTopData = function(markers, file, gtsNameReplace = list(sex = 'sex.plink')) {
	d = plink.fetchSnpsByName(pathGenotypes, markers,
		exclInd = con(qcIndExclPath, '.csv'),
		exclMarkers = con(qcMarkerExclPath, '.csv')
	);
	names(d) = vector.replace(names(d), c(gtsNameReplace, list(iid = 'id')));
	# assume dR (response data) to exist
	d0 = merge(dR, d, sort = F);
	write.csv(d0, file = file);
}


manhattanPlot = function(data, file,  title = '', pp = list(width = 12, height = 6, dpi = 300),
	significance = 5e-8)  {
	#mhtplot(mhpdf1, usepos = F);
	names(data) = c('CHR', 'BP', 'P');
	p = manhattan(data, title = title, max.y = 'max', significance = significance,
		size.x.labels = 9, size.y.labels = 10);
	ggsave(file, width = pp$width, height = pp$height, dpi = pp$dpi);
	file
}

getMap = function(input) {
	map = read.table(sprintf('%s.bim', input), header = F, sep = "\t", stringsAsFactors = F);
	names(map) = c('chr', 'marker', 'posGen', 'posPhy', 'a1', 'a2');
	map
}
readHwe = function(input, outputDir) {
	pathHwe = sprintf('%s/qc/%s-qc-markers-hwe.RData', outputDir, splitPath(input)$file);
	if (!file.exists(pathHwe)) return(NULL);
	r = get(load(pathHwe)[1]);
	names(r) = vector.replace(names(r), list(SNP = 'marker', P = 'hwe.plink'));
	r
}

resultFileName = function(outputDir, prefix, tag, postfix = '', extension = 'csv') {
	Sprintf('%{outputDir}s/%{prefix}s-%{tag}s%{sep}s%{postfix}s.%{extension}s',
		sep = if (postfix == '') '' else '-', postfix, extension)
}
resultFileNameAssTopList = function(outputDir, tag) {
	resultFileName(outputDir, 'association', tag2fn(tag), 'topList')
}
resultFileNameAss = function(outputDir, tag) {
	resultFileName(outputDir, 'association', tag2fn(tag))
}
resultFileNameManhattan = function(outputDir, tag) {
	resultFileName(outputDir, 'ass-manhatten', tag2fn(tag), extension = 'jpg')
}
exportFileName = function(o, tag, name, extension = NULL, mkdir = T) with(o, {
	path = Sprintf('%{outputDirExport}s/%{prefix}s-%{tag}s/%{name}s%{e}s',
		prefix = splitPath(input)$file, e = circumfix(extension, pre = '.'))
	if (mkdir) Dir.create(path, treatPathAsFile = T);
	Logs('Export to:%{path}s', logLevel = 5);
	path
})

associationAnalysisWriteSummariesForModel = function(input, outputDir = NULL, d, o = list()) with(o, {
})


summarizePvalues = function(outputDir,
	ps, Evars, map, tag, assParTopN = 50, afHwe = NULL, writeTopData = NULL, o, alpha = 0.05) {
#summarizePvaluesForVar  = function(ps, Pvar, Pvars, Evars, map, tag, assParTopN = 50, afHwe = NULL,
#	modelName = '', writeTopData = NULL) {
	# <p> embracing sub-template
	#REP.reportSubTemplateInitialize('variableAssociation');

	# <p> create tables and plots
	varsMap =  c('chr', 'posPhy', 'marker');
	mappedPs = merge(map[, varsMap], ps, sort = F, all.y = T);
	write.csv(mappedPs, file = exportFileName(o, tag, 'snpAssociations.csv'));
	write.csv(Df(marker = mappedPs$marker), file = exportFileName(o, tag, 'markers-included.csv'));

	# manhatten plot
	# we won't exclude any SNPs at this point any  more
	#mhpdf = mhpdf[-which.indeces(markersExcl, mhpdf$snp),]
	# manhatten plot data frame
	mhp = mappedPs[, c('chr', 'posPhy', 'P')];

	# top list
	psTop = mappedPs[order(mappedPs$P)[1:assParTopN], ];

	# add 
	if (!is.null(afHwe)) {
		psTop = merge(psTop, afHwe, all.x = T, sort = F);
		varsHwe = c('hwe');
		psTop = psTop[order(psTop$P), c(names(mappedPs), varsHwe)];
	}
	row.names(psTop) = NULL;

	# <p> reporting
	REP.tex('ASS:VARIABLE', 'P');
	outputDir =  sprintf('%s/results', outputDir);
	Log(sprintf('Output dir: %s tag: %s', outputDir, tag), 4);
	# <p> manhattan plot
	PvalueCutoff = if (o$assManhattenCutoff == 'bonferroni')
		alpha/nrow(mhp) else
		as.numeric(o$assManhattenCutoff);
	REP.tex('ASS:PvalueCutoff', PvalueCutoff, fmt = 'sci1');
	REP.tex('ASS:Nmarkers', nrow(mhp));
	REP.plot('ASS:MANHATTEN',
		manhattanPlot(mhp, title = sprintf('Model %s', tag), significance = PvalueCutoff,
			file = exportFileName(o, tag, 'manhattan.jpg')));
	# <p> table/table files
	write.csv(psTop, file = resultFileNameAssTopList(outputDir, tag));
	write.csv(mappedPs[order(mappedPs$P), ], file = resultFileNameAss(outputDir, tag));
	Log('... manhattenplot finished.', 1);

	#
	#	<p> report table
	#
	varsMap = c('marker', 'chr', 'posPhy', 'allele');
	varsAf = 'afD';
	varsBeta = names(psTop)[regexIdcs('^B.*', names(psTop))];
	varsHwe = 'hwe';
	varsP = 'P';
	varsAll = c(varsMap, varsAf, varsBeta, varsHwe, varsP);
	# remove CIs from reported frame
	psTop = psTop[, varsAll];
	# names of reported data frame
	rep.names = c(
		varsMap,	# variables describing markers
		'af',
		sapply(1:length(Evars), function(i)sprintf('$\\beta_%d$', i)),
		'P(hwe)',
		'P(snp)'
	);
	rep.names = vector.replace(rep.names, list(chr = 'C', allele = 'A'));
	caption = sprintf(con(
		'Top associations according to model \\texttt{%s}. ',
		'Effect size parameters correspond to variables as follows: (%s) = (%s). ',
		'{\\it P(snp)} association P-value, {\\it af} allele frequency in complete data. ',
		'{\\it C} chromosome, {\\it A} effect allele, {\\it af} allele frequency of effect allele.'),
		tag2tex(tag),
		join(sapply(1:length(Evars), function(i)sprintf('$\\beta_%d$', i)), ', '),
		join(sapply(Evars, function(n)sprintf('$\\beta(%s)$', n)), ', ')
	);
	REP.tex('ASS:TABLE', report.data.frame.toString(
		psTop,
		digits = c(rep(NA, length(varsMap)), 2, rep(2, length(Evars)), '#2', '#2'),
		names.as = rep.names, quoteHeader = F,
		caption = caption
	), fmt = 'tiny');
	if (!is.null(writeTopData)) writeTopData(markers = as.character(psTop$marker),
		file = sprintf('%s/association-%s-topData.csv', outputDir, tag2fn(tag)));

	# fix latex bug: only one '.' allowed per file name
	REP.plot('ASS:QQ:ASSOCIATION', Qplot(sample = ps$P, dist = qunif,
		file = sprintf('%s/ass-QQ-%s.jpg', outputDir, tag2fn(tag))));

	chisqs = qchisq(ps$P, df = 1, lower.tail = F);
	medianChisq = 0.4550757;	# median(rchisq(1e7, df = 1))
	inflation = (median(sqrt(chisqs), na.rm = T) / sqrt(medianChisq))^2;
	Log(sprintf('Inflation %.2f', inflation), 4);
	REP.tex('ASS:QQ:INFLATION', inflation, fmt = '.2');

	#REP.reportSubTemplate('variableAssociation', 'P');
	#REP.finalizeSubTemplate('variableAssociation');
}

rmn = replaceMarkerName = function(v, m)vector.replace(v, f = m, t = 'marker', regex = T);
summarizeResults = function(chunks, input, outputDir, d, o,
	f1, f0, tag = NULL, geneticModel = 'additive', 
	effectSizes = c(),
	assParTopN = 50, afHwe = NULL, writeTopData = NULL, model = list()) {

	REP.tex('ASS:MODEL', sprintf('\\texttt{%s}', tag2tex(abbr(tag, 20))));
	REP.tex('ASS:Formula1', f1);
	REP.tex('ASS:Formula0', f0);
	REP.tex('ASS:Subset', as.character(firstDef(model$subset, 'no subsetting')));
	REP.tex('ASS:GENMODEL', geneticModel);

	# describe variables and missingness
	describeVariables(input, outputDir, d, o, qcIndExcl = readExclusionsInds(o, 'all'),
		f1 = f1, f0 = f0, tag = tag);

	# <p> read summaries
	r0 = collectResults(chunks, o, tag);
if (any(NULL == 'CHILDREP50')) browser();
	# <A> cleanup code
	if (!length(r0)) {
		REP.reportSubTemplate('association', tag);
		return();
	}
	# snps filtered during testing
	mafExcl = nif(list.kp(r0, 'mafExcl', null2na = T, do.unlist = T));
	REP.tex('ASS:NmafExcl', sum(mafExcl));
	r = r0[!mafExcl];

	# <p> extract variable names
	snps =  list.key(r, 'marker');	#sapply(r, function(e)e$marker);
	# expect at least one marker in the analysis <N>
	# <!><i> interaction between several nominal variables
	#	all covariates, replacing 'marker' for the actual marker name
	Cvars = rmn(names(r[[1]]$r$effects1), r[[1]]$marker); #normalized names of predictors in model
	#	variables for which to report effect sizes
	Evars = c('marker', grep.infixes(effectSizes, Cvars));

	haveCi = list.kp(r, 'r$ci1', test = T, do.unlist = T);
	haveCis = any(haveCi);
	namesCi = pastem(c('ciL', 'ciU'), Cvars, sep = '.', revsort = F);

	# <A> as a special case we always collect P-values of markers
	#	other P-value collection is controlled by the PvalueVars variable
	#	all effect sizes are collected
	ps = sapply(1:length(snps), function(i) {
		coefs = r[[i]]$r$effects1;
		# replace SNP name by 'marker'
		names(coefs) = rmn(names(coefs), r[[i]]$marker);
		# concatenate, p.value and effect sizes
		ciBounds = if (haveCis)
			if (haveCi[i]) as.vector(t(r[[i]]$r$ci)) else rep(NA, length(namesCi)) else
			c();
		vars = c(r[[i]]$r$p.value, coefs[Evars], ciBounds);
		vars
	});
	ps = Df_(ps, t_ = T);
	dfNames = c('P', paste('B', Evars, sep = '.'));
	if (haveCis) dfNames = c(dfNames, namesCi);
	names(ps) = dfNames;

	# afD for data allele frequency
	ps = data.frame(marker = snps, allele = list.key(r, 'allele'), afD = list.key(r, 'af'), ps);
	# <p>> report results
	# reflect data frame naming conventions by using .dfns
	summarizePvalues(outputDir,
		ps, .dfns(Evars), getMap(input), tag, assParTopN, readHwe(input, outputDir), writeTopData, o = o);

	REP.reportSubTemplate('association', tag);
}


#
#	<p> analysis interface
#

readMDS = function(input, o = list()) {
	while (1) {
		if (firstDef(o$qcParMdsRule, '') != '' || o$runIndex == 1) {
			prefix = sprintf('%s/%s/qc/%s', o$outputBase, o$runName, splitPath(input)$file);
			mds = readTable(sprintf('[NAMES=fid;id,HEADER=T,SEP=S+]:%s-mds.mds', prefix));
			return(mds);
		}
		#o = propertyFromString(readFile(sprintf('%s/%s/options', o$outputBase, o$previousRun)));
		# <!> gwasMostRecentRun
		o = propertyFromString(readFile(sprintf('%s/options', o$previousOutputDir)));
	}
	NULL
}

analysisTag = function(m) {
	tag = Sprintf('%{formula}s%{tag}s%{select}s',
		formula = gwasFormula2tag(m$f1),
		tag = circumfix(m$tag, pre = ':'),
		select = circumfix(as.character(m$subset), pre = ';'));
	tag
}

expandModels = function(models, input, o, d, noMDS = FALSE) {
	# responses
	ms0 = lapply(models, function(m) {
		ms = if (!is.null(m$responses)) {
			lapply(m$responses, function(r) {
				merge.lists(m,
					mergeDictToDict(list(RESPONSE = r), m[c('tag', 'f1', 'f0')]),
					list(response = r, responses = c())
				)
			})
		} else list(m);
	});
	ms0 = do.call(c, ms0);
	# subsets
	# <%><N> merge.list fails on using subSets (contains expressions)
	#	-> otherwise merge with above
	ms = lapply(ms0, function(m) {
		ms = if (!is.null(m$subSets)) {
			lapply(m$subSets, function(ss) {
				m$subset = ss;
				m
			})
		} else list(m);
	});
	ms = do.call(c, ms);

	# <p> handle MDS components
	#if (length(o$assParCountMDScomponents) > 0) {
	# handle MDS components per model
	if (!noMDS) {
		mds = readMDS(input, o);
		d = merge(d, mds, all.x = T);
	}
	ms1 = lapply(ms, function(m) {
		#if (length(o$assParCountMDScomponents) > 0) {
		if (length(m$assParCountMDScomponents) > 0) {
			#components = o$assParCountMDScomponents;
			components = m$assParCountMDScomponents;
			mdsVars = paste('C', components, sep = '', collapse = ' + ');
			m$f1 = sprintf('%s + %s', m$f1, mdsVars);
			m$f0 = sprintf('%s + %s', m$f0, mdsVars);
		}
		m
	});
	r = list(models = ms1, data = d);
	#print(r);
	r
}

# <i> unify associationAnalysis, associationAnalysisSummary -> iterateAssociationModels
associationAnalysis = function(input, outputDir = NULL, d, o = list()) with(o, {
	prefix = sprintf('%s/qc/%s', outputDir, splitPath(input)$file);

	expandedModels = expandModels(assParModels, input, o, d);
	if (!is.null(o$assPerfModels)) expandedModels$models = expandedModels$models[assPerfModels];
	d = expandedModels$data;
	for (m in expandedModels$models) with(m, {
		print(f1);
		#print(stat);
		#print(analysisTag(m));
		varMiss = setdiff(setdiff(all.vars(as.formula(f1)), 'MARKER'), names(d));
		if (length(varMiss) > 0) {
			Logs('Variable(s) "%{varMiss}s" missing from data', varMiss = join(varMiss, ', '), 1);
			stop('missing varibles');
		}
		Logs("Subsetting with expression: %{subset}s",
			subset = as.character(firstDef(m$subset, 'no subset')), logLevel = 5);
		if (!is.null(m$subset)) d = subset(d, with(d, eval(m$subset)));
		if (!is.null(m$filter)) { d = filter(o, d, f1, f0, m); }
		analyzeData(input, outputDir, d, o,
			f1 = f1, f0 = f0, tag = analysisTag(m), stat = stat)	# <A> m$tag might be NULL
	})
})

associationAnalysisWriteSummaries = function(input, outputDir = NULL, d, o = list()) with(o, {
	e = readExclusions(o);
	d$included = !(d$id %in% e$individuals);
	write.csv(d, file = Sprintf('%{outputDirExport}s/data-included.csv'));
	dIdsIncl = data.frame(id = setdiff(d$id, e$individuals));
	write.csv(dIdsIncl, file = Sprintf('%{outputDirExport}s/individuals-included.csv'));
	dIdsExcl = data.frame(id = e$individuals);
	write.csv(dIdsExcl, file = Sprintf('%{outputDirExport}s/individuals-excluded.csv'));
	markersExcl = data.frame(id = e$markers);
	write.csv(markersExcl, file = Sprintf('%{outputDirExport}s/markers-excluded.csv'));
})

associationAnalysisSummary = function(input, outputDir = NULL, d, o = list()) with(o, {
	prefix = sprintf('%s/qc/%s', outputDir, splitPath(input)$file);

	if (!length(assParModels)) return();
	outputResults =  sprintf('%s/results', outputDir);
	if (!file.exists(outputResults)) dir.create(outputResults);

	# <A> no parallelization dt reporting side effects
	expandedModels = expandModels(assParModels, input, o, d);
	d = expandedModels$data;
	REP.tex('ASS:Nmodels', length(expandedModels$models));

	REP.reportSubTemplateInitialize('association');
	for (m in expandedModels$models) with(m, {
	#clapply(assParModels, function(m, input, outputDir, o) with(m, with(o, {
		#assParNchunks = 2;	# <!><%> debugging
		Logs("Subsetting with expression: %{subset}s",
			subset = as.character(firstDef(m$subset, 'no subset')), logLevel = 5);
		if (!is.null(m$subset)) d = subset(d, with(d, eval(m$subset)));
		if (!is.null(m$filter)) { d = filter(o, d, f1, f0, m); }
		summarizeResults(1:assParNchunks, input, outputDir, d, o,
			f1, f0,
			tag = analysisTag(m),
			effectSizes = firstDef(m$assParEffectSizes,
				o$assParEffectSizes, setdiff(formula.covariates(m$f1), 'MARKER')),
			assParTopN, afHwe = NULL, geneticModel = m$geneticModel, model = m);
	});
	#})), input = input, outputDir = outputDir, o = o);
	REP.finalizeSubTemplate('association');
	associationAnalysisWriteSummaries(input, outputDir, d, o);
})

#' Function to create tags used in file names for association models
associationModelTags = function(o, d, models = NULL) {
	mds = expandModels(o$assParModels, o$input, o, d)$m;
	if (!is.null(models))
		mds = mds[!is.na(which.indeces(list.key(mds, 'tag', null2na =T), models, ret.na = T))];
	sapply(mds, analysisTag);
}
associationResultFiles = function(o, d,
	fileFunction = resultFileNameAssTopList, outputDir = sprintf('%s/results', o$outputDir)) {
	mt = associationModelTags(o, d);
	files = sapply(mt, fileFunction, outputDir = outputDir);
	files
}

associationResultFilesTop = function(o, d) {
	associationResultFiles(o, d, resultFileNameAssTopList, sprintf('%s/results', o$outputDir));
}

associationTopSnpsForModels = function(o, d, models = NULL) {
	files = associationResultFiles(o, d, resultFileNameAssTopList, sprintf('%s/results', o$outputDir));
	snps = avu(sapply(files, function(f)read.csv(f)$marker));
}

associationSnpForModel = function(m, snp, data, o, ...) {
	statF = get(m$stat);
	f1 = as.formula(mergeDictToString(list(MARKER = sprintf("`%s`", snp)), m$f1));
	f0 = as.formula(mergeDictToString(list(MARKER = sprintf("`%s`", snp)), m$f0));
	# subset
	if (!is.null(m$subset)) data = subset(data, with(data, eval(m$subset)));
	# determine missing data
	rows = apply(data[, all.vars(f1)], 1, function(e)all(!is.na(e)));
	r = statF(data[rows, ], f1, f0, o, ...);
	r$name = Sprintf('%{modelName}s;SNP=%{snp}', modelName = analysisTag(m));
	r
}

associationExternalSnps = function(o, data, snps, models = o$assParModels, ...) {
	ms = expandModels(models, NULL, o, data, noMDS = T);
	modelList = list(model = inlist(ms$models), snp = as.list(snps));
	r = iterateModels(modelList, function(i, model, snp) {
		associationSnpForModel(model, snp, data, o, ...)
	}, lapply__ = lapply);
	r
}
