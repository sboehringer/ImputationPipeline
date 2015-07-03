#
#	RgeneticsImputation.R
#Mon Feb 15 16:13:15 CET 2010

# based on the RA project

# global default pathes
hapmapMapPath = "/Library/ProjectData/2009-10-Hapmap3/phased/hapmap3map.RData";
hapmapDir = "/Library/ProjectData/2009-10-Hapmap3/phased";

#
#	likelihood ratio
#

#
#	<p> likelihood for independent offspring, ascertainment: one affected offspring
#<A> realistic: familiy based random effect

# data is data frame with columns N, A (total, affecteds)
llSNPimp = function(mu, beta, scores = 0:2/2, y, z) {
	ll = sum(sapply(1:length(y), function(i) {
		lh0 = sum(logitI(mu + beta * scores) * z[i, ]);
		ll = log(ifelse(y[i] == 1, lh0, 1 - lh0));
		ll
	}));
	ll
}
spec_lhImp = list(
	ll = "llSNPimp",
	alt = list(
		start = c(0, 0),	# also specifies number of parameters
		pars = list(
			list(name = "mu", type = "real"),
			list(name = "beta", type = "real")
		)
	),
	null = list(
		start = c(0),	# assume same likelihood and therefore #pars from alternative
		pars = list(
			list(name = "mu", type = "real")
		),
		mapperPost = function(p)c(p, 0)
	)
);

# genotype based test
llSNPimpGt = function(mu, beta1, beta2, scores = NULL, y, z) {
	beta = c(beta1, beta2);
	scores = matrix(c(0, 0, 1, 0, 0, 1), nrow = 3, byrow = T);
	ll = sum(sapply(1:length(y), function(i) {
		lh0 = sum(logitI(mu + scores %*% beta) * z[i, ]);
		ll = log(ifelse(y[i] == 1, lh0, 1 - lh0));
		ll
	}));
	ll
}
spec_lhImpGt = list(
	ll = "llSNPimpGt",
	alt = list(
		start = c(0, 0, 0),	# also specifies number of parameters
		pars = list(
			list(name = "mu", type = "real"),
			list(name = "beta1", type = "real"),
			list(name = "beta2", type = "real")
		)
	),
	null = list(
		start = c(0),	# assume same likelihood and therefore #pars from alternative
		pars = list(
			list(name = "mu", type = "real")
		),
		mapperPost = function(p)c(p, 0, 0)
	)
);

testImputedSNPlr = function(d, vars, gtScores = 1 - scores$add, response = "y") {
	# descide between degrees of freedom
	spec_lh = if (is.factor(gtScores)) spec_lhImpGt else spec_lhImp;
	# prepare data
	y = d[[response]];
	z = as.matrix(d[, vars, drop = F]);
	if (dim(z)[2] == 2) z = cbind(z, 1 - z[, 1] - z[, 2]);

	# define null/alt
	ml1 = lhMl(spec_lh, type = "alt", scores = gtScores, y = y, z = z);
	ml0 = lhMl(spec_lh, type = "null", scores = gtScores, y = y, z = z);
	chi = 2 * (ml1$value - ml0$value);
	dfs = length(spec_lh$alt$start) - length(spec_lh$null$start);
	p = pchisq(chi, dfs, lower.tail = F);
	r = list(p.value = p, chiSq = chi, df = dfs, pars = ml1$par);
	r
}

testSNPimputedGt = function(postFreq, response, yB) {
	# pre-calculations
	N = length(response);
	postFreq = postFreq[, c(2,3)];		# ignore reference genotype
	#postFreq = postFreq[, c(1,2)];		# ignore reference genotype
	scoreE = apply(postFreq, 2, mean);	# expected score
	scoreC = postFreq - scoreE;			# centered score
	#print(as.vector(c(1 - sum(scoreE), scoreE)));

	IO = sapply(1:2, function(c)sapply(1:2, function(r){
		sum(sapply(1:N, function(i)((response[i] - yB)^2 * scoreC[i, r] * scoreC[i, c])))
	}));
	# matrix elements
	m11_1 = postFreq %*% c( (1 - scoreE[1]), -scoreE[1] );
	m11_2 = postFreq %*% c( (1 - scoreE[1])^2, scoreE[1]^2 );
	m22_1 = postFreq %*% c( -scoreE[2], (1 - scoreE[2]) );
	m22_2 = postFreq %*% c( scoreE[2]^2, (1 - scoreE[2])^2 );
	m21_o = postFreq %*% c( (1 - scoreE[1]) * scoreE[2], (1 - scoreE[2]) * scoreE[1] );
	# squared error for response
	responseC = (response - yB);
	seY = responseC^2;

	# expected information
	IE_21 = -sum(m21_o);
	IE = yB * (1 - yB) * matrix(c(sum(m11_2), IE_21, IE_21, sum(m22_2)), ncol = 2);

	# loss
	#IL = sum((response - yB)^2 * (postFreq %*% (gtScores^2) - postScores^2));
	ILS_21 = seY %*% (m11_1 * m22_1);
	ILS2 = matrix(c(seY %*% m11_1^2, ILS_21, ILS_21, seY %*% m22_1^2), ncol = 2);
	ILJ_21 = -seY %*% m21_o;
	ILJ = matrix(c(seY %*% m11_2, ILJ_21, ILJ_21, seY %*% m22_2), ncol = 2);
	IL = ILJ - ILS2;

	# lh scores
	lhs = as.numeric(apply(scoreC * responseC, 2, sum));
	#lhs = as.numeric(apply(postFreq * responseC, 2, sum));
	# test stat
	# IO = IE - IL;
	T = t(lhs) %*% solve(IO) %*% lhs;
	p.value = pchisq(T, 2, lower.tail = F);
	# relative information
	#IR = (IE - IL) %*% solve(IE) %*% scoreE[2:3];
	r = list(IE = IE, IL = IL, IO = IE - IL, V = solve(IE - IL), score = lhs, T = T, p.value = p.value);
	r
}
testSNPimputedScore = function(postFreq, response, yB, gtScores = 1 - scores$add) {
	# expected information
	gtScoresE = apply(postFreq, 2, mean) %*% gtScores;
	postScores = postFreq %*% (gtScores - gtScoresE);
	postScores2 = postFreq %*% ((gtScores - gtScoresE)^2);
	IE = yB * (1 - yB) * sum(postScores2);
	# loss
	#IL = sum((response - yB)^2 * (postFreq %*% (gtScores^2) - postScores^2));
	IL = sum((response - yB)^2 * (postScores2 - postScores^2));
	# squared lh scores
	lhs = sum(postScores * (response - yB));
	# test stat
	T = lhs^2 / (IE - IL);
	p.value = pchisq(T, 1, lower.tail = F);
	# relative information
	IR = (IE - IL) / IE;
	r = list(IE = IE, IL = IL, IO = IE - IL, Rsq = IR, score = lhs, T = T, p.value = p.value);
	r
}

writeSNPtestInput = function(data, snps, path) {
	N = dim(data)[1];
	# <p> genotype file
	snpIndex = paste("SNP", 1:length(snps), paste = "");
	dSt = t(sapply(1:length(snps), function(i) {
		colsSNP = paste(rep.each(snps[i], 2), c("i1", "i2"), sep = "_");
		postFreq = data[, colsSNP];
		postFreq = as.matrix(cbind(postFreq, abs(1 - postFreq[, 1] - postFreq[, 2])));
		c(snpIndex[i], snps[i], "A", "C", sapply(as.vector(t(postFreq)), function(e)sprintf("%.4f", e)))
	}));
	# write genotypes
	write.table(dSt, file = sprintf("%s.gen", path), quote = F, sep = " ", col.names = F, row.names = F);

	# <p> produce sample file
	#	accomodating the ill-designed sample file: extra row with column codes
	s = data.frame(ID_1 = 0, ID_2 = 0, missing = 0);
	s0 = data.frame.types(cbind(1:N, 1:N, rep(0, N)), names = names(s));
	s = rbind(s, s0);
	write.table(s, file = sprintf("%s.sample", path), sep = " ", row.names = F, quote = F);
}
# <!> only return genotype test values
testSNPimputedSnpTest = function(data, snps, response, covariates = NULL, type = NULL,
	gtScores = 1 - scores$add) {
	controls = tempfile();
	writeSNPtestInput(data[data[[response]] == 0, ], snps, controls);
	cases = tempfile();
	writeSNPtestInput(data[data[[response]] == 1, ], snps, cases);

	output = tempfile();
	command = sprintf("snptest -controls %s.gen %s.sample -cases %s.gen %s.sample -o %s -frequentist 1 2 3 4 5 -proper > /dev/null",
		controls, controls, cases, cases, output);
	System(command, 5, printOnly = F);
	gc();
	r = read.csv(file = output, sep = " ", as.is = T);
	r = list(p.value = r$frequentist_gen_proper, r2 = r$frequentist_gen_proper_info, T = NA);
	r
}
testSNPimputedSnpTestFormula = function(f, data, snpRegex = "rs.*", snps = NULL,
	gtScores = scores$add, type = "glm", ...) {
	r = snpCallFormula2vars(testSNPimputedSnpTest, f, data, snpRegex, snps, gtScores = gtScores, ...);
	r
}

testSNPimputed = function(data, snps, response, covariates = NULL, type = NULL, gtScores = 1 - scores$add) {
	# <!> assume a single SNP, posterior frequencies
	colsSNP = paste(rep.each(snps, 2), c("i1", "i2"), sep = "_");
	postFreq = data[, colsSNP];
	postFreq = as.matrix(cbind(postFreq, 1 - postFreq[, 1] - postFreq[, 2]));

	# response
	responseData = data[[response]];
	yB = mean(responseData);
	r = if (is.factor(gtScores)) testSNPimputedGt(postFreq, responseData, yB) else
#	r = if (is.factor(gtScores)) testSNPimputedSnpTest(data, snps, response, covariates, type) else
		testSNPimputedScore(postFreq, responseData, yB, gtScores);
	r
}

testSNPimputedFormula = function(f, data, snpRegex = "rs.*", snps = NULL,
	gtScores = scores$add, type = "glm", ...) {
	r = snpCallFormula2vars(testSNPimputed, f, data, snpRegex, snps, gtScores = gtScores, ...);
	r
}

testSNP = function(data, snp, response = "y", gtScores = scores$add) {
	r0 = testSNPimputed(sprintf("%s ~ %s", response, snp), data = data, gtScores = 1 - gtScores);
	r1 = testImputedSNPlr(data, paste(snp, c("i1", "i2"), sep = "_"), response = response);
	r = list(score = r0, lr = r1);
	r
}

#
#	<p> imputation functions
#

# <p> defaults for impute imputation run
imputeParsDefaultImpute = list(
	command = "cd '%s' ; impute2 -h %s -l %s -m %s -g %s -o %s -Ne %d -int %d %d -buffer %d  -k %d -iter %d -burnin %d -fix_strand",
	hapfile = "CEU+TSI.chrCHR.hap",
	legendfile = "hapmap3.r2.b36.chrCHR.legend",
	mapfile = "genetic_map_chrCHR_combined_b36.txt",
	Ne = 11418,
	buffer = 50,	# 250 <!>
	k = 10,
	iterations = 10,
	burnin = 3,

	runningDir = "global<<hapmapDir>>"
);

# <p> defaults for mach imputation run
imputeParsDefaultMach = list(
	command1 = "cd '%s' ; mach1 -h %s -s %s --hapmapFormat -d %s -p %s --greedy -r 5 --prefix %s --autoFlip",
	command2 = "cd '%s' ; mach1 -h %s -s %s --hapmapFormat -d %s -p %s --crossover %s --errormap %s --greedy --mle --mldetails --prefix %s --autoFlip",
	hapfile = "CEU.chrCHR.hap",
	legendfile = "hapmap3.r2.b36.chrCHR.legend",

	runningDir = "global<<hapmapDir>>",

	idType = "idRs",
	window = 200,
	printOnly = F
);

#
#
#

imputeAndTestSnps = function(snps, datasets, outputPrefix, map = get(load(hapmapMapPath)[1])) {

	fi = function(snp, datasets, map)imputeAndTestSnpForDatasets(datasets, snp, map);
	tests = clapply(snps, fi, datasets = datasets, map = map, .clRunLocal = F);

	save(tests, file = sprintf("%s.RData", outputPrefix));
	testsr = list.key(tests, "test", unlist = F, template = list(p = NA, chiSq = NA, mu = NA, beta = NA));
	ts = data.frame(snp = RAsnpsAll, listOfLists2data.frame(testsr, idColumn = NULL));
	write.csv(ts, file = sprintf("%s.csv", outputPrefix));
	ts
}

#
#	<p> helper functions
#

# allow a path to be specified as "global<<var>>" where this string is substituted by the global
#	variable "var" thus allowing lazy evalutation of pathes (remote execution)
localPath = function(path) {
	if (is.null(path)) return(NULL);
	path = if (is.character(path)) path else {
		if (is.null(path$dir)) path$file else sprintf("%s/%s", path$dir, path$file)
	}
	pathVars = fetchRegexpr("(?<=global<<).*?(?=>>)", path);
	#if (length(pathVars) == 0) path = get(pathVar);
	#print(listKeyValue(pathVars, sapply(pathVars, get)));
	path = mergeDictToString(listKeyValue(sapply(pathVars, function(s)sprintf("global<<%s>>", s)),
		sapply(pathVars, get)), path);
	path
}

#
#	<p> hapmap/generic methods
#

gwasFetcher.Rmap = function(path, dir = NULL) {
	o = list(path = list(dir = dir, file = path));
	class(o) = "Rmap";
	o
}
# returns a data.frame with idRs and pos columns
fetchSnpMap.Rmap = function(self) {
	map = get(load(localPath(self$path))[1]);
	map
}

#
#	<p> plink functions/methods S3
#
plink.postProcessDataFrame = function(d0) {
	# recode response
	d0$response[d0$response == 0] = NA; 
	d0$response = d0$response - 1;
	d0
}
plinkSampleCols = c("idFam", "id", "idF", "idM", "sex", "response");
# rangeSpec: plink option to select snps
fetchSnps.plink.generic = function(self, path, rangeSpec, snps = NULL, format = 1, returnMap = F) {
	sp = splitPath(localPath(path));
	output = sprintf("%s", tempfile());
	cmd = sprintf("cd '%s' ; plink --noweb --bfile %s --recode --out %s %s",
		sp$dir, sp$file, output, rangeSpec);
	System(cmd, 4);
	d0 = read.table(sprintf("%s.ped", output), header = F, sep = " ", as.is = T);

	# plink tends to drops SNPs, therefore re-reading output
	m0 = read.table(sprintf("%s.map", output), header = F, sep = "\t", as.is = T);
	names(m0) = c("chr", self$idType, "posGen", "pos");
	snps = m0[[self$idType]];

	snpCols = paste(rep.each(snps, 2), c("1", "2"), sep = "_");
	nonSnps = plinkSampleCols;
	names(d0) = c(nonSnps, snpCols);
	d0[, snpCols][d0[, snpCols] == "0"] = NA;
	d1 = if (format == 1) {
		gts = extractGenotypes2(d0, snps);
		data.frame(d0[, nonSnps], gts);
	} else d0;
	d1 = plink.postProcessDataFrame(d1);
	r = list(data = d1, snps = snps, response = "response", map = if (returnMap) m0 else NULL);
	r
}

fetchSnpsByName.plink = function(self, path, snps, format = 1, returnMap = F,
	exclInd = NULL, exclMarkers = NULL) {
	snpSelectionList = sprintf("%s-snps.txt", tempfile());
	write.table(data.frame(snps = snps), file = snpSelectionList, quote = F, col.names = F, row.names = F);
	fetchSnps.plink.generic(self, path, sprintf("--extract %s", snpSelectionList),
		snps, format, returnMap);
}
fetchSnpsByRange.plink = function(self, path, chr, from, to, format = 1, returnMap = F) {
	fetchSnps.plink.generic(self, path, sprintf("--chr %s --from-kb %d --to-kb %d", chr, from, to),
		snps = NULL, format, returnMap);
}
fetchSnpsBySpecSingle.plink = function(self, path, spec, format = 1, returnMap = F) {
	d0 = if (!is.null(spec$names)) { fetchSnpsByName.plink(self, path, spec$names, format, returnMap)
	} else { fetchSnpsByRange.plink(self, path, spec$chr, spec$from, spec$to, format, returnMap) };
	d0
}
fetchSampleSingle.plink = function(self, path) {
	# read raw table
	s0 = read.table(sprintf("%s.fam", localPath(path)), header = F, sep = " ", as.is = T);
	names(s0) = plinkSampleCols;
	s0 = plink.postProcessDataFrame(s0);
	r = list(data = s0, snps = NULL, response = "response", map = NULL);
	r
}

gwasFetcher.plink = function(path, dir = NULL, idType = "idRs", mapPath = NULL, fetchMapLazily = T) {
	if (is.character(path))
		path = lapply(path,	function(p)list(dir = dir, file = p));
	o = list(path = path, idType = idType, mapPath = mapPath, map = NULL);
	class(o) = "plink";

	o$map = if (!fetchMapLazily) fetchSnpMap.plink(o) else NULL;
	o
}

fetchSnpMap.plink = function(self) {
	if (!is.null(self$map)) return(self$map);
	mapPath = localPath(if (is.null(self$mapPath)) {
		if (is.list(self$path)) sprintf("%s/%s.bim",
			self$path$dir, self$path$files[[1]]$file) else
		sprintf("%s.bim", self$path)
	} else self$mapPath);
	sp = splitPath(mapPath);
	if (sp$ext == "RData") {
		map = get(load(mapPath)[1]);
		if (!is.data.frame(map)) map = map$data;
	} else {
		map = read.table(mapPath, header = F, sep = "\t", as.is = T);
		names(map) = c("chr", self$idType, "posGen", "pos", "a1", "a2");
	}
	map
}

#
#	<p> mach functions/methods S3
#

# snp: snp to impute
# snpFetcher: fetcher for data set to be imputed
imputeSnpByName.mach = function(self, snpFetcher, snp, parameters = list(), fetchFromRange = F)
	with(c(merge.lists(self$pars, parameters), self), {
	# <p> substitutions
	runningDir = localPath(runningDir);

	# <p> get SNP information
	m = fetchSnpMap(map);
	snp = as.list(m[which(m[[idType]] == snp), ]);
	if (!length(snp[[idType]])) return(NA);	# SNP not found
	if (is.null(snp)) return(NA);
	iv = c(snp$pos - window * 1e3, snp$pos + window * 1e3);
	ivKb = iv %/% 1e3 + c(-1, 1);

	# <p> export hapmap SNPs in window
	chrSnps = m[[idType]][m$chr == snp$chr & m$pos >= iv[1] & m$pos <= iv[2]];
	idFile = sprintf("%s-rsIds", tempfile());
	write.table(data.frame(chrSnps), file = idFile, row.names = F, col.names = F, quote = F);
	idcsFile = sprintf("%s-idcs", tempfile());
	subLegend = sprintf("%s-legend", tempfile());
	command = sprintf("cd '%s' ; csv.pl -s %s --selectRowsBy=rs --selectRowsIds=%s --logRowNumbers=%s > %s",
		runningDir, legendfile, idFile, idcsFile, subLegend);
	command = mergeDictToString(list(CHR = snp$chr), command);
	System(command, printOnly = printOnly);

	subHapfile = sprintf("%s-hap", tempfile());
	command = sprintf("cd '%s' ; csv.pl -s %s --no-header --selectRowsByRowNumbers='%s' > %s-1 ;
		csv.pl -s %s-1 --transpose --no-header > %s",
		runningDir, hapfile, idcsFile, subHapfile, subHapfile, subHapfile);
	command = mergeDictToString(list(CHR = snp$chr), command);
	System(command, printOnly = printOnly);
	#subHapfile = "/tmp/Rtmp7YeKp0/file5e884adc-hap";

	# <p> export dataset SNPs in window
	pedPref = tempfile();	# prefix
	#	ped file
	d = if (fetchFromRange)
		fetchSnpsBySpec(snpFetcher, list(chr = snp$chr, from = ivKb[1] , to = ivKb[2]),
			format = 2, returnMap = T) else {
		# use the hapmap map
		dataMap = fetchSnpMap(snpFetcher);
		dataSnps = which.indeces(chrSnps, dataMap[[idType]]);
		fetchSnpsBySpec(snpFetcher, list(names = dataMap[[idType]][dataSnps]), idType = idType,
			format = 2, returnMap = T);
	}
	# reformat
	d$data$sex = vector.replace(d$data$sex, list("1" = "M", "2" = "F", "0" = NA));
	d$data = d$data[, -which.indeces(d$response, names(d$data))];	# omit response
	# write file
	pedFileData = sprintf("%s-mach.ped", pedPref);
	pedFileDataMap = sprintf("%s-mach.map", pedPref);
	write.table(d$data, file = pedFileData,
		col.names = F, sep = " ", row.names = F, quote = F);
	write.table(cbind("M", as.character(d$map[[idType]])), file = pedFileDataMap,
		col.names = F, sep = " ", row.names = F, quote = F);

	# <p> imputation commands
	parOutput = sprintf("%s-imp-pars", tempfile());
	command = sprintf(command1, runningDir, subHapfile, subLegend,
		pedFileDataMap, pedFileData, parOutput)
	command = mergeDictToString(list(CHR = snp$chr), command);
	System(command, printOnly = printOnly);

	output = sprintf("%s-mach", tempfile());
	command = sprintf(command2, runningDir, subHapfile, subLegend,
		pedFileDataMap, pedFileData, parOutput, parOutput, output)
	command = mergeDictToString(list(CHR = snp$chr), command);
	System(command, printOnly = printOnly);

	# <p> read results
	gts = read.table(sprintf("%s.mlprob", output), header = F);
	info = read.table(sprintf("%s.mlinfo", output), header = T);
	snpI = which(info$SNP == snp[[idType]]);
	# output has two introductory columns (probId, "ML_PROB")
	r = list(gts = gts[, c(2*snpI + 1, 2*snpI + 2)], info = info[snpI,])
	r
});

# paramters
#	window: surrounding distance in kb
gwasSnpImputer.mach = function(map = gwasFetcher.Rmap("global<<hapmapMapPath>>"),
	parameters = list(window = 200), idType = "idRs") {
	pars = merge.lists(imputeParsDefaultMach, parameters);
	o = list(map = map, pars = pars, idType = idType);
	class(o) = "mach";
	o
}

#
#	<p> generic methods
#

# use gwasFetch.class to generate an object to be passed to fetchSnps

postProcessSampleSingle = function(d0, spec) {
	if (!is.null(spec$response)) d0$data[[d0$response]] = spec$response;
	d0
}

#	f: fetcher
#	format: 1 or 2 column SNP format
fetchSampleSingle = function(self, path)UseMethod("fetchSampleSingle");
fetchSample = function(self) {
	if (is.character(self$path)) return(fetchSampleSingle(self, self$path));
	ds = lapply(self$path$files, function(f) {
		d0 = fetchSampleSingle(self, list(dir = self$path$dir, file = f$file));
		postProcessSampleSingle(d0, f);
	});
	d0 = ds[[1]];
	d0$data = rbindDataFrames(list.key(ds, "data", unlist = F), useDisk = T);
	d0
}
fetchSnpsBySpecSingle = function(self, path, spec, format = 1, returnMap = F)
	UseMethod("fetchSnpsBySpecSingle");
fetchSnpsBySpecRaw = function(self, spec, format = 1, returnMap = F) {
	if (is.character(self$path))
		return(fetchSnpsBySpecSingle(self, self$path, spec, format, returnMap));

	ds = lapply(self$path$files, function(f) {
		d0 = fetchSnpsBySpecSingle(self, list(dir = self$path$dir, file = f$file), spec, format, returnMap);
		postProcessSampleSingle(d0, f)
	});
	d0 = ds[[1]];
	d0$data = rbindDataFrames(list.key(ds, "data", unlist = F), useDisk = T);
	d0
}
#
#	fetch SNP from data set which might automatically impute the SNP, if missing
#
fetchSnpsBySpec = function(self, spec, format = 1, returnMap = F, idType = "idRs"){
	# translate SNP names to proper naming scheme
	if (idType != self$idType & !is.null(spec$names)) {
		snps = spec$names;
		map = fetchSnpMap(self);
		snpsProp = map[[self$idType]][which.indeces(snps, map[[idType]], ret.na = T)];	#proprietary names
		# check id translation
		snpsMissing = snps[is.na(snpsProp)];
		snps = snps[!is.na(snpsProp)];
		snpsProp = snpsProp[!is.na(snpsProp)];
		spec$names = snpsProp;
		print(spec);
	}

	d0 = fetchSnpsBySpecRaw(self, spec, format, returnMap);

	# translate back
	# extract names back as fewer snps might be returned than requested
	if (idType != self$idType) {	#  & !is.null(spec$chr)
		map = fetchSnpMap(self);
		snpsProp = d0$snps;
		snps = map[[idType]][which.indeces(snpsProp, map[[self$idType]], ret.na = T)];	#required names
		snps[is.na(snps)] = snpsProp[is.na(snps)];	# substitute missing names <!>
		snpsMissing = NULL;
	}
	if (idType != self$idType) {	# convert back to required names
		if (format == 1) {
			snpsPropCols = gsub("-", ".", snpsProp);
			snpsCols = gsub("-", ".", snps);
		} else if (format == 2) {
			snpsPropCols = gsub("-", ".", paste(rep.each(snpsProp, 2), c("1", "2"), sep = "_"));
			snpsCols = gsub("-", ".", paste(rep.each(snps, 2), c("1", "2"), sep = "_"));
		}
		names(d0$data)[which.indeces(snpsPropCols, names(d0$data))] = snpsCols;
		snpNames = map[[idType]][which.indeces(snps, map[[idType]])];
		d0$missing = snpsMissing;
		d0$snps = snps;
	}
	if (returnMap & idType != self$idType) {
		d0$map = cbind(snps, d0$map);
		names(d0$map)[1] = idType;
	}
	d0
}
fetchSnpsByName = function(self, snps, format = 1, returnMap = F, idType = "idRs")
	fetchSnpsBySpec(self, list(names = snps),
		format = format, returnMap = returnMap, idType = idType);
fetchSnpsByRange = function(self, chr, from, to, format = 1, returnMap = F, idType = "idRs")
	fetchSnpsBySpec(self, list(chr = chr, from = from, to = to),
		format = format, returnMap = returnMap, idType = idType);
imputeSnpByName = function(i, fd, snp, parameters) UseMethod("imputeSnpByName");
fetchSnpMap = function(m) UseMethod("fetchSnpMap");

#
#	<p> integrating functions
#

imputeSnpsByName = function(imputer, fetcher, snps, parameters = list()) {
	sample = fetchSample(fetcher);
	fImp = function(snp, imputer, fetcher, parameters)imputeSnpByName(imputer, fetcher, snp, parameters);
	d0 = clapply(snps, fImp, imputer = imputer, fetcher = fetcher, parameters = parameters);
	gts = cbindDataFrames(list.key(d0[!is.na(d0)], "gts", unlist = F));
	names(gts) = paste(rep.each(snps[!is.na(d0)], 2), c("i1", "i2"), sep = "_");
	quality = data.frame(snp = snps[!is.na(d0)],
		Rsq = list.key(list.key(d0[!is.na(d0)], "info", unlist = F), "Rsq"));
	r = merge.lists(sample, list(data = cbind(sample$data, gts), snps = snps[!is.na(d0)],
		missingSnps = snps[is.na(d0)], quality = quality));
	r
}

# creates a data set that contains a marker selection and imputes markers if necessary
createDataSetForMarkers = function(fetcher, imputer, snps, format = 1, idType = "idRs", .clRunLocal = F) {
	map = fetchSnpMap(fetcher);
	# observed SNPs
	snpsO = map[[idType]][which.indeces(snps, map[[idType]])];
	dO = fetchSnpsByName(fetcher, snpsO, format = format, idType = idType);
	# snps to be imputed
	snpsI = setdiff(snps, dO$snps);
	d0 = imputeSnpsByName(imputer, fetcher, snpsI, .clRunLocal = .clRunLocal);
	d0$data = merge(dO$data, d0$data)
	d0$snps = union(d0$snps, dO$snps);
	d0
}

#
#	<p> special functions
#

# estimate an r2 (correlation observed, imputed) under a lot of assumptions
# d is a 3x2 table with exepcted genotype counts
r2fromMaxPost = function(d, maxp) {
	if (is.na(maxp)) return(list(r2 = NA));
	# estimated mean posterior
	# assume same distribution for all genotypes
	meanPost = c(maxp, (1 - maxp) * maxp, 1 - maxp * ((1 - maxp) + 1));
	gtfs = apply(d, 2, sum) / sum(d);
	#rs = sapply(1:3, function(gt)(gtfs[gt] * (mxp - gtfs[gt]))/(gtfs[gt]*(1 - gtfs[gt])));
	rs = sapply(gtfs, function(gtf)((maxp - gtf))/(1 - gtf));
	r2 = sum(rs^2 * gtfs);
	r = list(r2 = r2, meanPost = meanPost);
	r
}

#
#	<p> generic functions for manipulating genomic data
#

# v0.1

plink.detectInputFormat = function(prefix) {
	sp = splitPath(prefix);
	fileset = grep.infixes(sp$base, list.files(sp$dir));
	exts = sapply(fileset, function(f)splitPath(f)$ext);
	r = if ('bed' %in% exts) list(
			format = 'bed', option = c('--bfile', prefix), ped = sprintf('%s.fam', prefix)) else
		if ('tped' %in% exts) list(
			format = 'tped', option = c('--tfile', prefix), ped = sprintf('%s.tfam', prefix)) else
		if ('ped' %in% exts) list(
			format = 'ped', option = c('--file', prefix), ped = sprintf('%s.ped', prefix)) else
	stop(sprintf('plink format for file \'%s\' is unknown', prefix));
	r = c(r, list(dir = sp$dir));
	r
}

plink.PedHeader = c('fid', 'iid', 'pid', 'mid', 'sex', 'y');

plink.runCommand = function(inputPrefix, outputPrefix = NULL, command = '--recode', options = c(),
	readOutput = F) {
	i = plink.detectInputFormat(localPath(inputPrefix));
	if (is.null(outputPrefix)) outputPrefix = tempfile('pedFileExport');
	cmd = sprintf("cd '%s' ; plink %s --noweb %s --out %s",
		i$dir, join(c(i$option, options), ' '), command, outputPrefix);
	System(cmd, 4);

	d0 = NULL;
	if (readOutput) {
		d0 = read.table(sprintf("%s.ped", outputPrefix), header = F, sep = " ", as.is = T);
		snps = read.table(sprintf("%s.map", outputPrefix), header = F, as.is = T)[, 2];
		snpCols = paste(rep.each(snps, 2), c("1", "2"), sep = "_");
		names(d0) = c(plink.PedHeader, snpCols);
		gts = extractGenotypes2(d0, snps);
		d0 = data.frame(d0[, (1:length(plink.PedHeader))], gts);
	}

	r = list(out = outputPrefix, data = d0);
	r
}

# assume unique ids in ped files
# for exclInd, exclMarkers expect pathes to files containting ids in separate lines wihtout header
plink.fetchSnpsByName = function(inputPrefix, snps, exclInd = NULL, exclMarkers = NULL) {
	# <p> write SNP list
	snpSelectionList = sprintf('%s-snpSelection', tempfile());
	write.table(data.frame(snps = snps),
		file = snpSelectionList, quote = F, col.names = F, row.names = F);

	# <p> write indivuals exclusion file
	exclIndPed = NULL;
	if (!is.null(exclInd)) {
		ids = read.table(exclInd, header = F, as.is = T)[, 1];
		i = plink.detectInputFormat(inputPrefix);
		ped = read.table(i$ped, header = F, as.is = T)[, 1:2];
		ped0 = ped[which.indeces(ids, ped[, 2]),];
		exclIndPed = sprintf('%s-exclusionPed', tempfile());
		write.table(ped0, file = exclIndPed, quote = F, sep = "\t", col.names = F, row.names = F);
	}

	options = c(
		if (is.null(exclMarkers)) '' else c('--exclude', exclMarkers),
		if (is.null(exclInd)) '' else c('--remove', exclIndPed),
		c('--extract', snpSelectionList)
	);
	r = plink.runCommand(inputPrefix, options = options, readOutput = T);
	r$data
}
