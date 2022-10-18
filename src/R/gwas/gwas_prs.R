#
#	gwas_prs.R
#Wed Nov 21 11:40:52 CET 2018


# modelPattern: path Sprintf-template to create path to results file
prsCreateDb = function(
	modelPattern = '~/mnt/shark/exports/molepi/users/sboehringer/alpedpd-201710-assoc-R04/imputation_28/alpedpd_chr1_imputed-1_imputation_%{model2idx}02d',
	modelNo = 1,
	modelPath = Sprintf(modelPattern, model2idx = 2 * modelNo),
	pathResultsDBPattern = 'results/prs-%{run}s/prs-%{modelNo}d.sqlite',
	pathResultsDB = Sprintf(pathResultsDBPattern, modelNo = modelNo), run = 'R04') {

	# <p> IO preparation
	LogS(2, 'PRS db input: %{modelPath}s');
	LogS(2, 'PRS db output: %{pathResultsDB}s');
	Dir.create(pathResultsDB, treatPathAsFile = T);

	# <p> determine table structure
	rTop = read.table(modelPath, header = T, nrows = 1, sep = ',');
	nsNonNumeric = c('marker', 'chr', 'position',  'A0', 'A1'); 
	nsInteger = c('position');
	nsCharacter = setdiff(nsNonNumeric, nsInteger);
	nsNumeric = setdiff(names(rTop), nsNonNumeric);
	ns = list(recycle('integer', nsInteger), recycle('character', nsCharacter), recycle('numeric', nsNumeric));
	types = listKeyValue(list.kpu(ns, '[[2]]'), list.kpu(ns, '[[1]]'));
	sqlType = sqlTypeMap[unlist(types)];
	nsSel = names(rTop)[sapply(Regexpr('(sd|beta)0\\.', names(rTop)), function(e)length(e) == 0)];

	# <p> create SQLite DB
	csv2sqlite(modelPath, output = pathResultsDB, columnsSelect = nsSel,
		index = c('P.value', 'chr', 'position'), inputSep = 'C', inputHeader = T, types = types, newDb = T);
}


# approximate algorithms relying on sequential differences between SNP positions
pruneByDistanceRaw = function(r, dist = 1e5,
	colPvalues = 'P.value', colChr = 'chr', colPosition = 'position') {
	topO = r[order.df(r, c(colChr, colPosition)), ];

	diff = c(0, shift(topO[[colPosition]]) - pop(topO[[colPosition]]));
	# 	diffChr = !c(shift(topO[[colChr]]) == pop(topO[[colChr]]), TRUE);
	# 	breaks = diff > diffCutoff | diffChr;
	# rely on negative diff on chromosome changes
	breaks = diff > dist | diff < 0;
	groups = cumsum(breaks);

	NPgroup = by(topO, factor(groups), nrow);
	IPgroup = c(0, pop(cumsum(NPgroup)));
	minPgroup = by(topO, factor(groups), function(d)which.min(d[[colPvalues]]));
	sel = avu(IPgroup + minPgroup);
	topOpruned = topO[sel, , drop = F];
	topOprunedO = topOpruned[order(topOpruned[[colPvalues]]), , drop = F];
	return(topOprunedO);
}

# <!> at least 2 rows
# pruning is iterative as only distance between neighboring pairs is considered, not with the lead SNP
pruneByDistance = function(r, dist = 1e5, colPvalues = 'P.value', NroundsMax = 15, seqOffset = 2) {
	pruneSeq = as.integer(10^(seq(log10(dist) - seqOffset, log10(dist), length.out = NroundsMax)));
	Nrows = nrow(r);
	for (i in pruneSeq) {
		LogS(3, "Pruning by dist: %{i}d; Starting with Nrows: %{Nrows}d");
		r = pruneByDistanceRaw(r, dist = i, colPvalues);
		if (nrow(r) == Nrows) break;
		Nrows = nrow(r);
	}
	return(r);
}

prsGetPrunedSnps = function(modelNo = 1,
	pathResultsDBPattern = 'results/prs-%{run}s/prs-%{modelNo}d.sqlite',
	pathResultsDB = Sprintf(pathResultsDBPattern, modelNo = modelNo), run = 'R04',
	PvalueCutoff = 0.01, diffCutoff = 1e5, ...) {

	db = sqliteOpen(pathResultsDB);
	topRaw = sqliteQuery(db, list(`P.value` = list('<', PvalueCutoff)));
	r = pruneByDistance(topRaw, dist = diffCutoff, ...);
	return(r);
}

prsPipelinePostfix = function(run = 'R04', modelNo = 1)Sprintf('selection-prs-%{run}s-model-%{modelNo}d');

prsStartExtractSnps = function(imputationPrefix, optionsFile, run = 'R04', modelNo = 1, snps) {
	postfix = prsPipelinePostfix(run, modelNo);
	snpFile = writeSNPcsv(snps$marker, imputationPrefix, postfix);
	rSelection = postGWASsnpSelectionByFile(snpFile, imputationPrefix,
		optionsFile = optionsFile, run = run, postfix = postfix);
}

prsDataPath = function(run, modelNo, template = 'results/prs/gts-model-%{run}s-model-%{modelNo}d.RData')
	Sprintf(template, run = run, modelNo = modelNo)

prsLoadExtractedSnps = function(imputationPrefix, optionsFile, run = 'R04', modelNo = 1) {
	postfix = prsPipelinePostfix(run, modelNo);
	dataSnps = postGWASsnpSelectionRead(imputationPrefix,
		optionsFile = optionsFile, run = run, postfix = postfix);
	rTopSnps = postGWASwriteSnpSelectionData(dataSnps, imputationPrefix,
		optionsFile = optionsFile, run = run, postfix = postfix);
	prsData = list(snps = dataSnps, data = rTopSnps);
	save(prsData, file = prsDataPath(run, modelNo));
	return(prsData);
}

prsFitGlmnet = function(f1, data) {
	r = glmnet_re(f1, data, returnGlmnet = T);
	return(c(List_(r, min_ = 'glmnet'), model = r$glmnet));
}

prsFitGlm = function(f1, data, ...) {
	return(list(model = glm(f1, data, ...)));
}

# dataSplittingBy: assume 1 to be training, 2 to be test
prsBuildModel = function(dPrs, f, modelNo = 1, dataSplittingBy = NULL, fitModel = prsFitGlmnet, ...,
	noSNPs = F, predictWithMM = TRUE) {
	d = dPrs$data[[modelNo]]$dosage$data;
	snps = if (noSNPs) c() else intersect(dPrs$snps[[modelNo]]$snps$snp, names(d));
	f1 = formulaWith(formula.response(f), c(formula.covariates(f), snps));

	mm = model_matrix_from_formula(f1, d);
	d0 = d[mm$indeces, ];

	# <p> training
	dTr = if (notE(dataSplittingBy)) d0[d0[[dataSplittingBy]] == 1, ] else d0;
	r = fitModel(f1, dTr, ...);

	# <p> testing
	dTest = if (notE(dataSplittingBy)) d0[d0[[dataSplittingBy]] == 2, ] else d0;
	mmTest = if (predictWithMM) model_matrix_from_formula(f1, dTest)$mm else dTest;
	predData = minimax(predict(r$model, mmTest, type = 'response'), 0, 1);
	return(list(model = r, prediction = predData, outcome = dTest[[formula.response(f)]]));
}
