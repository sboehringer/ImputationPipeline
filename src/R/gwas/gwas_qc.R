#
#	gwas_qc.R
#Fri Apr  1 14:29:54 CEST 2011

library('ggplot2');
source('Rgenetics.R');
source('gwas_plink.R');

#	Variable conventions
#		o: options
#		o$plink: options passed to system/qsub

#
#	<p> text patterns
#

text = list(
	mdsJudgement = list(
		outlier = 'The sample was judged not to contain distinct clusters. Rather outliers were removed based on the rule \\texttt{QC:SAMPLE:MDS:RULE}. QC:EXCL:excl_ind:mds_CNT individuals were identified by the rule ($QC:EXCL:excl_ind:mds\\%$). After outlier removal MDS analysis was repeated resulting in the following plots.',
		default = 'Information from the MDS plot was not used in the ensuing analysis.'
	),
	inbreedingJudgement = list(
		outlier = 'The sample was judged to contain outliers. Those were removed based on the rule \\texttt{QC:IND:INBREEDING:RULE}. QC:EXCL:excl_ind:inbreeding_CNT individuals were identified by the rule ($QC:EXCL:excl_ind:inbreeding\\%$).',
		default = 'Information from the inbreeding analysis was not used in the ensuing analysis.'
	),
	individual = list(
		remove = 'Indviduals identified as outliers were removed from the sample (QC:EXCL:excl_ind:mds_CNT).'
	),
	sample.clust = list(
		remove = 'Indviduals identified as outliers were removed from the sample (QC:SAMPLE:CLUST:OutlierCnt).'
	)
);

#
#	<p> QC initialization
#

qcInitialize = function(input, output = NULL, o = list()) with(o, {
	# report parameters
	REP.tex('QC:PAR:IMISS_CUTOFF', qcParIMissCutOff, fmt = 'percent');
	REP.tex('QC:PAR:SEX_REPCNT', qcParSex.RepN);
	REP.tex('QC:PAR:AF_CUTOFF', qcParAfCutoff, fmt = 'percent');
	REP.tex('QC:PAR:HWE_CUTOFF', sprintf('%.1e', qcParHweCutoff));
	REP.tex('QC:PAR:LMISS_CUTOFF', qcParMissingLcutoff, fmt = 'percent');
	REP.tex('QC:PAR:HWE_ZOOMQ', qcParHWEzoomQuantile, fmt = 'percent');
	REP.tex('QC:PAR:MDS:DIMS', qcParMDSDims);
	REP.tex('QC:PAR:ASS:TopN', assParTopN);
})

#
#	<p> checks on individuals
#

qcIwriteExclusions = function(prefix, inds) {
	save(inds, file = sprintf('%s.RData', prefix));
	write.table(inds, file = sprintf('%s.csv', prefix), col.names = F, row.names = F, quote = F);
	inds
}

qcJoinIdFiles = function(idFiles, o, column = 'id') with(o, {
	ids = c();
	for (f in idFiles) {
		path = mergeDictToString(list(OUTPUT_PREFIX = outputPrefixQc), f);

		if (splitPath(splitExtendedPath(path)$path)$nonempty) {
			idTab = readTable(path);
			if (nrow(idTab) > 0) ids = c(ids, idTab[[column]]);
		}
	}
	ids
})

qcImissing = function(input, output = NULL, o = list(), pattern = NULL) with(o, {
	prefix = sprintf('%s/%s', output, splitPath(input)$file);
	missIndPath = plinkMissing(input, output, plinkOptions(o, exclusions = 'baseline'), pattern);
	out = sprintf('%s-missing', prefix);	# output prefix

	# <p> indidivuals
	missInd = read.table(sprintf('%s.imiss', missIndPath), header = T, stringsAsFactors = F);

	outputP = sprintf('%s-imissing.jpg', out);	# out prefix
	REP.plot('QC:IND:missing',
		Qplot(missInd$F_MISS, geom = "histogram", xlab = 'missingness', file = outputP)
	);
	qcIndMiss = missInd$F_MISS > qcParIMissCutOff | is.nan(missInd$F_MISS);
	#qcIndMiss = nit(missInd$F_MISS > qcParIMissCutOff);
	qcIndMissExcluded = missInd$IID[qcIndMiss];
	REP.tex('QC:IND:MISS', fraction(qcIndMiss), fmt = 'percent');
	REP.tex('QC:IND:MISS_CNT', sum(qcIndMiss));
	# return excluded individuals
	Sow(missing = qcIndMissExcluded, sow_field = 'excl_ind');
});

qcIsex = function(input, output = NULL, d, o = list(), pattern = NULL) with(o, {
	sexPath = plinkSex(input, output, plinkOptions(o, exclusions = 'baseline'), pattern);
	prefix = sprintf('%s/%s', output, splitPath(input)$file);
	sex = read.table(sexPath, header = T, stringsAsFactors = F);
	names(sex) = vector.replace(names(sex), list(IID = 'id', SNPSEX = 'sexSNP'));

	sexM = data.frame.types(merge(sex, d), integer = c('sex', 'sexSNP'));
	#sexM$sexSNP[is.na(sexM$sexSNP)] = 0;
	#sexM$sex[is.na(sexM$sex)] = 0;
	qcSexMismatches = nif((sexM$sex == 1 & sexM$sexSNP == 2) | (sexM$sex == 2 & sexM$sexSNP == 1));
	qcIndSexExcluded = sexM$id[qcSexMismatches];

	# <!> mention ignoring of sex mismatches
	if (qcIgnoreSexCheck) {
		qcSexMismatches = F;
		qcIndSexExcluded = c();
	}
	sexMMFreq = fraction(qcSexMismatches);
	REP.tex('QC:IND:SEX_CNT', sum(qcSexMismatches));
	REP.tex('QC:IND:SEX', fraction(qcSexMismatches), fmt = 'percent');
	REP.tex('QC:IND:MIXUP', 2 * fraction(qcSexMismatches), fmt = 'percent');
	REP.vector('QC:IND:SEX_IDS', if (sexMMFreq == 0) '' else sexM$id[qcSexMismatches], max = qcParSex.RepN);
	Sow(sex = qcIndSexExcluded, sow_field = 'excl_ind');
})


qcIinbreeding = function(input, output = NULL, d, o = list(), pattern = NULL) with(o, {
	inbreedingPath = plinkInbreeding(input, output, plinkOptions(o, exclusions = 'baseline'), pattern);
	prefix = sprintf('%s/%s', output, splitPath(input)$file);
	output = sprintf('%s-inbreeding', prefix);	# output prefix
	inbreeding = read.table(inbreedingPath, header = T, stringsAsFactors = F);

	# <p> outliers
	qcInbreedingOutliers = if (!noe(o$qcParIinbreedingRule))
		eval(parse(text = qcParIinbreedingRule), envir = inbreeding) else c();
	qcIndInbredExcluded = inbreeding$IID[qcInbreedingOutliers];
	# <p> statistics
	qcInbreedingMean = mean(inbreeding$F, na.rm = T);
	alpha = 1 - qcParInbreedingCIlevel;
	qcInbreedingCIl = qnorm(alpha/2, qcInbreedingMean, sd(inbreeding$F, na.rm = T));
	qcInbreedingCIu = qnorm(1 - alpha/2, qcInbreedingMean, sd(inbreeding$F, na.rm = T));

	# <p> reporting
	REP.plot('QC:IND:inbreeding',
		Qplot(inbreeding$F, geom = "histogram", xlab = 'Inbreeding Coefficient F',
			file = sprintf('%s.jpg', output))
	);
	REP.tex('QC:IND:INBREEDING:JUDGEMENT',
		text$inbreedingJudgement[[firstDef(o$qcInbreedingOutlierJudgement, 'default')]]);
	REP.tex('QC:IND:INBREEDING:RULE', firstDef(o$qcParIinbreedingRule, ''));
	REP.tex('QC:IND:INBREEDING:OutlierCnt', sum(qcInbreedingOutliers));
	REP.tex('QC:IND:INBREEDING:Outlier', fraction(qcInbreedingOutliers), fmt = 'percent');
	REP.tex('QC:INBREEDING:MEAN', sprintf('%.2f', qcInbreedingMean));
	REP.tex('QC:INBREEDING:CIL', sprintf('%.2f', qcInbreedingCIl));
	REP.tex('QC:INBREEDING:CIU', sprintf('%.2f', qcInbreedingCIu));
	# return excluded individuals
	Sow(inbreeding = qcIndInbredExcluded, sow_field = 'excl_ind');
})

qcIremoveDuplicates = function(input, output, d, o) with(o, {
	REP.reportSubTemplateInitialize('technical_duplicates')
	# <p> no analysis done
	if (!exists('removeDuplicatesColumn')) {
		REP.reportSubTemplate('technical_duplicates', tag = 'void');
		REP.finalizeSubTemplate('technical_duplicates')
		return();
	}
	# <p> filter by missingness
	missIndPath = sprintf('%s/%s-missing', output, splitPath(input)$file);
	missInd = Df(read.table(sprintf('%s.imiss', missIndPath), header = T, stringsAsFactors = F),
		headerMap = list(IID = 'id', FID = 'fid.miss', 'F_MISS' = 'miss'));
	# prepare d
	d = merge(d, missInd, by = 'id', all.x = T);
	linked = !apply(apply(d[, c('id', removeDuplicatesColumn)], 1:2, is.na), 1, any);
	d = d[linked, ];
	# find duplicates
	ids = d[[removeDuplicatesColumn]];
	duplicates = sort(unlist(sapply(unique(ids), function(id) {
		if (sum(ids == id) > 1) id else NULL})));
	duplicatesIdcs = which.indeces(duplicates, d[[removeDuplicatesColumn]], match.multi = T);
	# removed column 2.10.2014 <A>
	#d0 = d[duplicatesIdcs, c(removeDuplicatesColumn, 'miss', 'fid.gt', 'id')];
	d0 = d[duplicatesIdcs, c(removeDuplicatesColumn, 'miss', 'id')];
	d0 = d0[order.df(d0, c(removeDuplicatesColumn, 'miss', 'id'), na.last = T), ];
	# remove but last of duplicated ids
	remove = unlist(lapply(duplicates, function(id)d$id[ids == id][order(d$miss[ids == id])][-1]));
	# write exclusion list
	Sow(technical_dupl = remove, sow_field = 'excl_ind');

	# <p> reporting
	REP.tex('QC:TECHNICAL_DUPLICATES:table', report.data.frame.toString(
		d0, digits = c(NA, 7, NA, NA), quoteHeader = T, ignoreRowNames = T,
		caption = 'List of technical replicates with missingness rates.'
	), fmt = 'tiny');
	REP.tex('QC:TECHNICAL_DUPLICATES:var', removeDuplicatesColumn, quote = T);
	REP.tex('QC:TECHNICAL_DUPLICATES:count',length(remove));
	REP.tex('QC:TECHNICAL_DUPLICATES:countPerc', length(remove)/dim(d)[1], fmt = 'percent');
	REP.tex('QC:TECHNICAL_DUPLICATES:listCount', qcParTechnicalDuplicatesListCount);
	REP.vector('QC:TECHNICAL_DUPLICATES:list', remove, max = qcParTechnicalDuplicatesListCount);
	REP.reportSubTemplate('technical_duplicates', tag = 'main');
	REP.finalizeSubTemplate('technical_duplicates');
})

heterocygosityFor = function(i, pathGenotypes,
	indExcl = NULL, markerExcl = NULL, tempDir = plinkTempDir) {
#	indExcl = get(load(file = qcIndExclPath)[1]),
#	markerExcl = get(load(file = qcMarkerExclPath)[1]), tempDir = plinkTempDir) {

	d0 = readData(pathG = sprintf('%s/%s%03d', tempDir, splitPath(pathGenotypes)$base, i),
		indExcl, markerExcl);
	# non-genotype columns
	idI = which.indeces('id', names(d0$gts));
	Ni = dim(d0$gts)[1];
	r = t(sapply(1:Ni, function(j) {
		gts = as.integer(d0$gts[j, -idI]);
		r = c(count(gts == 1, na.rm = T), count(!is.na(gts)));
		print(r);
		r
	}));
	r0 = data.frame(id = d0$gts$id[1:Ni], chunk = i, Nhet = r[, 1], N = r[,2]);
	r0
}

heterocygosity = function(Nchunks, pathGenotypes, qcIndExcl = NULL, qcMarkerExcl = NULL) {
#	qcIndExcl = get(load(file = qcIndExclPath)[1]),
#	qcMarkerExcl = get(load(file = qcMarkerExclPath)[1])) {
	# <p> heterocygosity counts
	hets = clapply(1:Nchunks, function(i, pathGenotypes, indExcl, markerExcl){
		heterocygosityFor(i, pathGenotypes, indExcl, markerExcl)
	}, pathGenotypes = pathGenotypes, indExcl = qcIndExcl, markerExcl = qcMarkerExcl,
	.clRunLocal = F);

	# <p> heterocygosity rates
	hetsDf = rbindDataFrames(hets, useDisk = T);
	ids = unique(hetsDf$id);
	hetCnt = t(sapply(ids, function(id) {
		hetN = apply(hetsDf[hetsDf$id == id, c('Nhet', 'N')], 2, sum);
	}));
	hetRate = data.frame(id = ids, hetRate = hetCnt[, 1]/ hetCnt[, 2], stringsAsFactors = F);
	hetRate
}

qcIheterocygosity = function(input, output = NULL, d, o = list(), pattern = NULL) with(o, {
	#hcs = heterocygosity(1, pathGenotypes, qcIndExcl, qcMarkerExcl);
	het = heterocygosity(assParNchunks, pathGenotypes, qcIndExcl, qcMarkerExcl);
	REP.plot('QC:MARKER:HWE', Qplot(het$hetRate, geom = 'histogram', xlab = 'heterocygosity',
		file = 'results/qc-individuals-heterocygosity.jpg'));
})

stringAbbreviate = function(str, N, postfix = '...') {
	abbr = substr(str, 1, N);
	r = ifelse(nchar(str) > N, Sprintf('%{abbr}s%{postfix}s'), str);
	r
}

qcExclusionSummary = function(N, o = list(), category = 'excl_ind', type = 'marginal',
	abbreviateColNames = T, abbreviateTo = 4) {
	# <p> raw calculations
	excl = readExclusionsRaw(o, category, type = type, do_union = F);
	i = intersectSetsCount(excl);
	if (abbreviateColNames) dimnames(i)[[2]] = stringAbbreviate(dimnames(i)[[2]], abbreviateTo);

	# <p> characteristics of exclusions
	exclBase = readExclusionsRaw(o, category, type = 'baseline', do_union = T);
	exclExt = readExclusionsRaw(o, category, type = 'external', do_union = T);
	Ncurated = N - length(exclBase);
	Nin 	 = N - length(exclExt);
	exclCum = unionCum(excl);
	exclPars = lapply(1:length(excl), function(i) {
		# <p> individual parameters
		name = names(excl)[i];
		Ne = length(excl[[i]]);
		# <!> 7.9.2016: Ncurated -> N (should not make a difference as N is from a curated dataset)
		perc = if (name == 'external') NA else Ne / N;
		Ncum = if (name == 'external') NA else (length(exclCum[[i]]) - length(exclExt));

		REP.tex(Sprintf('QC:EXCL:%{category}s:%{name}s_CNT'), Ne);
		REP.tex(Sprintf('QC:EXCL:%{category}s:%{name}s'), perc, fmt = 'percent');
		r = list(Ne = Ne, perc = perc, Ncum = Ncum);
		r
	});
	exclParsDf = Df(sapply(exclPars, identity), t_ = T, row.names = names(excl));
	exclParsDf1 = cbind(exclParsDf, i);
	exclParsDf1
}

qcIsummary = function(input, output = NULL, d, o = list(), pattern = NULL) {
	es = qcExclusionSummary(nrow(d), o, category = 'excl_ind', type = 'marginal');
	REP.tex('QC:EXCL:excl_ind:summary', report.data.frame.toString(
		es, digits = c(0, '%1', 0, rep(0, nrow(es))), bars = c(F, F, T),
		quoteHeader = T, ignoreRowNames = F,
		caption = 'Summary of exclusions of individuals for QC steps. {\\it Ne}: number of exclusions. {\\it perc}: percentage of exclusions from total sample after accounting for external exclusions. {\\it Ncum}: cumulative number of excluded individuals. The second part of the table contains pairwise overlap between QC steps.'
	));
	NbaselineExcl = length(readExclusionsRaw(o, 'excl_ind', type = 'baseline', do_union = T));
	REP.tex('QC:EXCL:excl_ind:baselineexcl_CNT', NbaselineExcl);
	Nbaseline = nrow(d);
	REP.tex('QC:EXCL:excl_ind:baseline_CNT', Nbaseline);
	Nexcl = unlist(es$Ncum[nrow(es)]);
	REP.tex('QC:EXCL:excl_ind:total_CNT', Nexcl);
	REP.tex('QC:EXCL:excl_ind:total', Nexcl/Nbaseline, fmt = 'percent');
	REP.tex('QC:EXCL:excl_ind:included_CNT', nrow(d) - Nexcl);
	NULL
}

qcIndividuals = function(input, output = NULL, d, o = list(), .do.run = T) {
	excl = readExclusionsInds(o, 'baseline');
	if (length(excl) > 0) d = d[-which.indeces(excl, d$id), ];
	if (.do.run) {
		qcImissing(input, output, o);
		qcIsex(input, output, d, o);
		qcIinbreeding(input, output, d, o);
		qcIremoveDuplicates(input, output, d, o);
	}
	qcIsummary(input, output, d, o);
}

#
#	<p> QC: Markers
#

qcMprepare = function(input, output = NULL, o = list(), pattern = NULL) {
	# <p> hwe and allele frequency analysis
	hwePath = plinkHwe(input, output, plinkOptions(o, exclusions = 'baseline'), pattern);
	hwe0 = read.table(hwePath, header = T, stringsAsFactors = F);
	hwe0 = hwe0[hwe0$TEST == 'ALL', ];
	hwe.test = unlist(clapply(hwe0$GENO, function(g)hwe.test.fast(as.integer(splitString("/", g)))$p));

	# allele frequencies
	afs = unlist(clapply(hwe0$GENO, function(g)afForSNP(as.integer(splitString("/", g)), minor = T)));

	# return
	hwe = data.frame(hwe0, af = afs, hwe = hwe.test);
	save(hwe, file = sprintf('%s-qc-markers-hwe.RData', o$outputPrefixQc));
	hwe
}

qcMreadMissing = function(input, output) {
	missPath = sprintf('%s/%s-missing.lmiss', output, splitPath(input)$file);	# output prefix
	lmiss = read.table(missPath, header = T, stringsAsFactors = F);
}

qcMmissing = function(input, output = NULL, o = list(), pattern = NULL) with(o, {
	prefix = sprintf('%s/%s', output, splitPath(input)$file);

	# <p> marker missingness
	missPath = plinkMissing(input, output, plinkOptions(o, exclusions = 'baseline'), pattern);
	#lmiss = read.table(sprintf('%s.lmiss', missPath), header = T, stringsAsFactors = F);
	lmiss = qcMreadMissing(input, output);
	qcMarkerMiss = lmiss$F_MISS > qcParMissingLcutoff | is.nan(lmiss$F_MISS);
	#qcMarkerMissExcl = if (!sum(qcMarkerMiss)) c() else lmiss$SNP[qcMarkerMiss];
	qcMarkerMissExcl = lmiss$SNP[qcMarkerMiss];
	Sow(missing = qcMarkerMissExcl, sow_field = 'excl_marker');

	REP.plot('QC:MARKER:MISS', Qplot(lmiss$F_MISS, geom = 'histogram',
		xlab = 'missingness',
		file = sprintf('%s-qc-markers-miss.jpg', prefix)));

	REP.setConditional('PLOT_MISSING_PER_CHROMOSOME', T);
	if (sum(qcMarkerMiss, na.rm = T) == 0) {
		REP.setConditional('PLOT_MISSING_PER_CHROMOSOME', F);
	} else {
		REP.plot('QC:MARKER:MISS_perChr', Qplot(
			lmiss$CHR[qcMarkerMiss],
			geom = 'histogram', binwidth = 1, xlab = "Chromosome",
			file = sprintf('%s-qc-markers-miss-perChr.jpg', prefix))
		);
	}
	REP.tex('QC:MARKER:DIST_NOTO_TO', 'not to');
})

qcMaf = function(input, output = NULL, o = list(), pattern = NULL) with(o, {
	prefix = sprintf('%s/%s', output, splitPath(input)$file);
	# <p> load from prepare step
	hwe = get(load(sprintf('%s-qc-markers-hwe.RData', outputPrefixQc))[1]);

	# <p> allele frequencies
	qcMarkerAf = hwe$af < qcParAfCutoff;
	qcMarkerAfExcl = hwe$SNP[is.na(hwe$af) | hwe$af < qcParAfCutoff];
	Sow(maf = qcMarkerAfExcl, sow_field = 'excl_marker');

	# <p> report
	REP.plot('QC:MARKER:AF', Qplot(hwe$af, geom = "histogram",
		xlab = 'frequency',
		file = sprintf('%s-qc-markers-af.jpg', prefix)));

	REP.plot('QC:MARKER:AF_qc', Qplot(hwe$af[hwe$af >= qcParAfCutoff], geom = "histogram",
		xlab = 'frequency',
		file = sprintf('%s-qc-markers-af_qc.jpg', prefix)));
})

qcMhwe = function(input, output = NULL, o = list(), pattern = NULL) with(o, {
	# <p> load from prepare step
	hwe = get(load(sprintf('%s-qc-markers-hwe.RData', outputPrefixQc))[1]);

	# <p> hwe
	hwe1 = hwe[hwe$af >= qcParAfCutoff & hwe$CHR <= 22, ];
	qcMarkerHWE = !is.na(hwe1$hwe) & hwe1$hwe < qcParHweCutoff;
	#qcMarkerHWE = nif(hwe1$hwe < qcParHweCutoff);
	qcMarkerHWEExcl = hwe1$SNP[qcMarkerHWE];
	Sow(hwe = qcMarkerHWEExcl, sow_field = 'excl_marker');
	# prune
	#hwe1 = hwe1[!qcMarkerHWE, , drop = F];
	# show QQ before pruning

	# <p> reporting
	#REP.plot('QC:MARKER:HWE', Qplot(sample = hwe1$hwe, dist = qunif,
	#	file = sprintf('%s-qc-markers-hwe_qq.jpg', outputPrefixQc)));
	plotPathQQ = Sprintf('%{outputPrefixQc}s-qc-markers-hwe_qq.jpg');
	plot_save(QQunif(hwe1$hwe, p.bottom = 1e-20), plot_path = plotPathQQ);
	REP.plot('QC:MARKER:HWE', plotPathQQ);

	qcParHWEzoom = qcParHWEzoomQuantile *  length(hwe1$hwe);
	REP.plot('QC:MARKER:HWEZ', Qplot((1:qcParHWEzoom)/(qcParHWEzoom * length(hwe1$hwe)),
		sort(hwe1$hwe)[1:qcParHWEzoom], xlab = 'expected', ylab = 'observed',
		file = sprintf('%s-qc-markers-hwe_qqz.jpg', outputPrefixQc)));

	REP.plot('QC:MARKER:HWEbyAF', Qplot(-log10(hwe1$af), -log10(hwe1$hwe),
		xlab = '-log10(af)', ylab = '-log10(hwe)',
		file = sprintf('%s-qc-markers-hweByAf.jpg', outputPrefixQc)));
	hwe2 = merge(hwe1, qcMreadMissing(input, output));
	REP.plot('QC:MARKER:HWEbyMiss', Qplot(
		-log10(minimax(hwe2$F_MISS, min = 1e-10)),
		-log10(hwe2$hwe),
		xlab = '-log10(Mmiss)', ylab = '-log10(hwe)',
		file = sprintf('%s-qc-markers-hweByMiss.jpg', outputPrefixQc)));

	REP.tex('QC:MARKER:HWE_JUDGEMENT', 'an excellent');
})

qcMarkerStatistics = function(input, o) {
	Nmarkers = nrow(plinkMapFile(input));
	Nbaseline = Nmarkers - length(readExclusionsRaw(o, 'excl_marker', type = 'baseline', do_union = T));
	NpostQc = Nmarkers - length(readExclusionsRaw(o, 'excl_marker', type = 'all', do_union = T));
	r = list(
		Nall = Nmarkers,
		Nbaseline = Nbaseline,
		NpostQc = NpostQc
	);
	r
}

qcMsummary = function(input, output = NULL, d, o = list(), pattern = NULL) with(o, {
	Nmarkers = nrow(plinkMapFile(input));
	REP.tex('QC:INCL:markers', Nmarkers);

	es = qcExclusionSummary(Nmarkers, o, category = 'excl_marker', type = 'marginal');
	REP.tex('QC:EXCL:excl_marker:summary', report.data.frame.toString(
		es, digits = c(0, '%1', 0, rep(0, nrow(es))), bars = c(F, F, T),
		quoteHeader = T, ignoreRowNames = F,
		caption = 'Summary of exclusions of markers for QC steps. {\\it Ne}: number of exclusions. {\\it perc}: percentage of exclusions from total sample after accounting for external exclusions. {\\it Ncum}: cumulative number of excluded markers The second part of the table contains pairwise overlap between QC steps.'
	));
	Nbaseline = Nmarkers - length(readExclusionsRaw(o, 'excl_marker', type = 'baseline', do_union = T));
	REP.tex('QC:EXCL:excl_marker:baseline_CNT', Nbaseline);
	Nexcl = unlist(es$Ncum[nrow(es)]);
	REP.tex('QC:EXCL:excl_marker:total_CNT', Nexcl);
	REP.tex('QC:EXCL:excl_marker:total', Nexcl/Nbaseline, fmt = 'percent');
	REP.tex('QC:EXCL:excl_marker:included_CNT', Nmarkers - Nexcl);
	NULL
})

qcMarkers = function(input, output = NULL, d, o = list(), .do.run = T) {
	if (.do.run) {
		qcMprepare(input, output, o);
		qcMmissing(input, output, o);
		qcMaf(input, output, o);
		qcMhwe(input, output, o);
	}
	qcMsummary(input, output, d, o);
}

#
#	<p> integrative QC
#

# <N> excl instead of exclusions because of 'o' overwrite
qcMDSperform = function(input, output = NULL, o = list(), pattern = 'qsub', .do.run = T) with(o, {
	prefix = sprintf('%s/%s', output, splitPath(input)$file);

	t0 = system.time({
		# include exclusions from earlier steps
		if (qcParMdsUsePrunedSnps) {
			# <p> locate most recent pruning step
			oPre = gwasMostRecentRun(o, 'qcParPruning', skip_this = T);
			prefix1 = Sprintf('%{outputBase}s/%{run}s/sowreap', run = oPre$runName);
			# <p> re-sow markers from previous round
			prunedMarkers = ReapFromDisk(prefix1, sow_field = 'excl_marker', fields = 'pruning');
			Sow(pruning = prunedMarkers, sow_field = 'excl_marker');
		}
		if (.do.run)
			plinkMds(input, output,
				c(plinkOptions(o, exclusions = 'pruned'), list(qsubOptions = qsubOptions)),
				Nsplit = firstDef(o$qcParMdsNsplit, 6),
				Ndim = qcParMDSDims, pattern = pattern)
	});
	print(t0);
	mds = read.table(sprintf('%s-mds.mds', prefix), header = T, sep = '');
	mds
})

qcMDSplot = function(mds, input, output = NULL, d, o = list()) with(o, {
	prefix = sprintf('%s/%s', output, splitPath(input)$file);
	d0 = merge(d, Df(mds, headerMap = list(IID = 'id')), by = 'id');
	d0 = data.frame(d0, mds_all = as.factor(0));
	colVars = firstDef(o$qcParMdsColorBy, 'mds_all');
	# <p> reporting
	if (0*0) {	# supplanted by ensueing implementation
	r = sapply(2:qcParMDSDims, function(i) {
		plotName = sprintf('QC:SAMPLE:MDS%d%d', i - 1, i);
		REP.plot(plotName,
			Qplot(d0[[sprintf('C%d', i - 1)]], d0[[sprintf('C%d', i)]],
				xlab = sprintf('MDS%d', i - 1), ylab = sprintf('MDS%d', i),
					file = sprintf('%s-qc-mds%d%d.jpg', prefix, i - 1, i)));
		sprintf('%s_plot', plotName)
	});
	}
	r = iterateModels_old(list(var = colVars, dim = 2:qcParMDSDims), function(var, dim) {
		i = dim;
		plotName = sprintf('QC:SAMPLE:MDS%d%d:%s', i - 1, i, var);
# 		mdsPlot = qplot(
# 			d0[[sprintf('C%d', i - 1)]], d0[[sprintf('C%d', i)]],
# 			colour = d0[[var]], data = d0,
# 			xlab = sprintf('MDS%d', i - 1), ylab = sprintf('MDS%d', i)
# 		);
		# <!> as of 23.5.2018, data = d0 argument generates error
		mdsPlot = qplot(
			d0[[sprintf('C%d', i - 1)]], d0[[sprintf('C%d', i)]],
			colour = d0[[var]],
			xlab = sprintf('MDS%d', i - 1), ylab = sprintf('MDS%d', i)
		);
		mdsPlot = mdsPlot + scale_colour_brewer(name = var, palette = 'Set1');
		REP.plot(plotName, Qplot(mdsPlot, file = sprintf('%s-qc-mds%d%d_%s.jpg', prefix, i - 1, i, var),
			pp = list(width = 10, height = 10, dpi = 150)));
		sprintf('%s_plot', plotName)
	}, .resultsOnly = T, .unlist = 1, lapply__ = lapply);
	r
})

# read mds from a certain run and plot
# outputDir expected to be base dir of the run
qcMDSreportPlots = function(input, run = NULL, o = list(), namePostfix = '') with(o, {
	if (is.null(run)) run = runName;
	Log(sprintf('Plotting MDS for run %s', run), 4);
	prefix = sprintf('%s/%s/qc/%s', outputBase, run, splitPath(input)$file);
	colVars = firstDef(o$qcParMdsColorBy, 'mds_all');
	# <p> options for the run
	or = propertyFromString(readFile(sprintf('%s/%s/options', outputBase, run)));
	name = sprintf('QC:SAMPLE:MDS:PLOTS%s', namePostfix);
	figureTables = sapply(colVars, function(var) {
		mdsPlots = sapply(2:or$qcParMDSDims, function(i) {
			plotName = sprintf('QC:SAMPLE:MDS%d%d:%s_plot%s', i - 1, i, var, namePostfix);
			REP.tex(plotName, path.absolute(sprintf('%s-qc-mds%d%d_%s.jpg', prefix, i - 1, i, var)));
			plotName
		});
		r = report.figure.table(mdsPlots, cols = o$qcParMdsFigureCols, width = .33);
		r
	});
	REP.tex(name, paste(figureTables, collapse = "\n"));
})

# null or empty
noe = function(x)(length(x) == 0 || is.null(x) || x == '');

qcMDS = function(input, output = NULL, d, o = list(), pattern = 'qsub', .do.run = TRUE) with(o, {
	prefix = sprintf('%s/%s', output, splitPath(input)$file);

	# <p> MDS
	REP.setConditional('MDS_PRE_PLOTS', F);
	# report plots from previous run
	if (runIndex > 1) {
		#previousRun = 'R01';	# <d><%>
		REP.setConditional('MDS_PRE_PLOTS', T);
		# <p> reporting: plots from previous run
		oMds = gwasMostRecentRun(o, 'qcParMdsRule', skip_this = T);
		qcMDSreportPlots(input, oMds$runName, o, '_PRE');
		prefix1 = sprintf('%s/%s/qc/%s', outputBase, oMds$runName, splitPath(input)$file);
		mds = read.table(sprintf('%s-mds.mds', prefix1), header = T, sep = '', as.is = T);
	}

	# <N> for the first run, we do not expect an outlier excludsion rule, we therefore assume mds to exist
	qcMdsOutliers = if (!noe(o$qcParMdsRule)) eval(parse(text = qcParMdsRule), envir = mds) else c();
	if (exists('mds')) {
		qcIndExcludedMds = mds$IID[qcMdsOutliers];
		Sow(mds = qcIndExcludedMds, sow_field = 'excl_ind');
	}

	# <p> re-run MDS if exclusion rule is present
	if (runIndex == 1 || firstDef(o$qcParMdsRule, '') != '' || firstDef(o$qcParMdsForce, '') != '') {
		mds = qcMDSperform(input, output, o, pattern = pattern, .do.run = .do.run);
		mds = data.frame.types(read.table(sprintf('%s-mds.mds', prefix), header = T, sep = ''),
			character = 'IID');
		qcMDSplot(mds, input, output, d, o);
		# <p> reporting: plots
		qcMDSreportPlots(input, runName, o);
	} else {
		REP.tex('QC:SAMPLE:MDS:PLOTS', '');
	}

	# <p> reporting
	REP.tex('QC:SAMPLE:MDS:RULE', firstDef(o$qcParMdsRule, ''));
	REP.tex('QC:SAMPLE:MDS:JUDGEMENT', text$mdsJudgement[[firstDef(o$qcMDSOutlierJudgement, 'default')]]);
	REP.tex('QC:SAMPLE:MDS:ACTION', text$individual[[firstDef(o$qcMDSOutlierAction, 'remove')]]);
})

reportFigureTable = function(nameTag, namesPlots, cols = 2) {
	figureTable = report.figure.table(sapply(namesPlots, function(n)sprintf('%s_plot', n)),
		cols = cols);
	REP.tex(nameTag, figureTable);
}

# <i> qcMDS, qcDist to use these functions
reportFigureTableFromFiles = function(nameFt, pathesFiles, cols = 2) {
	fnames = iterateModels_old(list(path = pathesFiles), function(i, path) {
		plotName = sprintf("%s_%04d", nameFt, i);
		REP.plot(plotName, path);
		plotName
	}, lapply__ = lapply)$results;
	reportFigureTable(nameFt, fnames, cols = cols);
}

# strict: run analysis only if some keys is defined in current options
runAnalysisConditionally = function(input, output, d, o, conditionalKeys, analysisFunction, ..., strict = F,
	strictFunction = function(e)(!is.null(e) && e != '')) {
	# most recent run with this key(s) defined
	# files_pre is set to files_this of previous run
	o = gwasMostRecentRunOptions(o, conditionalKeys);
	# <p> run only if keys were redefined
	do.run = (o$files_pre$runIndex == o$runIndex) &&
		(!strict || any(sapply(conditionalKeys, function(key)strictFunction(o[[key]]))));
	r = analysisFunction(input, output, d, o, ..., do.run = do.run);
	r
}

# if conditional key is defined in variable o analysis is run, otherwise most recent results are reported
reportFigureTableConditionally = function(input, output, d, o,
	conditionalKeys, analysisFunction, nameFigureTable = sprintf('%sft', sys.call()[[5]]), cols = 2, ...) {
	r = runAnalysisConditionally(input, output, d, o, conditionalKeys, analysisFunction, ...);
	reportFigureTableFromFiles(nameFigureTable, list.kp(r, 'outputFile', do.unlist = T), cols = cols);
}

qcIBStemplates = list(
	list(
		name = 'IBS01',
		outputFile = 'OUTPUT_PREFIX_genome_NAME.png',
		cmd = "zcat INPUT_PREFIX.genome.gz | ~/bin/plink.pl --genome2ibs | gnuplot -e 'set terminal png size 1600, 1600 ; set output \"OUTPUT_FILE\" ; set datafile separator \",\" ; set logscale xy ; plot  \"-\" using 1:2 with points ps 4 ; ' ",
		desc = 'IBS0 sharing vs. IBS1 sharing'
	),
	list(
		name = 'IBS02',
		outputFile = 'OUTPUT_PREFIX_genome_NAME.png',
		cmd = "zcat INPUT_PREFIX.genome.gz | ~/bin/plink.pl --genome2ibs | gnuplot -e 'set terminal png size 1600, 1600 ; set output \"OUTPUT_FILE\" ; set datafile separator \",\" ; set logscale xy ; plot \"-\" using 1:3 with points ps 4 ; ' ",
		desc = 'IBS0 sharing vs. IBS2 sharing'
	),
	list(
		name = 'IBS12',
		outputFile = 'OUTPUT_PREFIX_genome_NAME.png',
		cmd = "zcat INPUT_PREFIX.genome.gz | ~/bin/plink.pl --genome2ibs | gnuplot -e 'set terminal png size 1600, 1600 ; set output \"OUTPUT_FILE\" ; set datafile separator \",\" ; set logscale xy ; plot \"-\" using 2:3 with points ps 4 ; ' ",
		desc = 'IBS1 sharing vs. IBS2 sharing'
	),
	list(
		name = 'RE',
		outputFile = 'OUTPUT_PREFIX_genome_NAME.png',
		cmd = "zcat INPUT_PREFIX.genome.gz | ~/bin/plink.pl --genome2ibsRE | gnuplot -e 'set terminal png size 1600, 1600 ; set output \"OUTPUT_FILE\" ; set datafile separator \",\" ; set logscale xy ; plot \"-\" with points ps 4 ;' ",
		desc = 'IBS sharing vs. excess sharing'
	),
	list(
		name = 'IBS012',
		outputFile = 'OUTPUT_PREFIX_genome_NAME.png',
		cmd = "zcat INPUT_PREFIX.genome.gz | ~/bin/plink.pl --genome2ibs | gnuplot -e 'set terminal png size 1600, 1600 ; set output \"OUTPUT_FILE\" ; set datafile separator \",\" ; set logscale xy ; splot \"-\" with points ps 4 ; ' ",
		desc = 'IBS0 sharing vs. IBS1 sharing vs. IBS2 sharing'
	)
);

qcIBSFilterTemplates = list(
	list(
		name = 'IBS01',
		outputFile = 'OUTPUT_PREFIX-genome-NAME-exclusions.txt',
		cmd = "zcat INPUT_PREFIX.genome.gz | ~/bin/plink.pl --genome2ibs --genomeRule 'RULE' --genomeRuleOut  OUTPUT_FILE >/dev/null"
	),
	list(
		name = 'RE',
		outputFile = 'OUTPUT_PREFIX-genome-NAME-exclusions.txt',
		cmd = "zcat INPUT_PREFIX.genome.gz | ~/bin/plink.pl --genome2ibsRE --genomeRule 'RULE' --genomeRuleOut OUTPUT_FILE >/dev/null"
	)
);

# <!> ibs plotting is based on the most recent MDS run
# do.run is passed by runAnalysisConditionally
qcIBSanalyze = function(input, output = NULL, d, o = list(), pattern = 'qsub', do.run, .do.run = T,
	qcIBStemplates = NULL) with(gwasMostRecentRunOptions(o, c('qcParMdsRule', 'qcParMdsForce')), {
	r = lapply(qcIBStemplates, function(ibsTemplate) with (ibsTemplate, {
		# <p> substitute variables
		sd = list(INPUT_PREFIX = files_pre$outputPrefixQc, OUTPUT_PREFIX = outputPrefixQc, NAME = name);
		outputFile = mergeDictToString(sd, firstDef(ibsTemplate$outputFile, 'OUTPUT_PREFIX-ibs-NAME.txt'));
		ruleName = join(c('qcParIBSrule', name), sep = '');
		# <p> command
		cmdS = mergeDictToString(c(sd, list(
			OUTPUT_FILE = outputFile, NAME = name, RULE = firstDef(o[[ruleName]], '')
			)), cmd
		);
		rS = if (do.run && .do.run) {
			System(cmdS, 4, pattern = pattern,
				qsubOptions = sprintf('--outputDir %s/qsub %s', outputDirQc, o$qsubOptions))
		} else {
			# report previous files
			outputFile = mergeDictToString(
				merge.lists(sd, list(OUTPUT_PREFIX = files_pre$outputPrefixQc)), ibsTemplate$outputFile);
			list();
		}
		r = merge.lists(rS, list(outputFile = outputFile));
		r
	}));
	if (do.run && .do.run) System.wait(r, pattern = pattern);
	r
})

qcIBSFilterReport = function(o, d) with(o, {
	r = sapply(qcIBSFilterTemplates, function(template) with(template, {
		# <p> init
		key = join(c('qcParIBSrule', name), sep = '');
		rule.used = !is.null(o[[key]]) && o[[key]] != '';
		REP.setConditional(join(c('QC:SAMPLE:USED_IBS_RULE_', name), sep = ''), rule.used);
		if (!rule.used) return(NULL);
		sd = list(INPUT_PREFIX = files_pre$outputPrefixQc, OUTPUT_PREFIX = outputPrefixQc, NAME = name);
		outputFile = mergeDictToString(sd, firstDef(template$outputFile, 'OUTPUT_PREFIX-ibs-NAME.txt'));
		# <p> report
		key = join(c('qcParIBSrule', name), sep = '');
		REP.tex(join(c('QC:SAMPLE:IBS_RULE_', name), sep = ''), o[[key]]);
		t0 = if (splitPath(outputFile)$nonempty)
			readTable(sprintf("[HEADER=F,SEP=S,NAMES=fid;id]:%s", outputFile)) else
			data.frame(fid = 0, id = 0)[-1, ];
		pw = t0$id;	# ids for which pairwise comparisons were triggered by the rule
		REP.tex(join(c('QC:SAMPLE:IBS_RULE_PAIRWISE_', name, '_cnt'), sep = ''), length(pw)/2);

		# <p> exclusions
		ids = unique(pw);
		args = c(List(ids, names_ = Sprintf('ibs_%{name}s')), list(sow_field = 'excl_ind'));
		do.call(Sow, args);
		NULL
	}));
})

qcIBS = function(input, output = NULL, d, o = list(), pattern = 'qsub') {
	# <p> make IBS figures
	reportFigureTableConditionally(input, output, d, o,
		c('qcParMdsRule', 'qcParMdsForce'), qcIBSanalyze, 'QC:SAMPLE:IBS:ft', cols = 2,
		qcIBStemplates = qcIBStemplates
	);
	# <p> filter exclusions
	keys = paste('qcParIBSrule', list.kp(qcIBSFilterTemplates, 'name', do.unlist = T), sep = '');
	runAnalysisConditionally(input, output, d, o, keys, qcIBSanalyze,
		qcIBStemplates = qcIBSFilterTemplates, strict = T);
	qcIBSFilterReport(o, d);
}


# modified from sparcl package
plot.dendro = function(hc, y, main = "", branchlength = 0.7, labels = NULL,
	xlab = '', sub = '', ylab = "", cex.main = 1, varName = '', colorHeight = 16) {
	# as of R 2.15.2: '' -> FALSE
	if (is.null(labels))  labels = rep(FALSE, length(y))
	plot(hc, hang = -1, main = main,
		labels = labels, xlab = xlab, sub = sub, ylab = ylab, cex.main = cex.main);
	colors = 1:length(levels(as.factor(y)));
	yCols = colors[y[hc$ord]];

	for (i in 1:length(hc$ord)) {
		o = hc$merge[, 1] == -hc$ord[i] | hc$merge[, 2] == -hc$ord[i];
		#segments(i, hc$he[o] - branchlength, i, hc$he[o], col = yCols[i]);
		segments(i, 0, i, -colorHeight, col = yCols[i]);
	}
	lx = length(hc$ord) *.9;
	ly = max(hc$he);
	colorY = ly / 8;
	legend(lx, ly, legend = levels(as.factor(y)), title = varName, cex = 3, lty = 1, border = NULL, col = colors);
}

qcDistPlotsIterate = function(d, output, o = list(), types = c('ibs', 'ibd'),
	f = function(var, type, plotName, ...)NULL, ...) with(o, {
	colVars = firstDef(o$qcParHClustColorBy, o$qcParHclustColorBy, 'dist_all');
	clustTypes = firstDef(o$qcParHClustTypes, types, c('ibs', 'ibd'));
	prefix = sprintf('%s/%s', output, splitPath(input)$file);
	# <p> reporting
	distPlots = iterateModels_old(list(var = colVars, type = clustTypes), function(var, type) {
		plotName = sprintf('QC:SAMPLE:CLUST:%s:%s', type, var);
		f(var, type, prefix, plotName, ...)
	}, ..., .resultsOnly = T, .unlist = 1, lapply__ = lapply);
	distPlots
})

qcDistPlot = function(input, output = NULL, d, types = c('ibs', 'ibd'), o = list()) with(o, {
	# <p> join data
	ped = plinkPedFile(input);
	ped = data.frame(ped, idx = 1:dim(ped)[1]);
	d0 = merge(Df(ped, headerMap = list(iid = 'id')), d, all.x = T, by = 'id');
	d0 = data.frame(d0, dist_all = as.factor(0));
	d0 = d0[order(d0$idx), ];
	N0 = dim(d0)[1];

	distPlots = qcDistPlotsIterate(d, output, types, o = o,
		f = function(var, type, prefix, plotName, d0) {
		dist1 = as.matrix(
			read.table(sprintf('%s.genome.%s', prefix, type), header = F, sep = ',',
				as.is = T, row.names = NULL)
		);
# 		v = which(is.nan(dist1[1,]));
# 		dist2 = as.dist(dist1[-v, -v]);
# 		d0 = d0[-v, ];	# ped files
		# <!> iterative NA cleaning
		repeat {
			cntNa = avu(apply(dist1, 1, function(r)sum(is.na(r))));
			if (length(cntNa) == 0 || all(cntNa == 0)) break;
			rmNa = which(cntNa == max(cntNa));
			dist1 = dist1[-rmNa, -rmNa];
			d0 = d0[-rmNa, ];	# ped files
		}
		dist2 = as.dist(dist1);
		# <i> report excluded count
		Nrm = N0 = dim(d0)[1];
		
		# <p> cluster analysis
		cl = hclust(dist2, method = 'ward');
		# <!> changed in 3.0.3
		# cl = hclust(dist2, method = 'ward.D');
		#dendro = as.dendrogram(cl);
		REP.plot(plotName,
			Plot(cl, y = d0[[var]], file = sprintf('%s-hclust-%s.jpg', prefix, type),
				f = plot.dendro, .plotType = 'jpeg',
				main = sprintf('Hierarchical clustering of %s distances based on the Ward statistic', type),
				xlab = '', ylab = 'Height', cex.main = 4, varName = var
			)
		);
		sprintf('%s_plot', plotName)
	}, d0 = d0)
	distPlots
})

qcDistPlotReport = function(o) with(o, {
	distPlots = qcDistPlotsIterate(d, o$outputDirQc, o,
		f = function(var, type, prefix, plotName) {
		REP.plot(plotName, sprintf('%s-hclust-%s.jpg', prefix, type));
		sprintf('%s_plot', plotName)
	});
	clustTypes = firstDef(o$qcParHClustTypes, c('ibs', 'ibd'));
	figureTable = report.figure.table(distPlots, cols = length(clustTypes));
	REP.tex('QC:SAMPLE:CLUST:ft', figureTable);
})

qcDist = function(input, output = NULL, d, o = list(), pattern = 'qsub', .do.run = nit(o$qcPerformDistRun)) {
	prefix = sprintf('%s/%s', output, splitPath(input)$file);

	oMds = gwasMostRecentRun(o, 'qcParMdsRule');
	genome = sprintf('%s/%s.genome', oMds$outputDirQc, splitPath(input)$file);
	if (!file.exists(sprintf('%s.gz', genome)))
		stop(sprintf('Genome file %s not found. qcMDS has to be run beforehand.', genome));

	# <p> distance matrices only if need be
	if (oMds$runIndex == o$runIndex) {
		r = lapply(c('ibs', 'ibd'), function(type) {
			cmd = sprintf('plink.pl --genome2dist %s.gz --distType %s %s', genome, type, input);
			if (.do.run) System(cmd, 4, pattern = pattern, qsubOptions = o$qsubOptionsBigMem);
		});
		if (.do.run) System.wait(r, pattern = pattern);
		distPlots = qcDistPlot(input, output, d, o = o);
		qcDistPlotReport(o);
	} else {
		qcDistPlotReport(oMds);
	}
	qcClustOutliers = if (!noe(o$qcParClustRule)) eval(parse(text = qcParClustRule), envir = cl) else c();
	REP.tex('QC:SAMPLE:CLUST:ACTION', text$sample.clust[[firstDef(o$qcClustOutlierAction, 'remove')]]);
	REP.tex('QC:SAMPLE:CLUST:OutlierCnt', sum(qcClustOutliers));
	REP.tex('QC:SAMPLE:CLUST:Outlier', fraction(qcClustOutliers), fmt = 'percent');

	# dysfunctional for now (10/2012)
	if (F) {
	# <p> heatmaps
	r = sapply(c('ibs', 'ibd'), function(type) {
		dist1 = as.matrix(
			read.table(sprintf('%s.%s', genome, type), header = F, sep = ',', as.is = T, row.names = NULL)
		);
		v = which(is.nan(dist1[1,]));
		dist2 = as.dist(dist1[-v, -v]);
		cl = hclust(dist2, method = 'ward');
		dendro = as.dendrogram(cl);
		plotName = sprintf('QC:SAMPLE:CLUST:%s', type);
		REP.plot(plotName, Plot(dendro, leaflab = 'none', file = sprintf('%s-hclust-%s.jpg', prefix, type)));
		plotName
	});
	}
	if (0*0) {
	hm = heatmap(dist2,
		Rowv = NA, Colv = as.dendrogram(cl),
		symm = T, distfun = function(e)e,
		labRow = NULL, labCol = NULL
	);
	}
}

qcIntegrativeSummarizeExclusions = function(d, o, exclusionFiles = qcIntegrativeIndExclusions) with(o, {
	Sow(all = readExclusionsInds(o, 'all'), sow_field = 'excl_ind');
	Sow(all = readExclusionsMarkers(o, 'all'), sow_field = 'excl_marker');

	es = qcExclusionSummary(nrow(d), o, category = 'excl_ind', type = 'all', abbreviateTo = 2);
	REP.tex('QC:EXCL:excl_ind:all', report.data.frame.toString(
		es, digits = c(0, '%1', 0, rep(0, nrow(es))), bars = c(F, F, T),
		quoteHeader = T, ignoreRowNames = F,
		caption = 'Summary of exclusions of individuals after QC steps. {\\it Ne}: number of exclusions. {\\it perc}: percentage of exclusions from total sample. {\\it Ncum}: cumulative number of excluded individuals. The second part of the table contains pairwise overlap between QC steps.'
	), fmt = 'tiny');
	NmarginalExcl = length(readExclusionsRaw(o, 'excl_ind', type = 'marginal', do_union = T));
	REP.tex('QC:EXCL:excl_ind:marginal_CNT', NmarginalExcl);
	Nbaseline = nrow(d);
	REP.tex('QC:EXCL:excl_ind:baselineall_CNT', Nbaseline);
	Nexcl = unlist(es$Ncum[nrow(es)]);
	REP.tex('QC:EXCL:excl_ind:excl_all_CNT', Nexcl);
	REP.tex('QC:EXCL:excl_ind:excl_all', Nexcl/Nbaseline, fmt = 'percent');
	REP.tex('QC:EXCL:excl_ind:included_all_CNT', nrow(d) - Nexcl);
	REP.tex('QC:EXCL:excl_ind:included_all', (nrow(d) - Nexcl)/Nbaseline, fmt = 'percent');
	NULL
})

qcPruneSnps = function(input, output, o, pattern = 'qsub', .do.run = T) with(o, {
	if (!.do.run)return(NULL);
	outputDir = sprintf('%s/pruning', outputDirQc);
	r = lapply(1:23, function(chr) {
		plinkPrune(input, outputDir = outputDir, plinkOptions(o, exclusions = 'baseline'),
			windowSize = qcParPruning$windowSize, windowShift = qcParPruning$windowShift,
			thresholdVIF = qcParPruning$thresholdVIF,
			chr = chr, pattern = pattern
		)
	});
	System.wait(r, pattern = pattern);

	snpsExcl = avu(sapply(list.key(r, 'output'), readTable));
	# <p> remove chromosomes 23-26
	m = plinkMapFile(o$input);
	snpsChrSpecial = m$id[m$chr %in% c(23:26)];

	# <p> pruned snps
	snpsExcl = union(snpsExcl, snpsChrSpecial);
	Sow(pruning = snpsExcl, sow_field = 'excl_marker');
	NULL
})

qcIntegrative = function(input, output = NULL, d, o = list(), .do.run = T)  {
	if (.do.run) {
		if (o$runName == 'R01' && nit(o$qcPerformPruning)) qcPruneSnps(input, output, o);
		gc();
		if (nit(o$qcPerformMDS))	qcMDS(input, output, d, o);
		gc();
		if (nit(o$qcPerformDist))	qcDist(input, output, d, o);
		gc();
		if (nit(o$qcPerformIBS))	qcIBS(input, output, d, o);
		gc();
	}
}

#
#	<p> summarize exclusions
#

qcSummarize = function(o, d) {
	qcIntegrativeSummarizeExclusions(d, o);
}

#
#	<p> all QC
#

qcAll = function(input, output = NULL, d, o = list()) {
	# <p> preparation
	output = o$outputDirQc;
	if (!file.exists(output)) dir.create(output);
	# <p> QC steps
	qcInitialize(input, output, o);
	# null is true
	if (nit(o$qcIndividuals))	qcIndividuals(input, output, d, o);
	if (nit(o$qcMarkers))		qcMarkers(input, output, d, o);
	if (nit(o$qcIntegrative))	qcIntegrative(input, output, d, o);
	qcSummarize(o, d);
}
