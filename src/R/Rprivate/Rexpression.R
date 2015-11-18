#
#	Rexpression.R
#

library('gplots');

#
#	<p> standard input
#

# <p> switch to customer Ids
customIdsFromIllumina = function(ids, sampleFile = NULL, customerId = 'Customer_Sample_Name') {
	if (is.null(sampleFile)) return(ids);
	t0 = readTable(sampleFile);
	idsN = t0[[customerId]][match(ids, t0$Sample.ID)];
	idsN
}

expressionMatrixFromIllumina = function(d0,
	reId = '.(.*)\\.AVG_Signal', reSignal = '~ %AVG_Signal', rePval = '~ %Detection.Pval', nmProbeId = 'PROBE_ID', sampleFile = NULL, substIds = list(patt = '\\.', repl = '-')) {

	vars = all.vars(formula.re(reSignal, data = d0));
	varsP = all.vars(formula.re(rePval, data = d0));
	ids = fetchRegexpr(reId, vars, captures = T);
	if (!is.null(substIds)) ids = gsub(substIds$patt, substIds$repl, ids, perl = TRUE);
	if (!is.null(sampleFile)) ids = customIdsFromIllumina(ids, sampleFile);

	arr = array(c(avu(d0[, vars]), avu(d0[, varsP])), dim = c(dim(d0[, vars]), 2));
	dimnames(arr) = list(d0[[nmProbeId]], ids, c('signal', 'pval'));
	expr = new('ExpressionMatrixRaw', exprArr = arr);
	expr
}

ExpressionMatrixFromIllumina = function(d0,
	reSignal = '.(?<id>\\d+)\\.AVG_Signal', rePval = '.(?<id>\\d+)\\.Detection(?:\\.Pval)?$',
	nmProbeId = 'PROBE_ID', sampleFile = NULL, substIds = list(patt = '\\.', repl = '-')) {

	varsSignalRaw = matchRegex(reSignal, names(d0), globally = F, removeNonMatch = T);
	vars = unlist(varsSignalRaw$match);
	ids = unlist(varsSignalRaw$capture);
	varsPvalsRaw = matchRegex(rePval, names(d0), globally = F, removeNonMatch = T);
	varsP = unlist(varsPvalsRaw$match);
	idCheck = ids == unlist(varsPvalsRaw$capture);
	if (!all(idCheck)) {
		Logs('Ids inconsistent between signal and detection-pval. Ids: %{ids}s != %{ids1}s',
			ids = join(ids[!idCheck], ', '), ids1 = join(unlist(varsPvalsRaw$capture)[!idCheck], ', '),
			logLevel = 1);
	}
	if (!is.null(substIds)) ids = gsub(substIds$patt, substIds$repl, ids, perl = TRUE);
	if (!is.null(sampleFile)) ids = customIdsFromIllumina(ids, sampleFile);

	arr = array(c(avu(d0[, vars]), avu(d0[, varsP])), dim = c(dim(d0[, vars]), 2));
	dimnames(arr) = list(d0[[nmProbeId]], ids, c('signal', 'pval'));
	expr = new('ExpressionMatrixRaw', exprArr = arr);
	expr
}

readIllumina = function(path) {
	d0 = readTable(path);
	expressionMatrixFromIllumina(d0);
}

#
#	<p> classes
#

removeClass('ExpressionMatrixRaw')
ExpressionMatrixRawClass = setRefClass('ExpressionMatrixRaw',
	fields = list(
		expr = 'array'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(exprArr = array(), ...) {
		if (is.matrix(expr)) exprArr = array(expr, dim = c(dim(exprArr), 1));
		.self$initFields(expr = exprArr, ...);
		.self
	},
	samples = function() {
		dimnames(expr)[[2]]
	},
	probes = function() {
		dimnames(expr)[[1]]
	},
	setProbes = function(newProbeNames) {
		m = expr;
		dimnames(m)[[1]] = newProbeNames;
		ExpressionMatrixRawClass(expr = m)
	},
	setSamples = function(newSampleNames) {
		m = expr;
		dimnames(m)[[2]] = newSampleNames;
		ExpressionMatrixRawClass(expr = m)
	},
	Nprobes = function()nrow(expr),
	Nsamples = function()ncol(expr),
	Qnormalize = function(reference = NULL) {
		mref = if (is.null(reference)) expr[,, 'signal'] else expr[, reference, 'signal'];
		m1 = quantileNormalization(mref, expr[,,'signal']);
		# <!><i> more layers than two
		m2 = array(c(as.vector(m1), as.vector(expr[,, 'pval'])), dim = dim(expr));
		dimnames(m2) = dimnames(expr);
		ExpressionMatrixRawClass(expr = m2)
	},
	range = function(i = NULL) {
		d0 = if (is.null(i)) expr[,, 'signal'] else expr[, i, 'signal'];
		mn = min(d0, na.rm = T);
		mx = max(d0, na.rm = T);
		list(range = mx - mn, min = mn, max = mx)
	},
	missingness = function()apply(expr[,, 'signal'], 2, function(col)mean(is.na(col))),
	selectSamples = function(idcs) {
		new('ExpressionMatrixRaw', expr = expr[, idcs, , drop = FALSE])
	},
	selectProbes = function(probeNames) {
		idcs = na.omit(match(probeNames, probes()));
		new('ExpressionMatrixRaw', expr = expr[idcs,,, drop = FALSE])
	},
	getProbesMatrix = function(probeNames = NULL, layer = 'signal') {
		is = if (!is.null(probeNames)) match(probeNames, row.names(expr)) else 1:Nprobes();
		expr[is, , layer]
	},
	diagnostics_skewness = function() {
		s = apply(log(expr[,, 'signal']), 1, function(row)c(
			mean(row), median(row), Skewness(row), var(row), Noutliers(row)));
		r = Df_(Df(t(s), medianDiff = s[2, ] - s[1, ], sdStd = sqrt(s[4, ])/ifelse(s[1, ] == 0, 1e-4, s[1,])),
			names = c('mean', 'median', 'skewness', 'variance', 'Nout'));
		r
	},
	diagnostics = function() {
		list(skewness = diagnostics_skewness())
	},
	exprDensities = function() {
		ids = samples();
		dfs = lapply(1:Nsamples(), function(i)
			data.frame(Expression = log(expr[, i, 'signal']), Sample = as.factor(ids[i]))
		);
		ddf = do.call(rbind, dfs);

		Ncols = floor(length(ids) / 30);
		p = ggplot(ddf, aes(x = Expression, colour = Sample)) + geom_density() +
			theme_bw() + guides(colour = guide_legend(ncol = Ncols));
		p
	},
	pca = function(dataStandardize = TRUE) {
		prcomp(t(expr[,, 'signal']), retx = T, scale. = dataStandardize)
	},
	plotPcs = function(Npcs = 5, color = NULL, dataStandardize = TRUE) {
		pr = pca(dataStandardize);
		ps = lapply(2:Npcs, function(i) {
			x = pr$x[, i - 1];
			y = pr$x[, i]
			p = (if (!is.null(color)) qplot(x, y, color = color) else qplot(x, y)) + theme_bw()
			p
		});
		ps
	},
	diagnostics_plots = function() {
		diag = diagnostics();
		ps = list(
			qplot(medianDiff, mean, data = diag$skewness) + theme_bw(),
			qplot(skewness, mean, data = diag$skewness) + theme_bw(),
			qplot(sdStd, mean, data = diag$skewness) + theme_bw(),
			qplot(Nout, mean, data = diag$skewness) + theme_bw()
		);
		ps
	},
	Eapply = function(f_apply, ..., layer = 'signal', simplify = TRUE) {
		r = lapply(1:nrow(expr), function(i)f_apply(expr[i, , layer], ...));
		if (simplify) r = sapply(r, identity);
		r
	},
	apply_with_data = function(f_apply, data, ..., layer = 'signal') {
		r = .self$Eapply(function(probe_expr) {
			d0 = data.frame(probe_expr = probe_expr, data);
			f_apply(d0, ...)
		}, layer = layer);
		r
	},
	apply_to_matrix = function(f_apply, ..., layer = 'signal') {
		r = do.call(rbind, .self$Eapply(f_apply, ..., layer = layer));
		dimnames(r) = dimnames(expr);
		r
	},
	apply_with_data_to_matrix = function(f_apply, data, ..., is_transform = FALSE, col_names = NULL) {
		r = do.call(rbind, .self$apply_with_data(f_apply, data, ...));
		if (is_transform) dimnames(r) = dimnames(expr) else dimnames(r)[[1]] = dimnames(expr)[[1]];
		if (!is.null(col_names)) dimnames(r)[[2]] = col_names;
		r
	},
	regressOutVarsFormula = function(f_corr, data) {
		if (is.null(f_corr)) return(.self);
		f = as.formula(con('probe_expr', formula.to.character(f_corr)));
		exprRes = apply_with_data_to_matrix(
			function(data){residuals(lm(f, data = data))},
			data, is_transform = TRUE
		);
		ExpressionMatrixRawClass(exprArr = exprRes)
	},
	filterByPvalue = function(pval = .05) {
		pass = Eapply(function(r)any(r < pval), layer = 'pval', simplify = TRUE);
		exprFiltered = expr[pass, ,];
		ExpressionMatrixRawClass(exprArr = exprFiltered);
	},
	merge = function(exprOther, idPrefix = c('A', 'B')) {
		# <p> align probes
		probeIds = intersect(probes(), exprOther$probes());
		exprs = list(expr, exprOther$expr);
		d = dim(expr);
		dO = dim(exprOther$expr);
		exprsNew = lapply(seq_along(exprs), function(i) {
			e = exprs[[i]][probeIds,,, drop = F];
			dimnames(e)[[2]] = paste(idPrefix[i], dimnames(e)[[2]], sep = '');
			e
		});
		exprL = lapply(1:d[3], function(i)do.call(cbind, lapply(exprs, function(e)e[,,i])))
		exprNew = array(unlist(exprL), dim = c(d[1], d[2] + dO[2], d[3]));
		dimnames(exprNew) = list(probeIds,
			unlist(sapply(exprsNew, function(e)dimnames(e)[[2]])), dimnames(expr)[[3]]);
		new('ExpressionMatrixRaw', expr = exprNew)
	}

	#
	#	</p> methods
	#
	)
);
ExpressionMatrixRawClass$accessors(names(ExpressionMatrixRawClass$fields()));

plotFoldChange = function(r, output = NULL) {
	iInter = regexIdcs('Intercept', names(r));
	iAver = regexIdcs('AveExpr', names(r));
	Ngroups = iAver - iInter - 1;	# <!> reliance on table coding, get from design matrix
	groups = names(r)[(iInter + 1):(iInter + 1 + Ngroups)];
	pDfs = lapply(seq(Ngroups), function(i) {
		#Df(logBaseExpr = r[, 1], logFc = r[, 1 + i], group = groups[i])
		Df(logAveExpr = r[, 'AveExpr'], logFc = r[, iInter + i], group = groups[i])
	});
	pDf = do.call(rbind, pDfs);
	#pltFc = ggplot(data = pDf, aes(x = logFc, y = logBaseExpr, color = group)) +
	pltFc = ggplot(data = pDf, aes(x = logFc, y = logAveExpr, color = group)) +
		geom_point(alpha = .5) + theme_bw();
	if (!is.null(output)) plot_save(pltFc, plot_path = output);
	pltFc
}

plotPvalues = function(r0, output = NULL) {
	pltPval = ggplot(data = r0, aes(x = -log10(P.Value), y = AveExpr)) + geom_point(alpha = .5) + theme_bw();
	if (!is.null(output)) plot_save(pltPval, plot_path = output);
	pltPval
}
outputPostfixed = function(output, model, correct, data) {
	modelS = gsub('[~]', '-', formula.to.character(model));
	var1 = all.vars(formula.rhs(model))[1];
	ls = join(levels(as.factor(data[[var1]])), ';');
	correctS = gsub('[~ ]', '', formula.to.character(correct));
	o = Sprintf('%{output}s:%{modelS}s[%{ls}s]+%{correctS}s');
	r = gsub('[~ ]', '', o);
	r
}
Heatmap = function(m, ..., ColSideColors = NULL) {
	if (is.null(ColSideColors))
		heatmap.2(m, ...) else heatmap.2(m, ..., ColSideColors = ColSideColors)
}


removeClass('ExpressionAnnotated')
ExpressionAnnotatedClass = setRefClass('ExpressionAnnotated',
	fields = list(
		expr = 'ExpressionMatrixRaw',
		annotation = 'data.frame',
		probeAnnotation = 'data.frame',
		idCol = 'character'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(..., probeAnnotation = NULL) {
		idCol <<- 'id';
		.self$initFields(...);
		.self$setProbeAnnotation(probeAnnotation);
		.self
	},
	newWithExpression = function(exprReplacement, ...) {
		new('ExpressionAnnotated',
			expr = exprReplacement, annotation = annotation,
			probeAnnotation = probeAnnotation, idCol = idCol, ...
		);
	},
	copyReplace = function(newAnnotation = annotation, newExpr = expr,
		newProbeAnnotation = probeAnnotation, newIdCol = idCol, ...) {
		new('ExpressionAnnotated',
			expr = newExpr, annotation = newAnnotation,
			probeAnnotation = newProbeAnnotation, idCol = newIdCol, ...
		);
	},
	addVariables = function(data) {
		d0 = data.frame(id = row.names(data), data);
		annotationNew = base::merge(annotation, d0, by.x = idCol, by.y = 'id');
		new('ExpressionAnnotated',
			expr = expr,
			annotation = annotationNew,
			probeAnnotation = probeAnnotation,
			idCol = idCol
		);
	},
	selectProbes = function(probes) {
		probes = na.omit(probes);
		if (length(unique(probes)) != length(probes))
			warning(Sprintf('selectProbes: Trying to select duplicate probes [%{probeNs}s]',
				probeNs = join(probes[duplicated(probes)], ', ')));
		copyReplace(newExpr = expr$selectProbes(unique(probes)))
	},
	merge = function(exprA, idPrefix = c('A', 'B'), as_factor = NULL) {
		as = list(annotation, exprA$annotation);
		idCols = c(idCol, exprA$idCol);
		asNew = lapply(seq_along(as), function(i) {
			d = Df(as[[i]],
				expression_batch = idPrefix[i],
				expression_id_orig = as[[i]][[idCols[i]]],
				min_ = idCols[i]
			);
			Df(d, id = paste(idPrefix[i], as[[i]][[idCols[i]]], sep = ''), as_factor = 'expression_batch')
		});
		annotationNew = rbindDataFrames(asNew);
		annotationNew = Df_(annotationNew, as_factor = as_factor);
		idColNew = 'id';
		probeAnnotationNew = if (is.null(probeAnnotation)) exprA$probeAnnotation else {
			if (is.null(exprA$probeAnnotation))
				probeAnnotation else
				base::merge(probeAnnotation, exprA$probeAnnotation, by = 'probe')
		};
		# <N> symbol column from self gets prescendence
		probeAnnotationNew = Df_(probeAnnotationNew, headerMap = list(symbol.x = 'symbol'));
		exprNew = expr$merge(exprA$expr, idPrefix = idPrefix);
		new('ExpressionAnnotated',
			expr = exprNew,
			annotation = annotationNew,
			probeAnnotation = probeAnnotationNew,
			idCol = idColNew
		);
	},
	setProbeAnnotation = function(d) {
		if (missing(d) || is.null(d) || ncol(d) == 0) return(NULL);
		if (is.na(which(names(d) == 'probe'))) {
			probeAnnotation <<- NULL;
			warning(con('Probeannotation does not contain "probe" column (used for merging). ',
				'Provided object is ignored.'));
			return();
		}
		#ns = names(d);
		#i = which(ns == 'probe');
		#ns[-i] = paste('probeAnnotation', ns[-i], sep = '.')
		#names(d) = ns;
		probeAnnotation <<- d;
	},
	annotateProbes = function(tab, useRowNames = T) {
		m = if (useRowNames) data.frame(probe = row.names(tab)) else tab[, 'probe', drop = F];
		tabAnn = base::merge(m, probeAnnotation, all.x = T);
		tab0 = cbind(tab, Df_(tabAnn, min_ = 'probe'));
		tab0
	},
	analyzeDesign = function(design, coef = NULL, Nresult = NULL) {
		if (is.null(Nresult)) Nresult = nrow(expr$expr);
		l = lmFit(log(expr$expr[,, 'signal']), design);
		lBayes = eBayes(l)
		r = topTable(lBayes, coef = coef, number = Nresult);
		r
	},
	analyzeRaw = function(data, model, correct, ...) {
		# Create design
		# <p> confounders
		# remove intercept
		d0 = droplevels(data);
		dCorrect = if (missing(correct) || is.null(correct) || length(all.vars(correct)) == 0)
			matrix(nrow = nrow(d0), ncol = 0, dimnames = list(1:nrow(d0), NULL)) else
			model.matrix(model.frame(correct, data = d0), data = d0)[, -1, drop = F];

		# <p> covariates of interest
		dModel = model.matrix(model.frame(formula.rhs(model), data = data), data = data);
		is = intersect(as.integer(row.names(dCorrect)), as.integer(row.names(dModel)));
		design = cbind(
			dModel[match(is, as.integer(row.names(dModel))), , drop = FALSE],
			dCorrect[match(is, as.integer(row.names(dCorrect))), , drop = FALSE]
		);
		# run analysis
		a = new('ExpressionAnnotated',
			expr = expr$selectSamples(is), annotation = annotation,
			probeAnnotation = probeAnnotation)$analyzeDesign(
			design, coef = 1:ncol(dModel), ...
		);
		r = list(analysis = a, samples = is);
		r
	},
	diagnostics = function(output = NULL) {
		if (is.null(output)) return();
		ps = expr$diagnostics_plots();
		plot_save(plot_grid(ps, ncol = 2), plot_path = Sprintf("%{output}s-diagnostics.jpg"));
	},
	plotPcs = function(Npcs = 5, color = NULL, output = NULL, dataStandardize = TRUE) {
		colorData = annotation[match(expr$samples(), annotation[, idCol]), color];
		ps = expr$plotPcs(Npcs, color = colorData, dataStandardize = dataStandardize);
		if (!is.null(output))
			plot_save(plot_grid(ps, ncol = 2), plot_path = Sprintf("%{output}s-pca.jpg"));
		ps
	},
	exprDensities = function(output = NULL) {
		p = expr$exprDensities();
		if (!is.null(output)) plot_save(p, plot_path = Sprintf("%{output}s-densities.jpg"));
		p
	},
	# hh: household aka correction Genes
	pcaCorrectionGenes = function(hh, colorBy = NULL, output = NULL, Npcs = 5) {
		hhProbes = base::merge(hh[, 'symbol', drop = F], probeAnnotation, by = 'symbol', all.x = TRUE);
		exprHh = expr$selectProbes(na.omit(hhProbes$probe));
		exprPca = exprHh$pca();
		if (!is.null(output)) {
			colorData = if (!is.null(colorBy))
				annotation[match(exprHh$samples(), annotation[, idCol]), colorBy] else NULL;
			pcaPlts = exprHh$plotPcs(color = colorData, Npcs = Npcs);
			plot_save(
				plot_grid(pcaPlts, ncol = 2),
				plot_path = Sprintf("%{output}s-pca-correction-genes.jpg")
			);
			# relevance of PCs
# 			batchData = Df(exprPca$x[, 1:Npcs],
# 				batch = exprQ$annotation[match(exprHh$samples(), exprQ$annotation[, 'Oog_PA']), 'Set']);
# 			f0 = as.formula(con('~', paste('PC', 1:Npcs, sep = '', collapse = ' + '), ' + ', 'batch'));
# 			f = as.formula(con('probe_expr', formula.to.character(f0)));
# 			exprPvals = expr$apply_with_data_to_matrix(
# 				function(data, batchRegr){
# 					l0 = lm(batchRegr, data = data);
# 					list(coefficients(summary(l0))[-1, 'Pr(>|t|)'])
# 				},
# 				batchData, batchRegr = f
# 			);
		}
		exprPca
	},
	# computePCA: list of symbols on which to compute the PCA
	qualityControl = function(output = NULL, exprPval = 0.05, qNormalization = 1, Npcs = 5, color = NULL,
		computePCA = NULL, pcaColor = color) {
		if (!is.null(output)) Dir.create(output, recursive = TRUE, treatPathAsFile = TRUE);
		diagnostics(con(output, '-preQc'));
		plotPcs(Npcs = Npcs, color = color, output = con(output, '-preQc'));
		exprDensities(output = con(output, '-preQc'));
		expr1 = expr$filterByPvalue(exprPval);
		expr2 = expr1$Qnormalize(qNormalization);
		exprQ = newWithExpression(expr2);
		exprQ$diagnostics(con(output, '-postQc'));
		exprQ$plotPcs(Npcs = Npcs, color = color, output = con(output, '-postQc'));
		exprQ$exprDensities(output = con(output, '-postQc'));
		exprQ2 = if (!is.null(computePCA)) {
			pcaCorr = exprQ$pcaCorrectionGenes(computePCA, colorBy = pcaColor,
				output = con(output, '-postQc-pcaControlGenes'), Npcs = Npcs);
			exprQ$addVariables(pcaCorr$x[, 1:Npcs]);
		} else exprQ;
		exprQ2
	},
	analyze = function(model, correct, ..., doAttachProbeAnnotation = TRUE, output = NULL,
		doWrite = T, doHeatmap = T, heatmapN = 5e3, doDiagnostics = TRUE, heatmapStandardize = TRUE,
		correctBatch = ~ 1, Ntop = 1e3) {
		i = match(expr$samples(), annotation[[idCol]]);
		if (any(is.na(i))) stop('Sample ids do not match');
		d0 = annotation[i, ];
		row.names(d0) = NULL;
		if (!is.null(output)) Dir.create(output, recursive = TRUE, treatPathAsFile = TRUE);
		# <!> removed lapply: only accept single batchCorrection formula
		#r = lapply(correctBatch, function(batch) {
		formCorrect = formula.add.rhs(correct, correctBatch);
		output = outputPostfixed(output, model, formCorrect, annotation);
		a = analyzeRaw(d0, model, formCorrect, ...);
		r = Df_(a$analysis, headerMap = list(ID = 'probe'));
		if (doAttachProbeAnnotation) r = annotateProbes(r, useRowNames = TRUE);
		if (!is.null(output)) {
			write.csv(r[1:(min(Ntop, nrow(r))), , drop = F], file = Sprintf("%{output}s.csv"));
			if (doWrite) {
				bz = bzfile(Sprintf("%{output}s.csv.bz2"));
				write.csv(r, file = bz);
			}
			plotFoldChange(r, output = Sprintf("%{output}s-foldChange.jpg"));
			plotPvalues(r, output = Sprintf("%{output}s-pvalues.jpg"));
			if (doHeatmap) {
				m1 = m0 = expr$getProbesMatrix(row.names(r)[1:min(heatmapN, nrow(r))])[, a$samples];
				#m0 = expr$getProbesMatrix(r$probe[1:min(heatmapN, nrow(r))])[, a$samples];
				#m1 = if (heatmapStandardize) t(apply(m0, 1, standardize)) else m0;

				var1 = all.vars(formula.rhs(model))[1];
				colors = if (is.factor(d0[[var1]])) {
					groups = droplevels(d0[a$sample, var1]);
					Ngroups = length(levels(groups));
					cols = rainbow(Ngroups, start=0, end=.3);
					cols[as.integer(groups)]
				} else NULL
				plot_save(
					Heatmap(m0, scale = if (heatmapStandardize) 'column' else 'none',
						labRow = '', ColSideColors = colors),
					#Heatmap(m1, labRow = '', ColSideColors = colors),
					plot_path = Sprintf("%{output}s-heatmap.jpg")
				);
				
			}
			if (doDiagnostics) diagnostics(output);
		}
		r
	},
	analyzeModels = function(models, ..., doAttachProbeAnnotation = T,
		doWrite = T, doWriteModel = F, output = NULL, doDiagnostics = T) {
		rs = ilapply(models, function(model, i) {
			if (!is.null(output)) output = Sprintf("%{output}s-%{i}d");
			analyze(model = model$model, correct = model$correct, ...,
				doWrite = doWriteModel,
				doAttachProbeAnnotation = FALSE, doDiagnostics = FALSE, output = output)
		});
		rs1 = ilapply(rs, function(r, i) {
			r = as.data.frame(r);
			names(r) = paste(names(r), i, sep = '-');
			r[, 'probe'] = row.names(r);
			r
		});
		r = merge.multi.dfs(rs1, by = 'probe');
		if (doAttachProbeAnnotation) r = annotateProbes(r, useRowNames = F);
		if (!is.null(output)) {
			if (doWrite) write.csv(r, file = Sprintf("%{output}s.csv"));
			if (doDiagnostics) diagnostics(output);
		}
#		r = r[Order(ilapply(), ];
		r
	},
	analyzeModelList = function(modelList, output = NULL) {
		iterateModels(modelList, function(Npcs, groupMap, model, correct, Ntop, groupVar) {
			d = annotation;
			d[[groupVar]] = recodeLevels(d[[groupVar]], groupMap);
			exprN = copyReplace(newAnnotation = d);
			correctBatch = if (Npcs == 0) ~ 1 else formulaWith('', paste('PC', 1:Npcs, sep = ''));
			rAnal = exprN$analyze(
				model = model,
				correct = correct,
				correctBatch = correctBatch,
				output = output,
				Ntop = Ntop
			);
		});
	}
	#
	#	</p> methods
	#
	)
);
#ExpressionAnnotatedClass$accessors(names(ExpressionAnnotatedClass$fields()));

ExpressionAnnotatedNonParametricClass = setRefClass('ExpressionAnnotatedNonParametric',
	contains = 'ExpressionAnnotated',
	methods = list(
	analyzeRaw = function(data, model, correct, ...) {
		expr1 = expr$regressOutVarsFormula(correct, data);
		r = expr1$apply_with_data_to_matrix(function(data, model) {
			r = kruskal.test(model, data);
			c(r$p.value, r$statistic, r$parameter)
		}, data, model = model, col_names = c('P.value', 'T', 'df'));
		r = r[order(r[, 'P.value']), ];
		r
	}
	)
);

ExpressionMatrixClass = setRefClass('ExpressionMatrix',
	fields = list(
		expr = 'matrix',
		group = 'integer'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		.self$initFields(...);
		.self
	},
	samples = function() {
		dimnames(expr)[[2]]
	},
	probes = function() {
		dimnames(expr)[[1]]
	},
	nrows = function()expr$nrows(),
	expressionReduce = function(re = '%Signal') {
		d0 = as.data.frame(d$expr);
		vars = all.vars(formula.re(Sprintf('~ %{re}s'), data = d0));
		ExpressionMatrixClass(expr = as.matrix(d0[, vars]), group = group)
		.self
	},
	filterPval = function(cutoff = .05, re = '%Pval') {
		# <p> create data set for limma analysis
		vars = all.vars(formula.re(Sprintf('~ %{re}s'), data = as.data.frame(expr)));
		dp = d0[, vars];
		filter = apply(dp, 1, min) < cutoff;
		ExpressionMatrixClass(expr = expr[filter, , drop = F], group = group)
	},
	Qnormalize = function(re = NULL) {
		mraw = if (!is.null(re)) {
			vars = all.vars(formula.re(Sprintf('~ %{re}s'), data = as.data.frame(expr)));
			as.matrix(d0[, vars])
		} else expr;
		m1 = quantileNormalization(mraw, mraw);
		ExpressionMatrixClass(expr = m1, group = group)
	},
	range = function(i = NULL) {
		d0 = if (is.null(i)) expr else expr[, i];
		mn = min(d0, na.rm = T);
		mx = max(d0, na.rm = T);
		list(range = mx - mn, min = mn, max = mx)
	},
	missingness = function()apply(expr, 2, function(col)mean(is.na(col)))

	#
	#	</p> methods
	#
	)
);
ExpressionMatrixClass$accessors(names(ExpressionMatrixClass$fields()));

ExpressionMatricesClass = setRefClass('ExpressionMatrices',
	fields = list(
		matrices = 'list'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		.self$initFields(...);
		.self
	},
	N = function()length(matrices),
	Mnames = function()names(matrices),
	setNames = function(ns){ names(matrices) <<- ns },
	# <i> <!> not fully translated
	merge = function(reIid = '\\AX(\\d+)', prefix = 'U%{id}d.') {
		# <p> create unique sample names
		iids = lapply(ems, function(d)
			as.integer(FetchRegexpr(reIid, d$samples(), captures = T))
		);
		iidUnique = lapply(iids, unique);
		iidNcum = c(0, cumsum(sapply(iids, function(i)length(unique(i)))));
		dsN = ilapply(ems, function(d, i) {
			iid = iids[[i]];
			iidU = iidUnique[[i]];
			nsL = do.call(rbind, lapply(1:length(iidU), function(j) {
				idcs = which(iid == iidU[j]);
				nsN = paste(Sprintf(prefix, id = j + iidNcum[i]), d$samples()[idcs], sep = '');
				data.frame(i = idcs, name = nsN)
			}));
			#d$samples()[nsL$i] <<- as.character(nsL$name);
			d
		});

		# <p> align probes
		probeIds = lapply(dsN, function(d)data.frame(probe = d$probes(), idx = 1:d$nrows()));
		selDf = merge.multi.dfs(probeIds, by = 'probe', all = F, .first.constant = F);
		exprAligned = lapply(1:length(dsN), function(i)dsN[[i]]$expr[selDf[, i + 1], ]);

		# <p> combine data
		expr = do.call(cbind, exprAligned);
		group = do.call(c, list.kp(dsN, 'group'));
		ExpressionMatrix(group = group, expr = expr)
	},
	# assume data sets of same structure (#samples)
	#	draw overlayn histograms
	histCompareSample = function(xlab = '', title = 'Expression comparison', Nbins = 50, textsize = 5) {
		# <p> assume homogeneous sample structure
		Nsample = ncol(matrices[[1]]$expr);
		NsSamples = matrices[[1]]$samples();
		range1 = matrices[[1]]$range();
		ps = lapply(1:Nsample, function(i) {
			data = as.data.frame(sapply(1:N(), function(j)matrices[[j]]$expr[, i]));
			names(data) = Mnames();
			phist = histograms_alpha(data, x_lab = xlab, title = Sprintf(title, sample = NsSamples[i]),
				origin = range1$min, binwidth = range1$range/Nbins, relative = T, textsize = textsize);
			phist
		});
		ps
	}

	#
	#	</p> methods
	#
	)
);
ExpressionMatrixClass$accessors(names(ExpressionMatrixClass$fields()));


# run limma
#' @param expr: Expression data including design matrix
expressionAssociation = function(e) with(e, {
	design = if (!is.null(e$group)) {
		model.matrix( ~ group, data = data.frame(group = as.factor(group)))
	} else {
		# remove missing data
		missing = apply(design, 1, function(r)any(is.na(r)));
		design = design[!missing, , drop = F];
		expr = expr[, !missing, drop = F];
		# add intercept, treat columns as contrasts
		cbind(1, design);
	}
	r = lmFit(expr, design);
	rEb = eBayes(r);
	rEb
})

#
#	PCA correction
#

regressOutVars = function(expr, dCorr) {
	exprCorr = lapply(1:nrow(expr), function(i) {
		d0 = data.frame(y = expr[i, ], dCorr);
		f = as.formula(con('y ~ ', join(dimnames(dCorr)[[2]], ' + ')));
		lm0 = lm(f, data = d0);
		residuals(lm0)
	});
	exprRes = do.call(rbind, exprCorr);
	dimnames(exprRes) = dimnames(expr);
	exprRes
}
regressOutPCs = function(d, Npcs) {
	pr = prcomp(t(d$expr), retx = T);
	pcs = pr$x[, 1:Npcs, drop = F];
	d$expr = regressOutVars(d$expr, pcs);
	d
}
plotPcs = function(d, Npcs, Nlayout = Npcs, doLayout = T) {
	pr = prcomp(t(d$expr), retx = T);
	if (doLayout) layout(matrix(1:Nlayout, ncol = 2));
	sapply(1:Npcs, function(i)plot(pr$x[, c(i, i+1)], col = d$group + 1));
}

#
#	global association
#

associationGlobal = function(d, probe2entrez, minsize = 10, maxsize= 500, focuslevel = 3) {
	require('org.Hs.eg.db');
	require('globaltest');
	desc = new('AnnotatedDataFrame',
		data = data.frame(group = as.factor(d$group), row.names = dimnames(d$expr)[[2]])
	);
	dExprSet = ExpressionSet(d$expr, phenoData = desc)
	#exprs(dExprSet) = exprs(vsn2(dExprSet));
	r = gtGO(group, dExprSet, ontology = "BP",
		minsize = minsize, maxsize = maxsize, focuslevel = focuslevel,
		annotation = 'org.Hs.eg.db', probe2entrez = probe2entrez
	);
	r
}

entrezUnique = function(mapRaw = as.list(org.Hs.egSYMBOL2EG)) {
	entrezMap = lapply(mapRaw, function(e)e[1]);
	entrezMap
}
