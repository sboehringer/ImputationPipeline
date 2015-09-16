#
#	Rexpression.R
#


#
#	<p> classes
#

ExpressionMatrixRawClass = setRefClass('ExpressionMatrixRaw',
	fields = list(
		expr = 'matrix'
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
	Qnormalize = function(reference = NULL) {
		mref = if (is.null(reference)) expr else expr[, reference];
		m1 = quantileNormalization(mref, expr);
		ExpressionMatrixRawClass(expr = m1)
	},
	range = function(i = NULL) {
		d0 = if (is.null(i)) expr else expr[, i];
		mn = min(d0, na.rm = T);
		mx = max(d0, na.rm = T);
		list(range = mx - mn, min = mn, max = mx)
	},
	missingness = function()apply(expr, 2, function(col)mean(is.na(col))),
	selectSamples = function(idcs) {
		new('ExpressionMatrixRaw', expr = expr[, idcs, drop = FALSE])
	},
	getProbesMatrix = function(probeNames) {
		is = match(probeNames, row.names(expr));
		expr[is, ]
	},
	diagnostics_skewness = function() {
		s = apply(log(expr), 1, function(row)c(
			mean(row), median(row), Skewness(row), var(row), Noutliers(row)));
		r = Df_(Df(t(s), medianDiff = s[2, ] - s[1, ], sdStd = sqrt(s[4, ])/ifelse(s[1, ] == 0, 1e-4, s[1,])),
			names = c('mean', 'median', 'skewness', 'variance', 'Nout'));
		r
	},
	diagnostics = function() {
		list(skewness = diagnostics_skewness())
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
	Eapply = function(f_apply, ...) {
		r = lapply(1:nrow(expr), function(i)f_apply(expr[i, ], ...));
		r
	},
	apply_with_data = function(f_apply, data, ...) {
		r = .self$Eapply(function(probe_expr) {
			d0 = data.frame(probe_expr = probe_expr, data);
			f_apply(d0, ...)
		});
		r
	},
	apply_to_matrix = function(f_apply, ...) {
		r = do.call(rbind, .self$Eapply(f_apply, ...));
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
		ExpressionMatrixRawClass(expr = exprRes)
	}

	#
	#	</p> methods
	#
	)
);
ExpressionMatrixRawClass$accessors(names(ExpressionMatrixRawClass$fields()));

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
	setProbeAnnotation = function(d) {
		if (is.null(d)) return(NULL);
		ns = names(d);
		i = which(ns == 'probe');
		if (is.na(i)) {
			probeAnnotation <<- NULL;
			warning('Probeannotation does not contain "probe" column (used for merging)');
			return;
		}
		ns[-i] = paste('probeAnnotation', ns[-1], sep = '.')
		names(d) = ns;
		probeAnnotation <<- d;
	},
	annotateProbes = function(tab, useRowNames = T) {
		m = if (useRowNames) data.frame(probe = row.names(tab)) else tab[, 'probe', drop = F];
		tabAnn = merge(m, probeAnnotation);
		tab0 = cbind(tab, Df_(tabAnn, min_ = 'probe'));
		tab0
	},
	analyzeDesign = function(design, coef = NULL, Nresult = NULL) {
		if (is.null(Nresult)) Nresult = nrow(expr$expr);
		l = lmFit(log(expr$expr), design);
		lBayes = eBayes(l)
		r = topTable(lBayes, coef = coef, number = Nresult);
		r
	},
	analyzeRaw = function(data, model, correct, ...) {
		# Create design
		# <p> confounders
		# remove intercept
		dCorrect = if (missing(correct) || is.null(correct))
			matrix(nrow = length(i), ncol = 0, dimnames = list(1:length(i), NULL)) else
			model.matrix(model.frame(correct, data = data), data = data)[, -1, drop = F];

		# <p> covariates of interest
		dModel = model.matrix(model.frame(formula.rhs(model), data = data), data = data);
		is = intersect(as.integer(row.names(dCorrect)), as.integer(row.names(dModel)));
		design = cbind(
			dModel[match(is, as.integer(row.names(dModel))), , drop = FALSE],
			dCorrect[match(is, as.integer(row.names(dCorrect))), , drop = FALSE]
		);
		# run analysis
		new('ExpressionAnnotated',
			expr = expr$selectSamples(is), annotation = annotation,
			probeAnnotation = probeAnnotation)$analyzeDesign(
			design, coef = 1:ncol(dModel), ...
		);
	},
	diagnostics = function(output = NULL) {
		if (is.null(output)) return();
		ps = expr$diagnostics_plots();
		plot_save(plot_grid(ps, ncol = 2), plot_path = Sprintf("%{output}s-quality.jpg"));
	},
	analyze = function(model, correct, ..., doAttachProbeAnnotation = TRUE, output = NULL,
		doWrite = T, doHeatmap = T, heatmapN = 1e3, doDiagnostics = TRUE) {
		i = match(expr$samples(), annotation[[idCol]]);
		if (any(is.na(i))) stop('Sample ids do not match');
		d0 = annotation[i, ];
		row.names(d0) = NULL;
		r = analyzeRaw(d0, model, correct, ...);
		if (doAttachProbeAnnotation) r = annotateProbes(r);
		if (!is.null(output)) {
			if (doWrite) write.csv(r, file = Sprintf("%{output}s.csv"));
			if (doHeatmap) {
				m = expr$getProbesMatrix(row.names(r)[1:heatmapN]);
				plot_save(heatmap(m), plot_path = Sprintf("%{output}s-heatmap.jpg"));
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

