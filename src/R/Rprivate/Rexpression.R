#
#	Rexpression.R
#

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

