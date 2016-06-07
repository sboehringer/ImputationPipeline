#
#	gwas_imputation.R
#Tue Jul 16 18:01:18 CEST 2013

library('ggplot2');
source('gwas_manhattan.R');

qplot_qqunif = function() {
	# obs <- readfile; p-values only
	obs <- -log(runif(99000,0,1),10)
	N <- length(obs) # number of p-values

	# create the null distribution (-log10 of the uniform)
	null <- -log(1:N/N,10)
	MAX <- max(c(obs,null))

	# create the confidence intervals
	c95 <- rep(0,N)
	c05 <- rep(0,N)

	# the jth order statistic from a  uniform(0,1) sample has a beta(j,n-j+1) distribution
	# (Casella & Berger, 2002, 2nd edition, pg 230, Duxbury)

	for(i in 1:N){
	c95[i] <- qbeta(0.95,i,N-i+1)
	c05[i] <- qbeta(0.05,i,N-i+1)
	}

	# plot the two confidence lines
	plot(null, -log(c95,10), ylim=c(0,MAX), xlim=c(0,MAX), type="l",
	axes=FALSE, xlab="", ylab="")
	par(new=T)
	plot(null, -log(c05,10), ylim=c(0,MAX), xlim=c(0,MAX), type="l",
	axes=FALSE, xlab="", ylab="")
	# add the diagonal
	abline(0,1,col="red")
	par(new=T)

	qqplot(null,obs, ylim=c(0,MAX),xlim=c(0,MAX), main="Yay! It's a QQPlot")
}

manhattanPlot = function(data, output,  title = '', pp = list(width = 12, height = 6, dpi = 300))  {
	#mhtplot(mhpdf1, usepos = F);
	d = Df_(data, headerMap = list(chr = 'CHR', position = 'BP', P.value = 'P'));
	p = manhattan(d, title = title, max.y = 'max', genomewideline = -log10(5e-8),
		size.x.labels = 9, size.y.labels = 10);
	ggsave(output, width = pp$width, height = pp$height, dpi = pp$dpi);
	output
}
gwas_topTable = function(o, ps, output = NULL) with(o, {
	# top list
	ns = avu(sapply(names(ps), function(e)fetchRegexpr('\\Abeta1\\.(.*)', e, captures = T)), toNA = F);
	
	caption = sprintf('Top associations according to model ASS:MODEL1. Effect size parameters correspond to variables as follows: (%s) = (%s).',
		join(sapply(1:length(ns), function(i)sprintf('$\\beta_%d$', i)), ', '),
		join(sapply(ns, function(n)sprintf('$\\beta(%s)$', n)), ', ')
	);
	ns1 = c('marker', 'chr', 'position', 'allele_freq', 'impute_info');
	ns2 = c('P.value');
	psTopWide = ps[order(ps$P.value)[1:Ntop], ];
	psTop = psTopWide[, c(
		which.indeces(ns1, names(ps)),
		which(!is.na(avu(sapply(names(ps), function(e)fetchRegexpr('\\Abeta1\\.(.*)', e, captures = T))))),
		which.indeces(ns2, names(ps))
	)];
	nsH = c(ns1, ns, ns2);
	nsH = vector.replace(nsH, list(
		MARKER_dosage = 'snp',
		allele_freq = 'af',
		impute_info = '$R^2$'
	), regex = T);
	REP.tex('ASS:TABLE', report.data.frame.toString(
		psTop,
		digits = c(NA, 0, 0, 2, 2, rep(2, length(ns)), '#2'),
		names.as = nsH, quoteHeader = T,
		caption = caption
	), fmt = 'tiny');
	if (!is.null(output)) write.csv(psTopWide, file = output);
})

gwas_report = function(o, path, outputDir = splitPath(path)$dir, nrows = -1, .do.run = T) {
	#nrows = 1e2;	#<!><%> debugging
	#
	#	<p> qq-plot
	#
	# fix latex bug: only one '.' allowed per file name
	# all P-values <N>
	if (.do.run) {
		psA = read.csv(sprintf('%s.pvalues', path), nrows = nrows);
		pValues = psA$P.value[psA$P.value > 0];
		qq = ggplot_qqunif(pValues);
		#qq = ggplot_qqunif(psA$P.value);
		qqPath = sprintf('%s/%s-pvalues-QQ.jpg', outputDir, splitPath(path)$base);
		ggsave(qqPath, qq);
		REP.plot('QQ:ASSOCIATION', qqPath);
# 		REP.plot('QQ:ASSOCIATION', Qplot(sample = psA$P.value, dist = qunif,
# 			file = sprintf('%s/%s-pvalues-QQ.jpg', outputDir, splitPath(path)$base)));
		REP.tex('G:N_SNPs', nrow(psA));
	}

	#
	#	<p> read filtered results
	#
	ps = read.csv(path, nrows = nrows);

	#
	# <p> inflation
	#
	chisqs = qchisq(psA$P.value, df = 1, lower.tail = F);
	medianChisq = qchisq(.5, 1);
	#medianChisq = 0.4550757;	# median(rchisq(1e7, df = 1))
	inflation = (median(sqrt(chisqs), na.rm = T) / sqrt(medianChisq))^2;
	Log(sprintf('Inflation %.2f', inflation), 4);
	REP.tex('ASS:QQ:INFLATION', inflation, fmt = '.2');

	#
	# <p> table/table files
	#
	gwas_topTable(o, ps, output = sprintf('%s/%s-topSnps.csv', outputDir, splitPath(path)$base));

	#
	#	<p> manhattan plot
	#
	REP.plot('ASS:MANHATTAN',
		manhattanPlot(ps, title = 'Manhattan plot',
			output = sprintf('%s/%s-ass-manhattan.jpeg', outputDir, splitPath(path)$base)));
}

pipeDefaults = list(
	`G:Model` = '\\texttt{standard imputation}',
	`ASS:MODEL1` = '\\texttt{standard alternative, based on dosage}',
	`ASS:MODEL0` = '\\texttt{standard null, based on dosage}',
	`G:Title` = 'generic GWAS',
	`G:Analyst` = 'Statistical Genetics (MSTAT)',
	`G:Department` = 'see upstream analysis',
	`G:Email` = 'see upstream analysis'
);
pipelineParameter = function(pipe = NULL, parameter) {
	if (is.null(pipe)) return(pipeDefaults[[parameter]]);
	#p = System(sprintf('(pipeline.pl --print-parameters %s | grep %s)', splitPath(pipe)$file, parameter),
	#	patterns = 'cwd', cwd = splitPath(pipe)$dir, return.output = T);
	p = System(Sprintf('cat %{pipe}s | grep %{parameter}s'), return.output = T);
	par = fetchRegexpr(sprintf('\\A%s\\s+(.*)\\n', parameter), p$output, captures = T);
	r = if (!length(par)) pipeDefaults[[parameter]] else avu(par);
	r
}

reportPipeParameters = function(o, pipe) {
	model = pipelineParameter(pipe, 'G:Model');
	REP.tex('G:Title',
		firstDef(pipelineParameter(pipe, 'G:pipeline_files'), pipeDefaults[['G:Title']]));
	REP.tex('G:Analyst',
		firstDef(pipelineParameter(pipe, 'G:pipeline_operator'), pipeDefaults[['G:Analyst']]));
	REP.tex('G:Department', pipeDefaults[['G:Department']]);
	REP.tex('G:Email', pipeDefaults[['G:Email']]);
	REP.tex('G:Model', o$formula1, quote = T);
	#REP.tex('ASS:MODEL1', pipelineParameter(pipe, sprintf('%s:formula1', model)), quote = T);
	#REP.tex('ASS:MODEL0', pipelineParameter(pipe, sprintf('%s:formula0', model)), quote = T);
	REP.tex('ASS:MODEL1', o$formula1, quote = T);
	REP.tex('ASS:MODEL0', o$formula0, quote = T);
}

initializeReporting = function(o, path, pipe = NULL) {
	REP.tex('G:PATH', path, quote = T);
	REP.tex('G:Ntop', o$Ntop);
	reportPipeParameters(o, pipe = pipe);
	#REP.tex
}

defaults = list(
	Ntop = 1e2
);
gwas_impuatation_summary = function(o, path, outputDir = splitPath(path)$dir, resetCache = T, pipe = NULL) {
	o = merge.lists(defaults, o);
	o = merge.lists(o, propertyFromString(o$meta));
	scriptDirs = splitString(':', Sys.getenv('RSCRIPTS'));
	REP.new(
		file.locate('gwas/reportGwasImputed.tex', scriptDirs),
		cache = sprintf('%s/reportGWAS_cache_%s', outputDir, splitPath(path)$base),
		setup = file.locate('gwas/setup-report.tex', scriptDirs), latex = 'lualatex',
		#parameters = list(copy.files = file.locate('gwas/setup.tex', scriptDirs)),
		resetCache = resetCache
	);
	initializeReporting(o, path, pipe);
	gwas_report(o, path, outputDir = outputDir);

	REP.finalize(
		verbose = T, output = sprintf('%s/reportGwas-%s.pdf', outputDir, splitPath(path)$base),
		cycles = 3
	);
}
