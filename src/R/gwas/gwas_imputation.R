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
	p = manhattan(d, title = title, max.y = 'max', significance = -log10(5e-8),
		size.x.labels = 9, size.y.labels = 10);
	ggsave(output, width = pp$width, height = pp$height, dpi = pp$dpi);
	output
}
# assume ps is already top-filtered
gwas_topTable = function(o, ps, tableOutput = NULL) with(o, {
	# <N> should be null operation
	psTop = ps[order(ps$P.value)[1:min(nrow(ps), Ntop)], ];

	# <p> columns
	nsBeta = avu(sapply(names(ps), function(e)fetchRegexpr('\\Abeta1\\.(.*)', e, captures = T)), toNA = F);
	ns1 = c('P.value', 'marker', 'chr', 'position', 'A0', 'A1', 'allele_freq', 'impute_info');

	# <p> confidence intervals
	cis = nlapply(nsBeta, function(beta, level = .95){
		ci = ciFromBetaSdev(psTop[[Sprintf('beta1.%{beta}s')]], psTop[[Sprintf('sd1.%{beta}s')]],
			level = level);
		r = cbind(ci$effect, ci$lower, ci$upper)
		dimnames(r)[[2]] = paste(c('beta', 'ciL', 'ciU'), beta, sep = '.');
		r
	});
	psCi = cbind(psTop[, ns1], do.call(cbind, cis));

	# <p> write tables
	Logs("Writing tables to %{tableOutput}s.", logLevel = 3);
	if (!is.null(tableOutput))
		writeTable(psCi, path = paste(tableOutput, c('.csv', '.xls'), sep = ''), row.names = F);

	#
	# <p> report tables
	#

	#	<p> association
	psP = psCi[, c(ns1, paste(c('beta', 'ciL', 'ciU'), 'MARKER_dosage', sep = '.'))];
	psPnames = c('P(snp)', 'marker', 'C', 'pos', 'A0', 'A1', 'af', '$R^2$',
		'$\\beta_M$', '$\\beta_{ML}$', '$\\beta_{MU}$');
	caption = con('Top associations according to model \\texttt{ASS:MODEL1}. ',
		'{\\it P(snp)} association P-value, {\\it af} allele frequency in complete data. ',
		'{\\it C} chromosome, {\\it A0/1} alleles, {\\it af} allele frequency. ',
		'$\\beta_M$: effect size of the marker, confidence bounds.');
	REP.tex('ASS:TABLE:P', report.data.frame.toString(psP,
		digits = c('#2', rep(NA, 5), 2, 2, rep(3, 3)),
		names.as = psPnames, quoteHeader = F, caption = caption
	), fmt = 'tiny');

	#	<p> table effect sizes
	psE = psCi[, c('P.value', 'marker', paste('beta', nsBeta, sep = '.'))];
	psEnames = c('P(snp)', 'marker',
		sapply(1:length(nsBeta), function(i)sprintf('$\\beta_{%d}$', i))
	);
	caption = sprintf(con(
		'Effect sizes for model \\texttt{ASS:MODEL1}. ',
		'Effect size parameters correspond to variables as follows: %s. '),
		join(ilapply(nsBeta, function(n, i)Sprintf("$\\beta_{%{i}d}$: \\texttt{%{n}s}",
			n = latex$quote(n))), ', ')
	);
	REP.tex('ASS:TABLE:Effects', report.data.frame.toString(psE,
		digits = c('#2', NA, rep(3, length(nsBeta))),
		names.as = psEnames, quoteHeader = F, caption = caption
	), fmt = 'tiny');
})

# columns: list(columnName = class, ...)
readCsvColumns = function(path, columns, nrows = -1) {
	t0 = read.csv(path, nrows = 1);
	colClasses = rep('NULL', ncol(t0));
	colClasses = vector.assign(colClasses,
		which.indeces(names(columns), names(t0)),
		as.vector(columns)
	);
	t1 = read.csv(path, colClasses = colClasses, nrows = nrows);
	t1
}

#
#	<p> read data from association pipeline
#		optimized for memory consumption and speed (files may be > 4G)
#		determine column types, only read needed columns
#	<i> integrate into readTable
#
readAssociation = function(path, nrows = -1) {
	readCsvColumns(path,
		list(marker = 'character', chr = 'integer', position = 'integer', P.value = 'numeric'),
		nrows = nrows
	)
}

gwas_report = function(o, path, outputDir = splitPath(path)$dir, nrows = -1, .do.run = T) {
	#
	#	<p> preparation
	#
	outputBase = Sprintf("%{outputDir}s/%{base}s", base = splitPath(path)$base);
	#	<p> read data
	ps = readAssociation(path, nrows = nrows);
	REP.tex('G:N_SNPs', nrow(ps));
	
	#nrows = 1e2;	#<!><%> debugging
	#
	#	<p> qq-plot
	#
	# fix latex bug: only one '.' allowed per file name
	# all P-values <N>
	qq = QQunif(ps$P.value[ps$P.value > 0]);
	qqPath = Sprintf('%{outputBase}s-pvalues-QQ.jpeg');
	ggsave(qqPath, qq);
	REP.plot('QQ:ASSOCIATION', qqPath);

	#
	# <p> inflation
	#
	chisqs = qchisq(ps$P.value, df = 1, lower.tail = F);
	medianChisq = qchisq(.5, 1);	#medianChisq = 0.4550757;	# median(rchisq(1e7, df = 1))
	inflation = (median(sqrt(chisqs), na.rm = T) / sqrt(medianChisq))^2;
	Log(sprintf('Inflation %.2f', inflation), 4);
	REP.tex('ASS:QQ:INFLATION', inflation, fmt = '.2');

	#
	# <p> create top table by file filtering
	#
	PvalueCutoff = Ceiling(sort(na.omit(ps$P.value))[o$Ntop], 9);
	filterExp = Sprintf('P.value < %{PvalueCutoff}e');
	outputTop = with(o, Sprintf('%{outputBase}s-topSnps-raw-%{Ntop}d.csv'));
	filterCommand = Sprintf("csv.pl --selectRowsByExpression '%{filterExp}s' -o %{outputTop}s %{path}q");
	System(filterCommand, 3);
	psTop = readTable(outputTop);

	#
	#	<p> create table output
	#
	gwas_topTable(o, psTop, tableOutput = Sprintf('%{outputBase}s-topTable'));

	#
	#	<p> manhattan plot
	#
	REP.plot('ASS:MANHATTAN',
		gwasManhattanPlot2file(P.value ~ chr + position, ps,
			output = Sprintf('%{outputBase}s-ass-manhattan'))
	);
#	psManhattan = Df_(ps, headerMap = list(chr = 'CHR', position = 'BP', P.value = 'P'));
# 	REP.plot('ASS:MANHATTAN',
# 		manhattanPlot(psManhattan, title = 'Manhattan plot',
# 			output = sprintf('%s/%s-ass-manhattan.jpeg', outputDir, splitPath(path)$base)));
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

initializeReporting = function(o, path, pipe = NULL, .no.parameters = FALSE) {
	REP.tex('G:PATH', path, quote = T);
	REP.tex('G:Ntop', o$Ntop);
	if (.no.parameters) reportPipeParameters(o, pipe = pipe);
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
		setup = file.locate('gwas/setup-report-lua.tex', scriptDirs), latex = 'lualatex',
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
