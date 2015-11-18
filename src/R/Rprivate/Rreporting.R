#
#	Rreporting.R
#Mon 06 Feb 2006 11:41:43 AM EST

#
#	<p> documentation (by example
#

# Example:
# 	create a Reporter instance to report to LaTeX
# r = new("Rreporter", final.path = "/tmp/output.pdf", patterns = "latex");

#
#	</p> end documentation
#

#
#	<p> generic reporting functions
#

row.standardFormatter = function(e, digits = NA) {
	f = if (is.na(digits) || substring(digits, 1, 1) == 'p') {
		e
	} else {
		e = as.numeric(e);
		if (substring(digits, 1, 1) == "#") {
			sprintf("%.*e", as.numeric(substring(digits, 2)), e)
		} else if (substring(digits, 1, 1) == "%") {
			sprintf('%.*f\\%%', as.numeric(substring(digits, 2)), e * 100)
		} else if (as.numeric(digits) < 0) {
			digits = as.integer(digits);
			ifelse(floor(log10(abs(e))) <= digits,
				sprintf("%.*g", -digits, e),
				sprintf("%.*f", -digits, e))
		} else { sprintf("%.*f", as.integer(digits), e) }
	}
	f
}

latex = list(
	# table patterns
	header = "{LONGTABLESTARTFMT\\begin{longtable}{COLUMN_FORMAT}\nLONGTABLECAPTION",
	columnNames = "%s%s %s\\hline\n",
	separator = " & ",
	hline = "\\hline\n",
	lineEnd = " \\\\\n",
	subHeading = function(h, rowSpan)
		sprintf("\\hline\n & \\multicolumn{%d}{l}{\\bf %s}\\\\\\hline\n", rowSpan, h),
	footer = "\\end{longtable}}\n\n",

	postProcess = function(s, df, row.formatters, digits, caption, na.value, subHeadings,
		ignoreRowNames, patterns, alignment, startFmt, bars) {
		if (is.null(alignment)) alignment = rep(NA, dim(df)[2]);
		alignment[is.na(alignment) & !is.na(digits)] = 'r';
		alignment[is.na(alignment)] = 'l';
		paragraphs = !is.na(digits) & substring(digits, 1, 1) == 'p';
		alignment[paragraphs] = digits[paragraphs];
		bars = c(bars, rep(F, length(alignment) - length(bars)));
		alignment = ifelse(!bars, alignment, paste(alignment, '|', sep = ''));
		colFmt = sprintf("%s%s", ifelse(ignoreRowNames, "", "r|"),
			paste(alignment, collapse = ""));
		captionPt = if (caption == '') list(LONGTABLECAPTION = '') else
			list(LONGTABLECAPTION = '\\caption{CAPTION}\\\\\n', CAPTION = caption)
		s = mergeDictToString(merge.lists(
			list(COLUMN_FORMAT = colFmt, LONGTABLESTARTFMT = startFmt),
			captionPt), s);
		s
	},
	quote = function(s, detectFormula = T) {
		s = gsub('_', '\\\\_', s, perl = T);
		s = gsub('&', '\\\\&', s, perl = T);
		s = gsub('~', '$\\\\sim$', s, perl = T);
		s = gsub('([<>])', '$\\1$', s, perl = T);
		s = gsub('\\^2', '$^2$', s, perl = T);
		#ifelse(length(grep('_', s)) > 0, gsub('_', '\\\\_', s, perl = T), s)
		s
	},

	# general text formatting
	newpage = "\n\n\\newpage\n\n",
	section = "\\section{SECTION_NAME}\n\n",
	subsection = "\\subsection{SECTION_NAME}\n\n",
	paragraph = "PARAGRAPH_TEXT\\par\n\n",

	# finalize
	document = "HEADER\n\\begin{document}\nDOC_HERE\n\\end{document}\n",
	docHeader = "\\documentclass[a4paper,oneside,11pt]{article}\n\\usepackage{setspace,amsmath,amssymb, amsthm, epsfig, epsf, amssymb, amsfonts, latexsym, rotating, longtable, setspace, natbib, a4wide,verbatim, caption}\n\\usepackage[utf8x]{inputenc}",
	docCmd = "cd TMP_DIR ; pdflatex TMP_FILE_BASE 1&>/dev/null ; cp TMP_FILE_BASE.pdf OUTPUT_FILE",

	# figure table
	figureTable = list(
		table = "\\begin{center}\\begin{tabular}{COLS}\nROWS\\end{tabular}\\end{center}",
		figure = '\\includegraphics[width=%.3f\\textwidth]{%s}',
		figureCaption = "\\begin{minipage}[b]{%.3f\\linewidth}\\centering
		\\begin{tabular}{c}
			%s\\\\
			\\includegraphics[width=\\textwidth]{%s}
		\\end{tabular}\\end{minipage}\n",
		formatTable = function(rows, cols = 2, template = latex$figureTable$table) {
			mergeDictToString(list(COLS = join(rep('c', cols), ''), ROWS = rows), template)
		},
		formatRows = function(rows, cols = 2) {
			sep = c(rep(' & ', cols - 1), "\\\\\n");
			seps = rep(sep, (length(rows) + cols - 1) %/% cols);
			seps = seps[1:length(rows)];
			rs = meshVectors(rows, seps);
			r = join(c(pop(rs), "\n"), '');
#browser();
#			texRows = sapply(1:(length(rows) - 1), function(i)sprintf('%s%s', rows[i],
#				ifelse(i %% cols == 1, ' & ', "\\\\\n")));
#			rs = join(c(texRows, rev(rows)[1], "\n"), '');
#			rs
		},
		formatFigure = function(figure, cols = 2, width = 1/cols - 0.05,
			template = latex$figureTable$figure, templateCaption = latex$figureTable$figureCaption,
			caption = '') {
			if (File.exists(figure)) figure = path.absolute(figure);
			caption = if (firstDef(caption, '') != '')
				sprintf(templateCaption, width, caption, figure) else
				sprintf(template, width, figure)
		}
	)
);

# bars: parallel structure to digits: where to insert vertical bars
report.data.frame.toString = function(df = NULL,
	row.formatters = c(row.standardFormatter), digits = NA, caption = "", na.value = "-",
	subHeadings = NULL, ignoreRowNames = F, patterns = latex, names.as = NULL, alignment = NULL,
	quoteHeader = T, quoteRows = T, quoteRowNames = quoteHeader, startFmt = '', bars = NULL) {
	with(patterns, {
	# <p> initialize
	rFmtC = length(row.formatters);
	if (length(digits) == 1) digits = rep(digits, dim(df)[2]);
	t = header;	# the nascent table as string
	if (!is.null(names.as)) names(df) = names.as;

	# <p> complete header
	header = if (quoteHeader) sapply(dimnames(df)[[2]], quote) else dimnames(df)[[2]];
	t = con(t, sprintf("%s%s%s%s", ifelse(!ignoreRowNames, separator, ""),
		paste(header, collapse = separator), lineEnd, hline));

	# <p> append rows
	for (i in Seq(1, nrow(df))) {
		row.fmt = row.formatters[[((i - 1) %% rFmtC) + 1]];

		if (i %in% subHeadings$indeces) {	# insert subheading
			j = which(subHeadings$indeces == i);
			t = con(t, subHeading(subHeadings$headings[j], dim(df)[2] - ignoreRowNames));
		}
		if (!ignoreRowNames) {
			rowName = dimnames(df)[[1]][i];
			t = con(t, sprintf("%s%s", if (quoteRowNames) quote(rowName) else rowName, separator));
		}
		# <p> formatting and quoting
		values = sapply(1:ncol(df), function(j)
			if (is.na(df[i, j])) na.value else row.fmt(as.character(df[i, j]), digits[j])
		);
		if (quoteRows) values = sapply(values, quote);
		t = con(t, sprintf("%s%s", paste(values, collapse = separator), lineEnd));
	}

	t = con(t, footer);
	t = postProcess(t, df, row.formatters, digits, caption, na.value, subHeadings,
		ignoreRowNames, patterns, alignment, startFmt, bars);
	})
}

report.figure.tableSingle = function(figures, cols = 2, width = 1/cols - 0.05, patterns = latex, captions = NULL)
	with(patterns, with(figureTable, {

	figs = sapply(1:length(figures), function(i){
		formatFigure(figures[i], cols = cols, width = width, caption = captions[i])
	});
	rows = formatRows(figs, cols = cols);
	table = formatTable(rows, cols = cols);
	table
}))
report.figure.table = function(figures, cols = 2, width = 1/cols - 0.05, patterns = latex,
	captions = NULL, maxRows = 5) with(patterns, {
	NfiguresPerPage = maxRows * cols;
	Nfigures = ceiling(ceiling(length(figures)/cols) / maxRows);
	if (Nfigures > 1) {
		tables = sapply(1:Nfigures, function(i) {
			Is = ((i - 1)*NfiguresPerPage + 1): min((i*NfiguresPerPage), length(figures));
			report.figure.tableSingle(figures[Is], cols, width, patterns, captions[Is])
		});
		join(tables, "\n")
	} else report.figure.tableSingle(figures, cols, width, patterns, captions)
})


#
#	<p> Rreporter (base on S4 methods)
#

setClass("Rreporter",
	representation(tmp.path = "character", final.path = "character", patterns = "list"),
	prototype(tmp.path = sprintf("%s.rr", tempfile()), final.path = NULL, patterns = latex)
);

setMethod("initialize", "Rreporter", function(.Object, final.path, patterns = latex) {
	.Object@final.path = final.path;
	.Object@patterns = if (is.character(patterns)) get(patterns) else patterns;
	# create temp file
	cat("", file = .Object@tmp.path);
	.Object
});

# <p> generic methods

report.data.frame = function(self, df = NULL, row.formatters = c(row.standardFormatter),
	digits = NA, caption = "", na.value = "-", subHeadings = NULL, ignoreRowNames = F, verbose = T) {
	patterns = self@patterns;
	s = report.data.frame.toString(df, row.formatters , digits, caption, na.value,
		subHeadings, ignoreRowNames, patterns);
	cat(s, file = self@tmp.path, append = T);
	if (verbose) cat(s);
	self
}

report.newpage = function(self) {
	cat(self@patterns$newpage, file = self@tmp.path, append = T);
}
report.newsection = function(self, name) {
	cat(
		mergeDictToString(list(SECTION_NAME = name), self@patterns$section),
		file = self@tmp.path, append = T
	);
}
report.newsubsection = function(self, name) {
	cat(
		mergeDictToString(list(SECTION_NAME = name), self@patterns$subsection),
		file = self@tmp.path, append = T
	);
}
report.paragraph = function(self, text) {
	cat(
		mergeDictToString(list(PARAGRAPH_TEXT = text), self@patterns$paragraph),
		file = self@tmp.path, append = T
	);
}

report.finalize = finalize = function(self) {
	cmd = sprintf("cp \"%s\" \"%s\"", self@tmp.path, absolutePath(self@final.path));
	System(cmd);
}

report.finalizeAsDocument = function(self) {
	# <p> read document to string
	doc = readFile(self@tmp.path);
	# <p> write processed document
	sp = splitPath(self@tmp.path);
	writeFile(sprintf("%s.tex", sp$fullbase),
		mergeDictToString(list(
			HEADER = self@patterns$docHeader, DOC_HERE = readFile(self@tmp.path)
		), self@patterns$document)
	);
	cmd = mergeDictToString(
		list(
			TMP_DIR = sp$dir,
			TMP_FILE = sp$path,
			TMP_FILE_BASE = sp$fullbase,
			OUTPUT_FILE = absolutePath(self@final.path)
		)
	, self@patterns$docCmd)
	System(cmd);
}

#
#	<p> end Rreporter (base on S4 methods)
#

#
#	<p> convenience methods
#

reportDataFrame2pdf = function(df, file = tempfile(), row.formatters = c(row.standardFormatter),
	digits = NA, caption = "", na.value = "-", subHeadings = NULL, ignoreRowNames = F, verbose = T) {
	r = new("Rreporter", final.path = file);
	report.data.frame(r, df,
		row.formatters, digits, caption, na.value, subHeadings, ignoreRowNames, verbose);
	report.finalizeAsDocument(r);
}

#
#	<p> sweave
#

swaeveIt = function(file = NULL, N = 1) {
	System(sprintf("R CMD Sweave '%s.Rnw'", file));
	cmd = sprintf("sh -c 'pdflatex \"./%s\"'", file);
	for (i in 1:N) System(cmd);
}

#
#	<p> Sweave replacement
#

.REP.standardTemplate = '\\input{SETUP}
\\begin{document}
TEMPLATE_MAIN
\\end{document}
';

# REP.plot('Tag', Qplot(rate, geom = 'histogram', xlab = 'heterocygosity', file = 'dest'));
# REP.plot('Tag', Qplot(sample = ps, dist = qunif, file = 'results/qc-markers-hweQQ.jpg'));

Qplot_defaults = list(
	width = 5, height = 5, dpi = 150,
	dimx = c(0, 1), dimy = c(0, 100)
);

Qplot = function(..., file = NULL, pp = Qplot_defaults) {
	pp = merge.lists(Qplot_defaults, pp);
	args = list(...);
	geom = firstDef(args$geom, 'default');
	# <b> workaround for QQ-plot instead of the expected qplot(...)
	p = if (any(class(args[[1]]) == 'ggplot')) {
		args[[1]]
	} else if (
		# histogram
		(all(is.na(args[[1]])) && geom == 'histogram')
		# xy-plot
		|| (all(is.na(args[[1]]) | is.na(args[[2]])))) {
		ggplot(data = data.frame()) + geom_point() +
			xlim(pp$dimx[1], pp$dimx[2]) +
			ylim(pp$dimy[1], pp$dimx[2]);
	} else do.call(qplot, list(...));
	ggsave(p, file = file, width = pp$width, height = pp$height, dpi = pp$dpi);
	file
}
GGplot = function(p, file = NULL, pp = list(width = 5, height = 5, dpi = 150)) {
	ggsave(p, file = file, width = pp$width, height = pp$height, dpi = pp$dpi, encoding = 'AdobeStd');
	file
}
PlotDefaults = list(
	pdf = list(width = 6, height = 6),
	jpeg = list(width = 2048, height = 2048)
);
Plot = function(..., file = NULL, .plotType = 'pdf', o = NULL, f = NULL) {
	if (is.null(file)) file = tempFileName('reporter', .plotType);
	device = get(.plotType);
	plotFunction = firstDef(f, plot);
	o = merge.lists(PlotDefaults[[.plotType]], o);
	do.call(device, c(list(file = file), o));
		do.call(plotFunction, list(...));
	dev.off();
	file
}

.REP.extractFromTemplates = function(templates, re = '(?s)(?<=TEMPLATE_BEGIN).*?(?=TEMPLATE_END)',
	locations = c('.', sprintf('%s/src/Rscripts', Sys.getenv('HOME')))) {
	nst = names(templates);

	# <p> set empty template names
	if (is.null(nst)) nst = rep('', length(templates));
	nst[nst == ''] = paste('TEMPL_', 1:sum(nst == ''), sep = '');

	# <p> parse template definitions
	ts = lapply(1:length(templates), function(i) {
		# raw read templates
		templ = readFile(templates[[i]], prefixes = locations);
		tsRaw = fetchRegexpr(re, templ);
		# inline templates
		r = if (length(tsRaw) != 0) {
			ns = sapplyn(tsRaw, function(e)fetchRegexpr('(?<=^:).*?(?=\\n)', e, globally = F));
			# colon, new-line
			ts = sapply(1:length(ns), function(i)substr(tsRaw[i], nchar(ns[i]) + 3, nchar(tsRaw[i])));
			listKeyValue(ns, ts);
		} else {
			listKeyValue(nst[i], templ);
		}
		r
	});
	#r = unlist.n(ts, 1);
	r = merge.lists(ts, listOfLists = T);
	r
}

.REP.getTemplates = function(templates, locations = c('.', sprintf('%s/src/Rscripts', Sys.getenv('HOME')))) {
	nst = names(templates);

	# <p> set empty template names
	if (is.null(nst)) nst = rep('', length(templates));
	nst[nst == ''] = paste('TEMPL_', 1:sum(nst == ''), sep = '');

	# <p> parse template definitions
	ts = lapply(1:length(templates), function(i) {
		# raw read templates
		templ = readFile(templates[[i]], prefixes = locations);
		tsRaw = fetchRegexpr('(?s)(?<=TEMPLATE_BEGIN).*?(?=TEMPLATE_END)', templ);
		# inline templates
		r = if (length(tsRaw) != 0) {
			ns = sapplyn(tsRaw, function(e)fetchRegexpr('(?<=^:).*?(?=\\n)', e, globally = F));
			# colon, new-line
			ts = sapply(1:length(ns), function(i)substr(tsRaw[i], nchar(ns[i]) + 3, nchar(tsRaw[i])));
			listKeyValue(ns, ts);
		} else {
			listKeyValue(nst[i], templ);
		}
		r
	});
	#r = unlist.n(ts, 1);
	r = merge.lists(ts, listOfLists = T);
	# backward compatibility: determine wether default template should be used
	if (length(r) > 0) {
		if (names(r)[1] != 'TEMPL_1') {	# expect full document template tb specified otherwise
			# interpolate first template into standard template
			r[[1]] = mergeDictToString(list(TEMPLATE_MAIN = r[[1]]), .REP.standardTemplate);
		}
	}
	r
}

.REP.getPatterns = function(templates) {
	.REP.extractFromTemplates(templates, '(?s)(?<=KEY_BEGIN).*?(?=KEY_END)');
}

.REP.defaultParameters = list(
	copy.files = 'setup.tex',
	setup = 'setup.tex',
	latex = 'pdflatex',
	useDefaultTemplate = T
);
# create new, global reporter
REP.new = function(templates = NULL, cache = NULL, parameters = .REP.defaultParameters, resetCache = F,
	latex = 'pdflatex', setup = 'setup.tex') {
	parameters = merge.lists(.REP.defaultParameters,
		parameters,
		list(copy.files = setup, latex = latex, setup = setup),
	concat = TRUE);
	if (!is.null(cache) && file.exists(cache) && !resetCache) {
		REP.tex('SETUP', setup);
		REP.setParameters(parameters);
		load(file = cache, envir = .GlobalEnv);
	} else {
		templatePathes = c(as.list(templates), parameters$subTemplates);
		ts = .REP.getTemplates(templatePathes);
		ps = merge.lists(
			list(SETUP = setup),
			.REP.getPatterns(templatePathes)
		);
		mainPath = splitPath(as.vector(templates)[1]);
		assign('.REPORTER.ITEMS', list(
			# list of named templates
			templates = ts,
			# patterns to be interpolated
			patterns = ps,
			# housekeeping: tags for consecutively reported subtemplates
			templateTags = list(),
			# parameters passed in
			parameters = parameters,
			# path to the cache file
			cache = cache,
			# create default output name
			output = sprintf('%s.pdf', mainPath$fullbase),
			# name of the template to be used for the global, final document
			mainTemplate = names(ts)[1],
			templatePathes = templatePathes,
			# conditionals
			conditionals = list()
			), pos = .GlobalEnv
		);
	}
	NULL
}
REP.refreshTemplates = function(templates) {
	if (!exists('.REPORTER.ITEMS')) return();
	templatePathes = templates;
	ts = .REP.getTemplates(as.list(templates));
	ps = .REP.getPatterns(templatePathes);
	.REPORTER.ITEMS$templates = ts;
	.REPORTER.ITEMS$mainTemplate = names(ts)[1];
	.REPORTER.ITEMS$templatePathes = templatePathes;
	.REPORTER.ITEMS$patterns = merge.lists(.REPORTER.ITEMS$patterns, ps);
	assign('.REPORTER.ITEMS', .REPORTER.ITEMS, pos = .GlobalEnv);
	REP.save();
}
REP.save = function() {
	if (!is.null(.REPORTER.ITEMS$cache)) {
		dir = splitPath(.REPORTER.ITEMS$cache)$dir;
		if (!file.exists(dir)) dir.create(dir, recursive = T);
		save(.REPORTER.ITEMS, file = .REPORTER.ITEMS$cache);
	}
	NULL
}

REP.setParameters = function(parameters = .REP.defaultParameters) {
	.REPORTER.ITEMS$parameters = merge.lists(.REP.defaultParameters, parameters);
	assign('.REPORTER.ITEMS', .REPORTER.ITEMS, pos = .GlobalEnv);
	REP.save();
}
REP.unreport = function(keys) {
	l = get('.REPORTER.ITEMS', pos = .GlobalEnv);
	idcs = which.indeces(keys, names(l$patterns));
	if (!length(idcs)) return(NULL);
	l$patterns = l$patterns[-idcs];
	assign('.REPORTER.ITEMS', l, pos = .GlobalEnv);
	REP.save();
}
setREPentry = function(key, value) {
	if (!exists('.REPORTER.ITEMS')) assign('.REPORTER.ITEMS', list(), pos = .GlobalEnv);
	l = get('.REPORTER.ITEMS', pos = .GlobalEnv);
	l$patterns[[key]] = value;
	assign('.REPORTER.ITEMS', l, pos = .GlobalEnv);
	REP.save();
}
setRI = function(ri)assign('.REPORTER.ITEMS', ri, pos = .GlobalEnv);

REP.setConditional = function(name, v) {
	l = get('.REPORTER.ITEMS', pos = .GlobalEnv);
	if (is.null(l$conditionals)) l$conditionals = list();
	l$conditionals[[name]] = v;
	assign('.REPORTER.ITEMS', l, pos = .GlobalEnv);
	REP.save();
}

outputOf = function(code, print = T, envir = parent.frame()) {
	tempFile = tempFileName('reporter', inRtmp = T);
	sink(tempFile);
		if (print) print(eval(code, envir = envir)) else eval(code, envir = envir);
	sink();
	output = readFile(tempFile);
	output
}
expression2str = function(exp, removeBraces = T) {
	strs = deparse(exp);
	if (removeBraces) strs = strs[2:(length(strs) - 1)];
	sprintf("%s\n", join(strs, "\n"))
}

codeRepresentation = function(code) {
	if (is.character(code)) {
		codeExp = parse(text = code);
		codeText = gsub('^\n?(.*)', '\\1', code);	# remove leading \n
	} else {
		codeExp = code;
		codeText = expression2str(code);
	}
	r = list(code = codeExp, text = codeText);
	r
}

REP.format.sci = function(s, digits = 1) {
	e = floor(log10(as.numeric(s)));
	m = as.numeric(s) * 10^(-e);
	if (round(m, digits) == 1) {
		sprintf("$10^{%d}$", e)
	} else {
		sprintf("$%.*f \\times 10^{%d}$", digits, m, e)
	}
}

REP.formats = list(
	small = function(s)sprintf("{\n\\small %s\n}", s),
	tiny = function(s)sprintf("{\n\\tiny %s\n}", s),
 	percent = function(s)sprintf("%.1f", 100 * as.numeric(s)),
  	`.1` = function(s)sprintf("%.1f", as.numeric(s)), 
	`.2` = function(s)sprintf("%.2f", as.numeric(s)), 
 	`.3` = function(s)sprintf("%.3f", as.numeric(s)), 
 	`.4` = function(s)sprintf("%.4f", as.numeric(s)), 
 	sci0 = function(s) REP.format.sci(s, 0), 
 	sci1 = function(s) REP.format.sci(s, 1), 
 	sci2 = function(s) REP.format.sci(s, 2), 
	file = function(f) {
		ri = .REPORTER.ITEMS;
		# due to caching choose a persistent location <!> uniqueness
		tdir = sprintf('/tmp/%s/Rpreporting/%s', Sys.getenv('USER'), names(ri$templates)[1]);
		if (!file.exists(tdir)) dir.create(tdir, recursive = T);
		tf = sprintf('%s/%s', tdir, splitPath(f)$file);
		unlink(tf);	# overwrite previous version
		# <!> expect relative filename, spaces in file name not eliminated
		file.symlink(sprintf('%s/%s', getwd(), f), tf);
		tf
	}
);

REP.tex = function(name, str, print = T, quote = F, fmt = NULL) {
	if (!is.null(fmt) && !is.na(fmt)) {
		str = if (is.null(REP.formats[[fmt]])) sprintf(fmt, str) else REP.formats[[fmt]](str);
	}
	if (quote) {	#<i> use backend quoting
		#str = gsub('_', '\\\\_', str, perl = T);	# replace _
		str = latex$quote(str);
	}
	setREPentry(sprintf('%s', name), str);
	str
}
REP.texq = function(name, str, print = T, quote = T, fmt = NULL)REP.tex(name, str, print, quote, fmt)
REP.vector = function(name, v, print = T, quote = T, typewriter = T, sep = ', ', max = 50) {
	if (max > 0) v = v[1:min(max, length(v))];
	if (typewriter) {
		v = sapply(v, function(s)sprintf('\\texttt{%s}', s));
	}
	REP.tex(name, sprintf('%s%s', join(v, sep), ifelse(length(v) > max, '...', '')), quote = quote);
}

REP = function(name, code, print = T, execute = T, envir = parent.frame()) {
	c = codeRepresentation(as.list(sys.call())[[3]]);
	setREPentry(sprintf('%s_code', name), c$text);
	if (execute) {
		output = outputOf(c$code, envir = envir);
		setREPentry(sprintf('%s_out', name), output);
		if (print) cat(output);
	}
	NULL
}
REP.plotDefaultOptions = list(width = 5, height = 5, dpi = 150);
REP.plot = function(name, code, ..., file = NULL, type = 'pdf', envir = parent.frame(),
	options = list(), copyToTmp = F) {
	#c = codeRepresentation(as.list(sys.call())[[3]]);
	c = codeRepresentation(sys.call()[[3]]);	# as of version R 3.0.1
	if (is.null(file)) file = tempFileName('reporter', 'pdf', inRtmp = T);
	if (type == 'ggplot') {
		o = merge.lists(REP.plotDefaultOptions, options, list(...));
		with(o, { ggsave(code, file = file, width = width, height = height, dpi = dpi) });
	} else if (is.character(code)) {
		file = code;
	} else {
		device = get(type);
		device(file, ...);
			eval(c$code, envir = envir);
		dev.off();
	}
	pathToFile = path.absolute(file);
	if (copyToTmp) {
		fileTmp = tempFileName('reporter', splitPath(pathToFile)$ext, inRtmp = T);
		file.copy(pathToFile, fileTmp, overwrite = T);
		pathToFile = fileTmp;
	}
	if (file.info(pathToFile)$size == 0) {
		pathToFile = '';
	}
	setREPentry(sprintf('%s_plot', name), pathToFile);
	setREPentry(sprintf('%s_code', name), c$text);
	NULL
}
# tag allows to search for overloading templates (_tag). This can be used in reportSubTemplate to
#	conditionally report templates
.REP.interpolateTemplate = function(templName, conditionals = list(), tag = NULL) {
	ri = .REPORTER.ITEMS;
	if (!is.null(tag) && !is.null(ri$templates[[sprintf('%s_%s', templName, tag)]]))
		templName = sprintf('%s_%s', templName, tag);
	s = ri$templates[[templName]]
	#s = readFile(tpath);
	s = mergeDictToString(.REPORTER.ITEMS$patterns, s, iterative = T);

	lengths = sapply(names(conditionals), nchar);
	for (n in names(conditionals)[rev(order(lengths))]) {
		s = gsub(sprintf('IF_%s(.*?)END_IF', n), if (conditionals[[n]]) '\\1' else '', s);
	}
	s
}

# initialize a series of reportSubTemplate calls followed by a finalizeSubTemplate call
REP.reportSubTemplateInitialize = function(subTemplate) {
	patterns = .REPORTER.ITEMS$patterns;
	subPatterns = sprintf('TEMPLATE:%s:subTemplates', subTemplate);
	REP.unreport(subPatterns);
}

REP.reportSubTemplate = function(subTemplate, tag = NULL, conditionals = list()) {
	ri = .REPORTER.ITEMS;
	# tag
	if (is.null(tag)) {
		tt = ri$templateTags;
		tag = ri$templateTags[[subTemplate]] =
			ifelse (is.null(tt[[subTemplate]]), 0, tt[[subTemplate]]) + 1;
		setRI(ri);
	}
	# finalize subTemplates
	patterns = ri$patterns;
	subPattern = sprintf('TEMPLATE:%s_%s', subTemplate, as.character(tag));
	subPatterns = sprintf('TEMPLATE:%s:subTemplates', subTemplate);

	# set own entry
	setREPentry(subPattern, .REP.interpolateTemplate(subTemplate, tag = tag));

	# collect all subTemplates
#	for (st in names(ri$parameters$subTemplates)) {
#		i = which.indeces(sprintf('TEMPLATE:%s_.*', st), names(.REPORTER.ITEMS$patterns), regex = T);
#		setREPentry(sprintf('TEMPLATE:%s:subTemplates', st), join(unlist(names(patterns[i])), "\n"));
#	}
	#i = which.indeces(sprintf('TEMPLATE:%s_.*', subTemplate), names(.REPORTER.ITEMS$patterns), regex = T);
	# append new element
	setREPentry(subPatterns, join(c(patterns[[subPatterns]], subPattern), "\n"));
	
	REP.save();
}

REP.finalizeSubTemplate = function(subTemplate) {
	# finalize subTemplates
	patterns = .REPORTER.ITEMS$patterns;
	subPatterns = sprintf('TEMPLATE:%s:subTemplates', subTemplate);

	text = mergeDictToString(patterns, patterns[[subPatterns]], iterative = T);
	setREPentry(sprintf('TEMPLATE:%s', subTemplate), text);
	# remove trail
	if (is.null(subPatterns)) return(NULL);
	subPattern = splitString("\n", .REPORTER.ITEMS$patterns[[subPatterns]]);
	#print(c(subPatterns, subPattern));

	REP.unreport(c(subPatterns, subPattern));

	REP.save();
}

REP.finalize = function(conditionals = list(), verbose = FALSE, cycles = 1, output = NULL) {
	# <p> vars
	ri = .REPORTER.ITEMS;
	
	# <p> prepare
	dir = tempFileName('rreporter', inRtmp = T);
	file.remove(dir);
	dir.create(dir);
	# <!> assume relative pathes
	for (cpath in .REPORTER.ITEMS$parameters$copy.files) {
		if (splitPath(cpath)$isAbsolute) {
			dest = sprintf('%s/%s', dir, splitPath(cpath)$file);
			Log(sprintf('Reporting: symlinking %s -> %s', cpath, dest), 4);
			file.symlink(cpath, dest);
		} else {
			for (sdir in c('', getwd(), sapply(ri$templatePathes, function(tp)splitPath(tp)$dir))) {
				source = sprintf('%s/%s/%s', getwd(), sdir, cpath);
				Log(sprintf('Reporting: dir %s', sdir), 4);
				if (file.exists(source)) {
					dest = sprintf('%s/%s', dir, cpath);
					Log(sprintf('Reporting: symlinking %s -> %s', source, dest), 4);
					file.symlink(source, dest);
					break;
				}
			}
		}
	}

	# <p> create final document
	tn = names(ri$templates)[1];
	allConditionals = merge.lists(ri$conditionals, conditionals);
	s = .REP.interpolateTemplate(ri$mainTemplate, allConditionals);

	# <p> run latex to produce temp file
	tmpPath = sprintf('%s/%s.tex', dir, tn);
	writeFile(tmpPath, s);
	Log(readFile(tmpPath), 5)
	latexCmd = firstDef(ri$parameters$latex, 'pdflatex');
	for (i in 1:cycles) {
		r = System(Sprintf('cd %{dir}s ; %{latexCmd}s -interaction=nonstopmode \"%{tn}s\"'),
			4, return.output = T);
		if (r$error > 0) Log(Sprintf("%{latexCmd}s exited with error."), 1);
		if (r$error > 0 || (verbose && i == 1)) Log(r$output, 1);
		#if (r$error > 0) break;
	}

	# <p> output
	postfix = join(names(conditionals[unlist(conditionals)]), '-');
	if (postfix != '') postfix = sprintf('-%s', postfix);
	#fileOut = sprintf('%s%s%s.pdf', splitPath(tpath)$base, if (postfix == '') '' else '-', postfix);
	#fileOut = sprintf('%s%s%s.pdf', tn, if (postfix == '') '' else '-', postfix);
	if (is.null(output))
		output = if (exists('.globalOutput'))
			.fn(sprintf('%s%s', splitPath(ri$output)$base, postfix), 'pdf') else ri$output;
	Log(sprintf('Writing to output %s', output), 4);
	
	file.copy(sprintf('%s.pdf', splitPath(tmpPath)$fullbase), output, overwrite = T);
	file.copy(sprintf('%s.tex', splitPath(tmpPath)$fullbase),
		sprintf('%s.tex', splitPath(output)$fullbase), overwrite = T);
}

#
#	<p> helpers
#

REP.reportFigureTable = function(nameTag, namesPlots, cols = 2, captions = NULL) {
	namesPlots = sapply(namesPlots, function(p) {
		path = if ('ggplot' %in% class(p)) {
			path = tempfile(fileext = '.pdf');
			ggsave(path, plot = p);
			path
		} else p;
		path
	});
	figureTable = report.figure.table(namesPlots, cols = cols, captions = captions);
	REP.tex(nameTag, figureTable);
}

#
#	Example code
#

# #	refresh only valid after a REP.new call
#	REP.refreshTemplates('gwas/reportGwas.tex')
# 	REP.new(
# 		'gwas/reportGwas.tex',
# 		cache = sprintf('%s/reportGWAS_cache', outputDir),
# 		resetCache = resetCache
# 	);
# # reporting
# 	REP.tex('G:DESCRIPTION', firstDef(o$studyDescription, ''));
# 	REP.tex('G:ROUNDNAME', firstDef(o$runName, 'unnamed'));
#	REP.finalize(verbose = T, output = sprintf('%s/reportGwas-%s.pdf', outputDir, o$runName), cycles = 3);

# # reporting patterns
# 	REP.tex('ASS:TABLE', report.data.frame.toString(
# 		psTop,
# 		digits = c(rep(NA, length(varsMap)), '#2', rep(2, length(Evars)), '#2', 2),
# 		names.as = rep.names, quoteHeader = F,
# 		caption = caption
# 	), fmt = 'tiny');
#	REP.tex('ASS:QQ:INFLATION', inflation, fmt = '.2');
# 	REP.plot('ASS:QQ:ASSOCIATION', Qplot(sample = ps$P, dist = qunif,
# 		file = sprintf('%s/ass-QQ-%s.jpg', outputDir, tag2fn(tag))));
#	REP.tex('QC:SAMPLE:MDS:Outlier', fraction(qcMdsOutliers), fmt = 'percent');
#
# # sub-templates
# 	REP.reportSubTemplateInitialize('association');
# 	for (m in expandedModels$models) with(m, {
#		REP.tex('ABC', 2);
#		REP.reportSubTemplate('association', tag);
# 	});
# 	REP.finalizeSubTemplate('association');

