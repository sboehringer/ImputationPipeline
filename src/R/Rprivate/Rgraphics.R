#
#	Rgraphics.R
#Mon 27 Jun 2005 10:52:17 AM CEST

#Require('grid');	# -> Rlibraries.R

#
#	<p> unit model
#
#	base unit is cm
setGeneric("factorToBase", function(this) standardGeneric("factorToBase"));
setGeneric("fromUnitToUnit", function(thisA, thisB) standardGeneric("fromUnitToUnit"));
setClass('unitGeneric', representation = list(value = 'numeric'), prototype = list(value = as.numeric(NA)));
setMethod('initialize', 'unitGeneric', function(.Object, value = as.numeric(NA)) {
	.Object@value = value;
	.Object
});
setMethod('fromUnitToUnit', c('unitGeneric', 'unitGeneric'), function(thisA, thisB)
	new(class(thisB), value = thisA@value * factorToBase(thisA) / factorToBase(thisB)));

setClass('unitCm', contains = 'unitGeneric');
setMethod('initialize', 'unitCm', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitCm', function(this)1);

setClass('unitInch', contains = 'unitGeneric');
setMethod('initialize', 'unitInch', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitInch', function(this)cm(1));

setClass('unitDpi150', contains = 'unitGeneric');
setMethod('initialize', 'unitDpi150', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitDpi150', function(this)cm(1)/150);

setClass('unitDpi200', contains = 'unitGeneric');
setMethod('initialize', 'unitDpi200', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitDpi200', function(this)cm(1)/200);

setClass('unitDpi300', contains = 'unitGeneric');
setMethod('initialize', 'unitDpi300', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitDpi300', function(this)cm(1)/300);

setClass('unitPoints', contains = 'unitGeneric');
setMethod('initialize', 'unitPoints', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitPoints', function(this)cm(1)/72);

valueU = valueUnited = function(value, unit) {
	class = getClass(Sprintf('unit%{unit}u'));
	new(class, value = value)
}
toUnit = function(value, unit)fromUnitToUnit(value, valueU(as.numeric(NA), unit));
ToUnit = function(value, unit)toUnit(value, unit)@value;

#
#	</p> unit model
#

cm2in = function(i) (i/2.54)

plotPoints = function(f=sin, interval=c(0,1), count = 1e2, steps = NULL, ...) {
	if (!is.null(steps))
		count = as.integer((interval[2] - interval[1]) / steps) else
		steps = (interval[2] - interval[1]) / (count + 1);
	xs = c(interval[1] + (0:(count - 1)) * steps, interval[2]);
	#ys = apply(t(xs), 2, function(x)(f(x)));
	#ys = Vectorize(function(x)f(x, ...))(xs);
	ys = Vectorize(function(x)f(x))(xs);
	data.frame(x = xs, y = ys)
}

plotRobust = function(f=sin, interval=c(0,1), count = 1e2, steps = NULL, points = F, ...) {
	pts = plotPoints(f, interval, count, steps, points, ...);
	if (points) {
		points(pts$x, pts$y, type="l");
	} else {
		plot(pts$x, pts$y, type="l");
	}
}

robustPlot = function(f=sin, interval=c(0,1), steps = 0.05, points = F, ...) {
	plotRobust(f, interval, steps = steps, points = points, ...);
}

#
# <p> vector functions
#

vNorm = function(v)sqrt(sum(v^2))
vToNorm = toNorm = function(v) {
	l = vNorm(v);
	if (l == 0) NA else v/l
}

# orthogonal vector in 2D
vPerp = function(v)rev(v) * c(-1, 1)

# the normal of a vector (in 2D), i.e. the perpendicular unit vector
vNormal = function(v)vToNorm(vPerp(v))

#
#	<p> graph drawing
#

# draw wedges
# x: x-coordinates
# y: y-coordinates
# w: widthes
wedge = function(x0, y0 = NULL, x1 = NULL, y1 = NULL, width = NULL, col = "black", ..., defaultWidth = .1) {
	d = if (!is.null(y0)) data.frame(x0, y0, x1, y1) else x0;
	if (is.null(width)) width = matrix(defaultWidth, ncol = 2, nrow = dim(x0)[1]);

	pts = matrix(sapply(1:dim(d)[1], function(i) {
		p1 = d[i, c("x0", "y0")];
		p2 = d[i, c("x1", "y1")];
		w = width[i, ];

		n = vNormal(p2 - p1); # normal of line
		c(p1 + n * w[1]/2, p1 - n * w[1]/2, p2 - n * w[2]/2, p2 + n * w[2]/2)
	}), ncol = 2, byrow = T);
	grid.polygon(x = pts[, 1], y = pts[, 2], id.lengths = rep(4, dim(d)[1]), gp = gpar(fill=1, col = col))
}

#
#	<p> ggplot2
#

#library('ggplot2');

qplotFaceted = function(f, from = 0, to = 1, data, facets, geom = 'line', ..., by = 0.02) {
	qplot.call = match.call(qplot);
	vars = formula.vars(facets);
	varLevels = unique(data[, vars, drop = F]);
	print(varLevels);
	xs = seq(from, to, by = by);
	r = apply(varLevels, 1, function(r) {
		environment(f) = f.env = new.env(parent = environment(f));
		fl = as.list(r);
		for (n in names(fl)) assign(n, fl[[n]], envir = f.env);
		ys = f(xs);
		d = data.frame(x = xs, y = ys, fl);
		d
	});
	d = rbindDataFrames(r);
	qplotArgs = c(as.list(qplot.call[-1]));
	p = qplot(x, y, data = d, facets = facets, geom = geom, ...);
	p
}

#
#	plot to file
#

plot_file_DefaultOptions = list(width = 12, height = 12, dpi = 200);

plot_file = function(code_or_object, file = NULL, options = list(), ..., envir = parent.frame()) {
	call = sys.call()[[2]];
	if (is.null(file)) file = tempFileName('plot_file', 'pdf', inRtmp = T);
	p = if (any(class(code_or_object) == 'ggplot')) {
		o = merge.lists(plot_file_DefaultOptions, options, list(...));
		with(o, { ggsave(code_or_object, file = file, width = width, height = height, dpi = dpi) });
		code_or_object
	} else {
		device = get(splitPath(file)$ext);
		device(file, ...);
			eval(call, envir = envir);
		dev.off();
		encapsulateCall(call, envir__ = envir);
	}
	p
}

#
#	<p> special plots
#

ggplot_qqunif = function(p.values, alpha = .05, fontsize = 6,
	tr = function(x)-log(x, 10), trName = '-log10(P-value)', colorCI = "#000099") {

	# <p> preparation
	if (any(p.values < 0 | p.values > 1)) stop("P.values not in interval (0, 1)");
	p.values = tr(sort(na.omit(p.values)));

	N = length(p.values);
	Ns = 1:N;
	# j-th order statistic from a uniform(0,1) sample has beta(j,n-j+1) distribution
	# (Casella & Berger, 2002, 2nd edition, pg 230, Duxbury)
	ciU = tr(qbeta(1 - alpha/2, Ns, N - Ns + 1));
	ciL = tr(qbeta(    alpha/2, Ns, N - Ns + 1));
	d = data.frame(theoretical = tr(Ns/N), ciU = ciU, ciL = ciL, p.value = p.values, colorCI = colorCI);
	p = ggplot(d) +
		geom_line(aes(x = theoretical, y = ciU, colour = colorCI)) +
		geom_line(aes(x = theoretical, y = ciL, colour = colorCI)) +
		geom_point(aes(x = theoretical, y = p.value), size = 1) +
		theme_bw() + theme(legend.position = 'none') + coord_cartesian(ylim = c(0, max(p.values)*1.1)) +
		scale_y_continuous(name = trName) +
		theme(text = element_text(size = fontsize));
	p
}

QQunifPlotDf = function(p.values, alpha = .05, tr = function(P)-log10(P), trName = Deparse(body(tr)),
	bins = c(2e2, 2e2), Nrep = 10, p.bottom = 1e-300, na.rm = T) {

	# <p> filter
	Logs('#p-values being NA: %{Nna}d', Nna = sum(is.na(p.values)), logLevel = 4);
	if (na.rm) p.values = p.values[!is.na(p.values)];
	Logs('#p-values < %{p.bottom}.1e: %{Nceil}d', Nceil = sum(p.values < p.bottom), logLevel = 4);
	p.values = ifelse(p.values < p.bottom, p.bottom, p.values);

	# <p> data frame
	if (any(p.values < 0 | p.values > 1)) stop("P.values not in interval (0, 1)");
	o = order(p.values)
	p.values = tr(p.values[o]);

	# <p> data frame
	N = length(p.values);
	d = data.frame(theoretical = tr((1:N)/N), p.value = p.values);

	# <p> binning
	Ns = if (!is.null(bins)) binPlot(d, bins = bins, Nrep = Nrep, returnIdcs = T) else Ns = 1:N;
	d = d[Ns, ];

	# j-th order statistic from a uniform(0,1) sample has beta(j,n-j+1) distribution
	# (Casella & Berger, 2002, 2nd edition, pg 230, Duxbury)
	dPlot = data.frame(d,
		ciU = tr(qbeta(1 - alpha/2, Ns, N - Ns + 1)), ciL = tr(qbeta(alpha/2, Ns, N - Ns + 1)));
	dPlot
}

#ggplot_qqunif(seq(1e-2, 3e-2, length.out = 1e2))
QQunif = Qqunif = function(p.values, alpha = .05, fontsize = 12,
	tr = function(P)-log10(P), trName = Deparse(body(tr)), colorCI = "#000099",
	bins = c(2e2, 2e2), Nrep = 10, p.bottom = 0) {

	dPlot = QQunifPlotDf(p.values, alpha, tr, trName, bins, Nrep, p.bottom, na.rm = T);
	dPlot = Df(dPlot, colorCI = colorCI);

	p = ggplot(dPlot) +
		geom_line(aes(x = theoretical, y = ciU, colour = colorCI)) +
		geom_line(aes(x = theoretical, y = ciL, colour = colorCI)) +
		geom_point(aes(x = theoretical, y = p.value), size = 1) +
		theme_bw() + theme(legend.position = 'none') + coord_cartesian(ylim = c(0, max(dPlot$p.value)*1.1)) +
		scale_y_continuous(name = trName) +
		theme(text = element_text(size = fontsize));

	p
}

cbPalette1 = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7");
cbPalette2 = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7");
QQunifDfPalette = c(cbPalette1, cbPalette2);

QQunifDf = function(f1, data, alpha = .05, fontsize = 12,
	tr = function(P)-log10(P), trName = Deparse(body(tr)), trI = function(Ptr)exp(Ptr * log(10)),
	colorCI = "#000099", bins = c(2e2, 2e2), Nrep = 10, p.bottom = 0, byCutoff = 1, legendTitle = 'Test') {
	# <p> variable names
	p.value = formula.response(f1);

	cats0 = factorFromFormula(data, formula.rhs(f1));
	plotDfs = by(data, cats0, function(d0)
		QQunifPlotDf(d0[[p.value]], alpha, tr, trName, bins, Nrep, p.bottom, na.rm = T));
	#color = recodeLevels(as.character(cats0), levels = c('odd', 'even'));
	dPlot = do.call(rbind, nlapply(plotDfs, function(n)Df(name = n, plotDfs[[n]])));
	dPlot = dPlot[!is.na(dPlot[[p.value]]),, drop = F];
	dPlot = dPlot[order(dPlot[[p.value]]), ];
	myBreaks = function()cats0;	# work around for ggplot2 behavior of lexically scoping to data set only
	palette = c(colorCI, QQunifDfPalette)[1:(length(cats0) + 1)];

	# <p> data frame CI
	pRangeTh = range(dPlot$theoretical);
	N = ceiling(trI(pRangeTh[2]));	# <!> inverse of tr
	dCi = data.frame(
		theoretical = tr((1:N - .5)/N),
		ciU = tr(qbeta(1 - alpha/2, 1:N, N - (1:N) + 1)), ciL = tr(qbeta(alpha/2, 1:N, N - (1:N) + 1))
	);

	p = ggplot(dPlot) +
		geom_point(aes(x = theoretical, y = p.value, colour = name), size = 1) +
		scale_color_manual(name = get('legendTitle'), breaks = get('cats0'), values = get('palette')) + 
		#theme_bw() + theme(legend.position = 'none') + coord_cartesian(ylim = c(0, max(dPlot$p.value)*1.1)) +
		theme_bw() + coord_cartesian(ylim = c(0, max(dPlot$p.value)*1.1)) +
		scale_y_continuous(name = trName) +
		theme(text = element_text(size = fontsize)) +
		# CI
		geom_line(data = dCi, aes(x = theoretical, y = ciU, colour = colorCI)) +
		geom_line(data = dCi, aes(x = theoretical, y = ciL, colour = colorCI))

	p
}


vp_at = function(x, y)viewport(layout.pos.row = x, layout.pos.col = y);
plot_grid_grid = function(plots, coords) {
	# <p> do plotting
	grid.newpage();
	# <!> layout might not be respected
	nrow = max(coords[, 1]);
	ncol = max(coords[, 2]);
	pushViewport(viewport(layout = grid.layout(nrow, ncol)));

	sapply(1:length(plots), function(i) {
		print(plots[[i]], vp = vp_at(coords[i, 1], coords[i, 2]));
	});
}

plot_error = function(msg, errN = 10) {
	plot(1:errN, type = 'n', xlab = '', ylab = '', axes = FALSE, frame.plot = FALSE);
	text(errN/2, errN/2, msg);
}

plot_grid_base = function(plots, coords, envirPlotVar = 'plot', verbose = F, errN = 10) {
	# <p> do plotting
	coordMat0 = matrix(0, nrow = max(coords[, 1]), ncol = max(coords[, 2]));
	coordMat = matrix.assign(coordMat0, coords, 1:prod(dim(coordMat0)));
	layout(coordMat);

	sapply(1:length(plots), function(i) {
		if (verbose) LogS(2, 'plot_grid#: %{i}d');
		this = try({
		cl = class(plots[[i]]);
			if ('environment' %in% cl) eval(get(envirPlotVar, plots[[i]])) else
			if ('function' %in% cl) do.call(plots[[i]], list()) else
			eval(plots[[i]])
		});
		if ('try-error' %in% class(this)) plot_error(this)
	});
# 			if (is.environment(plots[[i]])) eval(get(envirPlotVar, plots[[i]])) else print(plots[[i]]);
}

# plot_grid(list(
# 	quote(plot(t0, col = d$sex)),
# 	quote(plot(pca0$x[, 1:2], col = d$sex)),
# 	function() {
# 		plot(1:10, type = 'n', xlab = '', ylab = '');
# 		text(5, 5, join(c(Sprintf('Family: %{myFid}d'), pedPed), "\n")) }
# ), nrow = 2);
# plot_save(plts, file = .fn('tsne-pca', 'pdf'));


plot_grid = function(plots, nrow, ncol, byrow = T, mapper = NULL, envirPlotVar = 'plot', verbose = F) {
	if (missing(nrow)) {
		if (missing(ncol)) {
			ncol = 1;
			nrow = length(plots);
		} else {
			nrow = ceiling(length(plots) / ncol);
		}
	} else if (missing(ncol)) ncol = ceiling(length(plots) / nrow);

	coords = if (is.null(mapper))
		merge.multi(1:nrow, 1:ncol, .first.constant = byrow) else
		mapper(1:length(plots));

	cl = class(plots[[1]]);
	if ('viewport' %in% cl) plot_grid_grid(plots, coords) else
	plot_grid_base(plots, coords, envirPlotVar, verbose = verbose)
}

plot_grid_to_path =  function(plots, ..., path,
	width = valueU(21, 'cm'), height = valueU(29.7, 'cm'), NperPage = NULL, pdfOptions = list(paper = 'a4'),
	verbose = F) {

	if (class(width) == 'numeric') width = valueU(width, 'inch');
	if (class(height) == 'numeric') height = valueU(height, 'inch');
	Nplots = length(plots);

	pages = if (!is.null(NperPage)) {
		Npages = ceiling(Nplots / NperPage);
		lapply(1:Npages, function(i) {
			Istrt = (i - 1) * NperPage + 1;
			Istop = min(i * NperPage, Nplots);
			Istrt:Istop
		})
	} else list(1:length(plots));

	pdfArgs = c(list(
		file = path, onefile = TRUE, width = ToUnit(width, 'inch'), height = ToUnit(height, 'inch')
	), pdfOptions);
	do.call(pdf, pdfArgs);

	ilapply(pages, function(plotIdcs, i) {
		if (verbose) LogS(2, 'Plotting page: %{i}d');
		plot_grid(plots[plotIdcs], ..., verbose = verbose);
	});

	dev.off();
}

plot_adjacent = function(fts, factor, N = ncol(fts)) {
	ns = names(fts);
	ps = lapply(1:(N - 1), function(i){
		x = eval({fts[, i]});
		y = eval({fts[, i + 1]});
		qplot(x, y, color = as.factor(factor), xlab = ns[i], ylab = ns[i + 1]);
	});
}

plot_grid_pdf = function(plots, file, nrow, ncol, NperPage, byrow = T, mapper = NULL,
	pdfOptions = list(paper = 'a4'), verbose = F) {
	Nplots = length(plots);
	if (missing(nrow)) nrow = NperPage / ncol;
	if (missing(ncol)) ncol = NperPage / nrow;
	if (missing(NperPage)) NperPage = ncol * nrow;
	Npages = ceiling(Nplots / NperPage);

	do.call(pdf, c(list(file = file), pdfOptions));
	sapply(1:Npages, function(i) {
		if (verbose) LogS(2, 'Plotting page: %{i}d');
		Istrt = (i - 1) * NperPage + 1;
		Istop = min(i * NperPage, Nplots);
		plot_grid(plots[Istrt:Istop], nrow, ncol, byrow = byrow, mapper = mapper, verbose = verbose);
	});
	dev.off();
}

#
#	<p> Kaplan-Meier with ggplot
#

# stolen from the internet
createSurvivalFrame <- function(f.survfit){
	# initialise frame variable
	f.frame <- NULL
	# check if more then one strata
	if(length(names(f.survfit$strata)) == 0){
		# create data.frame with data from survﬁt
		f.frame <- data.frame(time=f.survfit$time, n.risk=f.survfit$n.risk, n.event=f.survfit$n.event,
			n.censor = f.survfit$n.censor, surv=f.survfit$surv, upper=f.survfit$upper, lower=f.survfit$lower)
		# create ﬁrst two rows (start at 1)
		f.start <- data.frame(time=c(0, f.frame$time[1]), n.risk=c(f.survfit$n, f.survfit$n), n.event=c(0,0),
		n.censor=c(0,0), surv=c(1,1), upper=c(1,1), lower=c(1,1))
		# add ﬁrst row to dataset
		f.frame <- rbind(f.start, f.frame)
		# remove temporary data
		rm(f.start)
	} else {
		# create vector for strata identiﬁcation
		f.strata <- NULL
		for(f.i in 1:length(f.survfit$strata)){
			# add vector for one strata according to number of rows of strata
			f.strata <- c(f.strata, rep(names(f.survfit$strata)[f.i], f.survfit$strata[f.i]))
		}
		# create data.frame with data from survﬁt (create column for strata)
		f.frame <- data.frame(time=f.survfit$time, n.risk=f.survfit$n.risk, n.event=f.survfit$n.event, n.censor = f.survfit
		$n.censor, surv=f.survfit$surv, upper=f.survfit$upper, lower=f.survfit$lower, strata=factor(f.strata))
		# remove temporary data
		rm(f.strata)
		# create ﬁrst two rows (start at 1) for each strata
		for(f.i in 1:length(f.survfit$strata)){
			# take only subset for this strata from data
			f.subset <- subset(f.frame, strata==names(f.survfit$strata)[f.i])
			f.start <- data.frame(time=c(0, f.subset$time[1]), n.risk=rep(f.survfit[f.i]$n, 2), n.event=c(0,0), n.censor=c(0,0), surv=c(1,1), upper=c(1,1), lower=c(1,1), strata=rep(names(f.survfit$strata)[f.i], 2))	
			# add ﬁrst two rows to dataset
			f.frame <- rbind(f.start, f.frame)
			# remove temporary data
			rm(f.start, f.subset)
		}
		# reorder data
		f.frame <- f.frame[order(f.frame$strata, f.frame$time), ]
		# rename row.names
		rownames(f.frame) <- NULL
	}
	# return frame
	return(f.frame)
}

# deﬁne custom function to draw kaplan-meier curve with ggplot
qplot_survival = function(f.frame, f.CI = "default", f.shape = 3, ..., title = NULL, layers = NULL,
	axes = NULL, legendTitle = T){

	strata = avu(Regexpr('=(.*)', levels(f.frame$strata), captures = T));
	factorNm = avu(Regexpr('(.*)=', levels(f.frame$strata)[1], captures = T));
	levels(f.frame$strata) = strata;

	# use different plotting commands dependig whether or not strata's are given
	p = if("strata" %in% names(f.frame) == FALSE) {
		# conﬁdence intervals are drawn if not speciﬁed otherwise
		if(f.CI == "default" | f.CI == TRUE ){
			# create plot with 4 layers (ﬁrst 3 layers only events, last layer only censored)
			# hint: censoring data for multiple censoring events at timepoint are overplotted
			# (unlike in plot.survﬁt in survival package)
			ggplot(data=f.frame, ...) +
			geom_step(aes(x=time, y=surv), direction="hv") +
			geom_step(aes(x=time, y=upper), directions="hv", linetype=2) +
			geom_step(aes(x=time,y=lower), direction="hv", linetype=2) +
			geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape)
		} else {
			# create plot without conﬁdence intervalls
			ggplot(data=f.frame) +
			geom_step(aes(x=time, y=surv), direction="hv") +
			geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape)
		}
	} else {
		# without CI
		if(f.CI == "default" | f.CI == FALSE){
			ggplot(data=f.frame, aes(group=strata, colour=strata), ...) +
			geom_step(aes(x=time, y=surv), direction="hv") +
			geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape)
		} else {
			ggplot(data=f.frame, aes(colour=strata, group=strata), ...) +
			geom_step(aes(x=time, y=surv), direction="hv") +
			geom_step(aes(x=time, y=upper), directions="hv", linetype=2, alpha=0.5) +
			geom_step(aes(x=time,y=lower), direction="hv", linetype=2, alpha=0.5) +
			geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape)
		}
	}
	if (!is.null(title)) p = p + labs(title = title);
	if (Nif(axes) && Nif(axes$x)) p = p + xlab(axes$x);
	if (Nif(axes) && Nif(axes$y)) p = p + ylab(axes$y);
	if (!is.null(layers)) p = p + layers;
	#p = p + theme(legend.title = if (legendTitle) factorNm else element_blank());
	legendNm = if (legendTitle) factorNm else NULL;
	p = p + scale_colour_discrete(name = legendNm, breaks = strata, labels = strata)
	p
}

quantileBinning = function(x, Nbins) {
	cut(x, quantile(x, seq(0, 1, length = Nbins + 1)), labels = seq_len(Nbins), include.lowest = TRUE)
}

kaplanMeierStrat = function(d1, f1, levels = NULL,
	title = '%{formula}s, [P = %{p}.2e]', axes = NULL, legendTitle = T) {
	# <i> only allow one covariate
	titlePre = stratVar = all.vars(formula.rhs(f1))[1];
	titleFormula = Sprintf('%{O}s ~ %{titlePre}s', O = all.vars(as.formula(f1))[1])
	if (!is.null(levels)) {
		d1[[stratVar]] = as.factor(quantileBinning(d1[[stratVar]], levels));
	}
	stratValue = levels(droplevels(d1[[stratVar]]));
	# <p> log-rank test
	lr = survdiff(as.formula(f1), data = d1);
	p.lr = pchisq(lr$chisq, df = dim(lr$n) - 1, lower.tail = F)
	# <p> kaplan-meyer
	fit = survfit(as.formula(f1), data = d1);
	fit.frame = createSurvivalFrame(fit);
	titleCooked = Sprintf(title, strata = titlePre, p = p.lr, formula = titleFormula)
	p = qplot_survival(fit.frame, F, 20, title = titleCooked, axes = axes, legendTitle = legendTitle,
		layers = theme_bw());
	list(plot = p, level = stratValue)
}

kaplanMeierNested = function(d, f1, strata, combine = FALSE) {
	Require('gdata');
	# <!><b> fix special case of length(strata) == 1
	d = Df(d, dummy_comb__kaplanMeierNested = 1);
	dStrat = d[, c(strata, 'dummy_comb__kaplanMeierNested'), drop = F];
	cbs = Df_(valueCombinations(dStrat), min_ = 'dummy_comb__kaplanMeierNested');
	dStrat = Df_(dStrat, min_ = 'dummy_comb__kaplanMeierNested');

	plots = apply(cbs, 1, function(r) {
		sel1 = nif(apply(dStrat == r, 1, all));
		sel = nif(sapply(1:nrow(d), function(i)all(dStrat[i, ] == r)));
		if (sum(sel) == 0) {
			warning('empty group selected');
			return(NULL);
		}
		#if (sum(sel) == 0) return(NULL);
		dSel = d[sel, , drop = F];
		N = sum(sel);
		title = Sprintf('Stratum: %{stratum}s, N = %{N}d',
			stratum = paste(names(r), r, sep = '=', collapse = ', '));
		kaplanMeierStrat(dSel, f1, title = title);
	});
	plots
}

# groups levels by group argument
kaplanMeierCovariate = function(covariate, outcome, groups = NA, data,
	title = '%{strata}s, [P = %{p}.2e]', axes = NULL, legendTitle = T) {
	pred = if (!nif(groups)) data[[covariate]] else recodeLevels(data[[covariate]], group = groups);
	LogS(5, 'Number of levels: %{N}d', N = length(levels(droplevels(pred))));
	#if (length(levels(droplevels(pred))) < 2) browser();
	d = Df(pred, data[, avu(outcome), drop = F], names = covariate);
	f1 = with(outcome, Sprintf('Surv(%{time}s, %{status}s) ~ %{covariate}s'))
	p = kaplanMeierStrat(d, f1, title = title, axes = axes, legendTitle = legendTitle);
	return(p);
}


#
#	<p> histograms
#


histogram_colors = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7");
histogram_colors = c('red', 'blue', 'green', 'yellow');
#dayColor = list(`0` = 'red', `1` = 'blue', `3` = 'green', `8` = 'yellow');

histogram_overlayed = function(data, f1,
	groupNames = levels(groups), palette = histogram_colors, log10 = T,
	x_lab = formula.response(f1), title = 'histogram', alpha = .3, breaks = 30) {

	# <p> column names, range
	xn = formula.response(f1);
	gn = formula.covariates(f1);
	lvls = levels(data[[gn]]);
	tab = table(cut(data[[xn]], breaks));
	#mx = if (log10) 10^ceiling(log10(max(tab))) else max(tab);
	mx = max(tab);

	# <p>  create legend using pseudo data (shifted out of view)
	dp = Df(x = rep(0, length(lvls)), y = rep(mx + 1, length(lvls)), group = lvls);
	p = ggplot(dp, aes(x = x)) +
		geom_rect(data = dp, aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 1, fill = group)) +
		scale_fill_manual(name = gn, values = palette);

	# <p> histograms
	for (i in 1:length(lvls)) {
		p = p + geom_histogram(data = data.frame(x = data[[xn]][data[[gn]] == lvls[i]]),
			fill = palette[i], alpha = alpha);
	}

	# <p> log transform
	if (log10) p = p + scale_y_continuous(trans = 'log10') + coord_cartesian(ylim = c(1, mx));

	# <p> final formatting
	p = p + ggtitle(title) + xlab(x_lab);
	p

}

#'@param data:	data frame or list
histograms_alpha = function(data, palette = histogram_colors, log10 = F,
	x_lab = '', title = 'histogram', alpha = .3, origin = NULL, binwidth = NULL, relative = FALSE,
	textsize = 20) {
	# <p> preparation
	N = length(as.list(data));
	columns = names(data);
	mx = max(unlist(as.list(data)), na.rm = T);
	mn = min(unlist(as.list(data)), na.rm = T);

	# <p>  create legend using pseudo data (shifted out of view)
	dp = Df(x = rep(2*mx + 2, N), y = rep(0, N), group = columns);
	p = ggplot(dp, aes(x = x)) +
		geom_rect(data = dp, aes(xmin = x, xmax = x + .01, ymin = y, ymax = y + .01, fill = group)) +
		scale_fill_manual(name = dp$group, values = palette);

	# <p> histograms
	for (i in 1:N) {
		col = columns[i];
		dfH = data.frame(x = data[[col]]);
		p = p + if (relative)
			geom_histogram(data = dfH, aes(y=..count../sum(..count..)),
				fill = palette[i], alpha = alpha, binwidth = binwidth, origin = origin
			) else
			geom_histogram(data = dfH, fill = palette[i], alpha = alpha, binwidth = binwidth, origin = origin)
	}

	# <p> log transform
	if (log10) p = p + scale_y_continuous(trans = 'log10') + coord_cartesian(ylim = c(1, mx));

	# <p> final formatting
	p = p + coord_cartesian(xlim = c(mn - 1, mx + 1)) + ggtitle(title) + xlab(x_lab) + theme_bw() +
		theme(text = element_text(size = textsize));
	if (relative) p = p + ylab('percentage');
	p

}

#
#	<p> saving of plots
#


# base unit is 600dpi
units_conv = list(
	cm = list(from = function(cm)(cm/2.54*600), to = function(b)(b/600*2.54)),
	points = list(from = function(points)(points/72*600), to = function(b)(b/600*72)),
	inch = list(from = function(i)(i*600), to = function(b)(b/600)),
	dpi150 = list(from = function(dpi)(dpi/150*600), to = function(b)(b*150/600)),
	dpi300 = list(from = function(dpi)(dpi/300*600), to = function(b)(b*300/600))
);
units_default = list(jpeg = 'dpi150', pdf = 'cm', png = 'points');

# autoscale scales canvas sizes to make scaling uniform between devices
#	increases the pixel resolution for pixel devices
deviceTypes = list(
	jpeg = list(hasDpi = TRUE, autoScale = 1),
	png = list(hasDpi = TRUE, autoScale = 1),
	pdf = list(forceUnit = 'inch', autoScale = 1)
);

Device = function(type, plot_path, width, height, ..., units = 'cm', dpi = NA, autoScale = FALSE) {
	device = get(type);
	o = deviceTypes[[type]];
	if (!is.null(o$forceUnit)) {
		width = toUnit(width, o$forceUnit);
		height = toUnit(height, o$forceUnit);
	}
	width = width@value;
	height = height@value;
	if (autoScale && !is.null(o$autoScale)) {
		width = width * o$autoScale;
		height = height * o$autoScale;
	}
	args = c(list(plot_path, width = width, height = height), list(...));
	if (nif(o$hasDpi)) do.call(device, c(args, list(units = units, res = dpi))) else do.call(device, args);
		#device(plot_path, width = width, height = height, units = units, res = dpi, ...) else
		#device(plot_path, width = width, height = height, ...);
}

# prepare plotting expression for later plotting
quotePlot = function(e, envir = parent.frame()) {
	list(plot = substitute(e), envir = envir)
}

# use 'cm' as output unit
# if unit_out is from a class beginning with unitDpi, produce a resolution argument correpsonding
#	to that many dpi

plot_draw = function(object, envir) {
	ret = if (any(class(object) == 'grob')) {
		grid.draw(object)
	} else if (any(class(object) %in% c('ggplot', 'plot'))) {
		print(object)
	} else if (any(class(object) %in% c('histogram'))) {
		plot(object)
	} else if (any(class(object) %in% 'list') && notE(plot$envir)) {
		eval(object$plot, envir = plot$envir);
	} else {
		eval(object, envir = envir);
	}
}

plot_save_raw = function(object, ..., width = 20, height = 20, plot_path = NULL,
	type = NULL, options = list(), unit = 'cm', unit_out = NULL, autoScale = FALSE, envir = parent.frame()) {

	if (is.null(unit_out)) unit_out = units_default[[type]];
	#width = toUnit(width, unit_out)@value;
	#height = toUnit(height, unit_out)@value;
	width = toUnit(width, 'cm');
	height = toUnit(height, 'cm');
	dpi = Nina(as.integer(FetchRegexpr('dpi(\\d+)', unit_out, captures = T)));
 	Logs('Saving %{type}s to "%{plot_path}s"  [width: %{w}f %{h}f dpi: %{dpi}d]',
		w = width@value, h = height@value, logLevel = 5);
	Device(type, plot_path, width = width, height = height, units = 'cm', dpi = dpi, ...,
		autoScale = autoScale);
		#ret = eval(object, envir = envir);
# 		ret = if (any(class(object) == 'grob')) {
# 			grid.draw(object)
# 		} else if (any(class(object) %in% c('ggplot', 'plot'))) {
# 			print(object)
# 		} else {
# 			eval(object, envir = envir);
# 		}
		ret = plot_draw(object, envir = envir);
	dev.off();
}
# <i> refactor with plot_save_raw
plot_save_eval = function(object, ..., width = 20, height = 20, plot_path = NULL,
	type = NULL, options = list(), unit = 'cm', unit_out = 'dpi150', autoScale = FALSE, envir = parent.frame()) {

	if (is.null(type)) type = splitPath(plot_path)$ext;
	if (is.null(unit_out)) unit_out = units_default[[type]];
	if (class(width) == 'numeric') width = valueU(width, 'cm');
	if (class(height) == 'numeric') height = valueU(height, 'cm');
	width = toUnit(width, 'cm');
	height = toUnit(height, 'cm');
	dpi = Nina(as.integer(FetchRegexpr('dpi(\\d+)', unit_out, captures = T)));
 	Logs('Saving %{type}s to "%{plot_path}s"  [width: %{w}f %{h}f dpi: %{dpi}d]',
		w = width@value, h = height@value, logLevel = 5);
	Device(type, plot_path, width = width, height = height, units = 'cm', dpi = dpi, ...,
		autoScale = autoScale);
		ret = eval(object, envir = envir);
	dev.off();
	return(ret);
}


plotSizes = list(
	a4 = list(width = valueU(21, 'cm'), height = valueU(21 * sqrt(2), 'cm')),
	a4Sq = list(width = valueU(21, 'cm'), height = valueU(21, 'cm')),
	a4R = list(width = valueU(21*sqrt(2), 'cm'), height = valueU(21, 'cm')),
	a4_15 = list(width = valueU(1.5*21, 'cm'), height = valueU(1.5 * 21 * sqrt(2), 'cm')),
	a4R_15 = list(width = valueU(1.5*21*sqrt(2), 'cm'), height = valueU(1.5 * 21, 'cm'))
);
#
#	Examples:
#	plot_save(object, c('a.jpeg', 'a.png'), options = list(jpeg = list(unit_out = 'dpi300')));
#	plot_save(quote(plot(res)), unit_out = 'cm', plot_path = 'resources/Q3-residuals.png',
#		width = 15, height = 15, options = list(png = list(unit_out = 'dpi150')));
#	plot_save(quote(plot(res)), plot_path = 'resources/Q3-residuals.png', width = 15, height = 15);
plot_typeMap = list(jpg = 'jpeg');
plot_optionsDefault = list(
	png = list(unit_out = 'dpi150'),
	jpeg = list(unit_out = 'dpi150')
);
plot_save = function(object, ..., size, width, height, plot_path = NULL,
	#type = firstDef(plot_typeMap[[splitPath(plot_path)$ext]], splitPath(plot_path)$ext),
	type = NULL,
	envir = parent.frame(), options = plot_optionsDefault,
	simplify = T, unit_out = NULL, createDir = TRUE) {

	# <p> plot size
	# default size
	if (missing(size) && (missing(width) || missing(height))) size = 'a4R';
	if (!missing(size)) {
		width = plotSizes[[size]]$width;
		height = plotSizes[[size]]$height;
	}
	if (class(width) == 'numeric') width = valueU(width, 'cm');
	if (class(height) == 'numeric') height = valueU(height, 'cm');

	if (is.null(plot_path)) file = tempFileName('plot_save', 'pdf', inRtmp = T);
	ret = lapply(plot_path, function(plot_path) {
		if (createDir) Dir.create(plot_path, recursive = T, treatPathAsFile = T);
		if (is.null(type) && !is.null(plot_path)) {
			ext = splitPath(plot_path)$ext;
			type = firstDef(plot_typeMap[[ext]], ext);
		}
		uo = firstDef(options[[type]]$unit_out, unit_out, units_default[[type]]);
		Logs(con("plot_path: %{plot_path}s, device: %{type}s, unit_out: %{uo}s "), logLevel = 5);
		args = c(list(object),
			list(type = type, plot_path = plot_path,
				width = width, height = height, unit_out = uo,
				options = options, envir = envir),
			list(...)
		);
		do.call(plot_save_raw, args);
	});
	if (length(plot_path) == 1 && simplify) ret = ret[[1]];
	r = list(path = plot_path, ret = ret);
	r
}

# USAGE:
# plts = exprR1$Eapply(function(data, probe_name) {
# 	delayedPlot({
# 		boxplot(model, data, main = main);
# 		beeswarm(model, data, add = T)
# 	})
# });
# eval(plts[[1]])

delayedPlot = function(plotExpr, envir = parent.frame()) {
	e = new.env(parent = envir);
	delayedAssign('plot', plotExpr, assign.env = e)
	e
}

#
#	<p> legacy function from other packages
#

# gridExtra
ebimageGrob = function (pic, x = 0.5, y = 0.5, scale = 1, raster = FALSE, angle = NULL, ...) {
    dims <- dim(pic)
    colours = t(channel(pic, "x11"))
    width = unit(scale * dims[1], "points")
    height = unit(scale * dims[2], "points")
    angle <- if (is.null(angle)) 
        0
    else angle
    vp <- viewport(x = x, y = y, width = width, height = height, 
        angle = angle)
    if (raster) {
        child <- rasterGrob(colours, vp = vp, ...)
    }
    else {
        colours <- colours[rev(seq_len(nrow(colours))), ]
        Require('RGraphics')
        child <- imageGrob(dims[2], dims[1], cols = colours, 
            gp = gpar(col = colours), byrow = FALSE, vp = vp, 
            ...)
    }
    gTree(width = width[[1]], height = height[[1]], children = gList(child), 
        cl = "ebimage")
}

#
#	<p> transformations
#

# <p> helper functions

# apply transformations on coordinates (given as row-wise points of a nx2 matrix)
applyT = function(coords, transf)t(transf %*% t(cbind(coords, 1)))[, -3]
# rectangle of width/height
rectWH = function(width, height)
	t(c(width, height) * (c(-.5, -.5) + t(matrix(c(0,0, 1,0, 1,1, 0,1), byrow = T, ncol = 2))))
# caculate circumference radius of rectangle
rectRad = function(width, aspectRatio, margin) {
	width = width * (1 + margin);
	height = width/aspectRatio;
	radius = norm(matrix(c(width, height)), '2') / 2;
}

# <p> actual transformations
# transformations are applied right to left (see applyT)

transform2dTranslate = function(delta) {
	matrix(c(1, 0, 0, 0, 1, 0, delta[1], delta[2], 1), ncol = 3)
}

transform2dRotation = function(alpha) {
	matrix(c(cos(alpha), sin(alpha), 0, -sin(alpha), cos(alpha), 0, 0, 0, 1), ncol = 3)
}

# tranlation after rotation
transform2dRotTrans = function(alpha, delta) {
	transform2dTranslate(delta) %*% transform2dRotation(alpha)
}

# move in direction of rotation after rotation
transform2dMoveYRot = function(dist, alpha) {
	transl = transform2dTranslate(c(0, dist));
	rot = transform2dRotation(alpha);
	r = rot %*% transl;
	r
}

# move in direction of rotation after rotation
transform2dRotMove = transform2dRotMoveY = function(alpha, dist) {
	rot = transform2dRotation(alpha);
	v = rot %*% c(dist, 0, 1);
	transform2dTranslate(v[1:2]) %*% rot
}


# create transformation that is created by rotating a points
#	the destination of the point is used to create a translation
transform2dRot2Move = function(alpha, point) {
	p = applyT(matrix(point, ncol = 2), transform2dRotation(alpha));
	transform2dTranslate(p);
}

dfTop = function(d, N)d[1:min(nrow(d), N),, drop = F]
vTop = function(v, N)v[1:min(length(v), N)]

#
#	<p> binning
#

# partition range of data into bins according to bins and choose Nrep representatives for each bin
binPlot = function(data, formula = NULL, bins = c(1e2, 1e2), Nrep = 3, eps = 1e-5, permute = TRUE,
	plotRange = NULL, returnIdcs = FALSE) {
	cols = if (is.null(formula)) 1:2 else match(all.vars(formula), dimnames(data)[[2]]);
	rangeX = if (is.null(plotRange$rangeX)) range(data[, cols[1]]) else plotRange$rangeX;
	rangeY = if (is.null(plotRange$rangeX)) range(data[, cols[2]]) else plotRange$rangeY;
	# bin number defined by numbering in 1st quadrant within range, row-first
	# i.e. (0, 0): origin, bins[1]: (0, 1), ...
	RX = (rangeX[2] - rangeX[1] + eps);
	RY = (rangeY[2] - rangeY[1] + eps);
	bin = as.vector(apply(data[, cols], 1, function(e) {
		binX = as.integer(floor(bins[1] * ((e[1] - rangeX[1]) / RX)));
		binY = as.integer(floor(bins[2] * ((e[2] - rangeY[1]) / RY)));
		bin = binY * bins[1] + binX;
		bin
	}));

	# <p> select data
	idcs = unlist(lapply(unique(bin), function(b) {
		idcs = which(bin == b)
		if (permute) idcs = Sample(idcs);
		vTop(idcs, Nrep)
	}));
	r = if (returnIdcs) idcs else data[idcs, , drop = F];
	r
}

binPlot_1 = function(data, formula = NULL, bins = c(1e2, 1e2), Nrep = 3, eps = 1e-5, permute = TRUE,
	plotRange = NULL, returnIdcs = FALSE) {
	cols = if (is.null(formula)) 1:2 else match(all.vars(formula), dimnames(data)[[2]]);
	rangeX = if (is.null(plotRange$rangeX)) range(data[, cols[1]]) else plotRange$rangeX;
	rangeY = if (is.null(plotRange$rangeX)) range(data[, cols[2]]) else plotRange$rangeY;
	# bin number defined by numbering in 1st quadrant within range, row-first
	# i.e. (0, 0): origin, bins[1]: (0, 1), ...
	RX = (rangeX[2] - rangeX[1] + eps);
	RY = (rangeY[2] - rangeY[1] + eps);
	bin = as.vector(apply(data[, cols], 1, function(e) {
		binX = as.integer(floor(bins[1] * ((e[1] - rangeX[1]) / RX)));
		binY = as.integer(floor(bins[2] * ((e[2] - rangeY[1]) / RY)));
		bin = binY * bins[1] + binX;
		bin
	}));

	# <p> select data
	dataSL = lapply(unique(bin), function(b) {
		d0 = data[which(bin == b), , drop = F];
		d1 = if (permute) d0[sample.int(nrow(d0)), , drop = F] else d0;
		dfTop(d1, Nrep)
	});
	dataS = do.call(rbind, dataSL);
	dataS
}
binPlot_0 = function(data, formula = NULL, bins = c(1e2, 1e2), Nrep = 3, eps = 1e-5, permute = TRUE) {
	cols = if (is.null(formula)) 1:2 else match(all.vars(formula), dimnames(data)[[2]]);
	rangeX = range(data[, cols[1]]);
	rangeY = range(data[, cols[2]]);
	# bin number defined by numbering in 1st quadrant within range, row-first
	# i.e. (0, 0): origin, bins[1]: (0, 1), ...
	bin = as.vector(apply(data[, cols], 1, function(e) {
		binX = as.integer(floor(bins[1] * ((e[1] - rangeX[1]) / (rangeX[2] - rangeX[1] + eps))));
		binY = as.integer(floor(bins[2] * ((e[2] - rangeY[1]) / (rangeY[2] - rangeY[1] + eps))));
		bin = binY * bins[1] + binX;
		bin
	}));
	data1 = cbind(data, bin);
	if (permute) data1 = data1[sample.int(nrow(data1)), ];

	# <p> ordered data
	# <A> assume order to be stable
	dataO = data1[order(data1[, 'bin']), ];

	# <p> selected data
	binsU = unique(dataO[, 'bin']);
	dataDf = as.data.frame(dataO);
	dataSL = lapply(binsU, function(b)subsetTop(dataO, with(dataDf, bin == b), Nrep));
	dataS = do.call(rbind, dataSL);
	dataS
}

#
#	<p> label annotation
#

# patch[XY]: multiply by constants to add a shift
labelAlignmentsStd = list(
	std = list(x = 0.5, y = 0, patchX = 0, patchY = 1, hjust = 0.5, vjust = 0, penalty = 0),
	leftL = list(x = 0, y = -1, patchX = -1, patchY = 0, hjust = 1, vjust = 1, penalty = 5),
	rightL = list(x = 1, y = -1, patchX = 1, patchY = 0, hjust = 0, vjust = 1, penalty = 5),
	left = list(x = 0, y = 0, patchX = 0, patchY = 1, hjust = 1, vjust = 0, penalty = 5),
	right = list(x = 1, y = 0, patchX = 0, patchY = 1, hjust = 0, vjust = 0, penalty = 5),
	high = list(x = 0.5, y = 1, patchX = 0, patchY = 1, hjust = 0.5, vjust = 0, penalty = 15)
);
layoutStd = list(extend = list(x = 20, y = 2, patchX = .1, patchY = .1, size = 1));

layoutForExtend = function(x = 50, y = 50, labelSzX = x/10, labelSzY = y/25) {
	Mx = max(x, y);
	return(list(extend = list(x = labelSzX, y = labelSzY, patchX = x/100, patchY = y/100, size = Mx / 15)));
}

labelBoundingBox = function(l, layout = layoutStd, alignments = labelAlignmentsStd) {
	a = FirstDef(alignments[[l$align]], alignments$std);
	e = FirstDef(l$extend, layout$extend);
	p = l$pos;
	s = layout$extend;

	x1 = p$x + (a$x - 1) * e$x + a$patchX * s$patchX;
	y1 = p$y + a$y * e$y + a$patchY * s$patchY;
	coords = matrix(c(x1, y1, x1 + e$x, y1, x1 + e$x, y1 + e$y, x1, y1+e$y), byrow = T, ncol = 2);
	return(coords);
}
labelsDf = function(labels, layout = layoutStd, alignments = labelAlignmentsStd) {
	rows = lapply(labels, function(l) {
		e = FirstDef(l$extend, layout$extend);
		a = FirstDef(alignments[[l$align]], alignments$std);
		with(l$pos, Df(x = x + a$patchX * e$patchX, y = y + a$patchY * e$patchY,
			annotation = l$annotation, hjust = a$hjust, vjust = a$vjust));
	});
	d = do.call(rbind, rows);
	return(d);
}

# close polygon, convert to SpatialPolygon, cartesian coordinates
labelBB2Spatial = function(coords) {
	poly = rbind(coords, coords[1, , drop = F]);
	return(SpatialPolygons(list(Polygons(list(Polygon(poly)), 'BB')), proj4string = CRS('+proj=cart')));
}

labelIntersections = function(bb, bbList) {
	lSpat = labelBB2Spatial(bb);
	diffs = sapply(bbList, function(e)area(intersect(lSpat, labelBB2Spatial(e))));
	return(diffs);
}

Area = function(p)if (is.null(p)) return(0) else area(p);
Intersect = function(p1, p2)suppressWarnings(intersect(p1, p2));
labelIntersectionsWeighted = function(bb, bbListW) {
	weights = list.kpu(bbListW, 'weight');
	lSpat = labelBB2Spatial(bb);
	diffs = sapply(bbListW, function(e)Area(Intersect(lSpat, labelBB2Spatial(e$coords))));
	return(diffs * weights);
}

# minmize overlap with labels in lList
labelFindBestAlignment = function(l, lList, alignments = labelAlignmentsStd, layout = layoutStd) {
	bbList = lapply(lList, labelBoundingBox, layout = layout);
	bbListL = list.embed(bbList, 'coords');
	weights = list.embed(list.kpu(lList, 'weight'), 'weight');
	bbListW = list.combine(list(bbListL, weights), doMerge = T);

	overlap = sapply(names(alignments), function(a) {
		bb = labelBoundingBox(merge.lists(l, list(align = a)), layout = layout);
		inter = labelIntersectionsWeighted(bb, bbListW);
		sum(inter);
	});
	o = overlap[which.min(overlap)];
	return(list(align = names(o), overlap = o));
}

labelsRearrangeSingle = function(labels, layout = layoutStd, alignments = labelAlignmentsStd) {
	overlap = 0;
	penalty = 0;
	for (i in 1:length(labels)) {
		a = labelFindBestAlignment(labels[[i]], labels[-i], layout = layout);
		labels[[i]]$align = a$align;
		overlap = overlap + a$overlap;
		penalty = penalty + alignments[[a$align]]$penalty;
	}
	return(list(labels = labels, overlap = overlap, penalty = penalty));
}

labelsRearrangeRaw = function(labels, layout = layoutStd, NiterMax = 2e2) {
	overlap = Inf;
	for (i in 1:NiterMax) {
		lsRearr = labelsRearrangeSingle(labels, layout = layout);
		if (lsRearr$overlap == overlap) break;
		overlap = lsRearr$overlap;
		labels = lsRearr$labels;
	}
	return(lsRearr);
}

labelsRearrange = function(labels, layout = layoutStd, Nperm = 3) {
	arrangements = lapply(1:Nperm, function(i) {
		labelsRearrangeRaw(if (i > 1) labels[Sample(1:length(labels))] else labels, layout = layout);
	});
	Iperm = which.min(list.kpu(arrangements, 'overlap'));
	LogS(3, "labelsRearrange: permuation with minimal overlap: %{Iperm}d");
	return(arrangements[[Iperm]]);
}

plotLabels = function(labels, layout = layoutStd) {
	dPts = Df_(do.call(rbind, list.kp(labels, 'pos')), deep_simplify = T);
	p = ggplot(data = dPts, aes(x = x, y = y)) + geom_point(size = 1) + theme_bw();
	dL = labelsDf(labels, layout = layout);
	print(dL);
	p = p + annotate('text', x = dL$x, y = dL$y, label = dL$annotation,
		hjust = dL$hjust, vjust = dL$vjust, size = lo$extend$size
	);

}

#
#	<p> longitudinal data
#

Aes = function(...)structure(list(...),  class = "uneval")
plotSpaghetti = function(f1, data, fSpaghetti = ~ time + StudyNumber) {
	dPairs = completeData(f1, data, collapse = T);
	data = droplevels(completeData(f1, data, collapse = T));

	time = all.vars(fSpaghetti)[1];
	group = all.vars(fSpaghetti)[2];

	response = formula.response(f1);
	pSp = ggplot(data, Aes(x = data[[time]], y = data[[response]], group = data[[group]])) +
		geom_line(aes(color = data[[group]])) +
		xlab(time) + ylab(response) + theme(legend.position = "none");
	return(pSp);
}

