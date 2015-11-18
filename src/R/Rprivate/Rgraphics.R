#
#	Rgraphics.R
#Mon 27 Jun 2005 10:52:17 AM CEST

require('grid');

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
	p.values = tr(sort(p.values));
	N = length(p.values);
	Ns = 1:N;
	# j-th order statistic from a uniform(0,1) sample has beta(j,n-j+1) distribution
	# (Casella & Berger, 2002, 2nd edition, pg 230, Duxbury)
	ciU = tr(qbeta(1 - alpha/2, Ns, N - Ns + 1));
	ciL = tr(qbeta(    alpha/2, Ns, N - Ns + 1));
	d = data.frame(theoretical = tr(Ns/N), ciU = ciU, ciL = ciL, p.value = p.values);
	p = ggplot(d) +
		geom_line(aes(x = theoretical, y = ciU, colour = colorCI)) +
		geom_line(aes(x = theoretical, y = ciL, colour = colorCI)) +
		geom_point(aes(x = theoretical, y = p.value), size = 1) +
		theme(legend.position = 'none') + coord_cartesian(ylim = c(0, max(p.values)*1.1)) +
		scale_y_continuous(name = trName) +
		theme(text = element_text(size = fontsize));
	p
}

vp_at = function(x, y)viewport(layout.pos.row = x, layout.pos.col = y);
plot_grid = function(plots, nrow, ncol, byrow = T, mapper = NULL) {
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
		
	# <p> do plotting
	grid.newpage();
	pushViewport(viewport(layout = grid.layout(nrow, ncol)));

	sapply(1:length(plots), function(i) {
		print(plots[[i]], vp = vp_at(coords[i, 1], coords[i, 2]));
	});
}

plot_grid_to_path =  function(plots, ..., path, width = 8, height = 8) {
	pdf(path, width = width, height = height);
		plot_grid(plots, ...);
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
	pdfOptions = list(paper = 'a4')) {
	Nplots = length(plots);
	if (missing(nrow)) nrow = NperPage / ncol;
	if (missing(ncol)) ncol = NperPage / nrow;
	if (missing(NperPage)) NperPage = ncol * nrow;
	Npages = ceiling(Nplots / NperPage);

	do.call(pdf, c(list(file = file), pdfOptions));
	sapply(1:Npages, function(i) {
		Istrt = (i - 1) * NperPage + 1;
		Istop = min(i * NperPage, Nplots);
		plot_grid(plots[Istrt:Istop], nrow, ncol, byrow = byrow, mapper = mapper);
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
qplot_survival = function(f.frame, f.CI = "default", f.shape = 3, ..., title = NULL, layers = NULL){
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
	if (!is.null(layers)) p = p + layers;
	p
}

quantileBinning = function(x, Nbins) {
	cut(x, quantile(x, seq(0, 1, length = Nbins + 1)), labels = seq_len(Nbins), include.lowest = TRUE)
}

kaplanMeierStrat = function(d1, f1, levels = NULL, title = NULL) {
	# <i> only allow one covariate
	stratVar = all.vars(formula.rhs(f1))[1];
	if (!is.null(levels)) {
		d1[[stratVar]] = as.factor(quantileBinning(d1[[stratVar]], levels));
	}

	stratValue = levels(d1[[stratVar]]);
	# <p> log-rank test
	lr = survdiff(as.formula(f1), data = d1);
	p.lr = pchisq(lr$chisq, df = dim(lr$n) - 1, lower.tail = F)
	# <p> kaplan-meyer
	fit = survfit(as.formula(f1), data = d1);
	fit.frame = createSurvivalFrame(fit);
	titleCooked = if (is.null(title))
		sprintf('%s, [P = %.2e]', stratVar, p.lr) else
		Sprintf('%{title}s, [P = %.2e]', p.lr)
	p = qplot_survival(fit.frame, F, 20, title = titleCooked,
		layers = theme_bw());
	list(plot = p, level = stratValue)
}

kaplanMeierNested = function(d, f1, strata, combine = FALSE) {
	dStrat = d[, strata];
	cbs = valueCombinations(dStrat);
	
	plots = apply(cbs, 1, function(r) {
		sel1 = nif(apply(dStrat == r, 1, all));
		sel = nif(sapply(1:nrow(d), function(i)all(dStrat[i,] == r)));
		if (sum(sel) == 0) browser();
		#if (sum(sel) == 0) return(NULL);
		dSel = d[sel, , drop = F];
		N = sum(sel);
		title = Sprintf('Stratum: %{stratum}s, N = %{N}d',
			stratum = paste(names(r), r, sep = '=', collapse = ', '));
		kaplanMeierStrat(dSel, f1, title = title);
	});
	plots
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
	dpi150 = list(from = function(dpi)(dpi/150*600), to = function(b)(b*150/600))
);
units_default = list(jpeg = 'dpi150', pdf = 'cm', png = 'points');

plot_save_raw = function(object, ..., width = 20, height = 20, plot_path = NULL,
	type = NULL, options = list(), unit = 'cm', unit_out = NULL, envir = parent.frame()) {

	device = get(type);
	if (is.null(unit_out)) unit_out = units_default[[type]];
	width = units_conv[[unit_out]]$to(units_conv[[unit]]$from(width));
	height = units_conv[[unit_out]]$to(units_conv[[unit]]$from(height));
	Log(Sprintf('Saving %{type}s to "%{plot_path}s"  [width: %{width}f %{height}f]'), 5);

	device(plot_path, width = width, height = height, ...);
		#ret = eval(object, envir = envir);
		ret = if (any(class(object) %in% c('ggplot', 'plot'))) {
			print(object)
		} else {
			eval(object, envir = envir);
		}
	dev.off();
}

plot_typeMap = list(jpg = 'jpeg');
plot_save = function(object, ..., width = 20, height = 20, plot_path = NULL,
	type = NULL,
	envir = parent.frame(), options = list(), simplify = T, unit, unit_out = NULL) {

	unitMissing = missing(unit);
	if (is.null(plot_path)) file = tempFileName('plat_save', 'pdf', inRtmp = T);
	ret = lapply(plot_path, function(plot_path) {
		if (is.null(type) && !is.null(plot_path)) {
			ext = splitPath(plot_path)$ext;
			type = firstDef(plot_typeMap[[ext]], ext);
		}
		if (unitMissing) unit = firstDef(units_default[[type]], 'cm');
		Logs("plot_path: %{plot_path}s, device: %{type}s", logLevel = 5);
		plot_save_raw(object, ..., type = type, width = width, height = height, plot_path = plot_path,
			options = options, unit = unit, unit_out = unit_out);
	});
	if (length(plot_path) == 1 && simplify) ret = ret[[1]];
	r = list(path = plot_path, ret = ret);
	r
}
