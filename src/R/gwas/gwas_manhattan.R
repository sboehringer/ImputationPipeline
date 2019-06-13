library(ggplot2)

# Stephen Turner
# http://StephenTurner.us/
# http://GettingGeneticsDone.blogspot.com/
# See license at http://gettinggeneticsdone.blogspot.com/p/copyright.html

# Last updated: Wednesday, June 23, 2010
# R code for making manhattan plots and QQ plots from plink output files. 
# With GWAS data this can take a lot of memory. 
# Recommended for use on 64bit machines only, for now. 


# This is for testing purposes.
# nchr=11
# nsnps=2000
# fakedata=data.frame(
	# CHR=rep(1:nchr,each=nsnps), 
	# BP=rep(1:nsnps,nchr), 
	# P=runif(nchr*nsnps)
# )


manhattan = function(dataframe, title=NULL, max.y="max", suggestiveline = 0,
	significance = 5e-8, size.x.labels=9, size.y.labels=10, annotate=F, SNPlist=NULL) {

	genomewideline = -log10(significance);
	if (annotate & is.null(SNPlist)) stop("You requested annotation but provided no SNPlist!")
	
	d=dataframe
	
	#limit to only chrs 1-23?
	d=d[d$CHR %in% 1:23, ]

	if ("CHR" %in% names(d) & "BP" %in% names(d) & "P" %in% names(d) ) {
	
		d=na.omit(d)
		d=d[d$P>0 & d$P<=1, ]
		d$logp = -log10(d$P)
		
		d$pos=NA
		ticks=NULL
		lastbase=0
		
		#new 2010-05-10
		CHRs = unique(d$CHR);
		numchroms = length(CHRs)
		if (numchroms==1) {
			d$pos=d$BP
		} else {
			for (j in 1:length(CHRs)) {
				i = CHRs[j];
				if (j == 1) {
					d[d$CHR == i, ]$pos = d[d$CHR == i, ]$BP;
				}	else {
					lastbase = lastbase + tail(subset(d, CHR == CHRs[j - 1])$BP, 1);
					d[d$CHR == i, ]$pos = d[d$CHR == i, ]$BP + lastbase;
				}
				ticks=c(ticks, d[d$CHR==i, ]$pos[floor(length(d[d$CHR==i, ]$pos)/2)+1])
			}
			ticklim=c(min(d$pos),max(d$pos))

		}
		
		mycols=rep(c("gray10","gray60"), length(CHRs))
		
		if (max.y=="max") maxy=ceiling(max(d$logp)) else maxy=max.y
		if (maxy<8) maxy=8

		if (annotate) d.annotate=d[as.numeric(substr(d$SNP,3,100)) %in% SNPlist, ]
		
		if (numchroms == 1) {
			plot=qplot(pos, logp, data = d, ylab = expression(-log[10](italic(p))),
				xlab = paste("Chromosome", CHRs, 'position'))
		}	else {
			plot = qplot(pos, logp, data=d, ylab=expression(-log[10](italic(p))) , colour = factor(d$CHR))
			plot = plot+scale_x_continuous(name="Chromosome", breaks=ticks, labels = CHRs)
			plot = plot+scale_y_continuous(limits=c(0,maxy), breaks=1:maxy, labels = 1:maxy)
			plot = plot+scale_colour_manual(values = mycols)
		}
		
		if (annotate) 	plot=plot + geom_point(data=d.annotate, colour=I("green3")) 
		
		plot = plot + theme(legend.position = "none") 
		plot = plot + ggtitle(title)
		plot = plot + theme(
			panel.background = element_blank(), panel.grid.minor = element_blank(),
			axis.text.x = element_text(size=size.x.labels, colour = "grey50"), 
			axis.text.y = element_text(size=size.y.labels, colour = "grey50"), 
			axis.ticks = element_line(colour = NA)
		)
		
		if (suggestiveline) plot = plot + geom_hline(yintercept=suggestiveline, colour="blue", alpha=I(1/3))
		if (genomewideline) plot = plot + geom_hline(yintercept=genomewideline, colour="red")
		
		plot
		
	}	else {
		stop("Make sure your data frame contains columns CHR, BP, and P")
	}
}



qq = function(pvector, title=NULL, spartan=F) {
	o = -log10(sort(pvector,decreasing=F))
	#e = -log10( 1:length(o)/length(o) )
	e = -log10( ppoints(length(pvector) ))
	plot=qplot(e,o, xlim=c(0,max(e)), ylim=c(0,max(o))) + stat_abline(intercept=0,slope=1, col="red")
	plot=plot+opts(title=title)
	plot=plot+scale_x_continuous(name=expression(Expected~~-log[10](italic(p))))
	plot=plot+scale_y_continuous(name=expression(Observed~~-log[10](italic(p))))
	if (spartan) plot=plot+opts(panel.background=theme_rect(col="grey50"), panel.grid.minor=theme_blank())
	plot
}

# Base graphics way faster.
qqbase = function(pvector, main=NULL) {
	o = -log10(sort(pvector,decreasing=F))
	#e = -log10( 1:length(o)/length(o) )
	e = -log10( ppoints(length(pvector) ))
	plot(e,o,pch=19,cex=1, xlab=expression(Expected~~-log[10](italic(p))), ylab=expression(Observed~~-log[10](italic(p))), xlim=c(0,max(e)), ylim=c(0,max(e)), main=main)
	abline(0,1,col="red")
}


qqman = function(data="plinkresults") {
	myqqplot = qq(data$P)
	mymanplot = manhattan(data)
	ggsave(file="qqplot.png",myqqplot,w=5,h=5,dpi=100)
	ggsave(file="manhattan.png",mymanplot,width=12,height=9,dpi=100)
}
	
qqmanall= function(command="ls *assoc") {
	
	filelist=system(command,intern=T)
	datalist=NULL
	for (i in filelist) {datalist[[i]]=read.table(i,T)}
	highestneglogp=ceiling(max(sapply(datalist, function(df) max(na.omit(-log10(df$P))))))
	print(paste("Highest -log10(P) = ",highestneglogp),quote=F)

	start=Sys.time()

	for (i in names(datalist)) {
		myqqplot=qq(datalist[[i]]$P, title=i)
		ggsave(file=paste("qqplot-",    i, ".png", sep=""),myqqplot, width=5, height=5,dpi=100)
		mymanplot=manhattan(datalist[[i]], title=i, max.y=highestneglogp)
		ggsave(file=paste("manhattan-", i, ".png", sep=""),mymanplot,width=12,height=9,dpi=100)
	}

	end=Sys.time()

	print(elapsed<-end-start)
}

#
#	<p> good versions
#

# <!> bug fix for formula interface
mlog10 = function(...)-log10(...);
# assume formula: p-value ~ chr + position
gwasManhattanPlotMatrix = function(formula, data, chromosomes = 1:23, yaxisCutoff = 15,
	colorBy = NULL, byCutoff = 3) {
	# <p> preparation
# 	vars = all.vars(formula);
# 	d1 = Df_(data, headerMap = listKeyValue(vars, nsStd));
	nsStd = c('p', 'chr', 'position');
	d1 = DfNames2std(data, formula, nsStd);
	chrs = sort(intersect(chromosomes, unique(d1$chr)));
	d1 = subset(d1, chr %in% chrs);

	# <p> raw data to plot
	f1 = ~ 0 + mlog10(p) + chr + position;
	mm = model.matrix(f1, model.frame(f1, data = d1));
	mm[, 'mlog10(p)'] = ifelse(mm[, 'mlog10(p)'] > yaxisCutoff, yaxisCutoff, mm[, 'mlog10(p)']);
	dimnames(mm)[[2]] = nsStd;

	# <p> coordinates
	chrPos = c(0, cumsum(sapply(chrs, function(c)max(mm[, 'position'][mm[, 'chr']== c], na.rm = T))));
	chrMap = listKeyValue(chrs, 1:length(chrs));
	plotMat = cbind(
		mm[, c('p', 'chr')],
		(mm[, 'position'] + chrPos[avu(chrMap[as.character(mm[, 'chr'])])])
	);
	dimnames(plotMat)[[2]] = nsStd;
	chrPositions = do.call(rbind, list(
		start = pop(chrPos), end = chrPos[-1], mid = (pop(chrPos) + chrPos[-1])/2));
	dimnames(chrPositions)[[2]] = chrs;
	# add categories
	# 	plotMat = cbind(plotMat, matrix(if (...)...
	# 		ncol = 1, dimnames = list(c(), 'color')));
	color = if (!is.null(colorBy)) {	# nif/Nif not working on formulas
		cats0 = as.character(factorFromFormula(data, colorBy));
		as.factor(with(Df_(plotMat), ifelse (p < byCutoff, ifelse(chr %% 2, 'odd', 'even'), cats0)));
	} else as.factor(ifelse(plotMat[, 'chr'] %% 2, 'odd', 'even'));
	color = recodeLevels(color, levels = c('odd', 'even'));
	r = list(plotMatrix = plotMat, chrPositions = chrPositions, chromosomes = chrs, color = color);
	r
}

# convert positions relative to chromosome start to absolute values across all chromosomes
chrPos2abs = function(dPos, chrPositions) {
	dPos$position = dPos$position + chrPositions['start', dPos$chr];
	dPos
}
absDistPairs = function(v)matrix(abs(kronecker(v, v, '-')), nrow = length(v));
# <!> choose moveY slightly larger than threshY to avoid endless loopc
pos2nonOverlapping = function(dPos, oderByP = TRUE, threshX = 1e7, threshY = 0.15, moveX = 0, moveY = 0.151) {
	if (oderByP) dPos = dPos[rev(order(dPos$p)), , drop = F];
	repeat {
		dX = absDistPairs(dPos$position);
		dY = absDistPairs(dPos$p);
		toMove = (dY > 0 & dY < threshY & dX > 0 & dX < threshX);
		# nothing to move
		if (!any(toMove)) break;
		pairs = sapply(which(toMove) - 1, ord2adic, rep(nrow(dX), 2)) + 1;
		pair = pairs[pairs[, 1] < pairs[, 2], , drop = F][1, ];
		LogS(2, 'MHP annotation: pairs to move: %{N}d', N = nrow(pairs));
		# <N> move second pair member
		if (moveY >= threshY) dPos$p[pair[2]] =
			dPos$p[pair[1]] + moveY * ifelse(dPos$p[pair[2]] < dPos$p[pair[1]], -1, 1);
		if (moveX >= threshX) dPos$position[pair[2]] = 
			dPos$position[pair[1]] + moveX * ifelse(dPos$position[pair[2]] < dPos$position[pair[1]], -1, 1)
	}
	dPos
}


#library(RColorBrewer)
#colorRampPalette(brewer.pal(9, "Set1"))(12)
gwas_manhattenPlotDefaults = list(
	title = 'Manhattan Plot',
	colors = c('gray10', 'gray50'),
	size = 1,
	#colorsCats = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
	colorsCats = c("#E41A1C", "#66628D", "#419486", "#5A9D5A", "#91569A", "#D96D3B", "#FFAD12",
		"#F6EF32", "#B6742A", "#D26D7A", "#DD87B4", "#999999"),
	textColor = 'grey50',
	extendYmin = mlog10(1e-8),
	extendY = 'data',
	yaxisCutoff = mlog10(1e-15),
	colorByCutoff = mlog10(1e-3),	# cutoff for choosing colors when using by argument
	size.x.labels = 10,
	size.y.labels = 11,
	thresholds = list(
		geom_hline(yintercept = mlog10(5e-6), colour = 'red', alpha = I(1/3)),
		geom_hline(yintercept = mlog10(5e-8), colour = 'red')
	),
	xaxisPhysical = FALSE
);
gwas_annotationOptionsDefaults = list(
	offX = 1e7, offY = 0.03
);

gwasManhattanPlotRaw = function(data, chromosomes, chrPositions, options = list(), annotation = NULL)
	with(merge.lists(gwas_manhattenPlotDefaults, options), {

	# <p> prepare parameters
	if (extendY == 'data') extendY = ceiling(max(data$p));
	if (extendY < extendYmin) extendY = extendYmin;

	# <p> base plot
	p = ggplot(data, aes(x = position, y = p, colour = color)) +
		geom_point(size = size) +
		scale_y_continuous(limits = c(0, extendY), breaks = 1:extendY, labels = 1:extendY) +
		ylab(expression(-log[10](italic(p)))) + 
		ggtitle(title) + theme_bw() + theme(
			panel.background = element_blank(), panel.grid.minor = element_blank(),
			axis.text.x = element_text(size = size.x.labels, colour = textColor), 
			axis.text.y = element_text(size = size.y.labels, colour = textColor), 
			axis.ticks = element_line(colour = textColor)
		);

	# <p> x-axis
	if (xaxisPhysical) {
		p = p + scale_x_continuous(name =
			join(c('Physical position, chromosomes: ', join(chromosomes, ','))));
	} else {
		noColors = setequal(levels(data$color), c('even', 'odd'));
		legendLevels = if (noColors) levels(data$color) else setdiff(levels(data$color), c('even', 'odd'));
		p = p + scale_x_continuous(name = 'Chromosome',
			breaks = chrPositions['mid', ], labels = chromosomes) +
			#scale_colour_manual(values = rep(colors, length(chromosomes))) +
			scale_colour_manual(breaks = legendLevels, values = c(colors, colorsCats));
		p = p + if (noColors) theme(legend.position = "none") else
			guides(colour = guide_legend(title = legendTitle));
	}
	for (t in thresholds) p = p + t;
	p = if (notE(annotation)) with(merge.lists(gwas_annotationOptionsDefaults, annotation), {
		dAnn = DfNames2std(annotation, pos, c('p', 'chr', 'position'));
		dAnn$p = mlog10(dAnn$p);	# <A> unify with general log10 transformation
		dAnnAbs = chrPos2abs(dAnn, chrPositions);
		dAnnNO = pos2nonOverlapping(dAnnAbs);
		p = p + annotate('text',
			x = dAnnNO$position + offX, y = dAnnNO$p + offY,
			label = dAnnNO[[all.vars(value)]],
			hjust = 0, vjust = 0.5
		);
	}) else p;
	p
})

#
#	Annotation example
#
# 	sel = r$marker %in% snps2ann;
# 	ann = r[sel, ];
# 	mhp = gwasManhattanPlot(P.value ~ chr + position, r,
# 		annotation = list(value = ~ marker, pos = P.value ~ chr + position, annotation = ann))


gwasManhattanPlot = function(formula, data, chromosomes = 1:23,
	bins = c(5e2L, 1e2L), Nrep = 5, options = list(), colorBy = NULL, annotation = NULL)
	with(merge.lists(gwas_manhattenPlotDefaults, options), {

	# <p> prepare data
	pm = gwasManhattanPlotMatrix(formula, data, chromosomes, yaxisCutoff, colorBy);
	Log("Transformed Manhatten data", 4, doPrint = gc());
	binned = binPlot(pm$plotMatrix, ~ position + p, bins = bins, returnIdcs = T);
	dataP = pm$plotMatrix[binned, , drop = F];
	Log("Binned Manhatten data", 4, doPrint = gc());

	# <p> prepare parameters
	dataPC = Df(dataP, color = pm$color[binned]);
	if (!is.null(colorBy) && nit(options$legendTitle))
		options = c(options, list(legendTitle = join(all.vars(colorBy), ':')));
		p = gwasManhattanPlotRaw(dataPC, pm$chromosomes, pm$chrPositions, options, annotation);
	Log("Created plot", 4, doPrint = gc());
	p
})

gwasManhattanPlot2file = function(formula, data, output, chromosomes = 1:23,
	bins = c(5e2L, 1e2L), Nrep = 5, extensions = c('jpeg', 'png', 'pdf'), options = list(),
	pp = list(width = valueU(29.7, 'cm'), height = valueU(21, 'cm'),
		options = list(jpeg = list(unit_out = 'dpi300'), png = list(unit_out = 'dpi150')))) {

	p = gwasManhattanPlot(formula, data, chromosomes = chromosomes, bins = bins, Nrep = Nrep);
	r = plot_save(p,
		plot_path = paste(output, extensions, sep = '.'),
		width = pp$width, height = pp$height, options = pp$options
	);
	output = r$path[1];
	output
}
