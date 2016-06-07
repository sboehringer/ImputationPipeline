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
		
		if (suggestiveline) plot=plot+geom_hline(yintercept=suggestiveline,colour="blue", alpha=I(1/3))
		if (genomewideline) plot=plot+geom_hline(yintercept=genomewideline,colour="red")
		
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
