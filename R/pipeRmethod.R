#
#	pipeRmethod.R

# Expample: R --vanilla --slave -f test_pipeRmethod.R --args ../../test_data/LLS_Offspring_Partners_Final_36_130_Overlap_chr9_imputed-12.gens 'lc_1,lc_2' 'sex,age' ../../test_data/LC_phenos_qnorm_LLSnr.txt ../../test_data/pedfile

source('../../RgenericAll.R');


pipeRmethod = function(input, phenos, covs, variableFile, pedFile) {
	# <p> create data frame
	dat1<-read.table(variableFile,header=T)
	dat2<-read.table(pedFile)
	names(dat2)[1:4]<-c("fid","iid","pid","mid")
	peddata<-merge(dat1,dat2,by.x=1:2,by.y=c("fid","iid"),sort=F,all.y=T)
	phenos<-unlist(strsplit(phenos,","))
	covs<-unlist(strsplit(covs,","))
	phenocols<-try(subset(peddata,select=phenos))
	if (class(phenocols) == "try-error") stop(paste("One or more of phenotypes",phenos,"not found",sep=" "))
	for (cov in covs) {
		covcol<-try(subset(peddata,select=cov))
		if (class(covcol) != "try-error"){ phenocols<-cbind(phenocols,covcol)}
		else{ print(paste("Covariate",cov,"skipped; it is not in the phenotype file",sep=" "))}
	}
	gens<-read.table(input)
	
	# <p> source input script
	script = "datframetest.R"
	#cat(readFile(script));
	Log(sprintf("Sourcing script @ %s\n", script), 5);
	source(script, chdir = T);
	for (i in 1:nrow(gens)){
		genos<-gens[i,6:ncol(gens)]
		genoarray<-t(array(unlist(genos),dim=c(3,length(genos)/3)))
		snpnames<-paste(gens[i,2],c("AA","AB","BB"),sep="")
		colnames(genoarray)<-snpnames
		datframe<-cbind(peddata[,1:2],phenocols,genoarray)
		# <p> call function
		do.call(get("callFunction"), list(datframe));
	}
}

