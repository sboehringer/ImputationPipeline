###############################################################################
### impGWASdb Object Utils: ###################################################
###############################################################################

read.gens_OLD <- function(pathIN,transform=T){
	#
	# Description: This function reads a .gens file and returns a data.frame.
	#
	# Input:
	# pathIN:	  	Location where file is stored.
	# transform:	Should the data be tranformed to a data.frame with data in
	#				the database format?
	#
	# Output:   	A data.frame is returned.
	#
	# Requires: 
	#
	# Remarks:        
	#
	# ModLog:    	  "Original Version"  			07/06/2011  EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) pathIN:
	if(!file.exists(pathIN)){
		stop("Please provide a valid path for 'pathIN'")
	}
	
	### Core: #################################################################
	
	# 1) Read:
### TODO: Write a version that cannot run out of memory
	D <- read.delim(pathIN,header=F,sep=" ")
	# 2) Transform:
	if(transform==T){
		Annot 	<- D[,1:5]
		### Data:
		# 2a) Check for NAs: (3 times 0):
		Ind <- matrix(6:dim(D)[2],byrow=T,ncol=3)
		NAind <- apply(Ind,1,function(x){
					length(which((D[,x[1]]==0)&(D[,x[2]]==0)&(D[,x[3]]==0)))})
		if(any(NAind!=0)){
			stop(paste("Data contains NAs encoded as 3 times 0.000 and is ",
							"currently not supported",sep=""))
		}
		# 2b) Force three digits:
		D 		<- as.matrix(format(D[,6:dim(D)[2]],nsmall=3))
		# 2b) Replace 1.000:
		D[which(D=="1.000")] <- "---"
		# 2c) Remove 0. :
		D <- gsub("0[.]","",D)
		# 2d) Collapse:
		D <- apply(D,1,function(x) paste(x,collapse=""))
		### Alleles:
		Annot$Alleles <- apply(Annot,1,function(x) paste(x[4],x[5],sep="|"))
		# 2e) Renice:
		D <- data.frame(cbind(Annot[,c(1,2,3,6)],D))
		colnames(D) <- c("imp.ID","gvar_meas","position","gvar_allele","gvar_data")
	} else {
		colnames(D) <- c("imp.ID","gvar_meas","position","Allele1","Allele2")
	}
	return(D)
}

read.gens <- function(pathIN){
	#
	# Description: This function reads a .gens file and returns a data.frame.
	#
	# Input:
	# pathIN:	  	Location where file is stored.
	#
	# Output:   	A data.frame is returned.
	#
	# Requires: 
	#
	# Remarks:        
	#
	# ModLog:    	  "Original Version"  			07/06/2011  EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) pathIN:
	if(!file.exists(pathIN)){
		stop("Please provide a valid path for 'pathIN'")
	}
	
	### Core: #################################################################
	
	# 1) Read:
	D <- readLines(pathIN)
	
	##### Annot:
	# 2) Cut part containing annotations:
	Ind <- regexpr("[ACGT] [ACGT]",D)
	Annot <- substring(D,1,(Ind+2))
	Annot <- strsplit(Annot," ")
	Annot <- data.frame(do.call("rbind",Annot))
	# 3) Alleles:
	Annot$Alleles <- apply(Annot,1,function(x) paste(x[4],x[5],sep="|"))
	# 4) Renice:
	Annot <- Annot[,c(1,2,3,6)]
	colnames(Annot) <- c("imp.ID","gvar_meas","position","gvar_allele")
	Annot$position 	<- as.numeric(Annot$position)
	
	##### Data:
	# 5) Cut part containing data:
	D <- substring(D,(Ind+3),nchar(D))
	# 6) Replace " 0" if not followed by a ".":
	D <- gsub("(?<= )0(?!\\.)","000",D,perl=T)
	# 7) Replace "1", if preceded by " " and not folowed by one of "0":"9":
	D <- gsub("(?<= )1(?![0-9])","---",D,perl=T)
	# 8) Remove "0." if preceded by a " " and followed by a "0":"9":
	D <- gsub("(?<= )0\\.(?=[0-9])","",D,perl=T)
	# 9) Check for NAs: (3 times 000): on positions 2+(n*12)
	Ind <- gregexpr("000 000 000",D)
	falseInd <- seq(2,(max(unlist(Ind))+12),12)
	NAind <- sapply(Ind,function(x) which(is.element(x,falseInd)))
	if(any(sapply(NAind,length)!=0)){
		stop(paste("Data contains NAs encoded as 3 times 0.000 and is ",
						"currently not supported",sep=""))
	}
	# 10) Remove spaces:
	D <- gsub(" ","",D)
	# 11) Merge with annnot:
	Annot$gvar_data <- D
	return(Annot)
}

read.gens.info <- function(pathIN,quiet=F){
	#
	# Description: This function reads the additional information in .gens 
	#			file(s) and returns this in a data.frame.
	#
	# Input:
	# pathIN:	  	Location(s) where file(s) are stored.
	# quiet:		Verbose when set to TRUE
	#
	# Output:   	A data.frame is returned.
	#
	# Requires: 
	#
	# Remarks:        
	#
	# ModLog:    	  	"Original Version"  		15/06/2011  EBvandenAkker
	#					"Multiple paths"			17/06/2011	EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) pathIN:
	if(any(!file.exists(pathIN))){
		stop("Please provide a valid path(s) for 'pathIN'")
	}
	# 2) quiet:
	if(!isSingleLogical(quiet)){
		stop("Please provide a single logical value for 'quiet'")
	}
	
	### Defining SubFunctions: ################################################
	
	### Defining a subFunction for extracting mapping info from a single file:
	read.single_gens.info <- function(x){
		if(quiet==F){
			cat(paste("Reading: ",x , "\n",sep=""))
		}
		# 1) Read Data:
		Annot <- scan(x,what=as.list(rep("character",5)),flush=T,quiet=T)
		Annot <- data.frame(do.call("cbind",Annot))
		# 2) Alleles:
		Annot$Alleles <- apply(Annot,1,function(x) paste(x[4],x[5],sep="|"))
		# 4) Renice:
		Annot <- Annot[,c(1,2,3,6)]
		colnames(Annot) <- c("imp.ID","gvar_meas","position","gvar_allele")
		Annot$position 	<- as.numeric(Annot$position)
		return(Annot)
	}
	
	### Core: #################################################################
	
	if(quiet==F){
		cat("Extracting mapping info: \n")
	}
	AllAnnot <- sapply(1:length(pathIN),function(x){
					cbind(read.single_gens.info(pathIN[x]),x)},simplify=F)
	if(quiet==F){
		cat("Done! \n")
	}
	AllAnnot <- data.frame(do.call("rbind",AllAnnot))
	colnames(AllAnnot) <- c("imp.ID","gvar_meas","position","gvar_allele","fileInd")
	rownames(AllAnnot) <- NULL
	return(AllAnnot)
}

create.impGWASdb.from.rens <- function(pathIN,dbName,folderOUT,parseChrom=F,
		mapSource="",mapVersion="",quiet=F){
	#
	# Description: This utility function builds a database of imputed GWAS data
	#				obtained from rens files.
	#
	# Input:
	# pathIN:	  	Location(s) where file(s) is stored.
	# dbName:		A string for naming your db.
	# folderOUT:	A path to a folder where to store the database.
	# parseChrom:	A logical indicating whether the chromosome information
	#					should be parsed from the filenames in pathIN.
	# mapSource:	Used to store the source of a mapping: "UCSC" / "HapMap"
	# mapVersion:	Used to store version info: "NCBI36/hg18_dbSNP130". Some
	#					strings are designed to be recognized.
	# quiet:		Verbose when set to TRUE
	#
	# Output:   	An object holding an environment poiting to a SQLite 
	#					database
	#
	# Requires: 
	#
	# Remarks:        
	#
	# ModLog:    	  "Original Version"  			15/06/2011  EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) pathIN:
	# 1a) character:
	if(!is.character(pathIN)){
		stop("Please provide strings for 'pathIN'")
	}
	# 1b) exist:
	if(any(!file.exists(pathIN))){
		stop("Please provide a valid path(s) for 'pathIN'")
	}
	# 2) dbName & folderOUT:
	# All Checks for dbName and folderOUT are handled by lower level functions.
	# 3) parseChrom:
	if(!isSingleLogical(parseChrom)){
		stop("Please provide a single logical value for 'parseChrom'")
	}
	# 4) mapSource:
	if(!isSingleString(mapSource)){
		stop("Please provide a single character string for 'mapSource'")
	}
	# 5) mapVersion:
	if(!isSingleString(mapVersion)){
		stop("Please provide a single character string for 'mapVersion'")
	}
	# 6) quiet:
	if(!isSingleLogical(quiet)){
		stop("Please provide a single logical value for 'quiet'")
	}

	### Core: #################################################################
	
	# 1) Database Initiation: (+Checks)
	db <- .init.GVARdb(dbType="impGWASdb",dbName,folderOUT)
	# 2) Loading Data:
	if(quiet==F){
		cat("Extracting data: \n")
	}
	for(i in 1:length(pathIN)){
		if(quiet==F){
			cat(paste("Reading ",pathIN[i],"\n",sep=""))
		}
		D <- read.gens(pathIN[i])
		D <- .normarg.data(D[,c("gvar_meas","gvar_data","gvar_allele")])
		Dummi <- .append.data.table(.dbConn(db),data=D)
	}
	if(quiet==F){
		cat("Done! \n")
	}
	# 3) Loading mapping:
	if(parseChrom==T){
		# 3a) Chrom:
		Start <- regexpr(paste("chr[",paste(1:22,collapse="|"),"|X|Y|Z]",sep=""),
				pathIN)
		if(any(Start==-1)){
			warning(paste("Chromosomes could not be extracted from filenames; ",
							"database will not contain mapping info",sep=""))
		} else {
			Chrom <- substring(pathIN,Start,((Start+attr(Start,"match.length")-1)))
			# 3b) Loading mapping data:
			Map <- read.gens.info(pathIN,quiet=quiet)
			# 3c) Add chromosome info:
			Map$chrom <- Chrom[Map$fileInd]
			# 3d) Add strand info:
			Map$strand <- "*"
			# 3e Renice:
			Map <- Map[,c("chrom","position","position","gvar_meas","strand")]
			colnames(Map) <- c("chrom","start","end","gvar_name","strand")
			# 3g) Add Map:
			Dummi <- addMap(db,map=Map,mapSource,mapVersion,overwrite=F,
					backupCopy=F)	
		}
	}
	return(db)
}

