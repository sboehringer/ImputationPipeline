###############################################################################
### GVARdb Object Utils: ######################################################
###############################################################################
#
# General Description: This file contains some general utility functions used 
# in the GVARdb package that work on all subclasses of the GVARdb class. 
# Some functions were inspired or even completely copied from previous packages
# featuring RSQLite databases (GenomicFeatures) or dealing with huge input 
# files (snpMatrix). Repeating some of these functions here ensures 
# independency from these packages for now.

###############################################################################
###### db Utils: Low level Validity Checks:
# These functions are automatically called upon creation of a new GVARdb

.valid.conn <- function(conn){
	#
	# Description: Low level check whether connection is valid.
	#
	# Input:				
	# conn:					A SQLite Connection
	#
	# Output:				Validity checks are performed.
	#
	# Requires:           	
	#
	# Remarks:            	
	#
	# ModLog:             	"Original Version"  	05/06/2011  EBvandenAkker 
	
	### Checking: #############################################################
	
	### Checking input:
	if(!is(conn, "SQLiteConnection")){
		stop("'conn' must be an SQLiteConnection object")
	}
}

.valid.TableName <- function(tablename){
	#
	# Description: Low level check whether tablename is valid.
	#
	# Input:				
	# tablename:			The specified table
	#
	# Output:				Validity checks are performed.
	#
	# Requires:           	
	#
	# Remarks:            	
	#
	# ModLog:             	"Original Version"  	05/06/2011  EBvandenAkker 
	
	### Checking: #############################################################
	
	### Checking input:
	# 1) type: single string:
	if(!isSingleString(tablename)){
		stop("Please provide a single character string for 'tablename'")
	}
	# 2) known:
	if(!any(is.element(tablename,.required_tables))){
		stop("'tablename' must be one of ", paste(.required_tables, collapse=" "))
	}
}

.valid.TableColnames <- function(colnames, tablename){
	#
	# Description: This function checks whether the provided colnames match the 
	# column names of a specified table.
	#
	# Input:
	# colnames:         A vector of characters
	# tablename:       	The specified table
	#
	# Output:      		Validity checks are performed.
	#
	# Requires:       	libraries: DBI & RSQLite
	#
	# Remarks:          Adjusted from GenomicFeatures
	#
	# ModLog:           "Original Version"  		06/10/2010  EBvandenAkker 
	#					"Rm check req_colnames" 	05/06/2011 	EBvandenAkker
	#				  	"Low Level Validity Funct"	05/06/2011  EBvandenAkker
	#
	
	### Checking: #############################################################
	
	### Checking input:
	# 1) colnames:
	if(!is.character(colnames)){
		stop("Please provide a vector of characters for 'colnames'")
	}
	# 2) tablename:
	.valid.TableName(tablename)
	# 3) check required colnames:
	Ind <- which(tablename==.required_tables)
	if(any(!is.element(.required_colnames[[Ind]],colnames))){
		stop("Provided values for 'colnames' do not match required values")
	}
	# 4) check optional colnames:
	if(length(colnames)>length(.required_colnames[[Ind]])){
		Opt <- sort(setdiff(colnames,.required_colnames[[Ind]]))
		ExpOpt <- gsub("[*]","",setdiff(.optional_colnames[[Ind]],"gvar_meas"))
		Nrep   <- length(Opt)/length(ExpOpt)
		if(as.integer(Nrep)!= Nrep){
			stop(paste("Provided additional values for 'colnames' do not ", 
								"match expected optional values",sep=""))
		}
		ExpOpt <- sort(paste(rep(ExpOpt,Nrep),
								rep(2:(Nrep+1),times=length(ExpOpt)),sep=""))
		if(any(ExpOpt!=Opt)){
			stop(paste("Provided additional values for 'colnames' do not", 
								"match expected optional values",sep=""))
		}
	}
}

.valid.dbTable <- function(conn,tablename){
	#
	# Description: This is a low-level check whether a database table is present
	# and has the correct header.
	#
	# Input:
	# tablename:	  The specified table
	# conn:           SQLite connection
	#
	# Output:   	  Validity checks are performed.
	#
	# Requires: 
	#
	# Remarks:       
	#
	# ModLog:         "Original Version"  			05/06/2011  EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking input:
	# 1) conn:
	.valid.conn(conn)
	# 2) tablename:
	.valid.TableName(tablename)
	# 3) existence tablename:
	res <- try(dbExistsTable(conn, tablename), silent=TRUE)
	if(is(res, "try-error")){
		stop("the provided SQLite file is invalid")
	}
	if(!res){
		stop(paste("Cannot find table named '",tablename,
						"' in provided SQLite database", sep=""))
	}
	# 4) Checking expected colnames:
	sql0 <- paste("SELECT * FROM ", tablename, " LIMIT 0", sep="")
	data0 <- dbGetQuery(conn, sql0)
	colnames0 <- colnames(data0)
	.valid.TableColnames(colnames0, tablename)
}

.valid.dbType <- function(dbType){
	#
	# Description: Low level check whether dbType is valid.
	#
	# Input:				
	# dbType:				The specified datbase type
	#
	# Output:				Validity checks are performed.
	#
	# Requires:           	
	#
	# Remarks:            	
	#
	# ModLog:             	"Original Version"  	05/06/2011  EBvandenAkker 
	
	### Checking: #############################################################
	
	### Checking input:
	# 1) type: single string:
	if(!isSingleString(dbType)){
		stop("Please provide a single character string for 'dbType'")
	}
	# 2) known:
	if(!any(is.element(dbType,.supported_classes))){
		stop("'type' must be one of ", paste(.supported_classes, collapse=" "))
	}
}

.valid.TableLength <- function(conn,tablename,L){
	# Description: Low level check whether number of records is valid.
	#
	# Input:
	# conn:         	A connection to a SQLite database.
	# tablename:	  	The specified table
	# L:				Expected number of records.
	#
	# Output:   	    Validity checks are performed.
	# 					
	#
	# Requires:     
	#				
	#				
	# Remarks:      
	#
	# ModLog:      	"Original Version"  		09/06/2011    	EBvandenAkker	
	
	### Checking: #############################################################
	
	### Checking input:
	# 1) conn & tablename
	.valid.dbTable(conn,tablename)
	# 2) L:
	if(!is.numeric(L)){
		stop("L should be a numeric ")
	}
	
	### Core: #################################################################
	# 1) Request number of records:
	L1 <- .dbTableLength(conn,tablename)
	# 2) Check:
	if(L1!=L){
		stop(paste("Unexpected number of records in '",tablename,"'",sep=""))
	}
	sql 	<- paste("SELECT COUNT(*) FROM ",tablename,sep="")
	L   	<- as.numeric(dbGetQuery(conn,sql)[1,1])
	return(L)
}

###############################################################################
###### db Utils: High Level Constructors:

GVARdb <- function(conn,dbType){
	#
	# Description: This is a low-level constructor function used to create an 
	# instance of a GVARdb object.
	#
	# Input:
	# conn:           SQLite connection
	# dbType:         One of the supported classes each holding different data 
	# 				  types
	#
	# Output:   
	# XXXDb object:   Initializes an object of the supported classes.
	#
	# Requires: 
	#
	# Remarks:        Adjusted from GenomicFeatures
	#
	# ModLog:         "Original Version"  			19/07/2010  EBvandenAkker
	#				  "Low Level Validity Funct"	05/06/2011  EBvandenAkker 	
	
	### Checking: #############################################################
	
	### Checking input:
	# 1) conn:
	.valid.conn(conn)
	# 2) dbType:
	.valid.dbType(dbType)
	
	### Core: #################################################################
	envir <- new.env(parent=emptyenv())
	assign("conn", conn, envir=envir)
	reg.finalizer(envir, function(e) dbDisconnect(.getConn(e)))
	eval(call("new",dbType,envir=quote(envir)))
}

###############################################################################
###### db Utils: Low Level Constructors:

.append.table <- function(conn,tablename,dataFrame){
	#
	# Description: This helper function appends a dataframe to a connection in
	# an existing table.
	#
	# Input:  
	# conn:          	A connection to a SQLite database.
	# dataFrame:        A data.frame
	#
	# Output:			The data table of the GVAR object is appended.
	# 
	# Requires:
	#
	# Remarks:          
	# 
	# ModLog:           "Original Version"  28/11/2010    	EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking input:
	# 1) existing table:
	.valid.dbTable(conn,tablename)
	# 2) dataFrame:
	# 2a) data.frame:
	if(!is.data.frame(dataFrame)){
		stop("'dataFrame' should be a data.frame")
	}
	# 2b) Checks for data types are handled by database
	# 2c) Checks for dimensions are handled by database
	
	### Core: #################################################################
	
	Dim <- dim(dataFrame)[2]
	sql <- paste("INSERT INTO ",tablename," VALUES (",
			paste(rep("?",Dim),collapse=","),")",sep="")
	dbBeginTransaction(conn) 
	res <- dbSendPreparedQuery(conn, sql, bind.data=dataFrame)
	dbClearResult(res)
	dbCommit(conn)
}

###### db Utils: Low Level Constructors: Table initiation:

.init.data.table <- function(conn){
	#
	# Description: Low Level Constructor for initiating the data table of a 
	#	GVARdb object.
	#
	# Input:
	# conn:          SQLite connection
	#
	# Output:   	 Initializes a data table.
	#
	# Requires: 
	#
	# Remarks:        
	#
	# ModLog:    	  "Original Version"  			05/06/2011  EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) conn:
	.valid.conn(conn)
	
	### Core: #################################################################
	
	# 1) Create Table:
	sql <- c(
			"CREATE TABLE data (\n",
			"  gvar_id INTEGER PRIMARY KEY,\n",
			"  gvar_meas TEXT NOT NULL,\n",
			"  gvar_data TEXT NOT NULL,\n",
			"  gvar_allele TEXT NOT NULL,\n",
			"  UNIQUE (gvar_meas))")
			
	res <- dbSendQuery(conn, paste(sql, collapse=""))
	dbClearResult(res)
}

.init.map.table <- function(conn){
	#
	# Description: Low Level Constructor for initiating the map table of a 
	#	GVARdb object.
	#
	# Input:
	# conn:          SQLite connection
	#
	# Output:   	 Initializes a table containing mappings.
	#
	# Requires: 
	#
	# Remarks:        
	#
	# ModLog:    	  "Original Version"  			09/06/2011  EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) conn:
	.valid.conn(conn)
	
	### Core: #################################################################
	
	# 1) Create Table:
	sql <- c(
			"CREATE TABLE map (\n",
			"  gvar_id INTEGER PRIMARY KEY,\n",
			"  chrom TEXT NOT NULL,\n",
			"  start REAL,\n",
			"  end REAL,\n",
			"  gvar_name TEXT NOT NULL,\n",
			"  strand TEXT NOT NULL,\n",
			"  map_incl INTEGER,\n",
			"  UNIQUE (gvar_name))")
	
	res <- dbSendQuery(conn, paste(sql, collapse=""))
	dbClearResult(res)
}

.init.qc.table <- function(conn,N_qc=1){
	#
	# Description: Low Level Constructor for initiating the quality control 
	# 	table of a GVARdb object.
	#
	# Input:
	# conn:          SQLite connection
	# N_qc:			 Number of entries for quality control measures per feature.
	#
	# Output:   	 Initializes a table containing mappings.
	#
	# Requires: 
	#
	# Remarks:        
	#
	# ModLog:    	  "Original Version"  			09/06/2011  EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) conn:
	.valid.conn(conn)
	# 2) N1:
	if(N_qc<1){
		stop("At least one qc score should be stored (N_qc>=1)")
	}
	
	### Core: #################################################################
	
	# 1) Create Table:
	sql <- c(
			"CREATE TABLE qc (\n",
			"  gvar_id INTEGER PRIMARY KEY,\n",
			paste("  qc_score_",(1:(N_qc))," REAL,\n",sep=""),
			"  qc_incl INTEGER)")
	res <- dbSendQuery(conn, paste(sql, collapse=""))
	dbClearResult(res)
}

.init.result.table <- function(conn,N_result=1){
	#
	# Description: Low Level Constructor for initiating the result 
	# 	table of a GVARdb object.
	#
	# Input:
	# conn:          SQLite connection
	# N_result:		 Number of entries for holding statistics per feature.
	#
	# Output:   	 Initializes a table containing results.
	#
	# Requires: 
	#
	# Remarks:        
	#
	# ModLog:    	  "Original Version"  			09/06/2011  EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) conn:
	.valid.conn(conn)
	# 2) N1:
	if(N_result<1){
		stop("At least one statistic and p.value should be stored (N_result>=1)")
	}
	
	### Core: #################################################################
	
	# 1) Create Table:
	if(N_result==1){
		sql <- c(
				"CREATE TABLE result (\n",
				"  gvar_id INTEGER PRIMARY KEY,\n",
				"  stat_1 REAL,\n",
				"  pval_1 REAL,\n")
	} else {
		sql <- c(
				"CREATE TABLE result (\n",
				"  gvar_id INTEGER PRIMARY KEY,\n",
				paste("  stat_",(1:(N_result))," REAL,\n",sep=""),
				paste("  pval_",(1:(N_result-1))," REAL,\n",sep=""),
				paste("  pval_",N_result," REAL)",sep=""))
	}
	res <- dbSendQuery(conn, paste(sql, collapse=""))
	dbClearResult(res)
}

.init.sample.table <- function(conn,N_trait=1){
	#
	# Description: Low Level Constructor for initiating the sample table of a 
	#	GVARdb object.
	#
	# Input:
	# conn:          SQLite connection
	# N_trait:		 Number of traits to be stored.
	# 
	#
	# Output:   	 Initializes a sample table.
	#
	# Requires: 
	#
	# Remarks:        
	#
	# ModLog:    	  "Original Version"  			05/06/2011  EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) conn:
	.valid.conn(conn)
	# 2) N_trait:
	if(N_trait<1){
		stop("At least one status should be stored (N_trait>=1)")
	}
	
	### Core: #################################################################
	
	# 1) Create Table:
	sql <- c(
			"CREATE TABLE sample (\n",
			"  sample_id INTEGER PRIMARY KEY,\n",
			"  family TEXT NOT NULL,\n",
			"  member INTEGER NOT NULL,\n",
			"  father INTEGER,\n",
			"  mother INTEGER,\n",
			"  sex TEXT NOT NULL,\n",
			paste("  status_",(1:(N_trait))," TEXT NOT NULL,\n",sep=""),
			paste("  QT_",(1:(N_trait))," REAL NOT NULL,\n",sep=""),
			"  sample_incl INTEGER)")
	res <- dbSendQuery(conn, paste(sql, collapse=""))
	dbClearResult(res)
}

.init.metadata.table <- function(conn,dbType){
	# Description: Low Level Constructor for initiating the metadata table of a
	#	GVARdb object.
	#
	# Input:
	# conn:           SQLite connection
	# dbType:         One of the supported classes each holding different data 
	# 				  	types
	# Output:   
	#				  Initializes a metadata table.
	#
	# Requires: 
	#
	# Remarks:        
	#
	# ModLog:    	  "Original Version"  			05/06/2011  EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) conn:
	.valid.conn(conn)
	# 2) dbType:
	.valid.dbType(dbType)
	
	### Core: #################################################################
	
	# 1) Filling a dataframe with automatically generated metadata:
	metadata <- matrix(c(
			"dbType"       	    , dbType,
			"Creation_time" 	, gsub(" ","",as.character(Sys.time())),
			"GVARdb_version"   	, "1.0 Beeeeeeeeta",
			"rsqlite_version"   , installed.packages()['RSQLite', 'Version'],
			"snpMatrix_version" , installed.packages()['snpMatrix', 'Version'],
			"iranges_version"   , installed.packages()['IRanges', 'Version'],
			"seqinr"			, installed.packages()['seqinr', 'Version']),
		ncol=2,byrow=TRUE)
	colnames(metadata) <- c("name", "value")
	metadata <- as.data.frame(metadata,stringsAsFactors=FALSE)
	
	# 2) Initiating SQLite table:
	sql <- c(
			"CREATE TABLE metadata (\n",
			"  name TEXT NOT NULL,\n",
			"  value TEXT NOT NULL,\n",
			"  UNIQUE (name))")
	res <- dbSendQuery(conn, paste(sql, collapse=""))
	dbClearResult(res)
	
	# 3) Populate:
	.append.table(conn,tablename="metadata",dataFrame=metadata) 
}

.init.GVARdb <- function(dbType,dbName,folderOUT){	
	#
	# Description: Low Level Constructor for initiating a GVARdb object.
	#
	# Input:
	# dbType:     	One of the supported classes each holding different data 
	# 				 	types
	# dbName:	  	A string naming the sqlite database
	# folderOUT:	A destination folder where the new database is initiated.
	#
	# Output:     
	#
	# Requires: 
	#
	# Remarks:      A .sqlite file is created in the destination folder 
	#				indicated by folderOUT under the filename given by dbName.
	#				An object is returned holding the connection to this 
	#				database indicated by dbName in R workspace.
	#
	# ModLog:    	"Original Version"  			05/06/2011  EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) dbType:
	.valid.dbType(dbType)
	# 2) dbname:
	if(!isSingleString(dbName)){
		stop("'dbName' should be a single string")
	}
	# 3) folderOUT:
	# 3a) is String?:
	if(!isSingleString(folderOUT)){
		stop("'folderOUT' should be a single string")
	}
	# b) folder exists?
	if(!file.exists(folderOUT)){
		stop("Destination folder 'folderOUT' does not exist")
	}
	pathOUT <- paste(folderOUT,"/",dbName,".sqlite",sep="")
	# 4) pathOUT:
	if(file.exists(pathOUT)){
		stop("File set for pathOUT already exists")
	}
	
	### Core: #################################################################
	
	# 1) Open a connection:
	conn   <- dbConnect(SQLite(), dbname=pathOUT)
	# 2) Initiate minimally required tables:
	.init.metadata.table(conn,dbType)
	.init.data.table(conn)
	# 3) Initiate object:
	dbName <- GVARdb(conn,dbType)
	return(dbName)
}

###### db Utils: Low Level Constructors: Prepare user provided tables:

.normarg.data <- function(data){
	#
	# Description: This function checks 'data' passed to .append.data.table. 
	# 
	# Input:              
	# data:			    A data.frame with the headers gvar_meas, gvar_data and
	#						gvar_allele.
	#
	# Requires:
	#
	# Remarks:          
	# 
	# ModLog:           "Original Version"  07/06/2011    	EBvandenAkker
	
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) data.frame:
	if(!is.data.frame(data)){
		stop("Please provide a data.frame for 'data'")
	}
	# 2) columns:
	if(any(!is.element(.colnames_data,colnames(data)))){
		stop(paste("Please provide the appropriate headers for 'data': ",
				paste(Dummi,collapse=", "),sep=""))
	}
	if(dim(data)[2]>length(.colnames_data)){
		warning("Additional provided data will be ignored")
	}
	data <- data[,.colnames_data]
	# 3) gvar_meas:
	# 3a) is.character:
	if(!is.character(data$gvar_meas)){
		stop("'gvar_meas' of 'data' should be provided as characters")
	}
	# 3b) unique:
	if(any(duplicated(data$gvar_meas))){
		stop("'gvar_meas' of 'data' should contain unique entries")
	}
	# 4) gvar_data:
	# 4a) character 
	if(!is.character(data$gvar_data)){
		stop("'gvar_data' of 'data' should be provided as characters")
	}
	# 4b) equal length:
	if(length(unique(lapply(data$gvar_data,nchar)))!=1){
		stop(paste("'gvar_data' of 'data' should contain records with equal ",
						"amounts of entries",sep=""))
	}
	# 4c) all 0,1,2,9,- (--- is used to encode a 1.000, instead of 000)
	Dummi <- regexpr("![0,1,2,9,-]",data$gvar_data)
	if(any(Dummi!=-1)){
		stop("'gvar_data' of 'data' contains non supported data values")
	}
	# 5) allele:
	# 5a) is.character:
	if(!is.character(data$gvar_allele)){
		stop("'gvar_allele' of 'data' should be provided as characters")
	}
	# 5b) Number of allele:
	allele <- sapply(data$gvar_allele,function(x){
							unlist(strsplit(x,"[|]"))},simplify=F)
	if(any(sapply(allele,length)!=2)){
		stop(paste("'gvar_allele' of 'data' should be provided as bi-allelic ",
								"separated by a '|'"))
	}
	# 5c) One of /ACTG:
	Dummi <- unlist(regexpr("![/ACGT12]",unlist(allele)))
	if(any(Dummi!=-1)){
		stop("'Not all gvar_allele' of 'data' are recognized")
	}
	# 5d) Different alllele:
	if(any(sapply(allele,function(x) x[1])==sapply(allele,function(x) x[2]))){
		stop("'Not all gvar_allele' of 'data' feature different alleles")
	}

	### Core: #################################################################
	
	# 1) Append gvar_id:
	data$gvar_id <- 1:dim(data)[1]
	data <- data[,.required_colnames$data]
	return(data)
}

.normarg.map <- function(conn,bed){
	# Description: This function checks and prepares mappings passed to 
	#	.append.map.table. 
	#
	# Input:
	# conn:          	A connection to a SQLite database.
	# bed:         		A data.frame containing the columns "chrom","start",
	#					"end","gvar_name","strand" and optionally "gvar_meas".
	#
	# Output:   	    Checks and minor tweaks are performed.
	# 					
	#
	# Requires:     
	#				
	#				
	# Remarks:      
	#
	# ModLog:      	"Original Version"  		09/06/2011    	EBvandenAkker	
	
	### Checking: #############################################################
	
	### Checking input:
	# 1a) data.frame:
	if(!is.data.frame(bed)){
		stop("Supply a data.frame for 'bed'")
	}
	# 1b) Check presence colnames & subselect:
	# 1b1) required:
	if(any(!is.element(.colnames_bed,colnames(bed)))){
		stop("Not all expected colnames present in 'bed'")
	}
	# 1b2) optional:
	if(length(colnames(bed))>length(.colnames_bed)){
		if("gvar_meas" %in% setdiff(colnames(bed),.colnames_bed)){
			bed <- bed[,c(.colnames_bed,"gvar_meas")]
		}
	} else {
		bed <- bed[,.colnames_bed]
		bed$gvar_meas <- bed$gvar_name
	}
	# 1c) chrom: non-missing and one of chr1..chrX,chrY,chrM.
	if(any(!is.character(bed$chrom))|any(!is.element(bed$chrom,.options_chrom))|
			any(is.na(bed$chrom))){
		stop("Not all reported chromosome locations are recognized")
	}
	# 1d) start:
	if(!is.numeric(bed$start)|any(is.na(bed$start))){
		stop("Only non-missing numeric start positions allowed")
	}
	# 1e) end:
	if(!is.numeric(bed$end)|any(is.na(bed$end))){
		stop("Only non-missing numeric end positions allowed")
	}
	# 1f) gvar_name:
	if(!is.character(bed$gvar_name)|any(is.na(bed$gvar_name))|any(duplicated(bed$gvar_name))){
		stop("Not all 'gvar_name' of 'bed' are recognized")
	}
	# 1g) strand:
	if(any(is.na(bed$strand))|any(!is.element(bed$strand,.options_strand))){
		stop("No missing values in positioning allowed")
	}
	# 1h) gvar_meas:
	if(!is.character(bed$gvar_meas)|any(is.na(bed$gvar_meas))){
		stop("Please provide characters for 'gvar_meas' of 'bed'")
	}
	# 2) table 'data':
	.valid.dbTable(conn,tablename="data")
	# 3) Consistency map and table 'data':
	sql  <- "SELECT gvar_meas from data"
	meas <- unlist(dbGetQuery(conn,sql))
	if(any(!is.element(meas,bed$gvar_meas))){
		stop(paste("Not all 'gvar_meas' in 'data' is matched ", 
						"with data supplied in 'bed'",sep=""))
	}
	
	### Core: #################################################################
	
	# 1) Resort bed according to 'data':
	Ind <- sortkey(key1=bed$gvar_meas,key2=meas)
	bed <- bed[Ind,]
	# 2) Append gvar_id:
	bed$gvar_id <- 1:dim(bed)[1]
	# 3) Append map_incl:
	bed$map_incl <- 0
	inclInd <- which((bed$chrom!="---")&(bed$start!=0)&(bed$end!=0)&
									(bed$gvar_name!="---")&bed$strand!="---")
	# 4) Sort on genomic positioning:
	X <- paste("chr",c(as.character(1:22),"X","Y","M"),sep="")
	ChromInd <- sapply(X,function(x){ 
		which(bed$chrom[inclInd]==x)[sort(bed$start[which(bed$chrom[inclInd]==x)],
			index.return=T)$ix]})
	bed$map_incl[inclInd] <- unlist(ChromInd)
	# 5) Renice:
	bed <- bed[,.required_colnames$map]
	return(bed)
}

.normarg.qc <- function(conn,qc_data){
	# Description: This function checks and prepares quality control measures 
	#	passed to .append.qc.table. 
	#
	# Input:
	# conn:          	A connection to a SQLite database.
	# qc_data:         	A data.frame containing the columns "gvar_meas", 
	#						"qc_score_1", "qc_incl" and optionally more 
	#						qc measures stored under "qc_score_*".
	#
	# Output:   	    Checks and minor tweaks are performed.
	# 					
	#
	# Requires:     
	#				
	#				
	# Remarks:      
	#
	# ModLog:      	"Original Version"  		10/06/2011    	EBvandenAkker	
	
	### Checking: #############################################################
	
	### Checking input:
	# 1a) data.frame:
	if(!is.data.frame(qc_data)){
		stop("Supply a data.frame for 'qc_data'")
	}
	# 1b) Check presence colnames & subselect:
	# 1b1) required:
	if(any(!is.element(.colnames_qc_data,colnames(qc_data)))){
		stop("Not all expected colnames present in 'qc_data'")
	}
	# 1b2) optional:
	if(length(colnames(qc_data))>length(.colnames_qc_data)){
		Opt 	<- sort(setdiff(colnames(qc_data),.colnames_qc_data))
		ExpOpt 	<- paste("qc_score_",as.character(2:(length(Opt)+1)),sep="")
		if(any(Opt!=ExpOpt)){
			stop("Unexpected optional column names in table 'qc_data'")
		}
		qc_data <- qc_data[,c(.colnames_qc_data[c(1,2)],ExpOpt,
						.colnames_qc_data[3])]
	} else {
		qc_data <- qc_data[,.colnames_qc_data]
	}
	# 1c) gvar_meas:
	if(!is.character(qc_data$gvar_meas)|any(is.na(qc_data$gvar_meas))){
		stop("Please provide characters for 'gvar_meas' of 'qc_data'")
	}
	# 1d) qc_score_:
	Ind <- which(substr(colnames(qc_data),1,9)=="qc_score_")
	for(i in 1:length(Ind)){
		if(!is.numeric(qc_data[,Ind[i]])|any(is.na(qc_data[,Ind[i]]))){
			stop("Only non-missing numeric qc_scores are allowed")
		}
	}
	# 1e) qc_incl:
	if(!is.numeric(qc_data$qc_incl)|any(is.na(qc_data$qc_incl))){
		stop(paste("Only non-missing numeric values for 'qc_incl' in 'qc_data'",
						"are allowed",sep=""))
	}
	# 2) table 'data':
	.valid.dbTable(conn,tablename="data")
	# 3) Consistency qc_data and table 'data':
	sql  <- "SELECT gvar_meas from data"
	meas <- unlist(dbGetQuery(conn,sql))
	if(any(!is.element(meas,qc_data$gvar_meas))){
		stop(paste("Not all 'gvar_meas' in 'data' are matched ", 
						"with data supplied in 'qc_data'",sep=""))
	}
	
	### Core: #################################################################
	
	# 1) Resort bed according to 'data':
	Ind <- sortkey(key1=qc_data$gvar_meas,key2=meas)
	qc_data <- qc_data[Ind,]
	# 2) Append gvar_id:
	ColNames <- setdiff(colnames(qc_data),"gvar_meas")
	qc_data$gvar_id <- 1:dim(qc_data)[1]
	# 3) Renice:
	qc_data <- qc_data[,c("gvar_id",ColNames)]
	return(qc_data)
}

.normarg.result <- function(conn,result_data){
	# Description: This function checks and prepares results passed to 
	#	.append.result.table. 
	#
	# Input:
	# conn:          	A connection to a SQLite database.
	# result_data:      A data.frame containing the columns "gvar_meas", 
	#						"stat_1", "pval_2" and optionally more 
	#						 results stored under "stat_*" and "pval_*".
	#
	# Output:   	    Checks and minor tweaks are performed.
	# 					
	#
	# Requires:     
	#				
	#				
	# Remarks:      
	#
	# ModLog:      	"Original Version"  		10/06/2011    	EBvandenAkker	
	
	### Checking: #############################################################
	
	### Checking input:
	# 1a) data.frame:
	if(!is.data.frame(result_data)){
		stop("Supply a data.frame for 'result_data'")
	}
	# 1b) Check presence colnames & subselect:
	# 1b1) required:
	if(any(!is.element(.colnames_result_data,colnames(result_data)))){
		stop("Not all expected colnames present in 'result_data'")
	}
	# 1b2) optional:
	if(length(colnames(result_data))>length(.colnames_result_data)){
		Opt 	<- sort(setdiff(colnames(result_data),.colnames_result_data))
		ExpOpt1 <- paste("stat_",as.character(2:((length(Opt)/2)+1)),sep="") 
		ExpOpt2 <- paste("pval_",as.character(2:((length(Opt)/2)+1)),sep="") 
		ExpOpt	<- sort(c(ExpOpt1,ExpOpt2))
		if(any(Opt!=ExpOpt)){
			stop("Unexpected optional column names in table 'result_data'")
		}
		result_data <- result_data[,c(.colnames_result_data[c(1,2)],ExpOpt1,
					.colnames_result_data[3],ExpOpt2)]
	} else {
		result_data <- result_data[,.colnames_result_data]
	}
	# 1c) gvar_meas:
	if(!is.character(result_data$gvar_meas)|any(is.na(result_data$gvar_meas))){
		stop("Please provide characters for 'gvar_meas' of 'result_data'")
	}
	# 1d) stat_:
	Ind <- which(substr(colnames(result_data),1,5)=="stat_")
	for(i in 1:length(Ind)){
		if(!is.numeric(result_data[,Ind[i]])|any(is.na(result_data[,Ind[i]]))){
			stop(paste("Only non-missing numeric values are allowed for 'stat_*' ",
							"in 'result_data'",sep=""))
		}
	}
	# 1e) pval_:
	Ind <- which(substr(colnames(result_data),1,5)=="pval_")
	for(i in 1:length(Ind)){
	if(!is.numeric(result_data[,Ind[i]])|any(is.na(result_data[,Ind[i]]))){
		stop(paste("Only non-missing numeric values are allowed for 'pval_*' ",
						"in 'result_data'",sep=""))
		}
	}
	# 2) table 'data':
	.valid.dbTable(conn,tablename="data")
	# 3) Consistency qc_data and table 'data':
	sql  <- "SELECT gvar_meas from data"
	meas <- unlist(dbGetQuery(conn,sql))
	if(any(!is.element(meas,result_data$gvar_meas))){
		stop(paste("Not all 'gvar_meas' in 'data' are matched", 
						"with data supplied in 'result_data'",sep=""))
	}
	
	### Core: #################################################################
	
	# 1) Resort result_data according to 'data':
	Ind <- sortkey(key1=result_data$gvar_meas,key2=meas)
	result_data <- result_data[Ind,]
	# 2) Append gvar_id:
	ColNames <- setdiff(colnames(result_data),"gvar_meas")
	result_data$gvar_id <- 1:dim(result_data)[1]
	# 3) Renice:
	result_data <- result_data[,c("gvar_id",ColNames)]
	return(result_data)
}

.normarg.sample <- function(conn,pheno){
	# Description: This function checks and prepares phenotypic data passed to 
	#	.append.sample.table. 
	#
	# Input:
	# conn:          	A connection to a SQLite database.
	# pheno:         	A data.frame containing the columns "family","member",
	#						"father","mother","sex","status_1","QT_1",
	#						"sample_incl" and optionally more traits stored 
	#						under "status_*" and "QT_*".
	#
	# Output:   	    Checks and minor tweaks are performed.
	# 					
	#
	# Requires:     
	#				
	#				
	# Remarks:      
	#
	# ModLog:      	"Original Version"  		10/06/2011    	EBvandenAkker	
	
	### Checking: #############################################################
	
	### Checking input:
	# 1a) data.frame:
	if(!is.data.frame(pheno)){
		stop("Supply a data.frame for 'pheno'")
	}
	# 1b) Check presence colnames & subselect:
	# 1b1) required:
	if(any(!is.element(.colnames_pheno,colnames(pheno)))){
		stop("Not all expected colnames present in 'pheno'")
	}
	# 1b2) optional:
	if(length(colnames(pheno))>length(.colnames_pheno)){
		Opt 	<- sort(setdiff(colnames(pheno),.required_colnames$sample))
		ExpOpt1 <- paste("status_",as.character(2:((length(Opt)/2)+1)),sep="") 
		ExpOpt2 <- paste("QT_",as.character(2:((length(Opt)/2)+1)),sep="") 
		ExpOpt	<- sort(c(ExpOpt1,ExpOpt2))
		if(any(Opt!=ExpOpt)){
			stop("Unexpected optional column names in table 'pheno'")
		}
		pheno <- pheno[,c(  .colnames_pheno[c(1:6)],ExpOpt1,
							.colnames_pheno[7],ExpOpt2,
							.colnames_pheno[8])]
	} else {
		pheno <- pheno[,.colnames_pheno]
	}
	# 1c) family: character:
	if(!is.character(pheno$family)|any(is.na(pheno$family))){
		stop("Supply non NA character string as family IDs")
	}
	# 1d) member: numeric:
	if(!is.numeric(pheno$member)|any(is.na(pheno$member))){
		stop("Only non-missing numeric member IDs are allowed")
	}
	# 1e) father: numeric:
	if(!is.numeric(pheno$father)|any(is.na(pheno$father))){
		stop("Only non-missing numeric father IDs are allowed")
	}
	# 1f) mother: numeric:
	if(!is.numeric(pheno$mother)|any(is.na(pheno$mother))){
		stop("Only non-missing numeric mother IDs are allowed")
	}
	# 1g) sex: character & one of "MALE","FEMALE" or "UNKNOWN":
	if(!is.character(pheno$sex)|any(is.na(pheno$sex))|
			any(!is.element(pheno$sex,.options_sex))){
		stop("Wrong sex found, choose one of: ",
				paste(.options_sex,collapse=", "))
	}
	# 1h) status: character & one of "CASE","CONTROL", "UNKNOWN":
	Ind <- which(substr(colnames(pheno),1,7)=="status_")
	for(i in 1:length(Ind)){
		if(!is.character(pheno[,Ind[i]])|any(is.na(pheno[,Ind[i]]))|
				any(!is.element(pheno[,Ind[i]],.options_status))){
			stop("Wrong statuses found, choose one of: ",
					paste(.options_status,collapse=", "))
		}
	}
	# 1i) QT: numeric
	Ind <- which(substr(colnames(pheno),1,3)=="QT_")
	for(i in 1:length(Ind)){
		if(!is.numeric(pheno[,Ind[i]])|any(is.na(pheno[,Ind[i]]))){
			stop(paste("Only non-missing numeric values for quantitative ",
							"traits are allowed"))
		}
	}
	# 2) table 'data':
	.valid.dbTable(conn,tablename="data")
	# 3) Consistency pheno and table 'data': (nchar(gvar_data) should be a 
	# 	multiple of N_feat)
	sql1 	<- paste("SELECT * FROM data LIMIT 1", sep="")
	Nchar 	<- nchar(dbGetQuery(conn, sql1)$gvar_data)
	Nrep 	<- Nchar/(dim(pheno)[1])
	if(as.integer(Nrep)!=Nrep){
		stop("Number of supplied samples does not match length of data")	
	}
	
	### Core: #################################################################
	
	# 1) Append sample_id:
	ColNames <- colnames(pheno)
	pheno$sample_id <- 1:dim(pheno)[1]
	# 2) Renice:
	pheno <- pheno[,c("sample_id",ColNames)]
	return(pheno)
}

.normarg.metadata <- function(metadata){
	#
	# Description: This function checks and prepares 'metadata' passed to 
	#	.append.metadata.table. 
	# 
	# Input:              
	# metadata:			A two-columned data.frame with headers "name" & "value" 
	#
	# Output:   	    Checks and minor tweaks are performed.
	#
	# Requires:
	#
	# Remarks:          
	# 
	# ModLog:           "Original Version"  07/06/2011    	EBvandenAkker
	
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) metadata:
	# 1a) is.data.frame:
	if(!is.data.frame(metadata)){
		stop("Supply a data.frame for 'metadata'")
	}
	# 1b) dimensions:
	if(dim(metadata)[2]!=2){
		stop("Supply a two-columned data.frame for 'metadata'")
	}
	# 1c) colnames:
	if(any(!is.element(.required_colnames$metadata,colnames(metadata)))){
		stop(paste("'metadata' should have column names: '",
				paste(.required_colnames$metadata,collapse="' & '"),"'",sep=""))
	}
	metadata <- metadata[,.required_colnames$metadata]
	# 1d) character:
	if(!is.character(metadata[,1])|!is.character(metadata[,2])){
		stop("'metadata' should contain characters only")
	}
	return(metadata)
}

###### db Utils: Low Level Constructors: Append user provided tables to database

.append.data.table <- function(conn,data){
	#
	# Description: This function appends the data table of a GVARdb object.
	#
	# Input:  
	# conn:          	A connection to a SQLite database.
	# data:            	Strings containing the data.
	#
	# Output:			The data table of the GVARdb object is appended.
	# 
	# Requires:
	#
	# Remarks:          
	# 
	# ModLog:           "Original Version"  		06/10/2010  EBvandenAkker
	#                   "ped to snpMatrix"  		10/10/2010  EBvandenAkker
	#                   "Append"            		21/11/2010  EBvandenAkker
	#					"No Recode"					28/11/2010	EBvandenAkker
	#					"Move Chunking"				28/11/2010	EBvandenAkker
	#					"Move SNP stuff" 			28/11/2010	EBvandenAkker
	#					".write.table"				28/11/2010	EBvandenAkker
	#				  	"Low Level Validity Funct"	05/06/2011  EBvandenAkker 
	
	### Checking: ############################################################# 
	
	### Checking input:
	# 1) conn & presence table:
	.valid.dbTable(conn,tablename="data")
	# 2) Checking for consistency already stored:
	# 2a) Get number of stored records (features):
	sql 		<- "SELECT max(gvar_id) from data"
	NFeat 		<- unlist(dbGetQuery(conn, sql))
	if(!is.na(NFeat)){
		# 2b) Get number of entries per record (samples):
		sql1 <- "SELECT length(gvar_data) FROM data LIMIT 1"
		NSamp <- unlist(dbGetQuery(conn,sql1))
		if(NSamp!=nchar(data$gvar_data[1])){
			stop("Previously stored data does not match newly supplied data")
		}
	} else {
		NFeat <- 0
	}
	
	### Core: #################################################################
	
	# 1) Tranform:
	data$gvar_id <- data$gvar_id+NFeat
	# 2) Append:
	.append.table(conn,tablename="data",data)
}

.append.map.table <- function(conn,map){
	#
	# Description: This function appends the 'map' table of a GVARdb object. 
	# 	
	#
	# Input:  
	# conn:          	A connection to a SQLite database.
	# data:            	Strings containing the data.
	#
	# Output:			The data table of the GVARdb object is appended.
	# 
	# Requires:
	#
	# Remarks:          
	# 
	# ModLog:           "Original Version"  		10/06/2011  EBvandenAkker
	
	### Checking: ############################################################# 
	
	### Checking input:
	# 1) conn & presence tables 'map' & 'data': 
	.valid.dbTable(conn,tablename="map")
	
	### Core: #################################################################
	
	# 1) Append:
	.append.table(conn,tablename="map",map)
	
}

.append.qc.table <- function(conn,qc_data){
	#
	# Description: This function appends the 'qc' table of a GVARdb object. 
	# 	
	#
	# Input:  
	# conn:          	A connection to a SQLite database.
	# data:            	Strings containing the data.
	#
	# Output:			The qc table of the GVARdb object is appended.
	# 
	# Requires:
	#
	# Remarks:          
	# 
	# ModLog:           "Original Version"  		10/06/2011  EBvandenAkker
	
	### Checking: ############################################################# 
	
	### Checking input:
	# 1) conn & presence tables 'qc' & 'data': 
	.valid.dbTable(conn,tablename="qc")
	
	### Core: #################################################################
	
	# 1) Append:
	.append.table(conn,tablename="qc",qc_data)
}

.append.result.table <- function(conn,result_data){
	#
	# Description: This function appends the 'result' table of a GVARdb object. 
	# 	
	#
	# Input:  
	# conn:          	A connection to a SQLite database.
	# data:            	Strings containing the data.
	#
	# Output:			The result table of the GVARdb object is appended.
	# 
	# Requires:
	#
	# Remarks:          
	# 
	# ModLog:           "Original Version"  		10/06/2011  EBvandenAkker
	
	### Checking: ############################################################# 
	
	### Checking input:
	# 1) conn & presence tables 'result' & 'data': 
	.valid.dbTable(conn,tablename="result")
	
	### Core: #################################################################
	
	# 1) Append:
	.append.table(conn,tablename="result",result_data)
}

.append.sample.table <- function(conn,sample_data){
	#
	# Description: This function appends the 'sample' table of a GVARdb object. 
	# 	
	#
	# Input:  
	# conn:          	A connection to a SQLite database.
	# data:            	Strings containing the data.
	#
	# Output:			The sample table of the GVARdb object is appended.
	# 
	# Requires:
	#
	# Remarks:          
	# 
	# ModLog:           "Original Version"  		10/06/2011  EBvandenAkker
	
	### Checking: ############################################################# 
	
	### Checking input:
	# 1) conn & presence tables 'sample' & 'data': 
	.valid.dbTable(conn,tablename="sample")
	
	### Core: #################################################################
	
	# 1) Append:
	.append.table(conn,tablename="sample",sample_data)
	
}

.append.metadata.table <- function(conn,metadata){
	#
	# Description: This function appends the metadata table of a GVARdb object 
	# 	using a two columned data.frame containing characters.
	#
	# Input:  
	# conn:          	A connection to a SQLite database.
	# metadata:         A two-columned data.frame with headers "name" & "value"
	#
	# Output:			The metadata table of the GVARdb object is appended.
	# 
	# Requires:
	#
	# Remarks:          
	# 
	# ModLog:           "Original Version"  		06/10/2010  EBvandenAkker
	#                   "RemoveIfExists"    		21/11/2010 	EBvandenAkker
	#					"Generalize"				26/11/2010 	EBvandenAkker
	#					.write.table				28/11/2010	EBvandenAkker
	#				  	"Low Level Validity Funct"	05/06/2011  EBvandenAkker 
	
	### Checking: ############################################################# 
	
	### Checking input:
	# 1) conn & presence table:
	.valid.dbTable(conn,tablename="metadata")
	# 2) uniqueness of first column is handled by database
	
	### Core: #################################################################
	
	.append.table(conn,tablename="metadata",dataFrame=metadata)
}

###############################################################################
######## db Utils: Low Level Accessors:

.getConn  <- function(envir){
	# Description: This is an accessor function used to grab and assign the 
	# SQLite connection.
	#
	# Input:
	# envir:        Environment holding the SQLite connenction.
	#
	# Output:   
	# conn          A SQLite connection.
	#
	# Requires: 
	#
	# Remarks:      Copied from GenomicFeatures
	#
	# ModLog:       "Original Version"  06/10/2010    EBvandenAkker 
	
	get("conn", envir=envir, inherits=FALSE)
} 

.dbConn    <- function(object){ 
	# Description: This is an accessor function used to grab a SQLite 
	# connection from the enironment specified by a db.
	#
	# Input:
	# object:       Any object containing an environment holding a SQLite 
	#				connection.
	#
	# Output:   
	# conn          A SQLite connection.
	#
	# Requires:     .getConn
	#
	# Remarks:      Copied from GenomicFeatures
	#
	# ModLog:      	"Original Version"  06/10/2010    EBvandenAkker 
	
	.getConn(object@envir)
}

.dbMeta 	<- function(conn,name){
	# Description: This is an accessor function used to grab the info from the 
	# metadata table.	
	#
	# Input:
	# conn:         A connection to a SQLite database.
	# name:			A character string describing metadata
	#
	# Output:   
	# The appropriate metadata value is returned.					
	#
	# Requires:     .getConn
	#				.dbConn
	#				
	# Remarks:      
	#
	# ModLog:      	"Original Version"  		28/11/2010    	EBvandenAkker
	#				"Low Level Validity Funct"	05/06/2011  	EBvandenAkker 	
	
	### Checking: #############################################################
	
	### Checking input:
	# 1) conn:
	.valid.conn(conn)
	# 2) name:
	if(!isSingleString(name)){
		return("Please provide a single character stirng for 'name'")
	}
	# 3) valid dbTable:
	.valid.dbTable(tablename="metadata",conn)
	
	### Core: #################################################################
	# Request:
	metadata <- dbReadTable(conn,"metadata")
	Result   <- metadata$value[which(metadata$name==name)]
	return(Result)
}

.dbTableLength <- function(conn,tablename){
	# Description: This is an accessor function used to get the number of 
	# records in a table.
	#
	# Input:
	# conn:         	A connection to a SQLite database.
	# tablename:	  	The specified table
	#
	# Output:   
	# The number of records is in table tablename is returned.					
	#
	# Requires:     
	#				
	#				
	# Remarks:      
	#
	# ModLog:      	"Original Version"  		09/06/2011    	EBvandenAkker	
	
	### Checking: #############################################################
	
	### Checking input:
	# 1) conn & tablename
	.valid.dbTable(conn,tablename)
	
	### Core: #################################################################
	sql 	<- paste("SELECT COUNT(*) FROM ",tablename,sep="")
	L   	<- as.numeric(dbGetQuery(conn,sql)[1,1])
	return(L)
}

.dbColnames <- function(conn,tablename){
	# Description: This is an accessor function used to get the column names 
	# 	of a table
	# Input:
	# conn:         	A connection to a SQLite database.
	# tablename:	  	The specified table
	#
	# Output:   
	# The number of records is in table tablename is returned.					
	#
	# Requires:     
	#				
	#				
	# Remarks:      
	#
	# ModLog:      	"Original Version"  		09/06/2011    	EBvandenAkker	
	
	### Checking: #############################################################
	
	### Checking input:
	# 1) conn & tablename
	.valid.dbTable(conn,tablename)
	
	### Core: #################################################################
	sql0 <- paste("SELECT * FROM ", tablename, " LIMIT 0", sep="")
	data0 <- dbGetQuery(conn, sql0)
	colnames0 <- colnames(data0)
	return(colnames0)
}

.dbShow 		<- function(object){
	#
	# Description: This is an accessor function used to represent the db as
	# a setMethod for show.
	#
	# Input:
	# object:       A GVARdb object.
	#
	# Output:   
	# Summary results describing the db are displayed.
	#
	# Requires:     libraries: DBI & RSQLite
	#
	# Remarks:      Copied from GenomicFeatures
	#
	# ModLog:       "Original Version"  		09/08/2010    	EBvandenAkker
	#				"Low Level Validity Funct"	05/06/2011  	EBvandenAkker 	
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) object / dbType:
	.valid.dbType(class(object))
	# 2) connection:
	conn <- .dbConn(object)
	.valid.conn(conn)
	# 3) metadata table
	.valid.dbTable(tablename="metadata",conn)
	
	### Core: #################################################################
	metadata <- dbReadTable(.dbConn(object), "metadata")
	cat(paste(metadata$value[which(metadata$name=="dbType")],
					" object:\n",sep=""))
	for(i in seq_len(nrow(metadata))){
		cat("| ", metadata[i, "name"], ": ", metadata[i, "value"],
				"\n", sep="")
	}
}
setMethod("show","GVARdb",.dbShow)

.getGVARRecord <- function(conn,index,maxN=.maxNQuery){
	# Description: This is an accessor function used to grab records form the 
	# 	'data' table.
	#
	# Input:
	# conn:			A SQLite connection
	# index:        vector of gvar_ids
	# maxN:			By default set to NULL. Can be set to a number to 
	#					indicate the number of records that can be fetched in 
	#					one go.
	#
	# Output:   
	# A vector of strings is returned.
	#
	# Requires: 
	#
	# Remarks:      
	#
	# ModLog:       "Original Version"  15/11/2011    EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) conn:
	.valid.conn(conn)
	# 2) index:
	# 2a) integers:
	if(any(as.integer(index)!=index)){
		stop("Please provide a vector of integers for 'index'")
	}
	# 2b) Valid range:
	#L <- .dbTableLength(conn,tablename="data")
	#if(any(index<=0)|any(index>L)){
	#	stop(paste("Please supply indices within the dimensions of the ",
	#					"database for 'index'",sep=""))
	#}
	# 3) maxN:
	if(!is.null(maxN)){
		if(length(index)>maxN){
			stop("Size of query is larger as pre set limitations")
		}
	}
	
	### Core: #################################################################
	
	sql <- paste("SELECT gvar_data FROM data WHERE gvar_id IN ('",
			paste(index,collapse="','"),"')",sep="")
	D <- unlist(dbGetQuery(conn,sql))
	# check:(Every gvar_id is unique in the database), therefore the length of
	# result should match length of query:
	if(length(D)!=length(index)){
		stop("Unable to find all requested genomic variants")
	}
	return(D)
}

.dbPath <- function(conn){
	# Description: This is an accessor function used to grab the path to the 
	#	location where the database is stored.
	#
	# Input:
	# conn:			A SQLite connection
	# index:        vector of gvar_ids
	#
	# Output:   
	# A path to a location is returned.
	#
	# Requires: 
	#
	# Remarks:      
	#
	# ModLog:       "Original Version"  17/11/2011    EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) conn:
	.valid.conn(conn)
	
	### Core: #################################################################
	# 1) Get path:
	path <- dbGetInfo(conn)$dbname
	return(path)
}

###############################################################################
######## db Utils: Load:

loadGVARdb <- function(pathIN){
	#
	# Description: This function is used to create a SQLite connection to a 
	# stored db.
	#
	# Input:
	# pathIN:       A path pointing to a file.
	#
	# Output:   
	# An instance of the GenomicVariants class is created holding a 
	# connection to a stored database pointed by pathIN. 
	#
	# Requires:     
	#
	# Remarks:      Adjusted from GenomicFeatures
	#
	# ModLog:       "Original Version"  06/10/2010    EBvandenAkker
	
	if(!isSingleString(pathIN)){
		stop("Please providfe a single character string for 'pathIN'")
	}
	if(!file.exists(pathIN)){
		stop("Please provide a valid path for 'pathIN'")
	}
	# Connect:
	conn   <- dbConnect(SQLite(), pathIN)
	# Request dbType:
	dbType <- .dbMeta(conn,name="dbType")
	# Load:
	db <- GVARdb(conn,dbType)
	return(db)
}

###############################################################################
###### db Utils: Low level Validity Checks:

.valid.metadata.table <- function(conn){
	#
	# Description: This function checks the validity of the metadata table.
	#
	# Input:
	# conn:         A connection to a SQLite database.
	#
	# Output:       Validity checks are performed.
	#
	# Requires:     libraries: DBI & RSQLite
	#
	# Remarks:      Adapted from GenomicFeatures
	#
	# ModLog:       "Original Version"  		11/10/2010    	EBvandenAkker
	#				".required_colnames$"		28/11/2010		EBvandenAkker
	#				"Low Level Validity Funct"	05/06/2011  	EBvandenAkker 	
	
	# 1) Check existence table and colnames:
	.valid.dbTable(conn,tablename="metadata")
	# 2) Obligatory names (column 1):
	metadata <- dbReadTable(conn,"metadata")
	if(any(!is.element(.required_metanames, metadata$name))){
		stop("invalid metadata table: not all required entries are present")
	}
	NULL
}

.valid.data.table <- function(conn){
	#
	# Description: This function checks the validity of the data table.
	#
	# Input:
	# conn:               A connection to a SQLite database.
	#
	# Output:             Validity checks are performed.
	#
	# Requires:           libraries: DBI & RSQLite
	#
	# Remarks:            Adapted from GenomicFeatures
	#
	# ModLog:             "Original Version"  	09/06/2010    	EBvandenAkker 
	
	# 1) Check existence table and colnames:
	.valid.dbTable(conn,tablename="data")
	# To Do
	NULL
}

.valid.map.table <- function(conn){
	#
	# Description: This function checks the validity of the map table.
	#
	# Input:
	# conn:               A connection to a SQLite database.
	#
	# Output:             Validity checks are performed.
	#
	# Requires:           
	#
	# Remarks:            Adapted from GenomicFeatures
	#
	# ModLog:             "Original Version"  	09/06/2011    	EBvandenAkker
	
	# 1) Check existence table and colnames:
	.valid.dbTable(conn,tablename="map")
	# 2) Matching number of records with data table:
	L <- .dbTableLength(conn,tablename="data")
	Dummi <- .valid.TableLength(conn,tablename="map",L)
	NULL
}

.valid.qc.table <- function(conn){
	#
	# Description: This function checks the validity of the qc table.
	#
	# Input:
	# conn:               A connection to a SQLite database.
	#
	# Output:             Validity checks are performed.
	#
	# Requires:          
	#
	# Remarks:            Adapted from GenomicFeatures
	#
	# ModLog:             "Original Version"  	09/06/2011    	EBvandenAkker
	
	# 1) Check existence table and colnames:
	.valid.dbTable(conn,tablename="qc")
	# 2) Optional colnames:
	ColNames 	<- .dbColnames(conn,tablename="qc")
	#if(length(ColNames)>length(.required_colnames$qc)){
	#	Opt 	<- sort(setdiff(ColNames,.required_colnames$qc))
	#	ExpOpt  <- paste("qc_score_",as.character(2:(length(Opt)+1)),sep="") 
	#		if(any(Opt!=ExpOpt)){
	#		stop("Unexpected optional column names in table 'qc'")
	#	}
	#}
	# 3) Matching number of records with data table:
	L <- .dbTableLength(conn,tablename="data")
	Dummi <- .valid.TableLength(conn,tablename="qc",L)
	NULL
}

.valid.result.table <- function(conn){
	#
	# Description: This function checks the validity of the result table.
	#
	# Input:
	# conn:               A connection to a SQLite database.
	#
	# Output:             Validity checks are performed.
	#
	# Requires:          
	#
	# Remarks:            Adapted from GenomicFeatures
	#
	# ModLog:             "Original Version"  	09/06/2011    	EBvandenAkker
	
	# 1) Check existence table and colnames:
	.valid.dbTable(conn,tablename="result")
	# 2) Optional colnames:
	#ColNames 	<- .dbColnames(conn,tablename="result")
	#if(length(ColNames)>length(.required_colnames$result)){
	#	Opt 	<- sort(setdiff(ColNames,.required_colnames$result))
	#	ExpOpt1  <- paste("pval_",as.character(2:((length(Opt)/2)+1)),sep="") 
	#	ExpOpt2  <- paste("stat_",as.character(2:((length(Opt)/2)+1)),sep="") 
	#	ExpOpt	 <- sort(c(ExpOpt1,ExpOpt2))
	#	if(any(Opt!=ExpOpt)){
	#		stop("Unexpected optional column names in table 'result'")
	#	}
	#}
	# 3) Matching number of records with data table:
	L <- .dbTableLength(conn,tablename="data")
	Dummi <- .valid.TableLength(conn,tablename="result",L)
	NULL
}

.valid.sample.table <- function(conn){
	#
	# Description: This function checks the validity of the sample table.
	#
	# Input:
	# conn:               A connection to a SQLite database.
	#
	# Output:             Validity checks are performed.
	#
	# Requires:           libraries: DBI & RSQLite
	#
	# Remarks:            Adapted from GenomicFeatures
	#
	# ModLog:             "Original Version"  	09/06/2010    	EBvandenAkker
	
	# 1) Check existence table and colnames:
	.valid.dbTable(conn,tablename="sample")
	# 2) Optional colnames:
	#ColNames 	<- .dbColnames(conn,tablename="sample")
	#if(length(ColNames)>length(.required_colnames$sample)){
	#	Opt 	<- sort(setdiff(ColNames,.required_colnames$sample))
	#	ExpOpt1  <- paste("status_",as.character(2:((length(Opt)/2)+1)),sep="") 
	#	ExpOpt2  <- paste("QC_",as.character(2:((length(Opt)/2)+1)),sep="") 
	#	ExpOpt	 <- sort(c(ExpOpt1,ExpOpt2))
	#	if(any(Opt!=ExpOpt)){
	#		stop("Unexpected optional column names in table 'sample'")
	#	}
	#}
	# 3) Matching number of samples with data table:
	NFeat 	<- .dbTableLength(conn,tablename="sample")
	sql1 	<- paste("SELECT * FROM data LIMIT 1", sep="")
	data1 	<- dbGetQuery(conn, sql1)$gvar_data
	if(nchar(data1)!=NFeat){
		stop("Unexpected number of samples in table 'sample'")
	}
	NULL
}

.valid.GVARdb <- function(object){
	#
	# Description: This function checks the validity of a GVARdb.
	#
	# Input:
	# object:               An instance of a GWASdb object.
	#
	# Output:             	Validity checks are performed.
	#
	# Requires:         	libraries: DBI & RSQLite
	#
	# Remarks:        		
	#
	# ModLog:             	"Original Version"  11/10/2010    EBvandenAkker 
	
	# Grab connection:  
	conn <- .dbConn(object)
	.valid.conn(conn)
	.valid.dbType(class(object))
	
	# Check Tables:
	.valid.data.table(conn)
	tableNames <- dbListTables(conn)
	if(is.element("map",tableNames)){
		Dummi <- .valid.map.table(conn)	
	}
	if(is.element("qc",tableNames)){
		Dummi <- .valid.qc.table(conn)	
	}
	if(is.element("result",tableNames)){
		Dummi <- .valid.result.table(conn)	
	}
	if(is.element("sample",tableNames)){
		Dummi <- .valid.sample.table(conn)	
	}
	Dummi <- .valid.metadata.table(conn)
}
# Set validity:
Dummi <- sapply(.supported_classes,function(x) setValidity2(x,.valid.GVARdb))

isGVARdb <- function(object,quiet=T){
	#
	# Description: This function checks whether the supplied object is a valid 
	#	GVARdb object.
	#
	# Input:
	# object:               An instance of a GWASdb object.
	# quiet:				When set to FALSE an error is reported if not a 
	#							valid object.
	#
	# Output:             	Validity checks are performed. TRUE when valid, 
	#							FALSE when not. 
	#
	# Requires:         	
	#
	# Remarks:        		
	#
	# ModLog:             	"Original Version"  17/06/2011    EBvandenAkker 
	
	### Checking: #############################################################
	# 1) quiet:
	if(!is.logical(quiet)){
		stop("'quiet' should be a logical value")
	}
	
	### Core: #################################################################
	
	# 1) Test object:
	tmp <- try(.valid.GVARdb(object),silent=T)
	if(is.null(tmp)){
		return(TRUE)
	} else {
		if(quiet==T){
			return(FALSE)
		} else {
			.valid.GVARdb(object)
		}
	}
}

###############################################################################
###### db Utils: Database Consistency Checks:  ### NOT USED ###

.checkdb.data.length <- function(conn){
	# Description: This function checks whether all entries in the data table
	#	in the gvar_data column have equal length.
	#
	# Input:
	# conn:         A connection to a SQLite database.
	#
	# Output:   
	# A consistency check is performed					
	#
	# Requires:     
	#				
	#				
	# Remarks:      
	#
	# ModLog:      	"Original Version"  		05/06/2011    	EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking input:
	# 1) conn:
	.valid.conn(conn)
	# 2) valid data table:
	.valid.dbTable(conn,tablename="data")
	
	### Core: #################################################################
	
	sql <- "SELECT length(gvar_data) FROM data"
	L   <- length(unique(unlist(dbGetQuery(conn,sql))))
	if(L!=1){
		return("Inconsistency in amounts of data in table per feature")
	}
	NULL
}

###############################################################################
###### db Utils: 

sortkey <- function(key1,key2){
	#
	# Description: This function resorts key1 to fit key2
	#
	# Input:
	# key1:				A character vector
	# key2:				A character vector
	#
	# Output:          	
	# Ind:				Indices are returned for sorting key1
	#
	# Requires:         
	#
	# Remarks:        		
	#
	# ModLog:             	"Original Version"  11/06/2011    EBvandenAkker
	
	### Checking: #############################################################
	
	### Checking Input:
	# 1) key1:
	if(!is.character(key1)){
		stop("'key1' should be a character vector")
	}
	# 2) key2:
	if(!is.character(key2)){
		stop("'key2' should be a character vector")
	}
	# 3) matching lengths:
	if(any(sort(key1) != sort(key2))){
		stop("'key1' and 'key2' contain different values")
	}
	
	### Core: #################################################################
	
	# 1) Prepare key1:
	key1 	<- data.frame(key1,1:length(key1))
	colnames(key1) <- c("key1","order1")
	# 2) Prepare key2:
	key2 	<- data.frame(key2,1:length(key2))
	colnames(key2) <- c("key2","order2")
	# 3) Merge:
	Dummi	<- merge(key1,key2,by.x="key1",by.y="key2")	
	# 4) Sort:
	Dummi 	<- Dummi[sort(Dummi$order2,index.return=T)$ix,]
	# 5) Renice:
	Result 	<- Dummi$order1
	names(Result) <- Dummi$key1
	return(Result)
}

isSingleLogical <- function(x){
	is.logical(x) && length(x) == 1 && !is.na(x)
}

.timeStamp <- function(Ndigits=3){
	Opt <- getOption("digits.secs")
	options(digits.secs = Ndigits)
	Result <- 	gsub(" ","_",as.character(Sys.time()))
	options(digits.secs = Opt)
	return(Result)
}

###############################################################################
###### db Utils: Utility Functions for adding tables:

addMap <- function(object,map,mapSource="",mapVersion="",overwrite=F,backupCopy=T){
	#
	# Description: This utility function adds a 'map' table to the database.  
	#
	# Input:  
	# object:          	A GVARdb object.
	# map:				A data.frame containing a mapping with columns:
	#						chrom: 		CHARACTER chr1:chr22 chrX chrY chrM
	#						start: 		NUMERIC
	#						end:   		NUMERIC
	#						gvar_name:	CHARACTER (unique)
	#						strand:		CHARACTER +/-/* 
	#						gvar_meas:	CHARACTER
	# mapSource:		Used to store the source of a mapping: "UCSC" / "HapMap"
	# mapVersion:		Used to store version info: "NCBI36/hg18_dbSNP130". Some
	#						strings are designed to be recognized.
	# overwrite:		Drop table when already present?
	# backupCopy:		Should a backup copy be made in the folder where the db
	#						is stored?
	#
	# Output:			The map table of the GVARdb object is appended.
	# 
	# Requires:
	#
	# Remarks:          
	# 
	# ModLog:           "Original Version"  		15/11/2011  EBvandenAkker
	
	### Checking: ############################################################# 
	
	### Checking input:
	# 1) object & conn:
	.valid.GVARdb(object)
	conn <- .dbConn(object)
	# 2) conn & presence table 'data':
	.valid.dbTable(conn,tablename="data")
	# 2) map:
	map <- .normarg.map(conn,bed=map)
	# 3) mapSource:
	if(!isSingleString(mapSource)){
		stop("Please provide a single character string for 'mapSource'")
	}
	# 4) mapVersion:
	if(!isSingleString(mapVersion)){
		stop("Please provide a single character string for 'mapVersion'")
	}
	# 5) overwrite:
	if(!isSingleLogical(overwrite)){
		stop("Please provide a logical value for 'overwrite'")
	}
	# 6) backupCopy:
	if(!isSingleLogical(backupCopy)){
		stop("Please provide a logical value for 'backupCopy'")
	}
	
	### Core: #################################################################
	
	# 1) backup if about to overwrite:
	Tables <- dbListTables(conn)
	if(is.element("map",Tables)){
		if(overwrite==T){
			if(backupCopy==T){
				pathIN 	<- .dbPath(conn)
				Dummi 	<- .timeStamp(3)
				pathOUT <- paste(dirname(pathIN),"map_backup_",Dummi,sep="")
				oldmap  <- dbReadTable(conn,"map")
				write.table(oldmap,pathOUT,sep="\t",quote=F,colnames=T,rownames=F)
			}
			# Remove table:
			Dummi <- dbRemoveTable(conn,"map")
		} else {
			stop("Database already contains a table 'map'")
		}
	} else {
		# Initiate table 'map':
		.init.map.table(conn)
	}
	# 2) Load 'map' table into database:
	Dummi <- .append.map.table(conn,map)	
	# 3) Update metadata:
	metadata <- matrix(c(
		"AddMap_time",			.timeStamp(0),
		"AddMap_source",		mapSource,
		"AddMap_Version",		mapVersion)	
			,ncol=2,byrow=TRUE)
	colnames(metadata) <- c("name", "value")
	metadata <- as.data.frame(metadata,stringsAsFactors=FALSE)
	# 3) Load 'metadata' table into database:
	Dummi <- .append.metadata.table(conn,metadata)
	NULL
}

