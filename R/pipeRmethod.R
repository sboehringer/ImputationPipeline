#
#	pipeRmethod.R
#
#	Author(s): Q Helmer, S Boehringer

# Expample: R --vanilla --slave -f test_pipeRmethod.R --args ../../test_data/LLS_Offspring_Partners_Final_36_130_Overlap_chr9_imputed-12.gens 'lc_1,lc_2' 'sex,age' ../../test_data/LC_phenos_qnorm_LLSnr.txt ../../test_data/pedfile

#source('../../RgenericAll.R');


#pipeRmethod = function(input, output, phenos, covs, variableFile, pedFile, writeAsTable = T) {
pipeRmethod = function(input, output, variableFile, pedFile, writeAsTable = T, digits = NULL, ...,
	RfunctionSource, RfunctionName, prefixes = splitString(':', Sys.getenv('RSCRIPTS')),
	by = NULL, do_debug = F){
	# <p> create data frame w/o genotypes
	Log(sprintf("Trying to read variable file '%s'", variableFile), 2);
	vars = readTable(variableFile);
	Log(sprintf("... read columns: [%s]", join(names(vars), ' ')), 2);
	Log(sprintf("Trying to read ped file '%s'", pedFile), 2);
	ped = readTable(pedFile);
	Log(sprintf("... read columns: [%s]", join(names(ped), ' ')), 2);
	Nids = nrow(ped);

	# <p> merge by 'id' and 'iid' or 'iid' alone
	if (is.null(by)) by = intersect(intersect(names(vars), names(ped)), c('fid', 'iid'));
	Logs('pipeRmethod: merging by [%{by}s]' by = join(by, ' '), logLevel = 2);
	peddata = Merge(vars, ped, sort = F, all.y = T, by = by);

	# <p> read genotypes <A> expect impute format
	genotypeFile = sprintf('%s.gens', input);
	genotypeInfofile = sprintf('%s.gens_info', input);
	N = length(readLines(genotypeInfofile)) - 1;
	if (!file.exists(genotypeFile)) {
		Log(sprintf("Input file '%s' does not exist", genotypeFile), 2);
		return(NULL);
	}
	#classes<-c("character","character","integer","character","character",rep("numeric",Nids*3));
	#gens = read.table(genotypeFile, comment.char = "", colClasses = classes, nrows = N); #specify column classes decreases memory usage
	Log(sprintf('#SNPs:%d file:%s', N, genotypeFile), 3);
	chromosome = strsplit(strsplit(genotypeFile,"chr")[[1]][2],"_")[[1]][1];

	# <p> source input script
	script = file.locate(RfunctionSource, prefixes = prefixes);

	#cat(readFile(script));
	Log(sprintf("Sourcing script @ %s\n", script), 5);
	source(script, chdir = T);
	#N = nrow(gens);
	#N = 100;
	Tfile = file(genotypeFile, "r")
	Ifile = file(genotypeInfofile, "r")
	infocols = scan(Ifile, what=character(0), n=10, quiet=T) #discard header
	r = lapply(1:N, function(i) {
		firstcols = scan(Tfile, what = character(0), n = 5, quiet=T)
		infocols = scan(Ifile, what = character(0), n = 10, quiet=T)
		snpname = firstcols[2];
		snpinfo = firstcols[3:5];
		snpinfo2 = infocols[4:5];
		genos = scan(Tfile, what=numeric(0), n = 3 * Nids, quiet=T)
		# <p> read impute file format <A>
		#genos <- gens[i, 6:ncol(gens)];
		genoarray = t(array(unlist(genos), dim = c(3, length(genos)/3)));
		# <p> create data frame
		#snpname = gens[i,2];
		colnames(genoarray) = paste('MARKER', c("AA", "AB", "BB"), sep = "_");
		dosage = genoarray %*% 0:2;
		dataGts = data.frame(ped[, by], genoarray, MARKER_dosage = dosage);

		# <p> merge to produce output
		data = Merge(dataGts, vars, sort = F, all.x = T, by = by);
		if (do_debug) print(head(data));
		
		# <p> call function
		Log(sprintf('Calling %s for snp %s', RfunctionName, snpname), 5);
		r = try(
			do.call(get(RfunctionName), c(list(data = data, snp = snpname), list(...)))
			#do.call(get(RfunctionName), c(list(data = data, snp = snpname), formula0=formula0, formula1=formula1))
		);
		if (i %% 5e2 == 0) Log(sprintf('Processed %d snps', i), 3);
		if (class(r) == 'try-error') r = NA;
		r = c(snpname, chromosome, snpinfo, snpinfo2, r);
		names(r)[1:7] = c('marker', 'chr', 'position', 'A0', 'A1', 'allele_freq', 'impute_info');
		r
	});
	close(Tfile);
	#names(r) = gens[1:N, 2];

	if (writeAsTable) {
		# <p> check return values
		l0 = sapply(r, length);
		l1 = table(l0);
		if (length(l1) > 2) {
			Log('Irregular table produced; no output written.', 1);
			sink(stderr());
				print(table(l0));
			sink();
			return();
		}
		l2 = as.integer(names(l1))[length(l1)];
		first = which(l0 == l2)[1];	# first result with regular length
		n0 = names(r[[first]]);
		Log(sprintf('Regular line has %d columns.', l2), 5);

		# <p> regularize table
		t0 = t(sapply(r, function(e) {
			if (length(e) < l2) c(e[1],rep(NA, l2-1)) else e
		}));
		t0 = data.frame(t0);
		names(t0) = n0;

		# <p> format output
		if (!is.null(digits)) {
			Log(sprintf('Formatting table to %d significant digits.', digits), 5);
			t0 = format(t0, digits = digits, scipen = 0);
# 			digitsOld = options()$digits;
# 			scipenOld = options()$scipen;
# 			options(digits = digits);
# 			options(scipen = 1);
		}
#		sink(stderr());
#			print(head(t0));
#		sink();

		# <p> write output
		Log(sprintf('Writing table (%d, %d) to "%s".', dim(t0)[1], dim(t0)[2], output), 3);
		t1<-data.frame(sapply(t0,function(x) unlist(x))) #columns in t0 are lists and cannot be written
		write.csv(t1, file = output, quote = F, row.names = F);

# 		if (!is.null(digits)) {
# 			options(digits = digitsOld);
# 			options(scipen = scipenOld);
# 		}
	} else {
		save(r, file = output);
	}
}

#
#	<p> attic
#

# 	if (0) {
# 	phenos<-unlist(strsplit(phenos,","))
# 	covs<-unlist(strsplit(covs,","))
# 	phenocols<-try(subset(peddata,select=phenos))
# 	if (class(phenocols) == "try-error") stop(paste("One or more of phenotypes",phenos,"not found",sep=" "))
# 	for (cov in covs) {
# 		covcol<-try(subset(peddata,select=cov))
# 		if (class(covcol) != "try-error"){ phenocols<-cbind(phenocols,covcol)}
# 		else{ print(paste("Covariate",cov,"skipped; it is not in the phenotype file",sep=" "))}
# 	}
# 	}
