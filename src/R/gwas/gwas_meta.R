#
#	gwas_meta.R
#Thu Mar 14 14:56:26 CET 2019


# if (T) {
#	# qw('id chr pos allele maf P beta');
# 	metaSpec = list(
# 		riken = list(
# 			input = 'dataMeta/riken/sunitinib_result_RIKEN.txt', 'results/meta/db-meta.sqlite',
# 			columns = qw('SNP CHR BP allele MAF  P BETA.marker ciL.marker ciU.marker'),
# 			sep = 'S+',
# 			type = 'ci'
# 		),
# 		eurotarget = list(
# 			input = 'dataMeta/eurotarget/clean_chr1_imputed-1_imputation_02', 'results/meta/db-meta.sqlite',
# 			columns = qw('marker chr position A0 allele_freq P.value beta1.MARKER_dosage sd1.MARKER_dosage'),
# 			sep = 'C',
# 			type = 'se'
# 		)
# 	);
# }

metaNamesStdRaw = qw('id chr pos allele maf P beta');
metaNamesStd = list(ci = c(metaNamesStdRaw, qw('ciL ciU')), se = c(metaNamesStdRaw, qw('sd')));
metaTypesStd = merge.lists(
	listKeyValue(Union(metaNamesStd), 'numeric'),
	listKeyValue(qw('id allele'), 'character')
);


createMetaDb = function(output, spec, metaTypes = metaTypesStd) {
	einlapply(spec, function(spec, i, n) with(spec, {
		csv2sqlite(input, output, inputSep = sep, newDb = i == 1,
			columnsSelect = columns, tableName = Sprintf("summ%{n}u"), index = qw('id chr pos P'),
			headerMap = listKeyValue(columns, metaNamesStd[[type]]), types = metaTypes
		)
	}));
}



#cat 'dataMeta/riken/sunitinib_result_RIKEN.txt' | perl -pe 's/(rs\d+):\S+/\1/; s/(\s*\d+\s*\d+:\d+):\S+/\1/' > 'dataMeta/riken/sunitinib_result_RIKEN-snps.txt'
# dbPath = 'results/meta/db-meta.sqlite';
# if (T) {
# 	metaSpec = list(
# 		riken = list(
# 			input = 'dataMeta/riken/sunitinib_result_RIKEN-snps.txt', dbPath,
# 			columns = qw('SNP CHR BP allele MAF  P BETA.marker ciL.marker ciU.marker'),
# 			sep = 'S+',
# 			type = 'ci'
# 		),
# 		eurotarget = list(
# 			input = 'dataMeta/eurotarget/clean_chr1_imputed-1_imputation_02', dbPath,
# 			columns = qw('marker chr position A0 allele_freq P.value beta1.MARKER_dosage sd1.MARKER_dosage'),
# 			sep = 'C',
# 			type = 'se'
# 		)
# 	);
# }

createSummaryTable = function(input, output,
	type, sep, columns, tableName, newDb = F, metaTypes = metaTypesStd)
	csv2sqlite(input, output, inputSep = sep, newDb = newDb,
		columnsSelect = columns, tableName = tableName, index = qw('id chr pos'),
		headerMap = listKeyValue(columns, metaNamesStd[[type]]), types = metaTypes)

# <i><!> refactor to use createSummaryDb
createMetaDb = function(output, spec, metaTypes = metaTypesStd) {
	einlapply(spec, function(spec, i, n) with(spec, {
		csv2sqlite(input, output, inputSep = sep, newDb = i == 1,
			columnsSelect = columns, tableName = Sprintf("summ%{n}u"), index = qw('id chr pos'),
			headerMap = listKeyValue(columns, metaNamesStd[[type]]), types = metaTypes
		)
	}));
}

metaColumns = c('chr', 'id', 'pos', 'allele', 'maf', 'beta', 'sd');
metaColumnsR = setdiff(metaColumns, 'id');
joinMetaDb = function(dbPath, spec) {
	db = sqliteOpen(dbPath);
	ds = einlapply(spec, function(spec, i, n) with(spec, {
		d0 = sqliteQuery(db, query = list(id = list('like', 'rs%')), table = Sprintf("summ%{n}u"));
		if (spec$type == 'ci') d0$sd = ciToSd(d0$ciL, d0$ciU);
		return(d0[, metaColumns]);
	}));
	dMerge = merge.multi.dfs(ds, by = 'id');
	return(dMerge);
}

metaGWASsingle = function(r, mafEps = .1) {
	postf = unlist(unique(Regexpr('[^.]+[.](.*)', setdiff(names(r), 'id'), captures = T)));
	rMaf = lapply(postf, function(p) {
		metaCols = paste(metaColumnsR, p, sep = '.');
		r1 = setNames(r[metaCols], metaColumnsR);
		if (is.na(r1$maf) || r1$maf <= .5 + mafEps) return(r1);
		r1$beta = -r1$beta;
		r1$maf = 1 - r1$maf;
		return(r1);
	});
	rMeta = rma.uni(list.kpu(rMaf, 'beta'), sei = list.kpu(rMaf, 'sd'));
	r1 = list(
		chr = na.omit(unique(list.kpu(rMaf, 'chr'))),
		pos = na.omit(unique(list.kpu(rMaf, 'pos'))),
		id = r$id,
		maf = mean(list.kpu(rMaf, 'maf'), na.rm = T),
		mafSd = sd(list.kpu(rMaf, 'maf'), na.rm = T),
		allele = join(list.kpu(rMaf, 'allele'), ':'),
		beta = rMeta$beta[, 1],
		se = rMeta$se,
		P = rMeta$pval,
		warnings = NA
	);
	w = list();
	if (length(r1$chr) > 1) {
		w = Sprintf('C: %{chr}s', chr = join(r1$chr, ':'));
		r1$chr = r1$chr[1];
	}
	if (length(r1$pos) > 1) {
		w = c(w, Sprintf('P: %{pos}s', pos = join(r1$pos, ':')));
		r1$pos = r1$pos[1];
	}
	if (length(w) > 0) r1$warnings = join(w, ', ');
	return(r1);
}

metaGWASapply = function(dMerge, tempFile = tempfile()) {
	rMeta = dfapply(dMerge, metaGWASsingle);
	row.names(rMeta) = NULL;
	return(rMeta);
}

metaGWASapplyDisk = function(dMerge, dbFile = NULL, tempFile = tempfile()) {
	N = nrow(dMerge);
	LogS(4, "Meta-analyzing %{N}d SNPs");
	rMeta = sapply(1:N, function(i) {
		r0 = metaGWASsingle(as.list(dMerge[i, ]));
		write.table(Df_(r0), file = tempFile, col.names = i == 1, append = i != 1, row.names = F);
		return(NULL);
	});
	return(tempFile);
}
