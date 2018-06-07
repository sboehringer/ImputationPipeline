#
#	gwas_qc_af.R
#Fri Jan  7 16:34:00 CET 2011


systemQ = function(cmd, logLevel, jidFile = tempfile()) {
	qcmd = sprintf('qsub.pl --jidFile %s %s -- %s', jidFile, o$qsubOptions, cmd);
	Log(qcmd);
}

plinkFormats = list(
	ped = list(format = '[HEADER=F,SEP=S,NAMES=fid;iid;pid;mid;sex;stat]', id = 'iid', ext = 'fam'),
	map = list(format = '[HEADER=F,SEP=T,NAMES=chr;id;mapGen;mapPhy;a1;a2]', id = 'id', ext = 'bim')
);
plinkFile = function(input, type = 'ped') {
	ped = readTable(sprintf('%s:%s.%s', plinkFormats[[type]]$format, input, plinkFormats[[type]]$ext));
}
plinkPedFile = function(input) plinkFile(input, type = 'ped');
plinkMapFile = function(input) plinkFile(input, type = 'map');

plinkFileForIds = function(input, ids, output = NULL, type = 'ped', exclude = F, write = T) {
	# <p> exclusion
	f = plinkFile(input, type = type);
	idcs = which.indeces(ids, f[[plinkFormats[[type]]$id]]);
	f = f[idcs * ifelse(exclude, -1, 1), ];

	if (write) {
		write.table(f, file = output, col.names = F, row.names = F, quote = F);
		return(output);
	}
	f
}

plinkPedFileForIds = function(input, inds, output) {
	# <p> exclusion
	ped = plinkPedFile(input);
	pedExcl = ped[which.indeces(inds, ped$iid), ];
	write.table(pedExcl, file = output, col.names = F, row.names = F, quote = F);
	output
}

plinkPedFile2founders = function(input, output) {
	# <p> exclusion
	ped = plinkPedFile(input);
	ped$fid = 1:(length(ped$fid));
	ped$pid = 0;
	ped$mid = 0;

	write.table(ped, file = output, col.names = F, row.names = F, quote = F);
	output
}

# assume bfile format
plinkFile2founders = function(input, output) {
	plinkPedFile2founders(input, sprintf('%s.fam', output));
	File.symlink(splitPath(sprintf('%s.bim', input))$absolute, sprintf('%s.bim', output), logLevel = 4);
	File.symlink(splitPath(sprintf('%s.bed', input))$absolute, sprintf('%s.bed', output), logLevel = 4);
	output
}

# # missing per individual
# # missing per SNP
# plink --noweb --bfile cc --missing --out cc-missing
# 
# # HWE
# plink --noweb --bfile cc --hardy --out cc-hwe
# 
# # sex check
# plink --noweb --bfile cc --check-sex --out cc-sex
# 
# # population stratification
# #plink --noweb --bfile cc --cluster --distance-matrix --out cc-dist
# # distance matrix
# plink --noweb --bfile cc --Z-genome --out cc-dist
# plink --noweb --bfile cc --read-genome cc-dist.genome.gz --cluster --mds-plot 4 --out cc-mds

plinkOptions = function(o, exclusions = 'baseline', qsubOptions = NULL) {
	o = list(exclusions = list(
		individuals = readExclusionsInds(o, exclusions),
		markers = readExclusionsMarkers(o, exclusions),
		qsubOptions = qsubOptions
	));
	o
}

callPlink = function(input, output, command, o = NULL, pattern = NULL, ...) {
	options = '';
	markers = o$exclusions$markers;
	if (!is.null(markers)) {
		pathMarkers = tempfile();
		write.table(markers, file = pathMarkers, col.names = F, row.names = F, quote = F);
		options = join(c(options, sprintf('--exclude %s', pathMarkers)));
	}
	#inds = firstDef(o$exclusions$inds, o$exclusions$individuals);
	# <!> inds -> individuals 15.9.2016
	inds = o$exclusions$individuals;
	if (!is.null(inds)) {
		pathInds = plinkPedFileForIds(input, inds, tempfile());
		options = join(c(options, sprintf('--remove %s', pathInds)));
	}
	cmd = sprintf('plink --noweb --allow-no-sex %s --bfile %s --out %s %s',
		options, input, output, command);
	if (!is.null(pattern) && pattern == 'qsub') {
		outputDir = splitPath(output)$dir;
		System(cmd, 4, pattern = pattern, ...,
			qsubOptions = sprintf('--outputDir %s/qsub %s', outputDir, firstDef(o$qsubOptions, '')));
	} else {
		System(cmd, 4, pattern = pattern, ...);
	}
}

plinkExportPrunedFile = function(input, output, o) {
	callPlink(input, output, '--make-bed', o);
}

plinkMissing = function(input, outputDir = NULL, o = NULL, pattern = NULL, ...) {
	output = sprintf('%s/%s-missing', outputDir, splitPath(input)$file);	# output prefix
	callPlink(input, output, '--missing --nonfounders', o, pattern, ...);
	output
}

plinkHwe = function(input, outputDir = NULL, o = NULL, pattern = NULL, ...) {
	output = sprintf('%s/%s-hwe', outputDir, splitPath(input)$file);	# output prefix
	callPlink(input, output, '--hardy --nonfounders', o, pattern, ...);
	sprintf('%s.hwe', output)
}

plinkSex = function(input, outputDir = NULL, o = NULL, pattern = NULL, ...) {
	output = sprintf('%s/%s-sex', outputDir, splitPath(input)$file);	# output prefix
	callPlink(input, output, '--check-sex', o, pattern, ...);
	sprintf('%s.sexcheck', output)
}

plinkInbreeding = function(input, outputDir = NULL, o = NULL, pattern = NULL, ...) {
	output = sprintf('%s/%s-inbr', outputDir, splitPath(input)$file);	# output prefix
	callPlink(input, output, '--het --nonfounders', o, pattern, ...);
	sprintf('%s.het', output)
}

plinkPrune = function(input, outputDir = NULL, o = NULL, pattern = NULL, ...,
	windowSize = 50, windowShift = 5, thresholdVIF = 2, chr = NULL) {
	tag = if (is.null(chr)) '' else sprintf('-%d', chr);
	output = sprintf('%s/%s-prune%s', outputDir, splitPath(input)$file, tag);	# output prefix

	chr = if (is.null(chr)) '' else sprintf('--chr %d', chr);
	r = callPlink(input, output,
		sprintf('--indep %d %d %.2f %s', windowSize, windowShift, thresholdVIF, chr),
		o, pattern, ...
	);
	r = c(r, list(output = sprintf('%s.prune.out', output)));
	r
}

# names(ped): fid, iid, pid, mid, sex, stat
plinkMds = function(input, outputDir = NULL, o = NULL, Nsplit = 6, Ndim = 4, pattern = 'qsub') {

	# <p> prepare filenames and pathes
	output = sprintf('%s/%s', outputDir, splitPath(input)$file);	# output prefix
	if (!file.exists(outputDir)) dir.create(outputDir);
	outputDist = sprintf('%s/%s-dist', outputDir, splitPath(input)$file);
	if (!file.exists(outputDist)) dir.create(outputDist);

	# <p> force all individuals to be founders
	inputFounders = sprintf('%s-founders', output);
	plinkFile2founders(input, inputFounders);
	input = inputFounders;	# <!> replace input variable (dt later need of founder replacement)

	# <p> read ped-file
	pathPed = sprintf('[HEADER=F,SEP=S,NAMES=fid;iid;pid;mid;sex;stat]:%s.fam', input);
	ped = readTable(pathPed);

	# <p> compute frequencies
	freqFile = sprintf('%s-frq', output);
	#rF = callPlink(input, freqFile, sprintf('%s --freq', exclusionO), o);
	rF = callPlink(input, freqFile, '--freq', o);

	# <p> split files
	parts = counts2idcs(splitSeatsForFractions(dim(ped)[1], rep(1/Nsplit, Nsplit)));
	for (i in 1:Nsplit) {
		e = parts[i, ];
		pedPart = sprintf('%s/%03d.ped', outputDist, i);
		Log(pedPart);
		write.table(ped[e[1]:e[2], ], file = pedPart, quote = F, row.names = F, col.names = F);
	}

	# <p> distances
	mdList = list(list(1:Nsplit), list(1:Nsplit));
	if ( T) {
	#pattern = NULL;	# <%><t>
	rD = iterateModels_old(mdList, function(i, X1, X2) {
		prefix = sprintf('%s/%03d-%03d', outputDist, X1, X2);
		r0 = callPlink(input, prefix, sprintf(
			'--genome --genome-full --nonfounders --read-freq %s.frq --genome-lists %s/%03d.ped %s/%03d.ped',
			freqFile, outputDist, X1, outputDist, X2), o, 
			#pattern = pattern, qsubOptions = sprintf('--outputDir %s/qsub', outputDir));
			pattern = pattern);
		# remove header
		r1 = System(sprintf('tail -n +2 %s.genome | gzip -9 > %s.genome.gz', prefix, prefix),
			pattern = pattern, waitForJids = r0$jid, qsubOptions = o$qsubOptions);
		r1
	}, lapply__ = lapply);
	System.wait(rD$results, pattern = pattern);
	}
	# <p> file lists
	genomeFiles = iterateModels_old(mdList, function(i, X1, X2) {
		sprintf('%s/%03d-%03d.genome', outputDist, X1, X2)
	}, lapply__ = lapply)$results;
	# <p> extract header from first file
	System(sprintf('head -n 1 %s | gzip -9 > %s.genome.gz', genomeFiles[1], output));
	# <p> concatenate results
	System(sprintf('cat %s >> %s.genome.gz', join(paste(genomeFiles, '.gz', sep = ''), sep = ' '), output));

	# <p> remove temp-files
	file.remove(unlist(c(genomeFiles, paste(genomeFiles, '.gz', sep = ''))));

	# <p> MDS
	callPlink(input, sprintf('%s-mds', output),
		sprintf('--cluster --nonfounders --read-genome %s.genome.gz --mds-plot %d', output, Ndim), o
	);
	NULL
}

plinkIBS = function(input, outputDir = NULL, o = NULL, Nsplit = 6, Ndim = 4,
	excludeInds = NULL, excludeMarkers = NULL, pattern = 'qsub') {
}

# run all plink QC steps as parallel as possible
runPlinkQC = function(o) {
	jidFile = tempfile();

	plinkMissing(o, jidFile);
	plinkHwe(o, jidFile);

}

#
#	<p>
#

plinkSplitData = function(path = pathGenotypes, outputDir = plinkTempDir, Nchunks = 64) {
	# <p> read map
	m = read.table(sprintf('%s.bim', path), stringsAsFactor = F);
	names(m) = c('chr', 'marker', 'posGen', 'posPhy', 'a1', 'a2');

	# <p> break up genome
	chrs = unique(m$chr);
	Nchr = sapply(chrs, function(chr)count(m$chr == chr));
	Nmarkers = dim(m)[1];
	Ncc = splitSeatsForFractions(Nchunks, Nchr/Nmarkers);
	Log(Ncc);

	# <p> prepare plink
	if (!file.exists(outputDir)) dir.create(outputDir);
	NccCum = c(0, cumsum(Ncc));
	cmds = unlist(sapply(1:length(chrs), function(i) {
		chr = chrs[i];
		ms = m$marker[m$chr == chr];
		spl = splitListIndcs(length(ms), Ncc[i]);
		msRanges = apply(spl, 1:2, function(i)ms[i]);
		r = sapply(1:dim(msRanges)[1], function(k) {
			cmd = Sprintf(con('plink --noweb --bfile %{path}Q ',
				'--from %{from}Q --to %{to}Q --make-bed --out %{outputDir}Q/%{base}s%{chunk}03d'),
				from = msRanges[k, 1], to = msRanges[k, 2], base = splitPath(path)$base,
				chunk = NccCum[i] + k);
			cmd
		});
		r
	}));
	clapply(cmds, function(cmd)System(cmd, 2));
	#Log(Ncc);
	NULL
}

plinkSnpQuery = function(o, query) with(o, {
	require('RSQLite');
	if (!exists('globalBimDb', envir = .GlobalEnv)) {
		assign('globalBimDb', sqliteOpen(Sprintf('%{outputPrefixGlobal}s_bim.sqlite')), envir = .GlobalEnv);
	}
	sqliteQuery(globalBimDb, query)
})

#runPlinkQC(o);
plinkGetSnpInfo = function(o, id) with(o, {
	plinkSnpQuery(o, list(id = id))
})

plinkRangeAroundSnp = function(o, snp, range = 3e3) {
	info = plinkGetSnpInfo(o, snp)[1, ];
	pos = as.integer(info$mapPhy);
	query = list(chr = info$chr, mapPhy = list('> Int', pos - range), mapPhy = list('< Int', pos + range));
	plinkSnpQuery(o, query)
}

plinkReadGenotypes = function(o, snps) {
	e = readExclusions(o);
	snps = setdiff(snps, e$markers);
	gts = readPlinkBed(o$input, snps, usePedIid = T);
	gtsClean = gts[!(row.names(gts) %in% e$individuals), , drop = F];
	gtsClean
}

#
#	<p> data management
#

bimCols = c('chr', 'idSnp', 'posGen', 'posPhy', 'A0', 'A1');
plinkRemapBim = function(
	pathMap = '[HEADER=F,SEP=T,NAMES=idSnp;rsId]:data/plink/GSA_variantID_renaming.txt',
	pathBim = 'data/plink/cyptam.bim',
	outputPostfix = '-rs',
	printRanges = list(1000:1020, 10000:10020),
	outputFormat = '[HEADER=F,SEP=T,QUOTE=F]:') {

	map = readTable(pathMap);
	bim = readTable(con('[HEADER=F,SEP=T,NAMES=', join(bimCols, ';'),']:', pathBim));

	bimNew = merge(bim, map, by = 'idSnp', all.x = T);
	bim0 = bimNew = bimNew[order_align(bim$idSnp, bimNew$idSnp), , drop = F];
	if (nrow(bim) != nrow(bimNew)) stop('makers missing in bimNew');
	if (any(bimNew$idSnp != bim$idSnp)) stop('markers not in correct order');

	# <p> unmapped markers (not in map data frame)
	LogS(1, '#Non rs-mapped SNPs: %{Nsnps}d', Nsnps = sum(is.na(bimNew$rsId)));
	bimNew$idSnp[!is.na(bimNew$rsId)] = bimNew$rsId[!is.na(bimNew$rsId)];
	if (any(bimNew$idSnp[is.na(bimNew$rsId)] != bim0$idSnp[is.na(bimNew$rsId)]))
		stop('mapped markers overwritten');

	# <p> duplicates
	dI = which(duplicated(bimNew$idSnp));
	LogS(1, '#duplicated SNPs: %{Nsnps}d', Nsnps = length(dI));
	printComp = function(range, bim, bimNew) print(head(cbind(bimNew[range, ], bim[range, ])));
	bimNew$idSnp[dI] = paste(bimNew$idSnp[dI], 1:length(dI), sep = '-');
	if (any(duplicated(bimNew$idSnp))) stop('duplicated marker names present');

	# <p> debugging
	for (r in printRanges) printComp(r, bim, bimNew);

	# <p> output
	output = con(outputFormat, splitPath(pathBim)$fullbase, outputPostfix, '.bim');
	LogS(1, 'Writing new bim to "%{output}s"');
	writeTable(bimNew[, bimCols], output);
	with(splitPath(pathBim), {
		System(Sprintf('ln -s %{base}q.bed %{fullbase}q%{outputPostfix}q.bed'));
		System(Sprintf('ln -s %{base}q.fam %{fullbase}q%{outputPostfix}q.fam'));
	});
}


plinkResaveFam = function(fam,
	path = 'data/plink/aplpedpd',
	outputPostfix = '-ids',
	outputFormat = '[HEADER=F,SEP=S,QUOTE=F]:') {

	with(splitPath(path), {
		output = con(fullbase, outputPostfix);
		LogS(1, 'Writing new fam to "%{output}s.fam"');
		writeTable(fam, Sprintf('%{outputFormat}s%{output}s.fam'));
		System(Sprintf('ln -s %{base}q.bed %{output}q.bed'));
		System(Sprintf('ln -s %{base}q.bim %{output}q.bim'));
	});
}
