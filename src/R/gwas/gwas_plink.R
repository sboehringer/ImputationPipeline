#
#	gwas_qc_af.R
#Fri Jan  7 16:34:00 CET 2011


systemQ = function(cmd, logLevel, jidFile = tempfile()) {
	qcmd = sprintf('qsub.pl --jidFile %s %s -- %s', jidFile, o$qsubOptions, cmd);
	Log(qcmd);
}

# defaults for readBim
bimColsReadBim = c('chr', 'idSnp', 'posGen', 'posPhy', 'A0', 'A1');
# defaults by other plink related functions
bimCols = c('chr', 'id', 'mapGen', 'mapPhy', 'a1', 'a2');
pedCols = c('fid', 'iid', 'pid', 'mid', 'sex', 'stat');
plinkFormats = list(
	ped = list(format = con('[HEADER=F,SEP=S,NAMES=', join(pedCols, ';'), ']'), id = 'iid', ext = 'fam'),
	map = list(format =  con('[HEADER=F,SEP=T,NAMES=', join(bimCols, ';'), ']'), id = 'id', ext = 'bim')
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

callPlink = function(input, output, command, o = NULL, pattern = NULL, ..., options = '') {
	markers = if (notE(o)) o$exclusions$markers else NULL;
	if (notE(markers)) {
		pathMarkers = tempfile();
		write.table(markers, file = pathMarkers, col.names = F, row.names = F, quote = F);
		options = join(c(options, sprintf('--exclude %s', pathMarkers)));
	}
	#inds = firstDef(o$exclusions$inds, o$exclusions$individuals);
	# <!> inds -> individuals 15.9.2016
	inds = if (notE(o)) o$exclusions$individuals else NULL;
	if (notE(inds)) {
		pathInds = plinkPedFileForIds(input, inds, tempfile());
		options = join(c(options, sprintf('--remove %s', pathInds)));
	}
	cmd = Sprintf('plink --noweb --allow-no-sex %{options}s --bfile %{input}s --out %{output}s %{command}s');
	if (notE(pattern) && pattern == 'qsub') {
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

plinkReadGenotypes = function(o, snps, usePedIid = TRUE) {
	e = readExclusions(o);
	snpsBim = with(o, readBim(Sprintf('%{input}s.bim')))$idSnp;
	snps = intersect(setdiff(snps, e$markers), snpsBim);
	gts = readPlinkBed(o$input, snps, usePedIid = usePedIid);
	gtsClean = gts[!(row.names(gts) %in% e$individuals), , drop = F];
	gtsClean
}

#
#	<p> data management
#


plinkRemapBim = function(
	pathMap = '[HEADER=F,SEP=T,NAMES=idSnp;rsId]:data/plink/GSA_variantID_renaming.txt',
	pathBim = 'data/plink/cyptam.bim',
	outputPostfix = '-rs',
	printRanges = list(1000:1020, 10000:10020),
	outputFormat = '[HEADER=F,SEP=T,QUOTE=F]:',
	namesBim = bimColsReadBim) {

	map = readTable(pathMap);
	bim = readBim(pathBim);

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
	writeTable(bimNew[, namesBim], output);
	with(splitPath(pathBim), {
		System(Sprintf('ln -s %{base}q.bed %{fullbase}q%{outputPostfix}q.bed'));
		System(Sprintf('ln -s %{base}q.fam %{fullbase}q%{outputPostfix}q.fam'));
	});
}

# remap physical positions
#	pathMap: bimFile
plinkRemapBimPhy = function(pathMap, pathTarget = 'data/plink/cyptam', output,
	postfix = '-remapped', outputFormat = '[HEADER=F,SEP=T,QUOTE=F]:') {

	bcrb = bimColsReadBim;
	namesMap = c(bcrb[1:4], paste(bcrb[-(1:4)], 'x', sep = '.'));
	map = readBim(pathMap, namesBim = namesMap);
	namesBim = c('chr.y', bcrb[2], paste(bcrb[3:4], 'y', sep = '.'),bcrb[-(1:4)]);
	bim = readBim(pathTarget, namesBim = namesBim);

	bimNew = merge(bim, map, by = 'idSnp', all.x = T);
	bimNew = bimNew[order_align(bim$idSnp, bimNew$idSnp), , drop = F];
	if (nrow(bim) != nrow(bimNew)) stop('makers missing in bimNew');
	if (any(bimNew$idSnp != bim$idSnp)) stop('markers not in correct order');
	# <p> unmapped markers (not in map data frame)
	LogS(1, '#Map-unknown SNPs: %{Nsnps}d', Nsnps = sum(is.na(bimNew$posPhy) | is.na(bimNew$chr)));
	# <p> revert to old position, if not mapped
	# <N> unexpected results if chr/posPhy partially missing
	bimNew$chr[is.na(bimNew$chr)] = bimNew$chr.y[is.na(bimNew$chr)];
	bimNew$posPhy[is.na(bimNew$posPhy)] = bimNew$posPhy.y[is.na(bimNew$posPhy)];
	bimNew$posGen[is.na(bimNew$posGen)] = bimNew$posGen.y[is.na(bimNew$posGen)];
	Nremap = sum(bimNew$posPhy != bim$posPhy | bimNew$chr != bim$chr);
	LogS(1, '#Re-mapped SNPs: %{Nremap}d');

	# <p> duplicates
	dI = which(duplicated(bimNew$idSnp));
	LogS(1, '#duplicated SNPs: %{Nsnps}d', Nsnps = length(dI));

	# <p> output
	if (missing(output)) output = Sprintf('%{pathTarget}q%{postfix}q');
	LogS(1, 'Writing new bim to "%{output}s"');
	writeTable(bimNew[, bimColsReadBim], Sprintf('%{outputFormat}s%{output}q.bim'));
	with(splitPath(pathTarget), {
		System(Sprintf('ln -s %{base}q.bed %{output}q.bed'));
		System(Sprintf('ln -s %{base}q.fam %{output}q.fam'));
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

plinkSelectSnps = function(input, output, snps, snpListFormat = '[HEADER=F,SEP=T,QUOTE=F]:') {
	snpList = tempfile();
	writeTable(Df(snp = sort(snps)), Sprintf('%{snpListFormat}s%{snpList}s'));
	plinkCommand = Sprintf('--make-bed --extract %{snpList}q')
	callPlink(input, output, plinkCommand);
}

plinkReadMap = function(input, formatMap = con(plinkFormats$map$format, ':'), autoExt = T) {
	if (is.null(splitPath(input)$ext) && autoExt) input = Sprintf('%{input}s.bim');
	readTable(Sprintf('%{formatMap}s%{input}s'));
}
# extension: ignored, kept for compatibility
readBim = function(pathBim, extension = '', autoExt = T, namesBim = bimColsReadBim) {
	bim = plinkReadMap(pathBim,
		formatMap = con('[HEADER=F,SEP=T,NAMES=', join(namesBim, ';'),']:'),
		autoExt = autoExt
	);
	return(bim);
}

plinkMerge = function(input1, input2, output) {
	command = Sprintf(con('plink --noweb ',
		'--bfile %{input1}q ',
		'--bmerge %{input2}q.bed %{input2}q.bim %{input2}q.fam ', 
		'--make-bed --out %{output}q'
	));
	System(command,	2);
}

plinkLink = function(i, d) with(splitPath(i), {
	output = Sprintf('%{d}q/%{base}q');
	System(Sprintf('ln -s %{absolute}q.bed %{output}q.bed'));
	System(Sprintf('ln -s %{absolute}q.bim %{output}q.bim'));
	System(Sprintf('ln -s %{absolute}q.fam %{output}q.fam'));
	return(output);
})

namesFrqStd = c('chr', 'snpId', 'a0', 'a1', 'maf', 'Nobs');
formatFrqStd = con('[HEADER=T,SEP=S+,NAMES=', join(namesFrqStd, ';'), ']:');
plinkAf = function(i, formatFrq = formatFrqStd) {
	td = tempdir();
	tf = plinkLink(i, td);
	with(splitPath(i), SystemS('cd %{td}q ; plink --noweb --bfile %{tf}q --freq', 2));
	# <!> '.f' triggers special case in formatting recognition
	af = readTable(Sprintf('%{formatFrq}s%{td}s/plink.frq'));
	return(af);
}

flipMap = list(A = 'T', C = 'G', G = 'C', T = 'A');
flip = function(as)sapply(as, function(a)flipMap[[a]]);
doesFlip = function(as1, as2)all(as1 == flip(as2));

# ('data/plink/mtxdili-merged', 'data/plink/rams-remapped', 'data/plink/rams-alleles');
# aligning towards ref, all inputs are plink binaries
plinkAlignAlleles = function(ref, target, output, postfix = '-flipped',
	snpListFormat = '[HEADER=F,SEP=T,QUOTE=F]:') {
	afR = plinkAf(ref);
	afT = plinkAf(target);
	afM = merge(afT, afR, by = c('chr', 'snpId'));

	# <p> replace 0 alleles
	selA0 = afM$a0.y == '0';
	selA1 = afM$a1.y == '0';
	afM$a0.y[selA0] = afM$a0.x[selA0];
	afM$a1.y[selA1] = afM$a1.x[selA1];

	# <p> monomorphic SNPs
	selMon = afM$a0.y == afM$a1.y;
	# minor allele set to other allele of reference
	afM$a0.y[selMon] = apply(afM[selMon, c('a0.x', 'a1.x', 'a0.y')], 1, function(r)setdiff(r[1:2], r[3]));

	# <p> strand flips
	snps = apply(afM, 1, function(r) with(as.list(r), {
		a1 = c(a0.x, a1.x);
		a2 = c(a0.y, a1.y);
		# ignore minor/major mismatch
		if (all(a1 == a2) || all(a1 == rev(a2))) return(NULL);
		if (doesFlip(a1, a2)) return(T);
		LogS(2, 'Non-flippable SNP: %{snpId}s');
		return(NA);
	}));
	snpsTbIgnored = afM$snpId[unlist(sapply(snps, function(.)length(.) > 0 && is.na(.)))];
	snpsTbFlipped = afM$snpId[unlist(sapply(snps, function(.)length(.) > 0 && !is.na(.)))]

	# <p> update map
	# <p> delete snps
	# <p> flip
	if (missing(output)) output = Sprintf('%{target}q%{postfix}q');
	tf = tempfile();
	writeTable(Df(snps = snpsTbFlipped), path = Sprintf('%{snpListFormat}s%{tf}q'));
	tfEx = tempfile();
	writeTable(Df(snps = snpsTbIgnored), path = Sprintf('%{snpListFormat}s%{tfEx}q'));
	with(splitPath(output),
		SystemS(con('cd %{dir}q ; ',
			'plink --bfile %{i}q --exclude %{tfEx}q --flip %{tf}q --recode --noweb --make-bed --out %{base}q'),
			2, i = splitPath(target)$absolute)
	);
	
}

# from GWAS-MTX-DILI: remove trailing position, alleles from SNP names
plinkRenameSnpsDefaultRe =
	'{(?:(?:(?<=\\s)(rs\\d+):\\d+:\\S+:\\S+)(?=\\s)|(?:(?<=\\s)(\\d+:\\d+):\\S+:\\S+)(?=\\s))}{$1.$2}';
plinkRenameSnps = function(input, output, re = plinkRenameSnpsDefaultRe, reModifier = 'eo', deduplicate = T) {
	dedupCmd = if (deduplicate) "| csv.pl -t --deduplicate=1 --noheader --joinwith '-'" else '';
	command = Sprintf("cat %{input}q.bim | perl -pe 's%{re}s%{reModifier}s' %{dedupCmd}s > %{output}q.bim");
	System(command, 2);
	System(Sprintf('ln -s %{input}q.bed %{output}q.bed'), 2);
	System(Sprintf('ln -s %{input}q.fam %{output}q.fam'), 2);
}
