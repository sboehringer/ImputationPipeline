#
#	Rgenetics.R
#Thu Dec  4 13:00:03 CET 2008

# currently not part of RgenericAll.R <!>

#
#	<p> coding conventions
#
#	Alleles are coded as 0, 1
#	Haplotype frequencies are numbered as binary digits h_{dnd{n-1}...d0},
#		with d0 being the allele at locus 0
#

#
#	<p> parametrization
#


# functions are parX2Y, where X and Y denote parameter space names, X is given in ucfirst
# X, Y \in { hfreq, cond, ld, r }
# parameters are always assumed to be complete rather than free
# use parCompleteX, parReduceX

# Interpretation of parameters (L number of loci 1, ..., L):
# 	we assume biallelic loci, A(h) denotes binary representation of haplotype h
#	h_{-k} is the haplotype after locus k is removed from h
#
#	hfreqs: are enumerated 1:2^L, binary representation (i1, ..., iL) represents allele ij at locus j
#		i1 is lowest valued bit
#		(p_1, ..., p_{2^L})
#	cond: hfreqs for loci 1, ..., L - 1 and conditional freqs P(A(h)[L] == 0 | h_{-L})
#		(p_1 + p_{1 + 2^L/2}, ..., p_{2^L/2} + p_{2^L},
#		 P(A(h_1)[L] == 0 | p_{h_{-L}, 1}), ...,  P(A(h)[L] == 0 | h_{-L}))
#	ld: hfreqs for loci 1, ..., L - 1, allele frequency for locus L (allele 0 =: a),
#			ld between hts and allele
#		(p_1 + p_{1 + 2^L/2}, ..., p_{2^L/2} + p_{2^L},
#		 Cov(I{h_1}, I{a}), ...,  Cov(I{h_{2^L/2}}, I{a})
#		 p_a)
#	r: correlation instead of covariance (compare ld)
#	cm: hfreqs for loci 1, ..., L - 1, conditional freqs P(A(h)[L] == 0 | h_{-L}), allele freq at L: P(A(h)[L] == 0)
#		conditional mariginal, the marginal allele frequency substitutes one conditional frequency
#		(p_1 + p_{1 + 2^L/2}, ..., p_{2^L/2 - 1} + p_{2^L - 1},
#		 P(A(h_1)[L] == 0 | p_{h_{-L}, 1}), ...,  P(A(h)[L] == 0 | h_{-L}))
#		P(A(h)[L] == 0)

#
#	<p> conventions association
#
#	The lower numbered allele is assumed to be associated, reflected e.g. by the scores for
#	the dominant model (1, 1, 0)
#
#	</p> convenction association
#

#
#	conversion base between parametrizations
#

parCond2hfreq = function(cf) {
	N = length(cf);	# == 2^L
	hfs = cf[1:(N/2)];
	cfs = cf[(N/2 + 1):N];
	# joint frequencies
	jf = cfs * hfs;
	c(jf, hfs - jf);
}

parHfreq2cond = function(hf) {
	N = length(hf);	# == 2^L
	# marginal frequencies
	mfs = apply(matrix(hf, ncol = 2), 1, sum);
	cfs = hf[1:(N/2)] / mfs;
	c(mfs, cfs)
}
parCm2hfreq = function(cm) { parCond2hfreq(cm[-length(cm)]) }
parHfreq2cm = function(hf) {
	cf = parHfreq2cond(hf);
	af = sum(hf[(1:(length(hf)/2))]);
	c(cf, af)
}

parHfreq2ld = function(hf) {
	N = length(hf);
	# marginal frequency of allele 0 at locus L
	pa = sum(hf[1:(N/2)]);
	# marginal frequencies haplotypes at loci 1, ..., L-1
	mfs = apply(matrix(hf, ncol = 2), 1, sum);
	# ld parameters
	ld = (hf[1:(N/2)] - pa * mfs);
	c(mfs, ld, pa)
}

parLd2hfreq = function(ld) {
	N = length(ld) - 1;	# such that N == 2^L
	# allele frequency of allele 0 at L
	pa = ld[N + 1];
	# marginal hfs at loci 1, ..., L - 1
	mfs = ld[1:(N/2)];
	# ld parameters
	lds = ld[(N/2 + 1): N];
	hf = lds + mfs * pa;
	hf = c(hf, mfs - hf);
	hf
}

parLd2r = function(ld) {
	N = length(ld) - 1;	# such that N == 2^L
	# allele frequency of allele 0 at L
	pa = ld[N + 1];
	# marginal hfs at loci 1, ..., L - 1
	mfs = ld[1:(N/2)];
	# ld parameters
	lds = ld[(N/2 + 1): N];
	rs = lds / sqrt(pa * (1 - pa) * (mfs * (1 - mfs)));
	ldr = c(mfs, rs, pa);
	ldr
}

parR2ld = function(ldr) {
	N = length(ldr) - 1;	# such that N == 2^L
	# allele frequency of allele 0 at L
	pa = ldr[N + 1];
	# marginal hfs at loci 1, ..., L - 1
	mfs = ldr[1:(N/2)];
	# r parameters
	ldrs = ldr[(N/2 + 1): N];
	ld = ldrs * sqrt(pa * (1 - pa) * (mfs * (1 - mfs)));
	ld = c(mfs, ld, pa);
	ld
}

#
#	derived conversions
#

parHfreq2r = function(hf) parLd2r(parHfreq2ld(hf));
parR2hfreq = function(ldr) parLd2hfreq(parR2ld(ldr));

#
#	free vs. complete parameters
#

parReduceHfreq = function(hf) {
	N = length(hf);
	hf[-N]
}
parCompleteHfreq = function(hf) {
	c(hf, 1 - sum(hf));
}
parReduceCond = function(cf) {
	N = length(cf);
	cf[-(N/2)]
}
parCompleteCond = function(cf) {
	N = length(cf) + 1;
	c(cf[1:(N/2 - 2)], 1 - sum(cf[1:(N/2 - 1)]), cf[(N/2 - 1):(N - 1)])
}
parReduceCm = function(cm) {
	N = length(cm) - 1;
	cm[-c(N/2, N)]
}
parCompleteCm = function(cm) {	# expands to a completeCond representation + allele frequency of allele 0 at locus n
	N = length(cm) + 1;
	af = cm[N - 1];
	shfs = sum(cm[1:(N/2 - 1)] * cm[(N/2):(N - 2)]);	# probability mass of haplotypes lacking the last category
	smhfs = sum(cm[1:(N/2 - 1)]);	# probability mass of marginal haplotype frequency excluding last haplotype
	lhf = 1 - sum(cm[1:(N/2 - 1)] * cm[(N/2):(N - 2)]);	# haplotype frequencies
	c(cm[1:(N/2 - 1)], 1 - smhfs, cm[(N/2):(N - 2)], (af - shfs) / (1 - smhfs), af)
}
parReduceLd = parReduceR = function(ld) {
	N = length(ld) - 1;
	ld[c(-(N/2), -N)]
}
parCompleteLd = function(ld) {
	N = length(ld) + 1;
	# allele frequency of allele 0 at L
	pa = ld[N - 1];
	# marginal hfs at loci 1, ..., L - 1
	mfs = ld[1:(N/2 - 1)];
	# ld parameters
	lds = ld[N/2: (N - 2)];
	# completion
	mfs = c(mfs, 1 - sum(mfs));
	lds = c(lds, -sum(lds));
	c(mfs, lds, pa)
}
parCompleteR = function(ld) {
	N = length(ld) + 1;
	# allele frequency of allele 0 at L
	pa = ld[N - 1];
	# marginal hfs at loci 1, ..., L - 1
	mfs = ld[1:(N/2 - 1)];
	# ld parameters
	ldrs = ld[N/2: (N - 2)];
	sumLd = sum(ldrs * sqrt(pa * (1 - pa) * mfs * (1 - mfs)));
	# completion
	mfs = c(mfs, 1 - sum(mfs));
	ldrs = c(ldrs, -sumLd  / sqrt(pa * (1 - pa) * (mfs[N/2] * (1 - mfs[N/2]))));
	c(mfs, ldrs, pa)
}

#
#	<p> single SNP parameter systems
#

parP2RE = function(p) {
	# <p> calculate allele frequency, assuming p = (p_1, p_2, p_3) are gt freqs of AA Aa aa
	q = p[1] + .5 * p[2];
	if (q %in% c(0,1)) return(c(q, 0));
	# <p> HWE statistic on expectations, carry over sign
	qE = af2hwe(q);		# calculate expectation under HWE
	E = sign(p[2] - qE[2]) * sqrt(sum(sapply(1:3, function(i)( (p[i] - qE[i])^2 / qE[i]))));
	r = c(q, E);
	r

}
parRE2P = qg2gt = function(r) {
	# <p> components of r
	q = r[1];
	E = r[2];
	# <p> expectation
	qE = af2hwe(q);	# calculate expectation under HWE
	p2 = 2 * q * (1 - q) * (1 + E);
	p1 = q - p2/2;
	p3 = 1 - p1 - p2;
	# <p> return vector
	p = round(c(p1, p2, p3), 8);
	p
}
# transform data to count + parameter estimates
counts2NRE = function(d) {
	n = sum(d);
	ps = d / n;
	c(n, parP2RE(ps))
}
NRE2counts = function(d) {
	ps = parGE2P(d[2:3]);
	counts = d[1] * ps;
	counts
}

#
#	<p/> parametrization
#

#
#	<p> misc methods
#

# copy-edit from r-wrappers.R (2007-05-likelihoodFamily)
# complete indicates, whether the mapped p should sum to 1 or to < 1
polyLogistic = function(p, complete = F) {
	p = sapply(p, exp);
	sumG = sum(p);
	p / (1 - complete + sumG)	#sapply(p, function(g){g/(1 - complete + sumG)})
}
polyLogit = function(p) {
	logitD = function(S) {	# logit delta
		z = sapply(p, function(gi)(gi * (1 + S)));
		Sz = sum(z);
		d = (S - Sz)^2;
		d
	};
	o = optimize(logitD, c(0, 10));
	S = o$minimum;
	g = sapply(p, function(gi)(log(gi) + log(1 + S)));
	g
}

#
#	<p> simulation
#

simulateDiplotypesNuclearFamily = function(count = 1, countLoci = 2, countOffspring = 1,
	hapDist = rep(2 ^ -countLoci, 2 ^ countLoci)) {

	co = countOffspring;
	# draw parental haplotypes
	phts = as.vector(t(rmultinom(4 * count, 1, hapDist)) %*%
		t(t(0:(length(hapDist) - 1))));
	phtsm = matrix(phts, ncol = 2, byrow=TRUE);
	# draw inheritance vectors
	hsel = rbinom(2 * countOffspring * count, 1, .5);	# haplotype selection
	iv = as.vector(rbind(hsel, 1 - hsel));				# inheritance vector
	ivm = t(matrix(iv, ncol = 2, byrow= TRUE));			# corresponding matrix
	# offspring haplotypes
	ohts = apply(to.col(1:count), 1, function(i){
		# current parental diplotypes
		pthis = phtsm[ ((i - 1) * 2 + 1): (i * 2), ];
		# compute inherited hts (includes paternal and maternal transm for each 
		# iv, one of which has to be eliminated)
		raw = (pthis %*%
			ivm[, ((i - 1) * 2 * countOffspring + 1): (i * 2 * countOffspring)]);
		c(as.vector(t(pthis)), as.vector(rbind(raw[1, 1:co], raw[2, (co + 1):(2*co)])))
	});

	#c(phts, ohts)
	#print(list(diplotypes = ohts, dvec = as.vector(ohts)));
	as.vector(ohts);
}

# sort haplotypes within diplotypes, assuming a vector of 2N haplotypes
normalizeGenotypes = normalizeDiplotypes = function(dts) {
	dtsN = t(apply(matrix(dts, ncol = 2, byrow = T), 1, sort));
	dtsN = as.vector(t(dtsN));
	dtsN
}

# remove loci from diplotypes
removeLociFromDiplotypes = function(dts, loci = c(), countLoci = 2) {
	sapply(dts, function(d) { bin2ord(ord2bin(d, digits = countLoci)[-loci]) })
}


# simulate N diplotypes from haplotype distribution hapDist
#	hapDist: haplotype distribution, sum(hapDist) == 1, passed to rmultinom
#	names: numerical names for haplotypes (used in a matrix multiplication)
# returns a vector of 2N haplotypes =^= N diplotypes
simulateDiplotypes = function(N = 1, hapDist = rep(2^-2, 2^2), names = 0:(length(hapDist) - 1)) {
	#countLoci = log2(length(hapDist));
	hts = as.vector(t(rmultinom(2 * N, 1, hapDist)) %*% t(t(names)));
	hts
}

simulateLocus = function(N, af)as.vector(0:2 %*% rmultinom(N, 1, af2hwe(af)));
# simulate SNP-genotypes and phenotype for a single locus
simulateLocusCaseControl = function(N, af, mu, beta, scores = scoresStd$add, O = 2, controlsRandom = T) {
	# number of samples to simulate
	Ns = O * sum(N);
	d = NULL;
	if (controlsRandom && N[1] > 0) d = data.frame(y = 0, gts = simulateLocus(N[1], af));
	while (sum(d$y == 0) < N[1] || sum(d$y == 1) < N[2]) {
		# <p> simulate unconditionally
		gts = simulateLocus(Ns, af);
		y = simulatePhenotypesFromGenotypes(gts, scores = scores, beta = beta, mu = mu);
		ds = data.frame(y = y, gts = gts);

		# <p> select samples from simulation
		for (y in 0:1) {
			NyTot = sum(d$y == y);	# total so far
			NyThis = sum(ds$y == y);	# new batch
			Nm = N[y + 1] - NyTot;	# number still to be simulated (missing)
			if (NyThis > 0 && Nm > 0) {
				dy = ds[ds$y == y, ];
				dy = dy[1:min(Nm, dim(dy)[1]), ];
				d = rbind(d, dy);
			}
		}
	}
	row.names(d) = NULL;
	d
}

testLocusCaseControl = function(d, scores = c(0, .5, 1),
	formula = y ~ gtScores,
	test = function(formula, data) {
		r = glm(formula, family = binomial(), data = data);
		list(p.value = summary(r)$coefficients['gtScores', 'Pr(>|z|)'])
	}, ...) {
	d = data.frame(d, gtScores = scores[d$gts + 1]);
	t = test(as.formula(formula), d, ...);
	t$p.value
}

# general (including genotype model):
# 	test = function(formula, data)regressionCompareModels(f1 = formula, f0 = y ~ 1,
#		type = 'glm', family = binomial(), data = data),

powerLocusCaseControl = function(M = 1e2, N, af, mu, beta,
	scoresSim = scoresStd$add, scoresTest = scoresStd$add, level = .05,
	formula = y ~ gtScores,
	test = function(formula, data) {
		cfs = coefficients(summary(glm(formula, family = binomial(), data = data)));
		list(p.value =  if (length(row.names(cfs)) <= 1) NA else cfs['gtScores', 'Pr(>|z|)'])
	}, ...){

	ps = Sapply(1:M, function(i, N, af, mu, beta, scoresSim, scoresTest, test, formula) {
		d = simulateLocusCaseControl(N, af, mu, beta, scores = scoresSim);
		testLocusCaseControl(d, scores = scoresTest, formula = formula, test = test, ...)
	}, N, af, mu, beta, scoresSim, scoresTest, test, formula);
	power = sum(ps < level)/length(ps);
	power
}

simulateSampleMixup = function(d, ratio = .08, permuteColumns = which(names(d) == 'y'), .fuse = 5) {
	N = dim(d)[1];
	# number to permute + one sentinel
	Np = round(N * ratio) + 1;
	# fuse
	j = 1;
	repeat {
		if (j > .fuse) stop(sprintf('No mixup sample produced after %d rounds.', j - 1));
		# un-normalized probabilities, successor falls into remaining probability chunk
		psU = cumsum(runif(Np));
		# normalize, remove sentinel
		ps = (psU / psU[length(psU)])[-length(psU)];
		# samples to be permuted
		idcs = floor(ps * N);
		# avoid identical permutations through rounding
		repeat {
			id = c(-1, idcs) == c(idcs, N + 1);
			if (!any(id)) break;
			idcs = idcs + id[-length(id)];
			if (any(idcs > N)) break;
		}
		if (idcs[length(idcs)] <= N) break;
		j = j + 1;
	}
	# create permutation
	permutation = Sample(idcs, length(idcs));
	# apply permutation
	d[idcs, permuteColumns] = d[permutation, permuteColumns];
	r = list(data = d, idcs = idcs);
	r
}

snp.hom = function(snp)(snp != 1)
gtHomozygous = function(gt, test = snp.hom)all(sapply(gt, test))
gtsHomozygous = function(gts, test = snp.hom) apply(gts, 1, gtHomozygous, test = test)

sum.snp = function(gt){ sum(gt) }
summarizeGts = function(gts, countLoci, summarize, ...) {
	gts = t(apply(gts, 1, function(gt){
		apply(matrix(gt, ncol = 2, byrow = T), 1, summarize, ...)
	}));
	gts
}

# convert diplotypes to genotypes
# genotypes are vector of alleles, 2 alleles per genotype, countLoci times per individual
# summarize is applied to pairs of alleles, sum applies for SNPs
dts2gts = function(dts, countLoci, summarize = NULL, flat = F, ...) {
	if (is.matrix(dts) | is.data.frame(dts)) dts = as.vector(t(dts));
	gts = t(sapply(1:(length(dts)/2), function(d){
		g1 = ord2bin(dts[d * 2 - 1], digits = countLoci);
		g2 = ord2bin(dts[d * 2    ], digits = countLoci);
		as.vector(rbind(g1, g2));	# cog alleles
	}));
	if (!is.null(summarize)) gts = summarizeGts(gts, countLoci, summarize, ...);
	if (flat) gts = as.vector(gts);
	gts
}
dt2gt = function(dt, countLoci, summarize = sum.snp, flat = F, ...) {
	dts2gts(dt, countLoci, summarize, flat, ...)
}

countHets = function(g) {
	pos = which(apply(matrix(g, ncol = 2, byrow = T), 1, function(gt){ gt[1] != gt[2]}));
	r = list(positions = pos, count = length(pos));
	r
}

# compute the set of compatible dts for a single multi locus genotype given as vecotr of length 2*L
#	and return a vector of pairs of diplotypes
# <i> this implementation is a tiny bit sub-optimal as all positions of het are changed when one never changes
#	if parental origin is not to be heeded
# <!>multi-allelic version of gts2dtsSingle
# g is matrix of alleles with two rows
gts2dtsSingle = function(g, parentalOrigin = F, summary = bin2ord) {
	het = countHets(g);
	countDts = ifelse (parentalOrigin, 2^het$count,  ifelse(het$count <= 1, 1, 2^(het$count - 1)));

	gm = matrix(g, nrow = 2);	# matrix version of the gt (locus === row)
	dts = sapply(1:countDts, function(i) {
		dt.bin = ord2bin(i - 1, het$count);
		dt = gm;	# pre-initialize homocygous positions (and last heterozygous position)
		if (het$count > 0) dt[, het$positions] = rbind(
			mat.sel(gm[, het$positions, drop = F], 1 + dt.bin), mat.sel(gm[, het$positions, drop = F], 2 - dt.bin)
		);
		dt = if (!is.null(summary)) c(summary(dt[1, ]), summary(dt[2, ])) else t(dt);
	});
	as.vector(dts)
}

# the inverse of dts2gts
#	g: genotypes per row
gts2dts = function(g, ...) {
	dts = apply(g, 1, gts2dtsSingle, ...);
	# make sure dts is a list (when all dt explanations are of identical size)
	if (is.matrix(dts)) dts = unlist(apply(t(dts), 1, list), recursive = F);
	dts
}

# prepare collapsed genotypes for gts2dts (assuming biallelic SNPs)
gtT = list("0" = c(0, 0), "1" = c(0, 1), "2" = c(1, 1));
expandGtsSNPs = function(gts) as.vector(sapply(gts, function(gt)gtT[[C(gt)]]));

# count number of alleles '0' in genotypes
gtsByCount = function(gts, countLoci = NULL) {
	summarizeGts(gts, countLoci = countLoci, summarize = sum.snp)
}

# same parameters as simulateDiplotypes
simulateGenotypes = function(N, hfs, returnDiplotypes = F, summarize = sum.snp, ...) {
	countLoci = log2(length(hfs));
	# <p> dts
	dts = simulateDiplotypes(N = N, hfs);
	# <p> counts
	dtCts = table.n(dts, categories = 1:length(hfs) - 1, min = 0);
	#print(rbind(hfs, dtCts / sum(dtCts)), digits = 3);
	# <p> gts
	gts = dts2gts(dts, countLoci);
	if (!is.null(summarize)) gts = summarizeGts(gts, countLoci = countLoci, summarize = summarize, ...);
	r = list(gts = gts, dtfs = dtCts / sum(dtCts), dts = (if (returnDiplotypes) dts else NULL));
	r
}

# assume non-summarized gts
#	allele frequencies for alleles 0
afsForSNPs = function(gts, summarize = T) {
	if (summarize) gts = summarizeGts(gts, countLoci = countLoci, summarize = sum.snp);
	countLoci = dim(gts)[2];
	afs = sapply(1:countLoci, function(i) { afForSNP(table.n(2 - gts[, i], 2, 0)) });
	afs
}

#
# <p> phenotypes
#

# the associated allele is assumed to be allele 1, whereas the non-assicated alleles is 2
# <!> scores removed as global variable Fri Dec 17 15:17:25 2010
# <!> score coding reversed Fri Dec 17 15:17:25 2010
scoresL = scoresS = scoresStd = list(
	dom = c(0, 1, 1), add =  c(0, .5, 1), rec = c(0, 0, 1), gen = factor(0:2)
);
names(scoresL) = c("dominant", "additive", "recessive", 'genotype');
names(scoresS) = c("D", "A", "R", 'G');

modeFromScores = function(s, scores = scoresS) {
	s = round(s / max(as.numeric(s)), 3);	# normalize scores
	i = sapply(scores, function(m)all(m == s));
	mode = names(scores)[i];
	mode
}

# normalize the coefficient of genotype 0 to 0 and remove it
coefficientsForMode = function(mode, p) {
	ss = scores[[mode]] * p;
	ss[-1] - ss[1]
}

# probability of Y == 1, given penetrance parameters, data
# <!> change in genotype coding as of: Fri Dec 17 11:58:50 2010
#	genotypes are coded as 0, 1, 2
linearPredictorGt = function(beta, gt, scores, mu = -2)(mu + beta * scores[gt + 1]);
penetranceLogistic = function(gt, ...)(1 / (1 + exp(-linearPredictorGt(gt, ...))));
penetranceLogistic.re = function(beta, re.sigma, gt, re, scores, mu = -2) {
	(1 / (1 + exp(- (mu + beta * scores[gt] + re.sigma * re))))
}

# marginal probability of phenotype == Y
#	af: allele frequency
#	a = c(\beta_0, \beta_1, \beta_2): alternative as specified on inidcator variables on genotypes
#	P(Y = 1) = \sum_x P(Y = 1 | X = x) P(X = x)
probPhenotype = function(beta, af, penetrance = penetranceLogistic, Y = 1, ...) {
	gtf = qg2gt(c(af, 0));		# reparametrized gentoype frequencies
	pa = sum(sapply(1:length(gtf), function(i)(gtf[i] * penetrance(beta, i, ...))));
	r = ifelse(Y == 1, pa, 1 - pa);
	r
}
# rescale scores to yield the same marginal probability as some reference scoring scheme
rescaledScores = function(beta, af, scores, scoresRef, penetrance = penetranceLogistic, Y = 1, ...) {
	pyR = probPhenotype(beta, af, penetrance = penetrance, scores = scoresRef, Y = Y, ...);
	o = optimize(function(t){
		py = probPhenotype(beta, af, penetrance = penetrance, scores = t * scores, Y = Y, ...);
		r = (pyR - py)^2;
	}, c(.1, 10));
	r = scores * o$minimum;
	r
}

phenotypesFromDiplotypes = function(dts, diseaseLocus = 2, penetranceFunction = penetranceLogistic, ...) {
	sapply(1:(length(dts)/2), function(d){
		g1 = ord2bin(dts[d * 2 - 1])[diseaseLocus];
		g2 = ord2bin(dts[d * 2    ])[diseaseLocus];
		p = penetranceFunction(sum(c(g1, g2)), ...);
		rbinom(1, 1, p)
	})
}

simulatePropensitiesFromGenotypes = function(gts, predictor = linearPredictorGt, ...) {
	r = predictor(..., gt = gts);
	r
}

simulatePhenotypesFromGenotypes = function(gts, predictor = linearPredictorGt,
	penetranceFunction = logitI, ...) {
	p = penetranceFunction(predictor(..., gt = gts));
	ps = runif(length(gts));
	r = as.integer(ps < p);
	r
}

#
#	<p> hwe/association tests
#

normFs = normGtf = function(p) {
	if (length(p) < 3) p = c(p, max(1 - sum(p), 0));
	p / sum(p)
}

# estimate allele frequency from gts of allele 1 (homozygotes are gts[1])
afForSNP = afFromGts = function(gts, minor = F) {
	if (sum(gts) == 0) return(NA);
	af = (2*gts[1] + gts[2]) / (2 * sum(gts));
	if (minor && af > .5) af = 1 - af;
	af
}

# allele1: return af for the "second" allele (assuming alleles 0/1 coding)
#	<!> cave allele1 changed to T as of 18.6.2013
# @par minor: return minor allele frequency
afForGts = function(gts, allele1 = T, minor = F) {
	af = afForSNP(table.n(gts, 2, 0), minor = minor);
	ifelse(allele1, 1 - af, af)
}

# flip and inidicate whether there was a flip
gtsFlipToMinorInd = function(gts) {
	af = afForGts(gts);
	if (af > .5) gts = 2 - gts;
	list(gts = gts, didFlip = af > .5)
}
gtsFlipToMinor = function(gts)gtsFlipToMinorInd(gts)$gts;

triangle = function(n)(n*(n + 1)/2);

gtfsForGts = function(gts, countAlleles = 2) {
	table.n(gts, triangle(countAlleles) - 1, 0)/length(gts)
}

# HWE expectations for gt frequencies for allele frequency q
#af2hwe = function(q) { c(q^2, 2*q*(1-q), (1-q)^2 ) }
af2hwe = function(q) { c((1-q)^2, 2*q*(1-q), q^2 ) }


# general N-allele hwe test
# assume genotype coding 0, 1, ... (as given by cvtGts221)
hwe.test = function(gts, no.alleles = NULL, return.gtfs = F) {
	# <p> clean for missing data
	missing = fraction(is.na(gts));
	gts = gts[!is.na(gts)];
	# initialize various counts
	N = length(gts);
	gts2 = cvtGts122(gts, no.alleles = no.alleles)$genotypes;
	alleles = as.vector(gts2);
	lables = sort(unique(alleles));
	gtLables = 0:max(gts);
	Na = length(lables);

	# marginal frequencies
	afs = sapply(lables, function(l)fraction(alleles == l));
	# expected genotype counts
	egtc = sapply(1:length(lables), function(i) {
		sapply(1:length(lables), function(j) {
			ifelse(i <= j, choose(set.card(c(i, j)), 1) * N * afs[i] * afs[j], 0)
		})
	});
	# observed genotype counts
	ogtc = sapply(1:length(lables), function(i) {
		sapply(1:length(lables), function(j) {
			if (i > j) return(0);
			gt = pairsEnc(c(i - 1, j - 1), Na);
			count(gts == gt)
		})
	});
	# interpolate 0s
	egtcI = egtc + as.integer(egtc == 0);
	# test stat
	T = sum(as.vector( (ogtc - egtc)^2 / egtcI));
	dfs = pairsNo(Na) - length(afs);
	p = pchisq(T, df = dfs, lower.tail = F);

	gtfs = if (return.gtfs) sapply(gtLables, function(gt)fraction(gts == gt)) else NULL;

	r = list(p.value = p, T = T, dfs = dfs,
		afs = afs, gtc.exp = egtc, gtc.obs = ogtc, gtfs = gtfs, N = length(gts),
		missing = missing
	);
	r
}

# faster implementation for 2-allele system
# expects table of genotype counts
hwe.test.fast = function(gts) {
	# <p> compute test
	n = sum(gts);
	afH = ((2*gts[1] + gts[2]) / (2*n));
	# af2hwe based on non-reference allele frequency
	hwf = af2hwe(1 - afH);	# expected hardy-weinberg frequencies
	E = n * hwf;
	t = sum( (gts - E)^2 / E);
	# 	t0 = (gts[1] - n*afH^2)^2 / (n*afH^2) +
	# 		(gts[2] - n*2*afH*(1-afH))^2 / (n*2*afH*(1-afH)) +
	# 		(gts[3] - n*(1-afH)^2)^2 / (n*(1-afH)^2);
	p = 1 - pchisq(t, 1);
	list(p.value = p, T = t, af = afH, afs = c(afH, 1 - afH), Egt = hwf * n, hwf = hwf,
		gtfs = gts/n, gtcs = gts)
}

hwe.test.gts = function(gts) {
	# <p> clean for missing data
	missing = fraction(is.na(gts));
	gts = gts[!is.na(gts)];
	r = c(hwe.test.fast(table.n(gts, n = 2, min = 0)), list(missing = missing))
}

# gts is 2x3 table; groups by row; controls row 1
armitage.test = function(gts) {
	if (is.vector(gts)) gts = rbind(gts[1:3], gts[4:6]);
	# <p> summary statistics
	Ngs = apply(gts, 1, sum);	# counts for groups
	N1 = Ngs[1];
	N2 = Ngs[2];
	N = N1 + N2;
	Ngt = apply(gts, 2, sum);	# counts for genotypes

	# <p> test statistic [devlin, roeder 1999]
	t = N * (N*(gts[2, 2] + 2*gts[2, 3]) - N2*(Ngt[2] + 2*Ngt[3]))^2 /
		(N2 * (N - N2) * (N*(Ngt[2] + 4*Ngt[3]) - (Ngt[2] + 2*Ngt[3])^2 ));
	p = pchisq(t, 1, lower.tail = F);

	r = list(T = t, p.value = p);
	r
}
genotype.test = function(gts) {
	if (is.vector(gts)) gts = rbind(gts[1:3], gts[4:6]);
	# use robust chisq.test version that removes 0-marginals and bootstraps where needed
	chisq.test.robust(gts, bootstrapCellCount = 10);
}
# aggregation functions
is.monomorphic = function(snp, data) {
	r = sum(table(data[[snp]]) != 0) == 1;
	r
}
genotypeTableObserved = function(data, response, snp) {
	gts = t(sapply(sort(unique(data[[response]])), function(i) {
		table.n(data[[snp]][data[[response]] == i], min = 0, n = 2)
	}));
	gts
}

# assume table with 3 columns and strata per row
gtCnt2freq = function(d)t(apply(d, 1, function(r)r/sum(r)));

# gts is 2x3 table; groups by row; controls row 1
allelic.test = function(gts) {
	# <p> summary statistics
	Ngs = apply(gts, 1, sum);	# counts for groups
	N1 = Ngs[1];
	N2 = Ngs[2];
	N = N1 + N2;
	Ngt = apply(gts, 2, sum);	# counts for genotypes

	# <p> test statistic [devlin, roeder 1999]
	t = 2*N * (2*N*(gts[2, 2] + 2*gts[2, 3]) - 2*N2*(Ngt[2] + 2*Ngt[3]))^2 /
		(2*N2 * 2*(N - N2) * (2*N*(Ngt[2] + 4*Ngt[3]) - (Ngt[2] + 2*Ngt[3])^2 ));
	p = 1 - pchisq(t, 1);

	r = list(T = t, p.value = p);
	r
}

#
#	<p> population stratification
#

genomic.control.from.tests = function(ats) {
	r = list( mean = mean(ats), median = median(ats) / 0.456 );
	r
}
# assume data frame nx6, 3 control, 3 case columns
genomic.control = function(d) {
	N = dim(d)[1];	#1e3;
	at = unlist(lapply(1:N, function(i, d) {
		gts = matrix(unlist(d[i,]), nrow = 2, byrow = T);
		t = armitage.test(gts)$T;
		t
	}, d = d));
#print(unlist(at));
	genomic.control.from.tests(at)
}

#
# <p> family structure methods
#

pedigreesForNuclearFamilies = function(N, countOffspring = 1) {
	fs = 2 + countOffspring;	# family size
	ids = 1:(fs * N);
	idsP = cbind( (0:(N - 1))*fs + 1, (0:(N - 1))*fs + 2);	# parental ids as data.frame
	idsO = sapply(0:(N - 1), function(i) {
		idso = cbind(1:countOffspring + i*fs + 2,
			matrix(rep(idsP[i + 1,], countOffspring), byrow = T, ncol = 2));
		t(idso)
	});
	idsO = data.frame.types(matrix(as.vector(idsO), byrow = T, ncol = 3), names = c("id", "m", "p"));
	# construct data frame listing parents of each individual
	pedigree = rbind(
		data.frame(id = as.vector(t(idsP)),  m = rep(NA, 2*N), p = rep(NA, 2*N)),
		idsO
	);
	pedigree
}

# return trios each of which lead to one entry in the inheritance vector
# Algorithm:
#	- cfl (current found4er list) initialized with founders (m == p == NA)
#	- cnl (current non-founder list) initialized with non-founcer (m != NA & p != NA)
#	- iterate (until cfl empty)
#		- form trios (fts): non-founders (nfs) in cnl with both parents in cfl (spent founders; sfs)
#		- remove sfs from cfl, add fts-offspring to cfl
pedigreeInheritanceTrios = function(ped) {
	cfl = ifs = ped[is.na(ped$m) & is.na(ped$p), ];
	cnl = 		ped[!is.na(ped$m) & !is.na(ped$p), ];
	if (dim(cfl)[1] + dim(cnl)[1] < dim(ped)[1]) stop("Pedigree not well formed");
	its = NULL;	# inheritance trios each constituting one entry (bit) in the inheritance vector
	while (dim(cfl)[1] > 0 & dim(cnl)[1] > 0) {
		# current non-founders: which non-founders are being referenced
		cns = which(!is.na(which.indeces(cnl$m, cfl$id, ret.na = T)) &
					!is.na(which.indeces(cnl$p, cfl$id, ret.na = T)));
		if (length(cns) == 0) stop("Pedigree not well formed");
		# current founders: which founders reference non-founders
		cfs = union(cnl$m[cns], cnl$p[cns]);
		if (length(cfs) == 0) stop("Pedigree not well formed");
		# add inheritance trios to return list (order for convenience; by definition)
		its = rbind(its, cnl[order.df(cnl[cns, ], c("m", "p")), ]);
		cfl = rbind(cfl[- cfs, ], cnl[cns, ]);
		cnl = cnl[-cns, ];
	}
	r = list(its = its, ifs = ifs$id);	# inheritance trios, initial founders
	r
}

C = as.character;
U = unlist;
# split a pedigree data frame (collection of families) into a list of families
pedigreesSeparate = function(ped) {
	# <p> bring founders to the top
	ped = ped[order.df(ped, c("m", "p"), na.last = F), ];
	# <p> build clusters
	clusters = list();		# holds members of clusters
	n2c = list();	# maps members to clusters; name2cluster
	# <!> we can iterate the list, given it is ordered lexicographically
	for (i in 1:(dim(ped)[1])) {
		it = ped[i, ];			# inheritance trio
		# open new cluster for founders
		if (all(is.na(it[-1]))) {
			clusters[[C(i)]] = it;	# i is garuanteed to be a unique new cluster name
			n2c[[C(it$id)]] = C(i);
		} else {
			# join clusters if needed
			if (n2c[[C(it$m)]] != n2c[[C(it$p)]]) {
				clusters[[n2c[[C(it$m)]]]] = rbind(clusters[[n2c[[C(it$m)]]]], clusters[[n2c[[C(it$p)]]]]);
				clusters[[n2c[[C(it$p)]]]] = NULL;
				n2c[[C(it$p)]] = n2c[[C(it$m)]];
			}
			n2c[[C(it$id)]] = n2c[[C(it$m)]];
			clusters[[n2c[[C(it$id)]]]] = rbind(clusters[[n2c[[C(it$id)]]]], it);
		}
	}
	clusters
}

# mendProb:	mendelian probability of maternal inheritance
pedigreeSimulateDiplotypes = function(ped, N, hfs, mendProb = 0.5) {
	# <p> setup and founder diplotypes
	its = pedigreeInheritanceTrios(ped);
	Noff = dim(its$its)[1];
	Nfd = length(its$ifs);
	dtsF = matrix(simulateDiplotypes(Nfd, hfs), nrow = 2);				# founder diplotypes
	dtsM = listKeyValue(its$ifs, lapply(1:Nfd, function(i)dtsF[, i]));	# map ids to positions in dtsF
	iv = matrix(rbinom(Noff * 2, 1, mendProb), nrow = 2);				# inheritance vector
	dtsO = matrix(NA, nrow = 2, ncol = Noff);							# offspring diplotypes
	# <p> iterative construction of offspring diplotypes dt possible multiple generations
	for (i in 1:Noff) {
		dtsO[, i] = c(
			dtsM[[C(its$its$m[i])]][iv[1, i] + 1], dtsM[[C(its$its$p[i])]][iv[2, i] + 1]
		);
		dtsM[[C(its$its$id[i])]] = dtsO[, i];
	};
	listOfLists2data.frame(dtsM)
}

pedigreesSimulateDiplotypes = function(ped, N, hfs, ...) {
	peds = pedigreesSeparate(ped);
	dts = lapply(peds, function(p, N, hfs, ...) {
		lapply(1:N, function(i)pedigreeSimulateDiplotypes(p, N, hfs, ...))
	}, N = N, hfs = hfs, ...);
	list(peds = peds, dts = dts)
}

# genotypes are return per unique pedigree structure
pedigreesSimulateGenotypes = function(ped, N, hfs, ...) {
	Nl = log2(length(hfs));	# count loci
	dts = pedigreesSimulateDiplotypes(ped, N, hfs, ...);
	gts = lapply(dts$dts, function(dts) {
		dts = listOfDataFrames2data.frame(dts, idColumn = NULL);
		gts = data.frame(id = dts$id, dts2gts(dts[, -1], countLoci = Nl, summarize = sum.snp));
		gts
	});
	list(peds = dts$peds, gts = gts)
}

pedigreeDepthForId = function(id, ped) {
	scg = id;					# set of current generation ids
	i = 0;
	while (T) {
		idcs = union(which.indeces(scg, ped$m), which.indeces(scg, ped$p));
		if (length(idcs) == 0) return(i);
		scg = ped$id[idcs];
		i = i + 1;
	}
}
pedigreeCountOffspringForId = function(id, ped) {
	off = union(which.indeces(id, ped$m), which.indeces(id, ped$p));
	if (length(off) == 0) return(0);
	count = length(off) + sum(sapply(off, function(i)pedigreeCountOffspringForId(i, ped)));
	count
}
# coding: female == 1, male == 2
# <i><!> raise on incompatible sex
pedigreeSexForId = function(id, ped, default = 1) {
	if (length(which.indeces(id, ped$m)) > 0) return(1);
	if (length(which.indeces(id, ped$p)) > 0) return(2);
	return(default);
}

# Define the path length of a founder as the maximum generations descending from her
# the normal form of a pedigree is defined as follows
#	decreasing by path length, number offspring, increasing sex (m < f)
#		on ties: repeat comparison on ordered offspring (OO implementation)
#<!> there might be some possible reorderings that would have to be canonicalized

pedigreeNormalForm = function(ped) {
	#fds = ped[apply(ped, 1, function(pt) (all(is.na(pt[-1])))), ];
	#print(fds);
	prs = data.frame.types(sapply(ped$id, function(id)
		c(id, pedigreeDepthForId(id, ped), pedigreeCountOffspringForId(id, ped), pedigreeSexForId(id, ped))),
		do.transpose = T, names = c("id", "depth", "off", "sex")
	);
	o = order.df(prs, c("depth", "off", "sex"), decreasing = T);
	# rename ids according to canonical order
	mapping = list();
	mapping[C(ped$id[o])] = 1:dim(ped)[1];
	ped = ped[o, ];				# re-order pedigree
	ped$id = 1:dim(ped)[1];		# re-write ids
	ped$m = mapping[C(ped$m)];	# re-map maternal references
	ped$p = mapping[C(ped$p)];	# re-map maternal references
	#print(list(properties = prs[o, ], order = o, mapping = mapping));
	ped
}
pedigreeFingerPrint = function(ped) {
	pnf = pedigreeNormalForm(ped);
	fp = paste(as.character(t(as.matrix(pnf))), collapse = ":");
	fp
}

# check which paternal transmissions are compatible with offspring genotype gtsO
#	returns a vector of truth-values concerning the combinations
#	M-GM/P-GM, M-GP/P-GM, M-GM/P-GP, M-GP/P-GP (mother: grand-maternal/father: grand-paternal ...)
pedigreeCompatibleTransmissions = function(dtM, dtP, gtsO) {
	cl = length(gtsO);	# count loci
	# check all possible transmissions
	apply(merge.multi(1:2, 1:2), 1, function(t) {
		all(dt2gt(c(dtM[t[1]], dtP[t[2]]), cl) == gtsO)
	})
}

# extract DT for person i from a list of pairs of haplotypes
DT = function(dts, i)c(dts[(2 * i - 1):(2 * i)])
# index of mother for id-map idM and inheritance structure its for offspring i
IM = function(idM, its, i)idM[[C(its$its$m[i])]]
# index of father for id-map idM and inheritance structure its for offspring i
IP = function(idM, its, i)idM[[C(its$its$p[i])]]
# index of offspring for id-map idM and inheritance structure its for offspring i
IO = function(idM, its, i)idM[[C(its$its$id[i])]]
# diplotypes are given by column
#	ped: pedigree for a single family
#	dts: diplotypes of founders
#	gts: genotypes in the pedigree
pedigreeInheritanceVectorForDiplotypes = function(ped, dts, gts) {
	# <p> inheritance trios and setup
	its = pedigreeInheritanceTrios(ped);
	idM = listKeyValue(c(its$ifs, its$its$id), 1:length(gts$id));	# map ids to dt positions
	# arrange offspring gts
	gtsO = gts[which.indeces(its$its$id, gts$id), ];

	# <p> iterative construction of offspring diplotypes dt possible multiple generations
	iv = NULL;	# initialize inheritance vector
	for (i in 1:dim(its$its)[1]) {
		dtM = DT(dts, IM(idM, its, i));	# maternal diplotype
		dtP = DT(dts, IP(idM, its, i));	# paternal diplotype
		compat = pedigreeCompatibleTransmissions(dtM, dtP, gtsO[i, -1]);
		# founder diplotype incompatible with offspring genotypes
		if (all(!compat)) return(rep(NA, 2 * length(its$its$id)));
		# we need to record only one possible transmission (see technical report)
		ft = which(compat)[1];	# first transmission
		thts = ord2bin(ft - 1, digits = 2) + 1;	# transmitted haplotypes m/p
		dts = c(dts, dtM[thts[1]], dtP[thts[2]]);
		iv = c(iv, thts);
	};
	iv
}

# this is a naive implementation which interates all possible combinations of founder diplotypes
#	and checks for compatibility with offspring genotypes
pedigreeGts2Dts = function(ped, gts) {
	its = pedigreeInheritanceTrios(ped);
	idM = listKeyValue(gts$id, 1:length(gts$id));	# map ids to indeces
	# <p> compute inheritance vectors
	fdts = lapply(as.vector(unlist(idM[C(its$ifs)])), function(i) {
		gts = gts2dtsSingle(expandGtsSNPs(gts[i, -1]))
	});
	# founder diplotype combinations
	fdtCs = merge.multi.list(lapply(fdts, function(dts)1:(length(dts)/2)));
	# iterate dipolotype combinations
	ivs = t(apply(fdtCs, 1, function(dc) {
		dts = sapply(1:length(dc), function(i)fdts[[i]][(2 * dc[i] - 1):(2 * dc[i])]);
		iv = pedigreeInheritanceVectorForDiplotypes(ped, dts, gts);
		c(dts, iv)
	}));
	ivs = ivs[apply(ivs, 1, function(r)!any(is.na(r))), ];
	# <p> compute homyzgosity vector
	hv = gtsHomozygous(gts[which.indeces(c(its$ifs, its$its$id), gts$id), -1]);
	# <p> equalness of genotypes in inheritance trios (equal inheritance trios)
	eit = sapply(1:length(its$its$id), function(i) {
		all(gts[IO(idM, its, i), -1] == gts[idM[[C(its$its$m[i])]], -1]
		  &	gts[IO(idM, its, i), -1] == gts[idM[[C(its$its$p[i])]], -1])
	});

	list(its = its, ivs = ivs, hv = hv, eit = eit)
}

# gts:	assume a structure as returned by pedigreesSimulateGenotypes
pedigreesGts2Dts = function(gts) {
	dts = lapply(1:length(gts$peds), function(i) {
		Ni = dim(gts$peds[[i]])[1];	# number of individuals in this pedigree structure
		dts = lapply(1:(dim(gts$gts[[i]])[1] / Ni), function(j) {
			pedigreeGts2Dts(gts$peds[[i]], gts$gts[[i]][((j - 1) * Ni + 1):(j * Ni), ])
		});
		dts
	});
	dts
}

# log-likelihood no pedigrees
#	takes a diplotype reconstruction as emitted by pedigreeGts2Dts
# (1) lhFd: product of diplotype frequncies for founders (ordered)
# (2) lhFc: constant to account of unoreded nature of founder diplotypes (*)
# (2) lhMe: Mendelian probabilies for offspring and order-counts (*)
# (3) lhPe: the likelihood is the product of penetrance functions for all members
# (*) terms do not depent on particular diplotype explanation
# we only consider a single order and correct by a factor derived from dr$hv and dr$eit
# <N> we ignore parent-of-origin effects
pedigreeLL = function(hfs, dr, penetrance = NULL, ...) {
	hfs = hfs / sum(hfs);
	Nf = length(dr$its$ifs);	# number of founders
	Noff = dim(dr$its$its)[1];	# number offspring
	ids = c(dr$its$ifs, dr$its$its$id);
	idM = listKeyValue(ids, 1:length(ids));	# map ids to indeces

	lhFc = prod(dr$hv[1:Nf] + 1);	# number of diplotype configurations based on possible orderings
	lhMe = prod(sapply(1:Noff, function(i) {
		it = dr$its$its[i, ];		# inheritance trio
		pIds = as.vector(it[, -1]);	# parental ids
		# direction of transmission (genotypes in eit equal and heterocygous) / number of possibilities
		(2 - (dr$hv[idM[[C(it$id)]]] & dr$eit[i])) / prod(dr$hv[U(idM[C(pIds)])] + 1);
	}));
	# sum over all diplotype explanations for the family
	lh = sum(apply(MR(dr$ivs), 1, function(iv) {
		lhFd = prod(sapply(iv[1:(2*Nf)], function(ht)hfs[ht + 1]));
		lhPe = if (!is.null(penetrance)) 1 else 1;	# <i>
		lh = lhFd * lhFc * lhMe * lhPe;
		lh
	}));

	ll = log(lh);
	ll
}


#
#	<§> genotype serialization/deserialization encoding/decoding
#

# methods to enumerate pairs of alleles into genotype numbers and the other way around

# # of pairs 0 <= i <= j < N
pairsNo = function(N)(N * (N + 1)/2);
# # of elements N: 0 <= i <= j < N leading to M such pairs
pairsNoInv = function(M)ceiling(-.5 + sqrt(.25 + 2*M));
# # of pairs for 0 <= x <= j < N for x fixed
pairsBase = function(x, N)(pairsNo(N) - pairsNo(N -  x));
# assume 0 <= x[1] <= x[2] < N
pairsEnc = function(x, N)(pairsBase(x[1], N) + (x[2] - x[1]));
# smaller allele from gt encoding (postive solution from the quadratic equation)
pairsBaseInv = function(x, N)as.integer(((2*N + 1)/2 - sqrt((2*N + 1)^2/4 - 2*x)));
#pairsBaseInv = function(x, N)as.integer(N + sqrt(2*x - 2*N - N^2));
# decode gt to c(x[1], x[2]), with 0 <= x[1] <= x[2] < N	<!> werkt niet
pairsDec = function(gt, N) {
	if (is.na(gt)) return(c(NA, NA));
	z1 = pairsBaseInv(gt, N);
	r = c(z1, gt - pairsBase(z1, N) + z1);
	r
}

# <!> the cvt* functions map single SNPs
# map a two column genotype format to a one colomn genotype coding
cvtGts221 = function(d, cols = NULL, no.alleles = NULL, missing = '0') {
	d[d == missing] = NA;
	if (!is.null(cols)) d = d[, cols];
	unique.alleles = unique(unlist(as.vector(apply(d, 2, function(c)levels(as.factor(c))))));
	alleles = sort(unique.alleles);
	if (is.null(no.alleles)) no.alleles = length(alleles);
	gts = apply(d, 1, function(r){
		if (any(is.na(r))) return(NA);
		aNo = sort(which.indeces(r, alleles) - 1);	# normalized allele numbers
		gt = pairsEnc(aNo, no.alleles);
		#print(c(aNo, gt, pairsDec(gt, no.alleles)));
		gt
	});

	r = list(no.alleles = no.alleles, alleles = alleles, genotypes = gts);
	r
}

# map serealized genotype to ordered pair of alleles
# map is a vector of allele names
cvtGts122 = function(gts, no.alleles = NULL, alleles = NULL) {
	if (is.null(no.alleles)) no.alleles = pairsNoInv(max(gts, na.rm = T) + 1);
	if (is.null(alleles)) alleles = 0:(no.alleles - 1);
	pairs = t(sapply(gts, function(gt){
		if (is.na(gt)) return(c(NA, NA));
		pr = pairsDec(gt, no.alleles);
		as = alleles[pr + 1];	# alleles
		as
	}));

	r = list(no.alleles = no.alleles, alleles = alleles, genotypes = pairs);
	r
}

# convert whole data frame using cvtGts122
gts122 = cvtMarkers122 = function(d, cols = NULL, no.alleles = NULL, alleles = NULL) {
	if (is.null(cols)) cols = 1:dim(d)[2] else
	if (!is.numeric(cols)) cols = which.indeces(cols, names(d));

	# <p> gts2 are concatenated vectors formed from columns of gts
	gts2 = sapply(cols, function(i) {
		gts = cvtGts122(d[, i], no.alleles, alleles)$genotypes;
		gts
	});
	# <p> gts2 gets reshaped: is.vector proceeds by column as does matrix
	gts2 = data.frame(matrix(as.vector(gts2), nrow = dim(d)[1]));
	names(gts2) = as.vector(t(matrix(
		c(sprintf('%s_%d', names(d)[cols], 1), sprintf('%s_%d', names(d)[cols], 2))
	, ncol = 2)));
	gts2
}

# <!> bi-allelic markers
gts_recode = function(gt, reference) {
	if (is.na(reference) || gt$alleles[1] == reference) return(gt);
	Log(sprintf("Recoding snp %s to ref allele %s [Var: %s]", gt$name, gt$alleles[1], gt$alleles[2]), 5);
	gt$alleles = rev(gt$alleles);
	gt$genotypes = 2 - gt$genotypes;
	gt
}

gts_normalize = function(gts, referenceAlleles = list()) {
	gts = lapply(gts, function(gt) {
		hwe = hwe.test.gts(gt$genotypes);
		reference = firstDef(
			referenceAlleles[[gt$name]],
			ifelse(hwe$afs[1] < 0.5, gt$alleles[2], gt$alleles[1])
		);
		r = gts_recode(gt, reference);
		r = c(r, list(hwe = hwe.test.gts(r$genotypes)));
		r
	});
	gts
}

# recodes a single vector of genotypes assumed to be coded as AB where A and B are letters encoding
#	alleles
# return value: list with components
#	alleles: allele, the number of which is counted, other allele
#	gts: 0,1,2 coded genotypes
genotypesCharacterRecode = function(gts, na.values = c("", "null"), refGt = NULL, byMinor = T,
	splitBy = "") {
	# <p> transform to character vector
	gts = as.factor(as.character(gts));
	if (!length(levels(gts))) return(list(alleles = NA, gts = rep(NA, length(gts)), af = NA));
	# <p> detects nulls in dataset
	nulls = which.indeces(na.values, levels(gts));
	ls = if (length(nulls) > 0) levels(gts)[-nulls] else levels(gts);
	# <p> determine alleles
	alleles = sort(unique(unlist(sapply(ls, function(l)strsplit(l, splitBy)))));
	if (length(alleles) > 2) {
		warning("More than two alleles found. Only recodeing first two by alphabetical order.");
	}
	a1 = alleles[1];
	a2 = alleles[2];
	map = 0:2;
	# remap genotypes
	if (!is.null(refGt)) {
		if (refGt == sprintf("%s%s", a2, a2)) map = 2:0;
		if (refGt == sprintf("%s%s", a2, a1) | refGt == sprintf("%s%s", a1, a2)) map = c(1, 0, 2);
	}

	gtsn = rep(NA, length(gts));
	gtsn[gts == a1 | gts == sprintf("%s%s", a1, a1)] = map[1];
	gtsn[gts == sprintf("%s%s", a2, a1) | gts == sprintf("%s%s", a1, a2)] = map[2];
	gtsn[gts == a2 | gts == sprintf("%s%s", a2, a2)] = map[3];
	if (byMinor & is.null(refGt)) {
		if (afForGts(gtsn) > 0.5) {
			gtsn = 2 - gtsn;
			alleles = rev(alleles);
		}
	}
	r = list(alleles = alleles, gts = gtsn, af = afForGts(gtsn));
	r
}
genotypesFactorRecode = function(gts, na.values = c("", "null")) {
	# <p> reset missing
	gts = as.vector(gts);
	gts[gts %in% na.values] = NA;
	# <p> genotype levels
	gtsF = as.factor(gts);
	if (!length(levels(gtsF))) {
		warning("Only missing genotypes.");
		return(list(genotypes = NULL, gts = gts));
	}
	# raw map (maps NAs to NULLs)
	gtsM = listKeyValue(c(levels(gtsF)), c(0:(length(levels(gtsF)) - 1)));
	gts = gtsM[gtsF];
	gts[sapply(gts, is.null)] = NA;
	gts = unlist(gts, use.names = F);
	r = list(genotypes = levels(gtsF), gts = gts, af = afForGts(gts));
	r
}

snpNamesForDataFrame = function(d0, regex = "rs\\d+") {
	ns = names(d0);
	markers = ns[which.indeces(regex, ns, regex = T, ignore.case = T)];
	markers
}

# read data in one-column format
# 
readGenotypes1 = function(d0, markers = NULL, .as.factor = F, recoder = genotypesFactorRecode,
	removeMissingAt = 1, ...) {
	if (is.null(markers)) markers = names(d0);

	# <p> recode
	ld1 = nlapply(markers, function(m)recoder(d0[, m], ...));

	# <p> missing rate
	missing = nlapply(markers, function(m)fraction(is.na(ld1[[m]]$gts)));

	# <p> remove missing
	markers = markers[unlist(missing) < removeMissingAt];

	# <p> build genotype data
	dGts = data.frame.types(sapply(ld1[markers], function(e) {
		if (.as.factor) as.factor(e$gts) else e$gts;
	}));

	# <p> enumerate alleles / genotypes
	desc = data.frame.types(sapply(markers, function(m) {
		list(marker = m, alleles = paste(ld1[[m]]$alleles, collapse = " "),
			genotypes = paste(ld1[[m]]$genotypes, collapse = " "),
			af = ifelse(is.null(ld1[[m]]$af), NA, ld1[[m]]$af),
			missing = missing[[m]]
		)
	}), do.transpose = T);
	# <b> case distinction due to R type maneuvering
	d1 = if (length(markers) < dim(d0)[2]) cbind(.dfm(d0, markers), dGts) else dGts;
	r = list(data = d1, desc = desc);
	r
}

# read data in two-column format
extractGenotypes2 = function(d0, snps = NULL, missing = '0', snps_re = 'rs.*') {
	ns = names(d0);
	if (is.null(snps)) {
		snps = unique(sapply(sapply(
			ns[which.indeces(snps_re, ns, regex = T, ignore.case = T)],
				function(s)fetchRegexpr('rs\\d+', s, ignore.case = T)[1]
		), tolower));
	}
	idcs = if (!length(snps)) {
		matrix(1:dim(d0)[2], ncol = 2, byrow = T)
	} else {
		t(sapply(snps, function(s) {
			i = which.indeces(sprintf("%s_", s), names(d0), regex = T, ignore.case = T);
			if (length(i) != 2) stop(sprintf("SNP not in two column format in data frame [%s]: %s",
				s, paste(names(d0)[i], collapse = "; ")));
			i
		}))
	};
	dSnps = sapply(1:dim(idcs)[1], function(i) {
		# columns for SNP
		gts = cvtGts221(d0, idcs[i, ], missing = missing)$genotypes;
		gts
	});
	dSnps = data.frame.types(dSnps, do.transpose = F, names = snps);
	dSnps
}

# <N>: currently best method for SNP reading (2012/11);
#	autodetect genotype format: integer-based, string-based "A1 A2", string-based two columns
#	<!> regex applied on tolower
#' @param snps_re_prefix regular expression to detect two-column format
#' @gtRe regular expression to extract alleles from genotype character string, e.g. '([ATCG])/([ATCG])'
extractGenotypes = function(d0, snps = NULL, missing = '',
	snps_re = '^rs\\d+', snps_re_name = snps_re, snps_re_prefix = '^%s_', gtRe = NULL, gtSplit = '\\s*') {
	# define SNPs
	ns = names(d0);
	if (is.null(snps))
		snps = unique(sapply(ns[which.indeces(snps_re, ns, regex = T, ignore.case = T)],
			function(s)fetchRegexpr(snps_re_name, s, ignore.case = T)[1]
		));
	# one column format
	snps1 = sapply(snps, function(s)length(which(s == ns)) == 1);
	# two column format
	snps2 = sapply(snps, function(s)
		length(which.indeces(sprintf(snps_re_prefix, s), ns, regex = T, ignore.case = T)) == 2
	);
	# unspecified
	if (length(snps[!snps1 & !snps2]) > 0)
		stop(sprintf("Format for SNPs %s undetected.", paste(snps[!snps1 | !snps2], collapse = ", ")));
		
	# create 0:2 coded genotypes from SNPs encoded in a single column
	gts1 = lapply(snps[snps1], function(s) {
		gts = d0[[s]];
		r = if (all(is.integer(gts))) {
				# numerically coded genotype
				if (!is.null(missing)) gts[gts == missing] = NA;
				list(alleles = c('A1', 'A2'), genotypes = gts)
			} else {
				alleles = t(sapply(as.character(gts), function(gt){
					raw = if (is.null(gtRe)) {
						if (is.na(gt) || gt == missing || gt == '') return(c(NA, NA));
						raw = splitString(gtSplit, gt);
						raw[raw != ''];
					} else {
						unlist(fetchRegexpr(gtRe, gt, capturesAll = T))
					}
					if (all(length(raw) != c(0, 2))) stop(sprintf('Bad genotype: %s', gt));
					if (!length(raw)) c(NA, NA) else raw
			}));
			cvtGts221(alleles, missing = missing);
		}
		r = c(r, list(name = s));
		r
	});

	# create 0:2 coded genotypes from SNPs encoded in two columns
	gts2 = lapply(snps[snps2], function(s) {
		i = which.indeces(sprintf(snps_re_prefix, s), ns, regex = T);
		r = cvtGts221(d0, i, missing = missing);
		r = c(r, list(name = s));
		r
	});

	#	normalize genotypes, create descriptor
	gts = gts_normalize(c(gts1, gts2));
	desc = lapply(gts, function(g).List(g, min_ = 'genotypes'));
	names(desc) = list.kp(gts, 'name', do.unlist = T);
	dSnp = data.frame.types(
		list.kp(gts, 'genotypes'),
		names = list.kp(gts, 'name', do.unlist = T),
		do.unlist = T
	);

	#	determine monomorphic snps
	monomorphic_snps = names(desc)[(1 - list.kp(desc, 'hwe$af', do.unlist = T)) < 1e-5];

	# return value
	r = list(genotypes = dSnp, desc = desc, monomorphic_snps = monomorphic_snps,
		indeces = c(
			which.indeces(snps[snps1], ns),
			which.indeces(sprintf(snps_re_prefix, snps[snps2]), ns, regex = T, match.multi = T))
	);
	r
}

genotypesDescriptives = function(gts) {
	snps = names(gts$desc);
	ns = c('name', 'alleles', 'Maf', 'Miss', 'HWE');
	rows = lapply(snps, function(n) with(gts$desc[[n]], {
		Df_(list(n, join(alleles, ":"), 1 - hwe$af, hwe$missing, hwe$p.value), names = ns)
	}));
	r = do.call(rbind, rows);
	r
}

flipGtDesc = function(desc) {
	desc$alleles = rev(desc$alleles);
	desc$Egt = rev(desc$Egt);
	desc$gtfs = rev(desc$gtfs);
	desc$afs = rev(desc$afs);
	desc$gtcs = rev(desc$gtcs);
	desc$hwf = rev(desc$hwf);
	desc$af = 1 - desc$af;
	desc
}

genotypesFlipGtMatrix = function(genotypes, flipMask) {
	genotypes = Df_(sapply(1:ncol(genotypes), function(i) {
		if (flipMask[i]) (2 - genotypes[, i]) else genotypes[, i]
	}), names = names(genotypes));
	genotypes
}

genotypesFlip = function(gts, flipMask) {
	gts$genotypes = genotypesFlipGtMatrix(gts$genotypes, flipMask);

	names(gts$genotypes) = names(gts$desc);
	gts$desc = lapply(1:length(gts$desc),
		function(i) if (flipMask[i]) flipGtDesc(gts$desc[[i]]) else gts$desc[[i]]
	);
	gts
}

snpEffects = function(r, snpNames) {
	sapply(1:length(r), function(i)r[[i]]$r$effects1[[snpNames[i]]])
}
flipMaskFromModel = function(data, ns, model, modelGt = 'additive') {
	r = with(model, analyzeSnps(data, snps = ns, f1, f0, stat, gtScores = scoresL[[modelGt]]));
	flipMask = snpEffects(r, ns) < 0;
}

flipGenotypesFromModel = function(data, gts, model, modelGt = 'additive') {
	ns = names(gts$desc);
	# <p> determine flipMask
	flipMask = flipMaskFromModel(data, ns, model, modelGt);

	# <p> flip and return
	gts = genotypesFlip(gts, flipMask);
	data[, which.indeces(ns, names(data))] = gts$genotypes;
	r = list(data = data, gts = gts);
	r
}
# @param: ns snp names
flipGenotypesDataFromModel = function(data, ns, model, modelGt = 'additive') {
	flipMask = flipMaskFromModel(data, ns, model, modelGt);
	data[, which.indeces(ns, names(data))] = genotypesFlipGtMatrix(data[, ns], flipMask);
	data
}

#
#	<p> stratification analysis
#

# sensitivity
#  for which inflation factor would beta still be significant
sensitivityP = function(P, alpha = 0.05) {
	lambda = Vectorize(inverse(function(lambda)
		pchisq(qchisq(P, 1, lower.tail = F) / lambda, 1, lower.tail = F)
	, interval = c(1, 10)))(alpha);
	lambda
}

#
#	<p> correct effects
#


# correct a estimate based on p-value and correction factor
#	we assume positive beta
#	the following definition of lambda is used:
#	medianChisq = 0.4550757;	# median(rchisq(1e7, df = 1))
#	lambda = (median(sqrt(chisqs), na.rm = T) / sqrt(medianChisq))^2;
correctBeta = function(P, sigma, lambda = 1) {
	# correct p-value based on chisq-scores
	Pst = pchisq(qchisq(P, 1, lower.tail = F) / lambda, 1, lower.tail = F);
	# corrected normal score for beta; beta star
	Bst = max(0, qnorm(Pst, 0, sigma, lower.tail = F));
	Bst
}
# produce closure to apply a given IF
inflationCorrector = function(lambda) {
	rf = function(P) pchisq(qchisq(P, 1, lower.tail = F) / lambda, 1, lower.tail = F);
	rf
}

# snps:	SNP names in terms of rs system
# r: results (processGWAS)
correctSNPset = function(snps, df, lambda = 1, map = NULL) {
	# find snps in map or p
	idcs = if (is.null(map)) which.indeces(snps, r$snp, ret.na = T) else findSnps(map, snps);
	# overlap 
	snpsO = snps[!is.na(idcs)];
	idcs = idcs[!is.na(idcs)];
	# map back rs-names
	if (!is.null(map)) {
		idProp = snpsRs2prop(map, snpsO);
		idcs = which.indeces(idProp, df$snp);
	}
	r = sapply(1:length(idcs), function(i) {
		d = df[idcs[i], ];
		# <p> fit additive model
		# regenerate data frame (wheights do not seem to work)
		gdf = data.frame(y = as.numeric(c(rep(0, sum(d[2:4])), rep(1, sum(d[5:7])))),
			x = as.numeric(c(
				rep(0, d[2]), rep(.5, d[3]), rep(1, d[4]),
				rep(0, d[5]), rep(.5, d[6]), rep(1, d[7])
		)));
		gf0 = glm(y ~ 1, family = "binomial", data = gdf);
		gf1 = glm(y ~ x, family = "binomial", data = gdf);
		# get parameters + stddev
		cs = as.numeric(summary(gf1)$coefficients[2, ]);	# row 1 is intercept
		# nominal p-value
		p = anova(gf0, gf1, test = "Chisq")[2, "P(>|Chi|)"];
		p1 = 2 * pnorm(0, abs(cs[1]), cs[2]);	# p-value based on confidence interval
		Bst = sign(cs[1]) * correctBeta(p1, cs[2], lambda);
		Pst = 2 * pnorm(0, abs(Bst), cs[2]);	# p-value based on confidence interval
		r = list(snpsO[i], p, p1, Pst, cs[2], cs[1], Bst);
		r
	});
	r = data.frame.types(r, do.transpose = T,
		names = c("snp", "p-value", "p-value CI", "p*", "sdev", "beta", "beta*"));
	r
}

#
#	<p> regression and global testing
#

snpCallFormula2vars = function(func, formula, data, snpRegex = 'rs.*', snps = NULL,
	covariates = NULL, type = 'glm', ...) {
	# identify variables
	varResp = formula.response(formula);
	varCovs = c(formula.covariates(formula), covariates);
	snps = if (is.null(snps)) {
		if (length(varCovs)) varCovs[which.indeces(snpRegex, varCovs, regex = T)] else 
			names(data)[which.indeces(snpRegex, names(data), regex = T)];
	}
	varCovs = setdiff(varCovs, snps);

	func(data, snps, varResp, varCovs, type = type, ...)
}

# <!> homogeneous mode of inheritance
snpRegression = function(data, snps = NULL, response = 'y', covariates = NULL, type = 'glm',
	gtScores = scoresStd$add, ..., automaticFamily = T) {
	# recode SNPs to scores	; <t><!> only tested for a single SNP; assume 0,1,2 coding
	for (snp in snps) {
		data[[snp]] = gtScores[as.integer(data[[snp]]) + (if (is.factor(data[[snp]])) 0 else 1)];
	}

	f0 = as.formula(sprintf('%s ~ %s 1', response,
		ifelse(is.null(covariates), '', paste(covariates, collapse = ' + '))));
	f1 = as.formula(sprintf('%s ~ %s %s + 1', response,
		ifelse(is.null(snps), '1', paste(snps, collapse = ' + ')),
		ifelse(is.null(covariates), '', paste(covariates, collapse = ' + '))));

	p = if (automaticFamily & response.is.binary(data[[response]])) {
		regressionCompareModels(f1, f0, type = type, data = data, family = binomial(), ...);
	} else {
		regressionCompareModels(f1, f0, type = type, data = data, ...);
	}
	gc();
	p
}
# first row controls, 2x3 table
snpRegressionTable = function(data, snps = NULL, type = 'glm', gtScores = scoresStd$add, ...) {
	if (is.vector(data)) data = rbind(data[1:3], data[4:6]);
	d = data.frame(merge.multi.list(list(x = gtScores, y = 0:1)), weights = as.vector(data));
	p = regressionCompareModels('y ~ x + 1', 'y ~ 1', type = type, data = d,
		weights = d$weights, family = binomial(), ...);
	p
}


# assume genetic effects to be tested, adjusted for covariates, i.e. extract SNP from formula
snpRegressionFormula = function(f, data, snpRegex = 'rs.*', snps = NULL,  covariates = NULL,
	type = 'glm', ...) {
	r = snpCallFormula2vars(snpRegression, f, data, snpRegex, snps, covariates, type, ...);
	r
}

# test SNPs without covariates, assume binary phenotype
snpRegressionPlain = function(data, snps = NULL, response = 'y', covariates = NULL, type = 'glm',
	gtScores = 1 - scores$add, ...) {
	gts = genotypeTable(data, response, snps);
	r = if (is.factor(gtScores)) {
		genotype.test(gts);
	} else {
		armitage.test(gts);
	};
	r
}
# test SNPs without covariates, assume binary phenotype
snpRegressionPlainTable = function(gts, snps = NULL, response = 'y', covariates = NULL, type = 'glm',
	gtScores = 1 - scores$add, ...) {
	r = if (is.factor(gtScores)) {
		genotype.test(gts);
	} else {
		armitage.test(gts);
	};
print(r);
	r
}
# assume genetic effects to be tested, adjusted for covariates, i.e. extract SNP from formula
snpRegressionPlainFormula = function(f, data, snpRegex = 'rs.*', snps = NULL,  covariates = NULL,
	type = 'glm', ...) {
	r = snpCallFormula2vars(snpRegression, f, data, snpRegex, snps,  covariates, type, ...);
	r
}

# assume SNPs to be tested one by one
snpRegressionGlobal = function(data, snps, response, covariates, type = "glm", ...) {
	as = data.frame.types(sapply(snps, function(s, ...) {
		p = snpRegression(data, snps = s, response, covariates, type = type,  ...)$p.value;
		list(s, p)
	}), do.transpose = T, names = c("snp", "p.value"));
	as
}

snpRegressionGlobalFormula = function(f, data, snpRegex = "rs.*", snps = NULL,
	gtScores = scores$add, type = "glm", ...) {
	r = snpCallFormula2vars(snpRegressionGlobal, f, data, snpRegex, snps, type, ...);
	r
}

# permute covariates in order to obtain empirical p-values
# M: number of permutations
# assume SNPs to be tested one by one
snpRegressionGlobalEmp = function(data, snps, response, covariates = NULL,
	type = "glm", M = 5e3, ..., aggregator = min, Pmap = function(e)e, tester = snpRegressionGlobal, 
	.clRunLocal = F) {
	# <p> optimize data set
	data = data[!is.na(data[[response]]), ];
	mono = sapply(snps, is.monomorphic, data = data);
	snps = snps[!mono];
	# <p> test data set
	p = tester(data, snps, response, covariates, ...)$p.value;
	p.data = aggregator(Pmap(p));

	# <p> perform permutation
	f = function(i, data, snps, response, covariates, type, Pmap, ...) {
		d1 = data;
		d1[[response]] = d1[[response]][sample(dim(data)[1])];
		p = tester(d1, snps, response, covariates, type, ...)$p.value;
		p = p[p >= 0 & p <= 1];	# <!> remove faulty p-values
		p.agg = aggregator(Pmap(p));
		p.agg
	};
	ps = unlist(clapply(1:M, f, data = data, snps = snps,
		response = response, covariates = covariates, type = type, Pmap = Pmap, ...,
		.clRunLocal = .clRunLocal));
	#ps = unlist(clapply(1:M, f, data = data, snps = snps,
	#	response = response, covariates = covariates, type = type, Pmap = Pmap, ...));
	p.emp = count(ps[order(ps)] < p.data) / M;
	r = list(response = response, p.value = p.emp, p.data = p.data, ps.data = p, ps = ps);
	r
}
snpRegressionGlobalEmpFormula = function(f, data, snpRegex = "rs.*", snps = NULL,
	gtScores = scores$add, type = "glm", ..., M = 5e3, aggregator = min, Pmap = function(e)e) {
	r = snpCallFormula2vars(snpRegressionGlobalEmp, f, data, snpRegex, snps, type, ...,
		aggregator = aggregator, Pmap = Pmap);
	r
}

tailStrength = function(ps) {
	N = length(ps);
	r = -1/N * (N - sum(sort(ps) * (N + 1)/(1:N)));	# return negative as small values are deemed relevant
	r
}

testPairwiseInteractionsGlobalEmp = function(data, response, covariates, nuisanceCovariates = c("1"),
	type = "lm", M = 1e3, ...) {
	r = testPairwiseInteractionsEmp(data, response, covariates, nuisanceCovariates,
		type, M, ..., aggregator = tailStrength);
	r
}

#
#	<p> meta methods for testing genetic association
#

namesSnpImp = function(snp)paste(c(snp, snp), c("i1", "i2"), sep = "_");

isSnpImputed = function(d, snp) {
	is = as.vector(sapply(snp, function(s) {
		length(which.indeces(namesSnpImp(s), names(d))) == 2
	}));
	is
}

# determine imputation status and test accordingly
#	imputation status is determined by two snp_i1, snp_i2 solumns
associationTestSnpsMarginally = function(data, snps, response, covariates = NULL, type = 'glm', ...,
	snpRegressor = snpRegression) {
	impStatus = isSnpImputed(data, snps);
	r = lapply(1:length(snps), function(i){
		if (!impStatus[i])
			snpRegressor(data, snps[i], response, covariates, type = type, ...) else
			testSNPimputed(data, snps[i], response, covariates, type = type, ...)
	});
	names(r) = snps;

	r
}

associationTesterMarginally = function(data, snps, response, covariates = NULL, type = 'glm', ...,
	snpRegressor = snpRegression) {
	r0 = associationTestSnpsMarginally(data, snps, response, covariates, type, ...,
		snpRegressor = snpRegressor);
	r1 = data.frame(snp = names(r0), p.value = list.key(r0, 'p.value'));
	r1
}

# <!> assume no missing data
genotypeTableImputed = function(data, response, snp) {
	pts = sort(unique(data[[response]]));
	gts = t(sapply(pts, function(i) {
		d0 = data[data[[response]] == i, ];
		gts = sapply(namesSnpImp(snp), function(s)sum(d0[[s]]));
		gts = c(gts, dim(d0)[1] - sum(gts));	# <!> assume no missing data
		names(gts) = 0:2;
		gts
	}));
	row.names(gts) = pts;
	gts
}

genotypeTable = function(data, response, snps) {
	impStatus = isSnpImputed(data, snps);
	r = lapply(1:length(snps), function(i){
		if (!impStatus[i])
			genotypeTableObserved(data, response, snps[i]) else
			genotypeTableImputed(data, response, snps[i]);
	});
	names(r) = snps;

	r
}

#
#	<p> haplotype functions
#

#' Group haplotypes by frequency cut-off
#'
#' @param l is a list with haplotype names and frequencies. Assume names to be coercible to integers.
haplotypeFormGroups = function(l, cutoff = .01) {
	new = max(as.integer(names(l))) + 1;
	group = l[l < cutoff];
	self = l[l >= cutoff];
	r = listKeyValue(names(self), as.integer(names(self)));
	if (length(group) > 0) r = c(r, listKeyValue(names(group), rep(new, length(group))));
	r
}

#' Produce grouping list for haploytpes by keeping named haplotypes
#'
#' @param hts character vector of haplotypes
#' @param keep character vector of haplotypes to keep
haplotypeGroups = function(hts, keep, nameTemplate = '%{number}d') {
	new = Sprintf(nameTemplate, list(number = as.integer(max(as.integer(hts)) + 1)));
	r = listKeyValue(keep, which.indeces(keep, hts));
	group = setdiff(hts, keep);
	r = c(r, listKeyValue(group, rep(new, length(group))));
	r
}


#' Given a matrix of haplotype-pairs, construct a data frame with dosage information for each haplotype, suitable for regression analysis
haplotypes2df = function(hts, htMap = NULL) {
	uniqueHts = if (is.null(htMap)) sort(unique(avu(as.matrix(hts)))) else
		sort(unique(avu(htMap)))
	map = listKeyValue(as.character(uniqueHts), 1:length(uniqueHts));

	if (!is.null(htMap)) {
		map = listKeyValue(names(htMap), map[as.character(avu(htMap))]);
	}
	template = diag(length(uniqueHts));
	dosage = Df(t(apply(hts, 1, function(r) {
		r = as.character(r);
		template[map[[r[1]]], ] + template[map[[r[2]]], ]
	})), names = paste('ht', as.character(uniqueHts), sep = '_'));
	r = list(haplotypes = dosage, map = map);
	r

}

#
#	<p> mean impute SNPs
#


snpMeanImpute = function(snp, score = scoresL$additive, missingWarning = 1e-2, name = names(snp)) {
	# <p> check for missingness
	if (fraction(is.na(snp)) > missingWarning) {
		Log(Sprintf('Warning: Mean-imputing %{miss}.1f%% (more than %{missCo}.1f%%) for SNP %{name}s',
			miss = fraction(is.na(snp))*1e2, missCo =  missingWarning*1e2), 4)
	}
	# <p>  map to score
	snpS = score[snp + 1];
	# <p> produce matrix
	snpM = if (is.factor(snpS)) {
		m = model.matrix(~ x, data = data.frame(x = snpS))[, -1];
		i = as.integer(row.names(m));
		mN = matrix(NA, nrow = length(snp), ncol = ncol(m));
		mN[i, ] = m;
		mN
	} else
		t(t(snpS));
	# <p> perform imputation
	m = mean(apply(snpM, 2, mean, na.rm = T));
	snpM[is.na(snpS), ] = m;
	snpM = as.data.frame(snpM);
	names(snpM) = if (ncol(snpM) > 1) paste(name, 1:ncol(snpM), sep = '_') else name;
	snpM
}

snpsMeanImpute = function(data, score = scoresL$additive) {
	# <p> impute snps
	snpsImp = lapply(1:ncol(data), function(i) {
		snpMeanImpute(data[[i]], score, name = names(data)[i]);
	});
	r = do.call(cbind, snpsImp);
	r
}
#
#	RgeneticsImputation.R
#Mon Feb 15 16:13:15 CET 2010

# based on the RA project

# global default pathes
hapmapMapPath = "/Library/ProjectData/2009-10-Hapmap3/phased/hapmap3map.RData";
hapmapDir = "/Library/ProjectData/2009-10-Hapmap3/phased";

#
#	likelihood ratio
#

#
#	<p> likelihood for independent offspring, ascertainment: one affected offspring
#<A> realistic: familiy based random effect

# data is data frame with columns N, A (total, affecteds)
llSNPimp = function(mu, beta, scores = 0:2/2, y, z) {
	ll = sum(sapply(1:length(y), function(i) {
		lh0 = sum(logitI(mu + beta * scores) * z[i, ]);
		ll = log(ifelse(y[i] == 1, lh0, 1 - lh0));
		ll
	}));
	ll
}
spec_lhImp = list(
	ll = "llSNPimp",
	alt = list(
		start = c(0, 0),	# also specifies number of parameters
		pars = list(
			list(name = "mu", type = "real"),
			list(name = "beta", type = "real")
		)
	),
	null = list(
		start = c(0),	# assume same likelihood and therefore #pars from alternative
		pars = list(
			list(name = "mu", type = "real")
		),
		mapperPost = function(p)c(p, 0)
	)
);

# genotype based test
llSNPimpGt = function(mu, beta1, beta2, scores = NULL, y, z) {
	beta = c(beta1, beta2);
	scores = matrix(c(0, 0, 1, 0, 0, 1), nrow = 3, byrow = T);
	ll = sum(sapply(1:length(y), function(i) {
		lh0 = sum(logitI(mu + scores %*% beta) * z[i, ]);
		ll = log(ifelse(y[i] == 1, lh0, 1 - lh0));
		ll
	}));
	ll
}
spec_lhImpGt = list(
	ll = "llSNPimpGt",
	alt = list(
		start = c(0, 0, 0),	# also specifies number of parameters
		pars = list(
			list(name = "mu", type = "real"),
			list(name = "beta1", type = "real"),
			list(name = "beta2", type = "real")
		)
	),
	null = list(
		start = c(0),	# assume same likelihood and therefore #pars from alternative
		pars = list(
			list(name = "mu", type = "real")
		),
		mapperPost = function(p)c(p, 0, 0)
	)
);

testImputedSNPlr = function(d, vars, gtScores = 1 - scores$add, response = "y") {
	# descide between degrees of freedom
	spec_lh = if (is.factor(gtScores)) spec_lhImpGt else spec_lhImp;
	# prepare data
	y = d[[response]];
	z = as.matrix(d[, vars, drop = F]);
	if (dim(z)[2] == 2) z = cbind(z, 1 - z[, 1] - z[, 2]);

	# define null/alt
	ml1 = lhMl(spec_lh, type = "alt", scores = gtScores, y = y, z = z);
	ml0 = lhMl(spec_lh, type = "null", scores = gtScores, y = y, z = z);
	chi = 2 * (ml1$value - ml0$value);
	dfs = length(spec_lh$alt$start) - length(spec_lh$null$start);
	p = pchisq(chi, dfs, lower.tail = F);
	r = list(p.value = p, chiSq = chi, df = dfs, pars = ml1$par);
	r
}

testSNPimputedGt = function(postFreq, response, yB) {
	# pre-calculations
	N = length(response);
	postFreq = postFreq[, c(2,3)];		# ignore reference genotype
	#postFreq = postFreq[, c(1,2)];		# ignore reference genotype
	scoreE = apply(postFreq, 2, mean);	# expected score
	scoreC = postFreq - scoreE;			# centered score
	#print(as.vector(c(1 - sum(scoreE), scoreE)));

	IO = sapply(1:2, function(c)sapply(1:2, function(r){
		sum(sapply(1:N, function(i)((response[i] - yB)^2 * scoreC[i, r] * scoreC[i, c])))
	}));
	# matrix elements
	m11_1 = postFreq %*% c( (1 - scoreE[1]), -scoreE[1] );
	m11_2 = postFreq %*% c( (1 - scoreE[1])^2, scoreE[1]^2 );
	m22_1 = postFreq %*% c( -scoreE[2], (1 - scoreE[2]) );
	m22_2 = postFreq %*% c( scoreE[2]^2, (1 - scoreE[2])^2 );
	m21_o = postFreq %*% c( (1 - scoreE[1]) * scoreE[2], (1 - scoreE[2]) * scoreE[1] );
	# squared error for response
	responseC = (response - yB);
	seY = responseC^2;

	# expected information
	IE_21 = -sum(m21_o);
	IE = yB * (1 - yB) * matrix(c(sum(m11_2), IE_21, IE_21, sum(m22_2)), ncol = 2);

	# loss
	#IL = sum((response - yB)^2 * (postFreq %*% (gtScores^2) - postScores^2));
	ILS_21 = seY %*% (m11_1 * m22_1);
	ILS2 = matrix(c(seY %*% m11_1^2, ILS_21, ILS_21, seY %*% m22_1^2), ncol = 2);
	ILJ_21 = -seY %*% m21_o;
	ILJ = matrix(c(seY %*% m11_2, ILJ_21, ILJ_21, seY %*% m22_2), ncol = 2);
	IL = ILJ - ILS2;

	# lh scores
	lhs = as.numeric(apply(scoreC * responseC, 2, sum));
	#lhs = as.numeric(apply(postFreq * responseC, 2, sum));
	# test stat
	# IO = IE - IL;
	T = t(lhs) %*% solve(IO) %*% lhs;
	p.value = pchisq(T, 2, lower.tail = F);
	# relative information
	#IR = (IE - IL) %*% solve(IE) %*% scoreE[2:3];
	r = list(IE = IE, IL = IL, IO = IE - IL, V = solve(IE - IL), score = lhs, T = T, p.value = p.value);
	r
}
testSNPimputedScore = function(postFreq, response, yB, gtScores = 1 - scores$add) {
	# expected information
	gtScoresE = apply(postFreq, 2, mean) %*% gtScores;
	postScores = postFreq %*% (gtScores - gtScoresE);
	postScores2 = postFreq %*% ((gtScores - gtScoresE)^2);
	IE = yB * (1 - yB) * sum(postScores2);
	# loss
	#IL = sum((response - yB)^2 * (postFreq %*% (gtScores^2) - postScores^2));
	IL = sum((response - yB)^2 * (postScores2 - postScores^2));
	# squared lh scores
	lhs = sum(postScores * (response - yB));
	# test stat
	T = lhs^2 / (IE - IL);
	p.value = pchisq(T, 1, lower.tail = F);
	# relative information
	IR = (IE - IL) / IE;
	r = list(IE = IE, IL = IL, IO = IE - IL, Rsq = IR, score = lhs, T = T, p.value = p.value);
	r
}

writeSNPtestInput = function(data, snps, path) {
	N = dim(data)[1];
	# <p> genotype file
	snpIndex = paste("SNP", 1:length(snps), paste = "");
	dSt = t(sapply(1:length(snps), function(i) {
		colsSNP = paste(rep.each(snps[i], 2), c("i1", "i2"), sep = "_");
		postFreq = data[, colsSNP];
		postFreq = as.matrix(cbind(postFreq, abs(1 - postFreq[, 1] - postFreq[, 2])));
		c(snpIndex[i], snps[i], "A", "C", sapply(as.vector(t(postFreq)), function(e)sprintf("%.4f", e)))
	}));
	# write genotypes
	write.table(dSt, file = sprintf("%s.gen", path), quote = F, sep = " ", col.names = F, row.names = F);

	# <p> produce sample file
	#	accomodating the ill-designed sample file: extra row with column codes
	s = data.frame(ID_1 = 0, ID_2 = 0, missing = 0);
	s0 = data.frame.types(cbind(1:N, 1:N, rep(0, N)), names = names(s));
	s = rbind(s, s0);
	write.table(s, file = sprintf("%s.sample", path), sep = " ", row.names = F, quote = F);
}
# <!> only return genotype test values
testSNPimputedSnpTest = function(data, snps, response, covariates = NULL, type = NULL,
	gtScores = 1 - scores$add) {
	controls = tempfile();
	writeSNPtestInput(data[data[[response]] == 0, ], snps, controls);
	cases = tempfile();
	writeSNPtestInput(data[data[[response]] == 1, ], snps, cases);

	output = tempfile();
	command = sprintf("snptest -controls %s.gen %s.sample -cases %s.gen %s.sample -o %s -frequentist 1 2 3 4 5 -proper > /dev/null",
		controls, controls, cases, cases, output);
	System(command, 5, printOnly = F);
	gc();
	r = read.csv(file = output, sep = " ", as.is = T);
	r = list(p.value = r$frequentist_gen_proper, r2 = r$frequentist_gen_proper_info, T = NA);
	r
}
testSNPimputedSnpTestFormula = function(f, data, snpRegex = "rs.*", snps = NULL,
	gtScores = scores$add, type = "glm", ...) {
	r = snpCallFormula2vars(testSNPimputedSnpTest, f, data, snpRegex, snps, gtScores = gtScores, ...);
	r
}

testSNPimputed = function(data, snps, response, covariates = NULL, type = NULL, gtScores = 1 - scores$add) {
	# <!> assume a single SNP, posterior frequencies
	colsSNP = paste(rep.each(snps, 2), c("i1", "i2"), sep = "_");
	postFreq = data[, colsSNP];
	postFreq = as.matrix(cbind(postFreq, 1 - postFreq[, 1] - postFreq[, 2]));

	# response
	responseData = data[[response]];
	yB = mean(responseData);
	r = if (is.factor(gtScores)) testSNPimputedGt(postFreq, responseData, yB) else
#	r = if (is.factor(gtScores)) testSNPimputedSnpTest(data, snps, response, covariates, type) else
		testSNPimputedScore(postFreq, responseData, yB, gtScores);
	r
}

testSNPimputedFormula = function(f, data, snpRegex = "rs.*", snps = NULL,
	gtScores = scores$add, type = "glm", ...) {
	r = snpCallFormula2vars(testSNPimputed, f, data, snpRegex, snps, gtScores = gtScores, ...);
	r
}

testSNP = function(data, snp, response = "y", gtScores = scores$add) {
	r0 = testSNPimputed(sprintf("%s ~ %s", response, snp), data = data, gtScores = 1 - gtScores);
	r1 = testImputedSNPlr(data, paste(snp, c("i1", "i2"), sep = "_"), response = response);
	r = list(score = r0, lr = r1);
	r
}

#
#	<p> imputation functions
#

# <p> defaults for impute imputation run
imputeParsDefaultImpute = list(
	command = "cd '%s' ; impute2 -h %s -l %s -m %s -g %s -o %s -Ne %d -int %d %d -buffer %d  -k %d -iter %d -burnin %d -fix_strand",
	hapfile = "CEU+TSI.chrCHR.hap",
	legendfile = "hapmap3.r2.b36.chrCHR.legend",
	mapfile = "genetic_map_chrCHR_combined_b36.txt",
	Ne = 11418,
	buffer = 50,	# 250 <!>
	k = 10,
	iterations = 10,
	burnin = 3,

	runningDir = "global<<hapmapDir>>"
);

# <p> defaults for mach imputation run
imputeParsDefaultMach = list(
	command1 = "cd '%s' ; mach1 -h %s -s %s --hapmapFormat -d %s -p %s --greedy -r 5 --prefix %s --autoFlip",
	command2 = "cd '%s' ; mach1 -h %s -s %s --hapmapFormat -d %s -p %s --crossover %s --errormap %s --greedy --mle --mldetails --prefix %s --autoFlip",
	hapfile = "CEU.chrCHR.hap",
	legendfile = "hapmap3.r2.b36.chrCHR.legend",

	runningDir = "global<<hapmapDir>>",

	idType = "idRs",
	window = 200,
	printOnly = F
);

#
#
#

imputeAndTestSnps = function(snps, datasets, outputPrefix, map = get(load(hapmapMapPath)[1])) {

	fi = function(snp, datasets, map)imputeAndTestSnpForDatasets(datasets, snp, map);
	tests = clapply(snps, fi, datasets = datasets, map = map, .clRunLocal = F);

	save(tests, file = sprintf("%s.RData", outputPrefix));
	testsr = list.key(tests, "test", unlist = F, template = list(p = NA, chiSq = NA, mu = NA, beta = NA));
	ts = data.frame(snp = RAsnpsAll, listOfLists2data.frame(testsr, idColumn = NULL));
	write.csv(ts, file = sprintf("%s.csv", outputPrefix));
	ts
}

#
#	<p> helper functions
#

# allow a path to be specified as "global<<var>>" where this string is substituted by the global
#	variable "var" thus allowing lazy evalutation of pathes (remote execution)
localPath = function(path) {
	if (is.null(path)) return(NULL);
	path = if (is.character(path)) path else {
		if (is.null(path$dir)) path$file else sprintf("%s/%s", path$dir, path$file)
	}
	pathVars = fetchRegexpr("(?<=global<<).*?(?=>>)", path);
	#if (length(pathVars) == 0) path = get(pathVar);
	#print(listKeyValue(pathVars, sapply(pathVars, get)));
	path = mergeDictToString(listKeyValue(sapply(pathVars, function(s)sprintf("global<<%s>>", s)),
		sapply(pathVars, get)), path);
	path
}

#
#	<p> hapmap/generic methods
#

gwasFetcher.Rmap = function(path, dir = NULL) {
	o = list(path = list(dir = dir, file = path));
	class(o) = "Rmap";
	o
}
# returns a data.frame with idRs and pos columns
fetchSnpMap.Rmap = function(self) {
	map = get(load(localPath(self$path))[1]);
	map
}

#
#	<p> plink functions/methods S3
#
plink.postProcessDataFrame = function(d0) {
	# recode response
	d0$response[d0$response == 0] = NA; 
	d0$response = d0$response - 1;
	d0
}
plinkSampleCols = c("idFam", "id", "idF", "idM", "sex", "response");
# rangeSpec: plink option to select snps
fetchSnps.plink.generic = function(self, path, rangeSpec, snps = NULL, format = 1, returnMap = F) {
	sp = splitPath(localPath(path));
	output = sprintf("%s", tempfile());
	cmd = sprintf("cd '%s' ; plink --noweb --bfile %s --recode --out %s %s",
		sp$dir, sp$file, output, rangeSpec);
	System(cmd, 4);
	d0 = read.table(sprintf("%s.ped", output), header = F, sep = " ", as.is = T);

	# plink tends to drops SNPs, therefore re-reading output
	m0 = read.table(sprintf("%s.map", output), header = F, sep = "\t", as.is = T);
	names(m0) = c("chr", self$idType, "posGen", "pos");
	snps = m0[[self$idType]];

	snpCols = paste(rep.each(snps, 2), c("1", "2"), sep = "_");
	nonSnps = plinkSampleCols;
	names(d0) = c(nonSnps, snpCols);
	d0[, snpCols][d0[, snpCols] == "0"] = NA;
	d1 = if (format == 1) {
		gts = extractGenotypes2(d0, snps);
		data.frame(d0[, nonSnps], gts);
	} else d0;
	d1 = plink.postProcessDataFrame(d1);
	r = list(data = d1, snps = snps, response = "response", map = if (returnMap) m0 else NULL);
	r
}

fetchSnpsByName.plink = function(self, path, snps, format = 1, returnMap = F,
	exclInd = NULL, exclMarkers = NULL) {
	snpSelectionList = sprintf("%s-snps.txt", tempfile());
	write.table(data.frame(snps = snps), file = snpSelectionList, quote = F, col.names = F, row.names = F);
	fetchSnps.plink.generic(self, path, sprintf("--extract %s", snpSelectionList),
		snps, format, returnMap);
}
fetchSnpsByRange.plink = function(self, path, chr, from, to, format = 1, returnMap = F) {
	fetchSnps.plink.generic(self, path, sprintf("--chr %s --from-kb %d --to-kb %d", chr, from, to),
		snps = NULL, format, returnMap);
}
fetchSnpsBySpecSingle.plink = function(self, path, spec, format = 1, returnMap = F) {
	d0 = if (!is.null(spec$names)) { fetchSnpsByName.plink(self, path, spec$names, format, returnMap)
	} else { fetchSnpsByRange.plink(self, path, spec$chr, spec$from, spec$to, format, returnMap) };
	d0
}
fetchSampleSingle.plink = function(self, path) {
	# read raw table
	s0 = read.table(sprintf("%s.fam", localPath(path)), header = F, sep = " ", as.is = T);
	names(s0) = plinkSampleCols;
	s0 = plink.postProcessDataFrame(s0);
	r = list(data = s0, snps = NULL, response = "response", map = NULL);
	r
}

gwasFetcher.plink = function(path, dir = NULL, idType = "idRs", mapPath = NULL, fetchMapLazily = T) {
	if (is.character(path))
		path = lapply(path,	function(p)list(dir = dir, file = p));
	o = list(path = path, idType = idType, mapPath = mapPath, map = NULL);
	class(o) = "plink";

	o$map = if (!fetchMapLazily) fetchSnpMap.plink(o) else NULL;
	o
}

fetchSnpMap.plink = function(self) {
	if (!is.null(self$map)) return(self$map);
	mapPath = localPath(if (is.null(self$mapPath)) {
		if (is.list(self$path)) sprintf("%s/%s.bim",
			self$path$dir, self$path$files[[1]]$file) else
		sprintf("%s.bim", self$path)
	} else self$mapPath);
	sp = splitPath(mapPath);
	if (sp$ext == "RData") {
		map = get(load(mapPath)[1]);
		if (!is.data.frame(map)) map = map$data;
	} else {
		map = read.table(mapPath, header = F, sep = "\t", as.is = T);
		names(map) = c("chr", self$idType, "posGen", "pos", "a1", "a2");
	}
	map
}

#
#	<p> mach functions/methods S3
#

# snp: snp to impute
# snpFetcher: fetcher for data set to be imputed
imputeSnpByName.mach = function(self, snpFetcher, snp, parameters = list(), fetchFromRange = F)
	with(c(merge.lists(self$pars, parameters), self), {
	# <p> substitutions
	runningDir = localPath(runningDir);

	# <p> get SNP information
	m = fetchSnpMap(map);
	snp = as.list(m[which(m[[idType]] == snp), ]);
	if (!length(snp[[idType]])) return(NA);	# SNP not found
	if (is.null(snp)) return(NA);
	iv = c(snp$pos - window * 1e3, snp$pos + window * 1e3);
	ivKb = iv %/% 1e3 + c(-1, 1);

	# <p> export hapmap SNPs in window
	chrSnps = m[[idType]][m$chr == snp$chr & m$pos >= iv[1] & m$pos <= iv[2]];
	idFile = sprintf("%s-rsIds", tempfile());
	write.table(data.frame(chrSnps), file = idFile, row.names = F, col.names = F, quote = F);
	idcsFile = sprintf("%s-idcs", tempfile());
	subLegend = sprintf("%s-legend", tempfile());
	command = sprintf("cd '%s' ; csv.pl -s %s --selectRowsBy=rs --selectRowsIds=%s --logRowNumbers=%s > %s",
		runningDir, legendfile, idFile, idcsFile, subLegend);
	command = mergeDictToString(list(CHR = snp$chr), command);
	System(command, printOnly = printOnly);

	subHapfile = sprintf("%s-hap", tempfile());
	command = sprintf("cd '%s' ; csv.pl -s %s --no-header --selectRowsByRowNumbers='%s' > %s-1 ;
		csv.pl -s %s-1 --transpose --no-header > %s",
		runningDir, hapfile, idcsFile, subHapfile, subHapfile, subHapfile);
	command = mergeDictToString(list(CHR = snp$chr), command);
	System(command, printOnly = printOnly);
	#subHapfile = "/tmp/Rtmp7YeKp0/file5e884adc-hap";

	# <p> export dataset SNPs in window
	pedPref = tempfile();	# prefix
	#	ped file
	d = if (fetchFromRange)
		fetchSnpsBySpec(snpFetcher, list(chr = snp$chr, from = ivKb[1] , to = ivKb[2]),
			format = 2, returnMap = T) else {
		# use the hapmap map
		dataMap = fetchSnpMap(snpFetcher);
		dataSnps = which.indeces(chrSnps, dataMap[[idType]]);
		fetchSnpsBySpec(snpFetcher, list(names = dataMap[[idType]][dataSnps]), idType = idType,
			format = 2, returnMap = T);
	}
	# reformat
	d$data$sex = vector.replace(d$data$sex, list("1" = "M", "2" = "F", "0" = NA));
	d$data = d$data[, -which.indeces(d$response, names(d$data))];	# omit response
	# write file
	pedFileData = sprintf("%s-mach.ped", pedPref);
	pedFileDataMap = sprintf("%s-mach.map", pedPref);
	write.table(d$data, file = pedFileData,
		col.names = F, sep = " ", row.names = F, quote = F);
	write.table(cbind("M", as.character(d$map[[idType]])), file = pedFileDataMap,
		col.names = F, sep = " ", row.names = F, quote = F);

	# <p> imputation commands
	parOutput = sprintf("%s-imp-pars", tempfile());
	command = sprintf(command1, runningDir, subHapfile, subLegend,
		pedFileDataMap, pedFileData, parOutput)
	command = mergeDictToString(list(CHR = snp$chr), command);
	System(command, printOnly = printOnly);

	output = sprintf("%s-mach", tempfile());
	command = sprintf(command2, runningDir, subHapfile, subLegend,
		pedFileDataMap, pedFileData, parOutput, parOutput, output)
	command = mergeDictToString(list(CHR = snp$chr), command);
	System(command, printOnly = printOnly);

	# <p> read results
	gts = read.table(sprintf("%s.mlprob", output), header = F);
	info = read.table(sprintf("%s.mlinfo", output), header = T);
	snpI = which(info$SNP == snp[[idType]]);
	# output has two introductory columns (probId, "ML_PROB")
	r = list(gts = gts[, c(2*snpI + 1, 2*snpI + 2)], info = info[snpI,])
	r
});

# paramters
#	window: surrounding distance in kb
gwasSnpImputer.mach = function(map = gwasFetcher.Rmap("global<<hapmapMapPath>>"),
	parameters = list(window = 200), idType = "idRs") {
	pars = merge.lists(imputeParsDefaultMach, parameters);
	o = list(map = map, pars = pars, idType = idType);
	class(o) = "mach";
	o
}

#
#	<p> generic methods
#

# use gwasFetch.class to generate an object to be passed to fetchSnps

postProcessSampleSingle = function(d0, spec) {
	if (!is.null(spec$response)) d0$data[[d0$response]] = spec$response;
	d0
}

#	f: fetcher
#	format: 1 or 2 column SNP format
fetchSampleSingle = function(self, path)UseMethod("fetchSampleSingle");
fetchSample = function(self) {
	if (is.character(self$path)) return(fetchSampleSingle(self, self$path));
	ds = lapply(self$path$files, function(f) {
		d0 = fetchSampleSingle(self, list(dir = self$path$dir, file = f$file));
		postProcessSampleSingle(d0, f);
	});
	d0 = ds[[1]];
	d0$data = rbindDataFrames(list.key(ds, "data", unlist = F), useDisk = T);
	d0
}
fetchSnpsBySpecSingle = function(self, path, spec, format = 1, returnMap = F)
	UseMethod("fetchSnpsBySpecSingle");
fetchSnpsBySpecRaw = function(self, spec, format = 1, returnMap = F) {
	if (is.character(self$path))
		return(fetchSnpsBySpecSingle(self, self$path, spec, format, returnMap));

	ds = lapply(self$path$files, function(f) {
		d0 = fetchSnpsBySpecSingle(self, list(dir = self$path$dir, file = f$file), spec, format, returnMap);
		postProcessSampleSingle(d0, f)
	});
	d0 = ds[[1]];
	d0$data = rbindDataFrames(list.key(ds, "data", unlist = F), useDisk = T);
	d0
}
#
#	fetch SNP from data set which might automatically impute the SNP, if missing
#
fetchSnpsBySpec = function(self, spec, format = 1, returnMap = F, idType = "idRs"){
	# translate SNP names to proper naming scheme
	if (idType != self$idType & !is.null(spec$names)) {
		snps = spec$names;
		map = fetchSnpMap(self);
		snpsProp = map[[self$idType]][which.indeces(snps, map[[idType]], ret.na = T)];	#proprietary names
		# check id translation
		snpsMissing = snps[is.na(snpsProp)];
		snps = snps[!is.na(snpsProp)];
		snpsProp = snpsProp[!is.na(snpsProp)];
		spec$names = snpsProp;
		print(spec);
	}

	d0 = fetchSnpsBySpecRaw(self, spec, format, returnMap);

	# translate back
	# extract names back as fewer snps might be returned than requested
	if (idType != self$idType) {	#  & !is.null(spec$chr)
		map = fetchSnpMap(self);
		snpsProp = d0$snps;
		snps = map[[idType]][which.indeces(snpsProp, map[[self$idType]], ret.na = T)];	#required names
		snps[is.na(snps)] = snpsProp[is.na(snps)];	# substitute missing names <!>
		snpsMissing = NULL;
	}
	if (idType != self$idType) {	# convert back to required names
		if (format == 1) {
			snpsPropCols = gsub("-", ".", snpsProp);
			snpsCols = gsub("-", ".", snps);
		} else if (format == 2) {
			snpsPropCols = gsub("-", ".", paste(rep.each(snpsProp, 2), c("1", "2"), sep = "_"));
			snpsCols = gsub("-", ".", paste(rep.each(snps, 2), c("1", "2"), sep = "_"));
		}
		names(d0$data)[which.indeces(snpsPropCols, names(d0$data))] = snpsCols;
		snpNames = map[[idType]][which.indeces(snps, map[[idType]])];
		d0$missing = snpsMissing;
		d0$snps = snps;
	}
	if (returnMap & idType != self$idType) {
		d0$map = cbind(snps, d0$map);
		names(d0$map)[1] = idType;
	}
	d0
}
fetchSnpsByName = function(self, snps, format = 1, returnMap = F, idType = "idRs")
	fetchSnpsBySpec(self, list(names = snps),
		format = format, returnMap = returnMap, idType = idType);
fetchSnpsByRange = function(self, chr, from, to, format = 1, returnMap = F, idType = "idRs")
	fetchSnpsBySpec(self, list(chr = chr, from = from, to = to),
		format = format, returnMap = returnMap, idType = idType);
imputeSnpByName = function(i, fd, snp, parameters) UseMethod("imputeSnpByName");
fetchSnpMap = function(m) UseMethod("fetchSnpMap");

#
#	<p> integrating functions
#

imputeSnpsByName = function(imputer, fetcher, snps, parameters = list()) {
	sample = fetchSample(fetcher);
	fImp = function(snp, imputer, fetcher, parameters)imputeSnpByName(imputer, fetcher, snp, parameters);
	d0 = clapply(snps, fImp, imputer = imputer, fetcher = fetcher, parameters = parameters);
	gts = cbindDataFrames(list.key(d0[!is.na(d0)], "gts", unlist = F));
	names(gts) = paste(rep.each(snps[!is.na(d0)], 2), c("i1", "i2"), sep = "_");
	quality = data.frame(snp = snps[!is.na(d0)],
		Rsq = list.key(list.key(d0[!is.na(d0)], "info", unlist = F), "Rsq"));
	r = merge.lists(sample, list(data = cbind(sample$data, gts), snps = snps[!is.na(d0)],
		missingSnps = snps[is.na(d0)], quality = quality));
	r
}

# creates a data set that contains a marker selection and imputes markers if necessary
createDataSetForMarkers = function(fetcher, imputer, snps, format = 1, idType = "idRs", .clRunLocal = F) {
	map = fetchSnpMap(fetcher);
	# observed SNPs
	snpsO = map[[idType]][which.indeces(snps, map[[idType]])];
	dO = fetchSnpsByName(fetcher, snpsO, format = format, idType = idType);
	# snps to be imputed
	snpsI = setdiff(snps, dO$snps);
	d0 = imputeSnpsByName(imputer, fetcher, snpsI, .clRunLocal = .clRunLocal);
	d0$data = merge(dO$data, d0$data)
	d0$snps = union(d0$snps, dO$snps);
	d0
}

#
#	<p> special functions
#

# estimate an r2 (correlation observed, imputed) under a lot of assumptions
# d is a 3x2 table with exepcted genotype counts
r2fromMaxPost = function(d, maxp) {
	if (is.na(maxp)) return(list(r2 = NA));
	# estimated mean posterior
	# assume same distribution for all genotypes
	meanPost = c(maxp, (1 - maxp) * maxp, 1 - maxp * ((1 - maxp) + 1));
	gtfs = apply(d, 2, sum) / sum(d);
	#rs = sapply(1:3, function(gt)(gtfs[gt] * (mxp - gtfs[gt]))/(gtfs[gt]*(1 - gtfs[gt])));
	rs = sapply(gtfs, function(gtf)((maxp - gtf))/(1 - gtf));
	r2 = sum(rs^2 * gtfs);
	r = list(r2 = r2, meanPost = meanPost);
	r
}

#
#	<p> generic functions for manipulating genomic data
#

# v0.1

plink.detectInputFormat = function(prefix) {
	sp = splitPath(prefix);
	fileset = grep.infixes(sp$base, list.files(sp$dir));
	exts = sapply(fileset, function(f)splitPath(f)$ext);
	r = if ('bed' %in% exts) list(
			format = 'bed', option = c('--bfile', prefix), ped = sprintf('%s.fam', prefix)) else
		if ('tped' %in% exts) list(
			format = 'tped', option = c('--tfile', prefix), ped = sprintf('%s.tfam', prefix)) else
		if ('ped' %in% exts) list(
			format = 'ped', option = c('--file', prefix), ped = sprintf('%s.ped', prefix)) else
	stop(sprintf('plink format for file \'%s\' is unknown', prefix));
	r = c(r, list(dir = sp$dir));
	r
}

plink.PedHeader = c('fid', 'iid', 'pid', 'mid', 'sex', 'y');

plink.runCommand = function(inputPrefix, outputPrefix = NULL, command = '--recode', options = c(),
	readOutput = F) {
	i = plink.detectInputFormat(localPath(inputPrefix));
	if (is.null(outputPrefix)) outputPrefix = tempfile('pedFileExport');
	cmd = sprintf("cd '%s' ; plink %s --noweb %s --out %s",
		i$dir, join(c(i$option, options), ' '), command, outputPrefix);
	System(cmd, 4);

	d0 = NULL;
	if (readOutput) {
		d0 = read.table(sprintf("%s.ped", outputPrefix), header = F, sep = " ", as.is = T);
		snps = read.table(sprintf("%s.map", outputPrefix), header = F, as.is = T)[, 2];
		snpCols = paste(rep.each(snps, 2), c("1", "2"), sep = "_");
		names(d0) = c(plink.PedHeader, snpCols);
		gts = extractGenotypes2(d0, snps);
		d0 = data.frame(d0[, (1:length(plink.PedHeader))], gts);
	}

	r = list(out = outputPrefix, data = d0);
	r
}

# assume unique ids in ped files
# for exclInd, exclMarkers expect pathes to files containting ids in separate lines wihtout header
plink.fetchSnpsByName = function(inputPrefix, snps, exclInd = NULL, exclMarkers = NULL) {
	# <p> write SNP list
	snpSelectionList = sprintf('%s-snpSelection', tempfile());
	write.table(data.frame(snps = snps),
		file = snpSelectionList, quote = F, col.names = F, row.names = F);

	# <p> write indivuals exclusion file
	exclIndPed = NULL;
	if (!is.null(exclInd)) {
		ids = read.table(exclInd, header = F, as.is = T)[, 1];
		i = plink.detectInputFormat(inputPrefix);
		ped = read.table(i$ped, header = F, as.is = T)[, 1:2];
		ped0 = ped[which.indeces(ids, ped[, 2]),];
		exclIndPed = sprintf('%s-exclusionPed', tempfile());
		write.table(ped0, file = exclIndPed, quote = F, sep = "\t", col.names = F, row.names = F);
	}

	options = c(
		if (is.null(exclMarkers)) '' else c('--exclude', exclMarkers),
		if (is.null(exclInd)) '' else c('--remove', exclIndPed),
		c('--extract', snpSelectionList)
	);
	r = plink.runCommand(inputPrefix, options = options, readOutput = T);
	r$data
}
