#
#	testLm.R
# < Fri Feb 22 17:14:40 CET 2013
#
#	Author: Stefan Boehringer

testLmPerSnp = function(data, formula1, formula0, snp, ...) {
	r = regressionCompareModels(formula1, formula0, data, type = "lm", clusterCol = NULL, family = gaussian());
	#r = r[c('effects0', 'sdevs0', 'effects1', 'sdevs1', 'p.value')];
	r0 = c(r$effects0, r$sdevs0, r$effects1, r$sdevs1, r$p.value);
	names(r0) = c(
		paste('beta0', names(r$effects0), sep = '.'),
		paste('sd0', names(r$sdevs0), sep = '.'),
		paste('beta1', names(r$effects1), sep = '.'),
		paste('sd1', names(r$sdevs1), sep = '.'),
		'P-value'
	);
	r0
}
