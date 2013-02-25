#
#	testCoxPerSnp.R
#Fri Feb 22 17:14:40 CET 2013
#
#	Author: Stefan Boehringer

testCoxPerSnp = function(data, formula1, formula0, snp, ...) {
	m1 = coxph(formula1, data = data);
	m0 = coxph(formula0, data = data);

	r = as.list(c(
		coefficients(m0),
		sqrt(diag(m0$var)),
		coefficients(m1),
		sqrt(diag(m1$var)),
		anova(m0, m1)[2, 'P(>|Chi|)']
	));
	names(r) = c(
		paste('beta0', names(coefficients(m0)), sep = '.'),
		paste('sd0', names(coefficients(m0)), sep = '.'),
		paste('beta1', names(coefficients(m1)), sep = '.'),
		paste('sd1', names(coefficients(m1)), sep = '.'),
		'P-value'
	);
	r
}

#testCoxPerSnp(bladder1, Surv(stop, event) ~ (rx + size + number), Surv(stop, event) ~ (rx + size), snp = '')
