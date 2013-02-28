#
#	testCoxPerSnp.R
#Fri Feb 22 17:14:40 CET 2013
#
#	Author: Stefan Boehringer
library(survival)
testCoxPerSnp = function(data, formula1, formula0, snp, ...) {
print(data$sex);
print(data[1:20, ]);
	m1 = coxph(as.formula(formula1), data = data);
print(summary(m1));
	m0 = coxph(as.formula(formula0), data = data);
print(summary(m0));

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
