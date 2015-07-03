#
#	testCoxPerSnp.R
#Fri Feb 22 17:14:40 CET 2013
#
#	Author: Stefan Boehringer
library(survival)
testCoxPerSnp = function(data, formula1, formula0, snp, ...) {
	m1 = coxph(as.formula(formula1), data = data);
	m0 = coxph(as.formula(formula0), data = data);

	# can we calculate an anova, or was GEE used?
	p.value = if ('cluster' %in% all.vars(as.formula(formula1), functions = T)) {
		v = setdiff(all.vars(as.formula(formula1)), all.vars(as.formula(formula0)));
		summary(m1)$coefficients[v, 'Pr(>|z|)']
	} else {
		anova(m0, m1)[2, 'P(>|Chi|)']
	}

	r = as.list(c(
		coefficients(m0),
		sqrt(diag(m0$var)),
		coefficients(m1),
		sqrt(diag(m1$var)),
		p.value
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
