#
#	testCoxPerSnp.R
#Fri Feb 22 17:14:40 CET 2013
#
#	Author: Stefan Boehringer
library(survival)
testCoxPerSnp = function(data, formula1, formula0, snp, ...) {
	formula1 = as.formula(formula1);
	formula0 = as.formula(formula0);
	m1 = coxph(formula1, data = data);
	m0 = coxph(formula0, data = data);
	Nvars1 = length(all.vars(formula.rhs(formula1)));
	Nvars0 = length(all.vars(formula.rhs(formula0)));
	v = setdiff(all.vars(formula1), all.vars(formula0));

	# - can we calculate an anova, or was GEE used?
	# - ANOVA does not work if formula0 is the null model
	p.value = if (
		   'cluster' %in% all.vars(as.formula(formula1), functions = T)
		|| (Nvars0 == 0 || (Nvars1 - Nvars0) == 1)) {
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
		if (is.null(coefficients(m0))) NULL else paste('beta0', names(coefficients(m0)), sep = '.'),
		if (is.null(coefficients(m0))) NULL else paste('sd0', names(coefficients(m0)), sep = '.'),
		if (is.null(coefficients(m1))) NULL else paste('beta1', names(coefficients(m1)), sep = '.'),
		if (is.null(coefficients(m1))) NULL else paste('sd1', names(coefficients(m1)), sep = '.'),
		'P-value'
	);
	r
}

#testCoxPerSnp(bladder1, Surv(stop, event) ~ (rx + size + number), Surv(stop, event) ~ (rx + size), snp = '')
