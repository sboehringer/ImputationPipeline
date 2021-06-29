#
#	testGlmOrdPerSnp.R
#	Fri Jul  6 13:28:12 CEST 2018
#
#	Author: Stefan Boehringer
#	derived from testLm.R, testLogisticPerSnp

library('MASS');

testGlmOrdPerSnp = function(data, formula1, formula0, snp, ...) {
	#browser();
	m1 = polr(formula1, data, Hess = T);
	m0 = polr(formula0, data, Hess = T);
	# <N> workaround for above call in order to make confint work w/o Hess = T -> ... not successful
	#m1 = eval(parse(text = Sprintf('polr(%{f1}s)', f1 = formula.to.character(f1))), envir = data);
	#m0 = polr(f0, data);

# 	r = as.list(c(
# 		coefficients(m0),
# 		sqrt(diag(m0$var)),
# 		coefficients(m1),
# 		sqrt(diag(m1$var)),
# 		p.value
# 	));
# 	names(r) = c(
# 		if (is.null(coefficients(m0))) NULL else paste('beta0', names(coefficients(m0)), sep = '.'),
# 		if (is.null(coefficients(m0))) NULL else paste('sd0', names(coefficients(m0)), sep = '.'),
# 		if (is.null(coefficients(m1))) NULL else paste('beta1', names(coefficients(m1)), sep = '.'),
# 		if (is.null(coefficients(m1))) NULL else paste('sd1', names(coefficients(m1)), sep = '.'),
# 		'P-value'
# 	);

	r = as.list(c(
		coefficients(m0),
		summary(m0)$coefficients[names(coefficients(m0)), 'Std. Error'],
		coefficients(m1),
		summary(m1)$coefficients[names(coefficients(m1)), 'Std. Error'],
		anova(m0, m1)[2, 'Pr(Chi)']
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
