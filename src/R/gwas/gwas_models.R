#
#	gwas_models.R
#Fri Jul 19 14:28:46 2013

#
#	<p> dependencies
#

# ordinal regression
require('MASS');

# Define statistical models used by gwasAnalysis

#
#	<p> standard analysis functions
#

# <!><i> coefficient matrix 12.3.2018
glmBin = function(data, f1, f0, o) {
	r = regressionCompareModels(f1, f0, data, type = "glm", clusterCol = NULL, family = binomial());
	#r = c(r[c('effects0', 'effects1', 'p.value')], list(ci1 = confint(r$m1$r)));
	ci1 = simplify(betaSdToCi(r$effects1, r$sdevs1)[c('lower', 'upper')]);
	r = c(r[c('effects0', 'effects1', 'p.value')], list(ci1 = ci1));
	r
}

# <!><i> coefficient matrix 12.3.2018
glmLm = function(data, f1, f0, o) {
	r = regressionCompareModels(f1, f0, data, type = "glm", clusterCol = NULL, family = gaussian());
	r = c(r[c('effects0', 'effects1', 'p.value')], list(ci1 = confint(r$m1$r)));
	r
}

# ordinal logistic regression by polr(MASS)
# confidence interval calculation optimized as of 14.9.2018: get rid of confint function
glmOrd = function(data, f1, f0, o) {
	d0 = completeData(f1, data);
	m1 = polr(f1, d0, Hess = T);
	m0 = polr(f0, d0, Hess = T);
	# <N> workaround for above call in order to make confint work w/o Hess = T -> ... not successful
	#m1 = eval(parse(text = Sprintf('polr(%{f1}s)', f1 = formula.to.character(f1))), envir = data);
	#m0 = polr(f0, data);
	cfs = coefficients(summary(m1));
	cis = do.call(cbind, betaSdToCi(cfs[, 'Value'], cfs[, 'Std. Error']))[, -1];

	r = list(
		# R 3.0.3
		p.value = anova(m0, m1)[2, 'Pr(Chi)'],
		#p.value = anova(m0, m1)[2,'Pr(>Chisq)'],
		effects0 = coefficients(m0),
		effects1 = coefficients(m1),
		#ci1 = confint(m1),
		ci1 = cis,
		coeff0 = coefficients(summary(m0)),
		coeff1 = coefficients(summary(m1))
	);
	r
}

glmSurv = function(data, f1, f0, o) {
	complete = apply(data[, all.vars(f1)], 1, function(r)all(!is.na(r)));
	d0 = data[complete, ];
	m1 = coxph(f1, data = d0);
	m0 = coxph(f0, data = d0);
	# can we calculate an anova, or was GEE used?
	#p.value = if ('cluster' %in% all.vars(as.formula(formula1), functions = T)) {
	#	v = setdiff(all.vars(as.formula(formula1)), all.vars(as.formula(formula0)));
	#	summary(m1)$coefficients[v, 'Pr(>|z|)']
	vars0 = formula.predictors(f0, d0);
	Nvars0 = length(vars0);
	vars1 = formula.predictors(f1, d0);
	Nvars1 = length(vars1);
	p.value = if (Nvars0 == 0 || (Nvars1 - Nvars0) == 1) {
		v = setdiff(vars1, vars0);
		summary(m1)$coefficients[v, 'Pr(>|z|)']
	} else {
		anova(m0, m1)[2, 'P(>|Chi|)']
	}

	r = list(
		p.value = p.value,
		effects0 = m0$coef,
		effects1 = m1$coef,
		ci1 = confint(m1),
		coeff0 = coefficients(summary(m0)),
		coeff1 = coefficients(summary(m1))
	);
	r
}

glmBinRE = function(data, f1, f0, o) {
	m1 = glmer(f1, data = data, family = binomial());
	m0 = glmer(f0, data = data, family = binomial());

	r = list(
		#p.value = anova(m0, m1)[2,'Pr(Chi)'],
		p.value = anova(m0, m1)[2, 'Pr(>Chisq)'],
		effects0 = fixef(m0),
		effects1 = fixef(m1),
		coeff0 = coefficients(summary(m0)),
		coeff1 = coefficients(summary(m1))
	);
	r
}

glmRE = function(data, f1, f0, o) {
	m1 = lmer(f1, data = data);
	m0 = lmer(f0, data = data);

	r = list(
		#p.value = anova(m0, m1)[2,'Pr(Chi)'],
		p.value = anova(m0, m1)[2, 'Pr(>Chisq)'],
		effects0 = fixef(m0),
		effects1 = fixef(m1),
		ci1 = confint(m1),
		coeff0 = coefficients(summary(m0)),
		coeff1 = coefficients(summary(m1))
	);
	r
}
