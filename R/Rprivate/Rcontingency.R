#
#	contingency.R
#Thu 13 Jul 2006 06:03:58 PM EDT 

# rows are predicting factor; columns are truth
two.by.two = function(t, rows = NULL, cols = NULL, prevalence = 0.05) {
	f = fisher.test(t);
	c = chisq.test(t);
	c.bs = chisq.test(t, simulate.p.value = TRUE, B = 1e5);
	fmt = function(n) sprintf("%.4f", n)
	t1 = data.frame(t);
	if (!is.null(cols)) names(t1) = cols;
	if (!is.null(rows)) row.names(t1) = rows;
	have.names = !is.null(cols) && !is.null(rows);

	# named ppv
	ppv.name = if(have.names) paste(cols[1], rows[1], sep = " | ") else "";
	npv.name = if(have.names) paste(cols[2], rows[2], sep = " | ") else "";
	ppv.c.name = if(have.names) paste(cols[2], rows[1], sep = " | ") else "";
	npv.c.name = if(have.names) paste(cols[1], rows[2], sep = " | ") else "";

	# attributable risk
	# ar = (I-Io)/I
	# http://www.tylermedicalclinic.com/Aetiological%20Fraction.htm
	# The reduction in disease when a risk factor is removed. If I* is the number of people that a risk factor is responsible for and I is the total number of cases  over the same period, then the aetiological fraction is I*/I. Equivalently, the aetiological fraction is (I-Io)/I. where Io is the number of cases in the absence of the risk factor. Also known as the attributable fraction.
	I0 = prevalence * t[2, 1] / (prevalence * t[2, 1] + t[2, 2]);
	ar = (prevalence - I0) / prevalence;
	ar.name = if (have.names) paste("Risk factor:", cols[1], "->", rows[1], sep = " ") else "";
	ar.name = paste(ar.name, "Prev", fmt(prevalence));

	r = list(
		p.value.fisher = fmt(f$p.value),
		p.value.chisq = fmt(c$p.value),
		p.value.chisq.bs = fmt(c.bs$p.value),
		odds.ratio = paste(fmt(f$estimate), "(", paste(fmt(f$conf.int), collapse = ", "), ")", collapse = " "),
#		odds.ratio.ki = f$conf.int,
		ppv = paste(fmt(t[1, 1] / (t[1,1] + t[1, 2])), ppv.name, collapse = " "),
		npv = paste(fmt(t[2, 2] / (t[2,1] + t[2, 2])), npv.name, collapse = " "),
		ppv.c = paste(fmt(t[1, 2] / (t[1,1] + t[1, 2])), ppv.c.name, collapse = " "),
		npv.c = paste(fmt(t[2, 1] / (t[2,1] + t[2, 2])), npv.c.name, collapse = " "),
		attributable.risk = paste(fmt(ar), ar.name, sep = "; "),
	);
	list(
		table = t1,
		properties  = col.frame(r)
	)
}

odds.ratio = function(p, q){ (p/(1-p)) / (q/(1-q)) }

binom.test.desc = function(count.a, count.n, ref.p, name = NULL) {
	b = binom.test = binom.test(count.a, count.n, ref.p);
	freq = count.a / count.n;
	s = list(type = name, freq = freq, freq.ref = ref.p,
		p.value.exact = b[[3]],
		conf.int = sprintf("(%.4f, %.4f)", b$conf.int[[1]], b$conf.int[[2]]),
		or = odds.ratio(freq, ref.p),
		or.ci = sprintf("(%.4f, %.4f)",
			odds.ratio(b$conf.int[[1]], ref.p), odds.ratio(b$conf.int[[2]], ref.p)),
	);
	# true structure
	s.s = c(s, list(conf.int.s = b$conf.int));
	list(summary = col.frame(s), result = s.s)
}

#example
if (0) {
# cols: autosomal, sporadic
t = matrix(c(
	# unilateral
	3 + 6 + 0,	3 + 20 + 3,
	# bilateral
	4 + 15 + 5, 5 + 10 + 7
	), 2, 2
);
t1 = data.frame(rbind(t[2, ], t[1, ]));

#print(two.by.two(cbind(t[, 2], t[, 1])));
a.t1 = two.by.two(t1, rows = c("bilateral", "unilateral"), cols = c("autosomal", "sporadic"), prevalence = .3);
print(a.t1);
}
