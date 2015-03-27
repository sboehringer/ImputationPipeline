#
#	RlabBareExp.R
#Wed Jan 15 13:30:49 CET 2014

# test stuff not needing RgenericAll.R

library('devtools');

if (1) {
	#install_github('parallelize.dynamic', 'sboehringer');
	install_local('/home/pingu/src/Rpackages/parallelize.dynamic');
	require('parallelize.dynamic');
	#parallelize_initialize(list(sourceFiles = 'abc.R'), backend = 'local');
}

if (1) {
#library(parallelize.dynamic);
Parallelize_config = list(
	libraries = 'sets',
	freezerClass = 'LapplyFreezer',
	backends = list(snow = list(localNodes = 8, splitN = 2, stateDir = tempdir()))
);
parallelize_initialize(Parallelize_config, backend = "snow", parallel_count = 32, copy_environments = T, force_rerun = T);

library(sets);
data(iris)
d <- iris; response <- 'Species'; R <- .01; Nl <- 1e1; Nu <- 1e4
#d <- iris; response <- 'Species'; R <- .01; Nl <- 1e1; Nu <- 1e2

vars <- setdiff(names(d), response)
responseLevels <- levels(d[[response]])

minimax <- function(v, min = -Inf, max = Inf)ifelse(v < min, min, ifelse(v > max, max, v))
N <- function(p, r = R)(2*qnorm(p, lower.tail = F)/r)^2 * (1-p)/p
analysis <- function(data, vars) {
  f1 <- as.formula(sprintf('%s ~ %s', response, paste(vars, collapse = ' + ')));
  f0 <- as.formula(sprintf('%s ~ 1', response));
  a <- anova(glm(f0, data = data), glm(f1, data = data), test = 'Chisq')
  p.value <- a[['Pr(>Chi)']][[2]]
}
permute <- function(data, vars, f, ..., M) {
  ps <- Sapply(0:M, function(i) {
    d = data;
    if (i > 0) d[, vars] = d[sample(nrow(data)), vars];
    f(d, vars, ...)
  })
  p.data <- ps[1]
  ps <- ps[-1]
  list(p.raw = p.data, p.emp = mean(ps[order(ps)] < p.data))
}
subsetRegression = function() {
  r <- Lapply(responseLevels, function(l) {
    subsets <- as.list(set_symdiff(2^as.set(vars), 2^set()))
    r1 <- Sapply(subsets, function(subset) {
      d[[response]] = d[[response]] == l
      p.value <- analysis(d, unlist(subset))
      print(p.value)
      unlist(permute(d, unlist(subset), analysis, M = as.integer(minimax(N(p.value), Nl, Nu))))
    })
    output <- data.frame(subset = sapply(subsets, function(s)paste(s, collapse = '+')), t(r1))
  })
  names(r) <- responseLevels
  r
}
print(parallelize_call(subsetRegression()))
}

