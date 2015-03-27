#
#	Rsimulation.R
#Mon 07 Jan 2008 06:56:12 PM CET 

#
#	<§> setup
#

#library(MASS);
#source(sprintf("%s/Rgeneric.R", Sys.getenv("MYRLIB")), chdir=TRUE);
#library(ggplot2);	#firstUpper

#
#	<§> implementation
#

#
#	<p> helper methods
#

parameterCombinationsTwins = function(specification, parameters, twins) {
	pars = strsplit(twins, ".", fixed = T)[[1]];
	N = length(pars);
	M = length(parameters[[pars[1]]]);	# assume equal length here
	df = data.frame(matrix(1:M, ncol = N, nrow = M));
	names(df) = pars;
	df
}

parameterCombinations = function(specification, parameters) {
	# <p> initialization
	parCnts = lapply(parameters, length);

	# <p> handle constraints (<A> must not overlap)
	if (!is.null(specification$constraints)) {
		parsC = lapply(names(specification$constraints), function(c) {
			fn = get(con("parameterCombinations", firstUpper(specification$constraints[[c]]$type)));
			cs = fn(specification, parameters, c);
			cs
		})
		names(parsC) = names(specification$constraints);
	} else parsC = list();

	# <p> add remaining parameters
	parsF = if (!is.null(specification$constraints)) {
		parameters[-unlist(sapply(names(specification$constraints), function(p) {
			pars = strsplit(p, ".", fixed = T)[[1]];
			idcs = which.indeces(pars, parameters);
			idcs
		}))]
	} else parameters;
	parsF = lapply(parsF, function(p)1:length(p));
	parsA = c(parsC, parsF);

	# <p> construct natural joint: unconstraint combinations
	df = data.frame(..dummy = 1);
	for (i in 1:length(parsA)) {
		df = merge(df, parsA[i]);
	}
	df = df[, -1];

	# <p> cleanup (names of df)
	ns = unlist(lapply(parsC, function(p)names(p)));
	ns = c(ns, names(parsF));
	names(df) = ns;

	df
}

#	gIndex: global index for reference purposes
#		lists are interpolated with arrays such that the name of the array
#		becomes embedded as list element
collapseParameters = function(collapsingGroups, parameters, indeces, gIndex) {
	iNs = names(indeces);
	pars = lapply(collapsingGroups, function(g) {
#		p = unlist.n(sapply(g$names, function(nm){
#			as.list(parameters[[nm]][indeces[[nm]]])
#		}), firstDef(g$collapse, 0));
		p = unlist.n(lapply(g, function(nm){
			po = parameters[[nm]][[indeces[[nm]]]];	# parameter object
			if (!is.list(po)) {
				po = list(po);
				names(po) = nm;
			}
			po
		}), 1);
		p
	});
	#if (is.list(pars$system)) pars$system$globalIndex = gIndex;
	pars
}

#
#	<p> generic methods
#

parameterIteration = function(s, order = NULL, reverse = F) {
	o = firstDef(order, 1:dim(s@combinations)[1], .dfInterpolate = F);
	#order.df(s@combinations, names(s@parameters), reverse);
	ps = lapply(o, function(i) {
		p = collapseParameters(s@specification$collapse, s@parameters, as.list(s@combinations[i, ]), i);
		p
	});
	i = list(parameters = ps, order = o);
	i
}

# i is given in canonical ordering of parameters
simulationFile = function(s, i) {
	spec = s@specification;
	pars = parameterIteration(s);	# canonical ordering
	digits = ceiling(log10(length(pars$order)));	# digits needed for enumeration
	filename = sprintf("%s/%s-%0*d.RData", spec$resultsDir, spec$name, digits, i);
	filename
}

#	needs: spec$cluster(hosts, type), spec$resultsFile|spec$name, spec$simulationFunction
runIterationCluster = function(s, order = NULL, reverse = F) {
	# <p> initialization
	spec = merge.lists(list(doSave = T, delaySave = F, local = F), s@specification);
	simulationPars = parameterIteration(s, order = order, reverse = reverse);

	# <p> initialize
	if (!is.null(spec$init)) {	eval(parse(text = spec$init)); }
	f = get(spec$simulationFunction);

	# <p> iteration function
	clf = function(i, simulationPars, ...){
		p = simulationPars$parameters[[i]];
		t0 = sum(proc.time()[3]);
		sim = try(f(p, ...));
		t1 = sum(proc.time()[3]) - t0;
		if (class(sim) != "try-error" & spec$doSave & !spec$delaySave) {
			save(sim, file = simulationFile(s, simulationPars$order[i]));
		}
		r = list(
			time = t1,
			parameters = p,
			result = ifelse(spec$delaySave, sim, class(sim) != "try-error")
		);
		r
	};

	if (!spec$local) {
		# <p> make cluster
		library("snow");
		c = spec$cluster;
		hosts = if (is.null(c$hosts)) rep("localhost", 8) else c$hosts;	#<A> cave vectors
		cl = makeCluster(hosts, type = firstDef(c$type, "SOCK"));
		clusterSetupRNG(cl);
	
		# <p> cluster intitalizations
		if (!is.null(c$source)) {
			textSource = sprintf("clusterEvalQ(cl, { %s })",
				paste(c(sapply(c$source, function(s)sprintf("source(\"%s\")", s)), ""), collapse = "; ")
			);
			eval(parse(text = textSource));
		}
		clusterExport(cl, spec$simulationFunction);
	}

	# <p> iterate
	textExec = sprintf(
		"%s 1:length(simulationPars$parameters), clf, simulationPars = simulationPars, %s%s;",
			ifelse(spec$local, "lapply(", "clusterApplyLB(cl,"), paste(spec$args, collapse = ", "), ")"
	);
	print(textExec);
	simulations = eval(parse(text = textExec));
	#print(simulations);

	# <p> finish up
	if (!spec$local) stopCluster(cl)

	if (spec$delaySave) for (i in 1:length(simulations)) {
		sim = simulations[[i]];
		if (class(sim) != "try-error" & spec$doSave) save(sim, file = simulationFile(s, i, pars$order[i]));
	}
	simulationPars
}

runIterationPlain = function(s, order = NULL, reverse = F) {
	# <p> initialization
	spec = s@specification;
	pars = parameterIteration(s, order = order, reverse = reverse);

	f = get(spec$simulationFunction);
	# <p> iterate
	simulations = lapply(1:length(pars$parameters), function(i){
		p = pars$parameters[[i]];
		t0 = sum(proc.time()[1:2]);
		sim = try(f(p));
		t1 = sum(proc.time()[1:2]) - t0;
		if (class(sim) != "try-error" & spec$doSave & !spec$delaySave) {
			save(sim, file = simulationFile(s, pars$order[i]));
		}
		r = list(
			time = t1,
			parameters = p,
			result = ifelse(spec$delaySave, sim, class(sim) != "try-error"));
		r
	});

	if (spec$delaySave) for (i in 1:length(simulations)) {
		sim = simulations[[i]];
		if (class(sim) != "try-error" & spec$doSave) save(sim, file = simulationFile(s, i, pars$order[i]));
	}
	pars
}

summarizeIteration = function(s, order = NULL, reverse = F) {
	# <p> initialization
	spec = s@specification;
	pars = parameterIteration(s, order = order, reverse = reverse);
print(pars);
	f = if (is.null(spec$summaryFunctionSingle)) NULL else get(spec$summaryFunctionSingle);

	simulations = lapply(1:length(pars$order), function(i) {
		parIndex = pars$order[i];
		file = simulationFile(s, parIndex);
		sim = if (file.exists(file)) { get(load(file)[1]) } else NULL;
		# <%><N> interpolate old simulations
		#if (length(sim) == 1) sim = sim[[1]];
		r = if (is.null(f)){ NA } else f(s, sim, pars$parameters[[parIndex]]);
		r
	});

	r = NULL;
	if (!is.null(spec$summaryFunction)) {
		summary = get(spec$summaryFunction);
		r = summary(s, simulations, pars$order, pars);
	}
	r
}

runIteration = function(s, order = NULL, reverse = F) {
	spec = s@specification;
	methodName = sprintf("runIteration%s", firstUpper(firstDef(spec$iterationMethod, "plain")));
	method = get(methodName);
	Log(sprintf('Rsimulation: %s', methodName), 2);
	method(s, order, reverse);
}

#
#	<p> class
#

#	specification contains restrictions on parameter combinations, grouping
#	restrictions:
#		twins:	pair parameters as listed (e.g. model simulation, estimation)
#	grouping: build final parameters by merging sublists
#		conventional group:
#			system: parameters other than involved in statistical concepts
#			model: specification of the model
#			parameters: model parameters

setClass("Rsimulation",
	representation(specification = "list", parameters = "list", combinations = "data.frame",
		mode = "character"),
	prototype(specification = list(), parameters = list(), combinations = data.frame(), mode = NULL)
);

setMethod("initialize", "Rsimulation", function(.Object, simulationName, mode = NULL) {
	s = get(simulationName);
	specification = merge.lists(list(doSave = T, delaySave = F), s$specification);
	specification$name = simulationName;
	parameters = s$parameters;

	if (specification$needsMode & is.null(mode)) {
		stop(con("Need simulation mode [",
			paste(names(specification$mode), collapse = ", "), "]"));
	}
	if (!is.null(mode)) {
		specification = merge.lists(specification, specification$mode[[mode]]);
	}
	.Object@mode = mode;
	.Object@specification = specification;
	.Object@parameters = parameters;
	.Object@combinations = parameterCombinations(specification, parameters);
	.Object
});


