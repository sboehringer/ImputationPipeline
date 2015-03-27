#
#	RlabExp.R
#Thu Aug 30 14:48:14 EDT 2007

# tb run in ~/tmp/src/Rlab

system("cd ~/src/Rprivate ; ./exportR.sh");
source("RgenericAll.R"); loadLibraries();

if (0) {
	df = merge(merge(1:3, 4:5), data.frame(z=6:7));
	c = list(1, 2, "z");
	orderText = sprintf("order(%s)",
		paste(sapply(c, function(i) {
			if (is.numeric(i)) sprintf("df[, %d]", i) else sprintf("df$%s", i) }), collapse = ", "
		)
	);
	o = eval(parse(text = orderText));
	print(list(text = orderText, order = o, df=df));
}


# R reporting

if (0) {
	df = data.frame(name = c("tux", "pax", "pux"), number = 1:3);
	print(df);

	r = Rreporter(path = "/tmp/downloads/reportingTest.txt", to.screen = F);
	r$report.data.frame(df, digits = c(NA, 2));
	r$finalize();
}


# mergeDict2string

if (0) {
	s = "some TEXT to interpolate ANOTHER 1";
	d = list(TEXT = 1, ANOTHER = 2);
	print(mergeDictToString(d, s));
}

# report.data.frame.toString

if (0) {
	df = data.frame(name = c("tux", "pax", "pux"), number = 1:3);
	t = report.data.frame.toString(df, ignoreRowNames = T);
	cat(t);
}

# R reporting S4

if (0) {
	df = data.frame(name = c("tux", "pax", "pux"), number = 1:3);

	r = new("Rreporter");
	report.data.frame(r, df, ignoreRowNames = T);
	r
}

# R simulation

if (0) {
	test = list(
	specification = list(
		#constraints = list(sim.est = list(type = "twins")),
		collapse = list(
			system = c("base"),
			model = c("sim", "est", "zeta"),
			lhPars = c("beta", "mu")
		),
		iterationMethod = "cluster",
		cluster = list(hosts = rep("localhost", 8), type = "SOCK",
			source = c("RgenericAll.R")
		),
		resultsDir = "/tmp/results",
		mode = list(
			simulate = list( tmp.file = "simulate.txt", simulationFunction = "simulate", doSave = T ),
			summarize = list( tmp.file = "summary.pdf", summaryFunction = "summarize" )
		),
		needsMode = T
	),
	parameters = list(
		base = list(list( tmp.dir = "/tmp/hello.world/", message = "massage" )),
		sim = c("a", "d", "r"),
		est = c("ae", "de", "re"),
		beta = c(0, .5, 1),
		mu = c(-2, -1, 0),
		zeta = list(list(new = 1, old =2))
	)
	);

	simulate = function(p) { p$lhPars$beta * 2 }
	summarize = function(s, sims, order) {
		r = t(sapply(1:length(order), function(i){
			list(order[i], sims[[i]]$result)
		}));
		r
	}

	#print(r$parameters[[1]]);

	sim = new("Rsimulation", "test", mode = "simulate");
	pars = parameterIteration(sim);
	r = runIteration(sim, c("beta", "sim", "est", "zeta", "mu"), reverse = T);

	sum = new("Rsimulation", "test", mode = "summarize");
	r = summarizeIteration(sum, c("beta", "sim", "est", "zeta", "mu"));
	print(r);	
}

# unlist.n

if (0) {
	l = list(a = list(b = 2), c = list(d = 3));
	print(unlist.n(l, 1, T));
}

# swig

if (0) {
	module = "myLikelihood";
	c.interface = "
		%module myLikelihood
		%inline %{
		extern double complexCalculation(int);
		%}
	";
	c.function = "
	#include <math.h>
	double	complexCalculation(int j) {
		long double	f = exp(j + log(j));
		return f;
	}
	";

	dir = tempdir();
	ifile = sprintf("%s/%s.%s", dir, module, "i");
	base = splitPath(ifile)$fullbase;
	writeFile(ifile, c.interface);
	cfile = sprintf("%s.c", base);
	writeFile(cfile, c.function);
	print(list(i = ifile, c = cfile, so = sprintf("%s.so", base)));
	system(sprintf("swig -r %s", ifile));

	system(sprintf("cd %s ; gcc -c -fpic %s.c %s_wrap.c -I/usr/local/lib64/R/include -lm",
		splitPath(ifile)$dir, base, base));
	system(sprintf("cd %s ; gcc -shared %s.o %s_wrap.o -o %s.so", splitPath(ifile)$dir, base, base, base));
	#dyn.unload(sprintf("%s.so", base));
	dyn.load(sprintf("%s.so", base));
	source(sprintf("%s/myLikelihood.R", splitPath(ifile)$dir));

	r = t(sapply(-5:10, function(j) {
		complexCalculation(j)
	}));
	print(r);
}

if (0) {
	swigIt("extern double complexCalculation(int)", "
	#include <math.h>
	double	complexCalculation(int j) {
		long double	f = exp(j + log(j));
		return f;
	}
	");

	r = t(sapply(-5:10, function(j) {
		complexCalculation(j)
	}));
	print(r);

}

# bit fields

if (0) {
	source("Rbit.R");
	
}

# list.kp
if (0) {
	l = list(list(list(a = 10), list(a = 15, b = -1)));
	print(list.kp(l, "*$a", do.unlist = T, null2na = T));
	print(list.kp(l, "*$b", do.unlist = T, null2na = T));
}

#propertyList
if (0) {
	print(stringFromProperty(list(a= 1), list(forceVectors = '.a')));
}

# path.absolute: interpolate '~/'
if (0) {
	print(path.absolute('abc'));
	print(path.absolute('~/abc'));
	print(path.absolute('/tmp/abc'));
}

# prefix capability of readFile
if (0) {
	l = list(
		list(str = 'secret message', path = '/tmp/test/abc'),
		list(str = 'secret message1', path = '/tmp/test/deeper/abc'),
		list(path = 'abc', prefixes = c('/tmp/', '/tmp/test/', '/tmp/test/deeper/')),
		list(path = 'abc', prefixes = c('/tmp/', '/tmp/test/deeper/', '/tmp/test/'))
	);
	for (e in l) with (e, {
		if (splitPath(path)$isAbsolute) writeFile(path, str, mkpath = T);
		print(path);
		print(readFile(path, prefixes = e$prefixes));
	});

}

# list.kp
if (0) {
	input = '~/tmp/pipeline/test_imputation/imputation_04/files.spec';
	i = propertyFromString(readFile(input));
	r = list.kp(i$files, 'name', do.unlist = T);
	print(r[1:5]);
	print(i$files[1:3]);

	# two equivalent extractions
	r = list.kpr(i, 'files$*$name', unlist.pat = 'T$T$F');
	print(r[1:5]);
	r = list.kpr(i$files, '*$name', unlist.pat = 'T$F');
	print(r[1:5]);

	# leave as list
	r = list.kpr(i$files, '*$name', unlist.pat = 'F$F');
	print(r[1:5]);
}

# checkpointing

checkpointed = function(N = 10) {
	a = 10;
	r = lapply(1:N, function(i)i);
	a = a + 1;
	r
}

parallelize = function(f) {
	e = as.expression(as.list(checkpointed)[[2]])[[1]];	# function body
}

#
#	new parallelization strategy:
#		auto-parallelize Lapply
#		use pipeline description to parallelize/piplene series of functions


if (0) {
	source('Rparallel.R');
	Lapply_local = F;
	r = Lapply_probe(parallel0, Lapply_config = list(max_depth = 5, parallel_count = 1e2));
	print(r);

	r = Lapply_probe(parallel0.1, Lapply_config = list(max_depth = 5, parallel_count = 1e3));
	print(r);
}

if (0) {
	e = withCallingHandlers(stop(Lapply_error()), error = function(e) {
		#print(str(e$message));
		print('attributes');
		print(attr(e, 'error'));
		#print(f);
	});
}

if (0) {
	r = Try(Throw('some error', Lapply_error()), catch = list(
		Lapply_error = function(e)e
	));
	print(str(r));
}

if (0) {
	source('Rparallel.R');
	source('Rparallel.back.R');
	be = new('ParallelizeBackend');
	print(isSynchroneous(be));
}

if (T) {
	source('Rparallel.R');
	source('RlabParallel.R');
	Lapply_config = list(max_depth = 5, parallel_count = 24, offline = T, backends = list(
		snow = list(
			localNodes = 1, splitN = 1, sourceFiles = c('RgenericAll.R', 'Rgenetics.R', 'RlabParallel.R')
		),
		local = list(
			path = sprintf('%s/tmp/parallelize', Sys.getenv('HOME'))
		),
		`ogs-1` = list(
			backend = 'OGS',
			freezerClass = 'LapplyPersistentFreezer',
			sourceFiles = c('RgenericAll.R', 'RlabParallel.R', 'Rgenetics.R'),
			stateDir = sprintf('%s/tmp/remote', Sys.getenv('HOME')),
			qsubOptions = sprintf('--queue all.q --logLevel %d', Log.level() - 2),
			doNotReschedulde = T
		),
		`ogs-2` = list(
			backend = 'OGS',
			freezerClass = 'LapplyPersistentFreezer',
			sourceFiles = c('RgenericAll.R', 'RlabParallel.R', 'Rgenetics.R'),
			stateDir = sprintf('%s/tmp/remote', Sys.getenv('HOME')),
			qsubOptions = sprintf('--queue all.q --logLevel %d', Log.level() - 2),
			doSaveResult = T
		),
		`ogs-3` = list(
			backend = 'OGSremote',
			remote = 'pingu@localhost:tmp/remote/test',
			freezerClass = 'LapplyPersistentFreezer',
			sourceFiles = c('RgenericAll.R', 'RlabParallel.R', 'Rgenetics.R'),
			stateDir = sprintf('%s/tmp/remote/test_local', Sys.getenv('HOME')),
			qsubOptions = sprintf('--queue all.q --logLevel %d', Log.level() - 2),
			doSaveResult = T
		)
	));
	# <p> initialize
	Lapply_initialize(Lapply_config = Lapply_config, backend = 'local');
	Lapply_local = F;
}

#
#	intermittend testing of file helpers
#
if (0) {
	print(t(list2df(splitPath('my/path', ssh = T))));
	print(t(list2df(splitPath('host:my/path', ssh = T))));
	print(t(list2df(splitPath('user@host:my/path', ssh = T))));
	Save(Lapply__, file = 'pingu@localhost:tmp/remote/remotesave.RData');
	r = Load(file = 'pingu@localhost:tmp/remote/remotesave.RData');
	print(r);

}

if (0) {
	# test non-parallel execution
	r = parallelize(function()41, Lapply_config = list(max_depth = 5, parallel_count = 1e2));

}

if (0) {
	# <p> first round of parallelization
	r = parallelize(parallel0.2);
	print(r);
	r = parallelize(parallel0.2);
	print(r);
	# <p> local computation
	Lapply = lapply;
	parallelize = function(.f, ..., Lapply_config = NULL).f(...);
	r = parallelize(parallel0.2, Lapply_config = list(max_depth = 5, parallel_count = 1e2));
	print(r);
}

if (0) {
	Lapply_initialize(Lapply_config = Lapply_config, backend = 'snow');
	repeat {
		r = parallelize(parallel0.2);
		if (all(class(r) != 'Lapply_error')) break;
	}
	print(r);
}

if (0) {
	System('echo hello world > ~/tmp/remote/test.txt', pattern = 'ssh', ssh_host = 'localhost');

}
# nested patterns
if (0) {
	print(File.exists('tmp/remote/test.txt', 'localhost'));
	print(File.exists('tmp/remote/test.txt-1', 'localhost'));
	print(tempFileName('localhost:tmp/remote/tempfile'));
	r = System('echo hello world > ~/tmp/remote/test-qsub.txt',
		patterns = c('cwd', 'qsub', 'ssh'),
			ssh_host = 'localhost', qsubOptions = '--queue all.q', printOnly = F, cwd = '~/tmp/remote' );
	print(r);
}

if (0) {
	f1 = function(e){list(r = e * 1:10)};
	path = sprintf('%s/tmp/remote/t1.RData', Sys.getenv('HOME'));
	path = freezeCall(f1, 2, file = path);
	thawCall.this = function(...)thawCall(file = path, ...);
	thawCode = deparse(thawCall.this);
	print(thawCode);

	code2 = call('thawCall', file = path);
	print(code2);
	eval(code2);
}

if (0) {
	f1 = function(e){list(r = e * 1:10)};
	path = sprintf('%s/tmp/remote/t1.RData', Sys.getenv('HOME'));
	path = freezeCall(f1, 2, file = path, save_output = T);
	wrap = frozenCallWrap(path);
	r0 = System(wrap);
	print(r0);
	r = frozenCallResults(path);
	print(r);
	mycall = get(load('/home/pingu/tmp/remote/t1.RData')[[1]]);
	r1 = eval(get(load(path)[[1]]));
	print(r1);
}


if (0) {
	Log.setLevel(4);
	if (T) {
	# gold standard
	parallelize_initialize(Lapply_config = Lapply_config, backend = 'local');
	r = parallelize_call(parallel0.2());
	print(r);
	r = parallelize(parallel0.2);
	print(r);
	}

	if (F) {
	parallelize_initialize(Lapply_config = Lapply_config, backend = 'ogs-1');
	r = parallelize(parallel0.2);
	print(r);
	# assume doNotReschedulde == T
	Sys.sleep(30);
	System('qstat');
	r = parallelize(parallel0.2);
	print(r);
	Sys.sleep(30);
	System('qstat');
	r = parallelize(parallel0.2);
	print(r);
	}

	if (F) {
	parallelize_initialize(Lapply_config = Lapply_config, backend = 'ogs-2');
	r = parallelize(parallel0.2);
	repeat {
		p = pollParallelization(Lapply_backend__);
		cat(p$message);
		if (!p$continue) break;
		Sys.sleep(10);
	}
	r = parallelize_getResult();
	print(r);
	}

	if (T) {
	parallelize_initialize(Lapply_config = Lapply_config, backend = 'ogs-3', force_rerun = T);
	repeat {
		r = parallelize(parallel0.2);
		if (!is.null(r)) break;
		Sys.sleep(5);
	}
	if (0) repeat {
		p = pollParallelization(Lapply_backend__);
		cat(p$message);
		if (!p$continue) break;
		Sys.sleep(5);
	}
	#r = getResult(Lapply_backend__);
	print(r);
	}
}

if (0) {
	Log.setLevel(5);
	parallelize_initialize(Lapply_config = Lapply_config, backend = 'local');
	r = parallelize(parallel0.2);
}
if (0) {
	Log.setLevel(5);
	parallelize_initialize(Lapply_config = Lapply_config, backend = 'ogs-2');
	r = parallelize(parallel0.2);
}

if (0) {
	parallelize_initialize(Lapply_config = Lapply_config, backend = 'ogs-3', force_rerun = T);
	r = parallelize(parallel0.2);
}

#
#	<p> benchmark
#

benchmark.timed = function(.f, ..., N__ = 1e1) {
	t0 = Sys.time();
	for (i in 1:N__) {
		r = .f(...);
	}
	t1 = Sys.time();
	r = list(time = (t1 - t0)/N__, lastResult = r, t0 = t0, t1 = t1);
	print(r$time);
	print(r$t0);
	print(r$t1);
	r
}

if (0) {
	Log.setLevel(4);
	sink(sprintf('%s/Documents/Publikationen/2012-08-Rparallel/Resources/benchmark.txt', Sys.getenv('HOME')));
	print(Lapply_config);

	parallelize_setEnable(F);
	benchmark.timed(parallel0.2, N__ = 1e3);

	benchmark.timed(function(){
		parallelize_initialize(Lapply_config = Lapply_config, backend = 'local');
		r = parallelize(parallel0.2)
	}, N__ = 1e2);

	benchmark.timed(function(){
		parallelize_initialize(Lapply_config = Lapply_config, backend = 'snow');
		r = parallelize(parallel0.2)
	}, N__ = 1e1);

	benchmark.timed(function(){
		parallelize_initialize(Lapply_config = Lapply_config, backend = 'ogs-3', force_rerun = T);
		repeat {
			r = parallelize(parallel0.2);
			if (!is.null(r)) break;
			Sys.sleep(10);
		}
		r
	}, N__ = 1e1);
	sink();
}

if (0) {
	Lapply = lapply;
	r = Sapply_backup(1:10, function(x)rep(x, x));
	print(r);
	r = Sapply_backup(1:10, function(x)x^2);
	print(r);

	print('sum');
	m0 = matrix(1:16, ncol = 4, byrow = T);
	r = Apply_backup(m0, 1, sum);
	print(r);
	r = apply(m0, 1, sum);
	print(r);

	print('sum');
	r = Apply_backup(m0, 2, sum);
	print(r);
	r = apply(m0, 2, sum);
	print(r);

	print('square');
	r = Apply_backup(m0, 1:2, function(x)x^2);
	print(r);
	r = apply(m0, 1:2, function(x)x^2);
	print(r);

	print('list');
	r = Apply_backup(m0, 1:2, function(x)rep(x, x));
	print(r);
	r = apply(m0, 1:2, function(x)rep(x, x));
	print(r);
}

if (0) {
	source('Rgenetics.R');
	parallelize_initialize(Lapply_config = Lapply_config, backend = 'snow');
	models = list(
		mu = -3:-1,
		af = c(.01, .05, .3),
		beta = log(seq(1.1, 1.8, length.out = 3)),
		scoresSim = setdiff(names(scoresStd), 'gen'), scoresTest = setdiff(names(scoresStd), 'gen')
	);
	power = function(mu, af, beta, scoresSim, scoresTest, M, N, ...) {
		powerLocusCaseControl(M = M, N = N, af = af, mu = mu, beta = beta,
			scoresSim = scoresStd[[scoresSim]], scoresTest = scoresStd[[scoresTest]], ...)
	};
	r = parallelize_call(iterateModels(models, power, M = 5e3, N = c(1e3, 1e3)));

}

parallelPower = function(backend, M) {
	print(M);
	parallelize_initialize(Lapply_config = Lapply_config, backend = backend, force_rerun = T);
	models = list(
		mu = -3:-1,
		af = c(.01, .05, .3),
		beta = log(seq(1.1, 1.8, length.out = 3)),
		scoresSim = setdiff(names(scoresStd), 'gen'), scoresTest = setdiff(names(scoresStd), 'gen')
	);
	power = function(mu, af, beta, scoresSim, scoresTest, M, N, ...) {
		powerLocusCaseControl(M = M, N = N, af = af, mu = mu, beta = beta,
			scoresSim = scoresStd[[scoresSim]], scoresTest = scoresStd[[scoresTest]], ...)
	};
	r = parallelize_call(iterateModels(models, power, M = M, N = c(1e3, 1e3)));
	#r = parallelize_call(iterateModels(models, power, M = M, N = c(1e3, 1e3), lapply__ = clapply));
}

if (0) {
	# backend = 'snow', parallel_count == 24, localNodes == 8
	#Time difference of 1.945245 mins
	# backend = 'snow', parallel_count == 8, localNodes == 4
	#Time difference of 2.295961 mins
	# backend = 'snow', parallel_count == 4, localNodes == 2
	#Time difference of 3.955571 mins
	# backend = 'snow', parallel_count == 1, localNodes == 1
	#Time difference of 7.453227 mins
	# backend = 'snow', parallel_count == 1, localNodes == 1, splitN == 1
	#Time difference of 7.406333 mins
	benchmark.timed(parallelPower, backend = 'snow', M = 5e1, N__ = 2);
}
if (0) {
	#Time difference of 5.036571 mins
	benchmark.timed(parallelPower, backend = 'off',  M = 5e1, N__ = 2);
}
if (0) {
	source('Rgenetics.R');
	#Time difference of 17.54109 mins
	benchmark.timed(parallelPower, backend = 'snow', M = 5e2, N__ = 3);
	#Time difference of 10.03558 mins
	benchmark.timed(parallelPower, backend = 'off',  M = 1e2, N__ = 3);
}
if (0) {
	source('Rgenetics.R');
	# Time difference of 2.918331 hours
	# == 175.0999 mins
	benchmark.timed(parallelPower, backend = 'snow', M = 5e3, N__ = 1);
}

if (0) {
	source('Rgenetics.R');
	Log.setLevel(3);
	#Time difference of 2.499778 hours == 150 min
	# backend = 'ogs-3', parallel_count = 24, M = 5e2 
	r0 = benchmark.timed(parallelPower, backend = 'ogs-3', M = 5e2, N__ = 1);
	r1 = benchmark.timed(parallelPower, backend = 'off',   M = 5e2, N__ = 1);
}

directExpression = function(x = pi, N = 1e4)
	sum(sapply(1:N, function(i)(cosh(exp(pi))/cosh(exp(pi + 1)))))
evaledExpression = function(x = pi, N = 1e4) eval(directExpression(x = x, N = N));

if (0) {
	source('Rgenetics.R');
	Log.setLevel(5);
	#Time difference of 2.499778 hours == 150 min
	# backend = 'ogs-3', parallel_count = 24, M = 5e2 
	r0 = benchmark.timed(directExpression, N__ = 100);
	r1 = benchmark.timed(evaledExpression, N__ = 100);
}

# RNG


if (0) {
	i = as.character(Sys.time());
	print(i);
	sig = substr(md5sumString(i), 1, 8);
	h = hex2int(sig);
	h = hex2int('FFFFFFFF');
	h = hex2int('80000000');
	h = hex2ints(md5sumString(i));
}


if (0) {
	r = fetchRegexpr('a(..*?)b(..*?)', 'a1b1 a2b2', capturesAll = T);
	print(r);
	
}
if (0) {
	DOC = '
DOCUMENTATION_BEGIN:myFunction
\\section{intro}
comment
DOCUMENTATION_END

DOCUMENTATION_BEGIN:myFunction2
\\section{intro}
DOCUMENTATION_END
';
	r = getPatternFromStrings(DOC, '(?s)(?:\\nDOCUMENTATION_BEGIN:)([^\\n]+)\\n(.*?)(?:\\nDOCUMENTATION_END\\n)');
	print(r);
	
}

RdocumentationForObjects = function(items, envir, unparser = function(item, envir)item) {
	files = suppressMessages({
		sapply(items, function(item)unparser(item, envir));
	});
	docs = lapply(files, readFile);
	names(docs) = sapply(files, function(f)splitPath(f)$base);
	docs
}
RdocumentationForFunctions = function(items, envir) {
	docs = RdocumentationForObjects(items, envir, unparser = function(item, envir) {
		file = file.path(tempdir(), sprintf("%s.Rd", item));
		prompt(get(item, envir = envir), name = item, filename = file);
		file
	});
	docs
}
RdocumentationForClasses = function(items, envir) {
	docs = RdocumentationForObjects(items, envir, unparser = function(item, envir) {
		file = file.path(tempdir(), sprintf("%s-class.Rd", item));
		methods::promptClass(item, filename = file, where = envir);
		file
	});
	docs
}
RdocumentationForMethods = function(items, envir) {
	docs = RdocumentationForObjects(items, envir, unparser = function(item, envir) {
		file = file.path(tempdir(), sprintf("%s-methods.Rd", item));
		methods::promptMethods(item, filename = file, findMethods(item, where = envir));
		file
	});
	docs
}


# code from packages.skeleton
objectsFromCodeFiles = function(R_files, packageName = 'generic') {
	e = new.env(hash = T);
	methods::setPackageName(packageName, e);
	for (f in R_files) sys.source(f, envir = e);
	classes = getClasses(e);
	methods = getGenerics(e);
	others = ls(e, all.names = T);
	others = others[grep('^\\.', others, invert = T)];

	r = list(envir = e, classes = classes, methods = methods,
		others = setdiff(setdiff(others, classes), methods));
	r
}

RdocumentationSkeleton = function(R_files, output = NULL, packageName = 'generic') {
	os = objectsFromCodeFiles(R_files, packageName = packageName);
	docs = c(
		RdocumentationForFunctions(os$others, os$envir),
		RdocumentationForClasses(os$classes, os$envir),
		RdocumentationForMethods(os$methods, os$envir)
	);

	doc = join(nlapply(docs, function(n) {
		sprintf("\nDOCUMENTATION_BEGIN:%s\n%s\nDOCUMENTATION_END\n", n, docs[[n]])
	}), "\n");
	if (!is.null(output)) {
		if (File.exists(output)) {
			Log(sprintf("Move away file '%s' before writing new skeleton", output), 2);
		} else {
			writeFile(output, doc);
		}
	}
	doc
}

writeRdocumentationToDir = function(pathesIn, pathOut) {
	doc = sapply(pathesIn, readFile, USE.NAMES = F);
	r = unlist.n(getPatternFromStrings(doc, '(?s)(?:\\nDOCUMENTATION_BEGIN:)([^\\n]+)\\n(.*?)(?:\\nDOCUMENTATION_END\\n)'), 1);
	Dir.create(pathOut, recursive = T);
	nlapply(r, function(n) {
		output = file.path(pathOut, sprintf('%s.Rd', n));
		Log(sprintf('Writing to %s', output), 3);
		writeFile(output, r[[n]]);
	});
	names(r)
}


if (0) {
	objs = objectsFromCodeFiles('Rparallel.back.R', 'parallelize.dynamic');
	print(objs);
	docFile = sprintf('%s/tmp/docOut.Rd', Sys.getenv('HOME'));
	docDir = sprintf('%s/src/Rpackages/parallelize.dynamic/parallelize.dynamic/man', Sys.getenv('HOME'));
	docs = RdocumentationSkeleton('Rparallel.back.R', 'parallelize.dynamic', output = docFile);
	#print(docs);
	writeRdocumentationToDir(docFile, docDir);
}

if (0) {
	# code to be parallelized
	parallel8 = function(e) log(1:e) %*% log(1:e);
	parallel2 = function(e) rep(e, e) %*% 1:e * 1:e;
	parallel1 = function(e) Lapply(rep(e, 15), parallel2);
	parallel0 = function() {
		r = sapply(Lapply(1:50, parallel1),
		function(e){sum(as.vector(unlist(e)))});
		r0 = Lapply(1:49, parallel8);
		r
	}

	# run the code
	codeFile = tempcodefile(c(parallel0, parallel1, parallel2, parallel8));

	Lapply_config = list(max_depth = 5, parallel_count = 24, offline = F,
	backends = list(
		snow = list(localNodes = 2, splitN = 1, sourceFiles = codeFile),
		local = list(
		path = sprintf('%s/tmp/parallelize', Sys.getenv('HOME'))
		)
	));
	# !Warning! creates the directory $HOME/tmp/parallelize
	Lapply_initialize(Lapply_config = Lapply_config, backend = 'local');

	# put on SNOW cluster
	Lapply_initialize(Lapply_config, backend = 'snow');
	r = parallelize(parallel0);
	print(r);
}

if (0) {
	Log("hello world", 2);
}

if (0) {
	delayed_object = list(test = 1, abc = list(x = 5, z = rep(5, 1e6)));
	delayed_path = sprintf("%s/tmp/delayed_object.RData", Sys.getenv('HOME'));
	save(delayed_object, file = delayed_path);
	print(search());

	r = list(sapply(1:10, function(i)delayed_load(delayed_path)));
	r1 = thaw_object(r);
	print(gc());
	
	r = list(sapply(1:20, function(i)delayed_load(delayed_path)));
	r1 = thaw_object(r);
	print(gc());

	r1 = NULL;
	r = list(sapply(1:100, function(i){ delayed_load(delayed_path); 1 }));
	print(gc());
	browser();

	r1 = rep(delayed_object, 200);
	print(gc());
	print(search());
}

if (0) {
	R = 10;
	B = 2;
	f1 = function()B
	N <- function(p, r = R)( (2*qnorm(p, lower.tail = F)/r)^2 * (1-p)/p + f1() )
	e = environment_evaled(N, functions = T);
}

Substr = function(s, start, length, replacement) {
	if (missing(replacement)) return(substr(s, start, start + length - 1));
	start = c(start, nchar(s) + 1);
	l = sapply(seq_along(replacement), function(i)c(
		replacement[i],
		substr(s, start[i] + length[i], start[i + 1] - 1)
	));
	l = c(substr(s, 1, start[1] - 1), as.vector(l));
	r = join(as.vector(l), sep = '');
	r
}

if (0) {
	print(Substr("abc", c(2, 3), c(1, 1), c("def", 'jkl')));
	print(Substr("abcdef", c(2, 3, 5), c(1, 1, 1), c("123", '456', '789')));
	print(Substr("abcdef", c(1, 3, 5), c(1, 1, 1), c("123", '456', '789')));
	print(Substr("abcdef", c(1, 3, 5), c(0, 1, 0), c("123", '456', '789')));
	#print(Substr("abc", 2, 1));
}

Sprintf = sprintd = function(fmt, dict, ...) {
	extraValues = list(...);
	re = '(?x)(?:
		(?:^|[^%]|(?:%%)+)\\K
		[%]
			(?:[{]([^{}\\*\'"]*)[}])?
		((?:[-]?[*\\d]*[.][*\\d]*)?(?:[sdfe]|))(?=[^%sdfegG]|$)
	)';
	r = fetchRegexpr(re, fmt, capturesAll = T, returnMatchPositions = T);
	fmts = sapply(r$match, function(m)sprintf('%%%s', ifelse(m[2] == '', 's', m[2])));
	fmt1 = Substr(fmt, r$positions, attr(r$positions, 'match.length'), fmts);

	keys = sapply(r$match, function(i)i[1]);
	nonKeysI = cumsum(keys == '');	# indeces of values not passed by name
	print(nonKeysI);
	
	sprintfValues = lapply(seq_along(keys), function(i)
		ifelse(keys[i] == '', extraValues[[nonKeysI[i]]],
			firstDef(dict[[keys[i]]], rget(keys[i], default = '__no value__'), pos = -2)));
	s = do.call(sprintf, c(list(fmt = fmt1), sprintfValues));
	s
}


if (0) {
	fmt = '%{text}s %.3s %-.2s %{key} %% %%s';
	re = '(?x)(?:
		(?:^|[^%]|(?:%%)+)\\K
		[%]
			(?:[{]([^{}\\*\'"]*)[}])?
		((?:[-]?[*\\d]*[.][*\\d]*)?(?:[sdfe]|))(?=[^%sdfegG]|$)
	)';
	r = fetchRegexpr(re, fmt, capturesAll = T, returnMatchPositions = T);
	fmts = sapply(r$match, function(m)sprintf('%%%s', ifelse(m[2] == '', 's', m[2])));
	fmt1 = Substr(fmt, r$positions, attr(r$positions, 'match.length'), fmts);

	key = 'arbitrary value';

	keys = sapply(r$match, function(i)i[1]);
	nonKeysI = cumsum(keys == '');	# indeces of values not passed by name
	extraValues = list('ABC', 'DEF');
	dict = list(text = 'abc');

	sprintfValues = sapply(seq_along(keys), function(i)
		ifelse(keys[i] == '', extraValues[[nonKeysI[i]]],
			firstDef(dict[[keys[i]]], rget(keys[i], default = '__no value__'))));
	s = do.call(sprintf, c(list(fmt = fmt1), sprintfValues));
}

if (0) {
	fmt = '%{text}s %.3s %{num}-.2e %{key} %% %%s';
	s = Sprintf(fmt, list(text = 'ABC', key = 'KEY', num = 2.3e-2), 'abc');
	print(s);
}

