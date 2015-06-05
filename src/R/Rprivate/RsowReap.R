#
#	<p> RsowReap.R
#Wed May  7 18:16:23 CEST 2014

# <p> Design
#	These classes are meant to implement several Sow/Reap patterns
#	Standard Pattern
#	r = Reap(expression, returnResult = T);
#	print(r$result);
#	print(r$yield);
#
#	AutoPrint sowed values, reap later
#	SowerAddReaper(auto_reaper = printRepeaper, logLevel = 4);
#	{ Sow(my_tag = 4, logLevel = 3); }
#	r = Reap();
#
#	for (i in 1:10) {
#		Sow(my_number = i); 
#		Sow(my_greeting = 'hello world');
#	}
#	# prints list of list w/ each entry beting list(my_number = i, my_greeting = ..)
#	print(Reap(stacked = T));
#
#	Sow to different categories
#	SowerSetCatcher(default = StackingSowCatcherClass);
#	SowerSetCatcher(exclusions = SowCatcherClass);
#	Sow(1);
#	Sow(individuals = 1:10, sow_field = 'exclusions');
#	Collect(union, sow_field = 'exclusions');	# do not remove


ReaperAbstractClass = setRefClass('ReaperAbstract',
	fields = list(),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		.self$initFields(...);
		.self
	},
	reap = function(...) { }
	#
	#	</p> methods
	#
	)
);
#ReaperAbstractClass$accessors(names(ReaperAbstractClass$fields()));

SowCatcherClass = setRefClass('SowCatcher', contains = 'ReaperAbstract',
	fields = list(
		auto_reapers = 'list',
		seeds = 'list'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		auto_reapers <<- list();
		seeds <<- list();
		.self$initFields(...);
		.self
	},
	sow_raw = function(seed) {
		for (r in c(.self, auto_reapers)) r$reap(seed);
	},
	sow = function(...) {
		.self$sow_raw(list(...)[1]);
	},
	reap = function(seed) {
		seeds <<- c(seeds, seed);
	},
	last_seed = function() {
		seeds[length(seeds)];
	},
	seed_count = function()length(seeds),
	Seeds = function(fields = NULL) {
		if (is.null(fields)) seeds else seeds[which.indeces(fields, names(seeds))]
	},
	set_seed_at = function(seed, pos) {
		seeds[pos] <<- seed;
		names(seeds)[pos] <<- names(seed);
		NULL
	},
	push_reaper = function(r) {
		auto_reapers <<- c(auto_reapers, r);
		NULL
	},
	register = function(ensemble, field)NULL,
	# <p> end a global SowReap session
	conclude = function()NULL
	#
	#	</p> methods
	#
	)
);
SowCatcherClass$accessors(names(SowCatcherClass$fields()));

SowCatcherPersistentClass = setRefClass('SowCatcherPersistent', contains = 'SowCatcher',
	fields = list(
		path = 'character',
		splitRe = 'character',
		cursor = 'integer'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		splitRe <<- '';
		callSuper(...);
		cursor <<- 1L;
		.self
	},
	seed_path_name = function(n, i = length(seeds) + 1) {
		key = if (splitRe != '') splitString(splitRe, n) else n;
		key[1] = Sprintf('%{i}03d_%{k}s', k = key[1]);
		seedPath = Sprintf('%{path}s/%{keyComponents}s.RData', keyComponents = join(key, '/'));
	},
	seed_path = function(seed, i = length(seeds) + 1) .self$seed_path_name(names(seed), i),
	seed_save = function(seed, i = length(seeds) + 1) {
		seedPath = .self$seed_path(seed, i);
		s = seed[[1]];
		Save(s, file = seedPath);
	},
	set_seed_at = function(seed, i) {
		.self$seed_save(seed, i);
		if (names(seeds)[i] != names(seed))
			Logs('SowCatcherPersistent: Warning: seed key %{k2}s does not match seed slot %{k1}s',
				k1 = names(seeds)[i], k2 = names(seeds), logLevel = 3);
	},
	reap_raw = function(seed) {
		.self$seed_save(seed);
		seeds <<- c(seeds, listKeyValue(names(seed), NA));
		save(seeds, file = .self$seed_path_name('__seed_names', 0));
		NULL
	},
	reap = function(seed) {
		if (cursor > .self$seed_count()) {
			.self$reap_raw(seed);
			.self$setCursor(cursor + 1L);
			return(NULL);
		}
		seed_nm = names(seed);

		# <p> locate previous position
		ns = names(.self$getSeeds());
		occs = which(seed_nm == ns[Seq(1, cursor - 1, neg = T)]);
		if (length(occs) == 0) {
			Logs('SowCatcherPersistent: adding seed %{seed_nm}s of class %{cl}s not seen before.',
				cl = class(seed[[1]]), 3);
			.self$reap_raw(seed);
			return(NULL);
		}
		new_cursor = cursor + min(occs) - 1L;
		Logs('SowCatcherPersistent: Skipping to cursor %{new_cursor}s.', 5);
		.self$set_seed_at(seed, new_cursor);
		.self$setCursor(new_cursor + 1L);
	},
	Seeds = function(fields = NULL) {
		idcs = if (is.null(fields)) Seq(1, length(seeds)) else which.indeces(fields, names(seeds));
		r = lapply(idcs, function(i)get(load(.self$seed_path(seeds[i], i))[1]));
		names(r) = names(seeds)[idcs];
		r
	},
	register = function(ensemble, field, doReset = F) {
		# <N> if path was not specified yet, try to query from ensemble, should exit on NULL
		if (!length(.self$getPath())) {
			.self$setPath(ensemble$getPath());
			# <p> subpath for this field
			path <<- Sprintf('%{path}s/%{field}s');
		}
		# <p> keep track of seeds
		seedsPath = .self$seed_path_name('__seed_names', 0);
		if (file.exists(seedsPath)) seeds <<- get(load(seedsPath)[1]);
		if (doReset) {
			unlink(sapply(Seq(1, length(seeds)), function(i).self$seed_path(seeds[i], i)));
			if (file.exists(seedsPath)) unlink(seedsPath);
			seeds <<- list();
		}
		NULL
	}
	#
	#	</p> methods
	#
	)
);
SowCatcherPersistentClass$accessors(names(SowCatcherPersistentClass$fields()));


SowCatcherStackClass = setRefClass('SowCatcherStack',
	fields = list(
		sowCatchers = 'list',
		sowCatcherClass = 'character'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		sowCatchers <<- list();
		sowCatcherClass <<- 'SowCatcher';
		.self$initFields(...);
		.self
	},
	push = function(sowCatcher = getRefClass(.self$sowCatcherClass)$new(), ...) {
		sowCatchers[[length(sowCatchers) + 1]] <<- sowCatcher;
	},
	pop = function() {
		currentCatcher = sowCatchers[[length(sowCatchers)]];
		sowCatchers <<- sowCatchers[-length(sowCatchers)];
		currentCatcher
	},
	sowCatcher = function() {
		if (!length(sowCatchers)) .self$push();	# autovivify
		sowCatchers[[length(sowCatchers)]]
	},
	reap = function(fields = NULL) {
		r = lapply(sowCatchers, function(sc)sc$Seeds(fields))
	},
	register = function(ensemble, sow_field, ...)
		lapply(sowCatchers, function(sc)sc$register(ensemble, sow_field, ...)),
	conclude = function()lapply(rev(sowCatchers), function(sc)sc$conclude())
	#
	#	</p> methods
	#
	)
);
SowCatcherStackClass$accessors(names(SowCatcherStackClass$fields()));

SowCatcherEnsembleClass = setRefClass('SowCatcherEnsemble',
	fields = list(
		sowers = 'list',
		sowCatcherClass = 'character'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		sowers <<- list();
		sowCatcherClass <<- 'SowCatcher';
		.self$initFields(...);
		.self
	},
	push = function(sowCatcher = SowCatcherStackClass$new(), sow_field = 'default', ...) {
		# <b> default argument mechanism does not work
		#if (is.null(sowCatcher)) sowCatcher = getRefClass('SowCatcher')$new();
		if (is.null(sowers[[sow_field]])) sowers[[sow_field]] <<- SowCatcherStackClass$new();
		sowers[[sow_field]]$push(sowCatcher)
		sowCatcher$register(.self, sow_field, ...);
	},
	pop = function(sow_field = 'default')sowers[[sow_field]]$pop(),
	sowCatcher = function(sow_field = 'default')sowers[[sow_field]]$sowCatcher(),
	reap = function(sow_field = 'default', fields = NULL) sowers[[sow_field]]$reap(fields),
	conclude = function() sapply(sowers, function(sower)sower$conclude())
	#
	#	</p> methods
	#
	)
);
SowCatcherEnsembleClass$accessors(names(SowCatcherEnsembleClass$fields()));

SowCatcherEnsemblePersistentClass = setRefClass('SowCatcherEnsemblePersistent',
	contains = 'SowCatcherEnsemble',
	fields = list(
		path = 'character'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		callSuper(...)
		.self
	},
	push = function(sowCatcher = SowCatcherStackClass$new(), sow_field = 'default', ...) {
		r = callSuper(sowCatcher, sow_field, ...);
		.self$freeze();
		r
	},
	pop = function(sow_field = 'default') {
		r = callSuper(sow_field);
		.self$freeze();
		r
	},
	freeze_path = function()Sprintf('%{path}s/000_ensemble.RData'),
	freeze = function() {
		Save(.self, file = freeze_path());
		NULL
	},
	thaw = function() {
		e = get(load(freeze_path())[1]);
		# SowCatchers have to recover their own state
		lapply(names(e$sowers), function(n)e$sowers[[n]]$register(e, n));
		e
	}
	#
	#	</p> methods
	#
	)
);
SowCatcherEnsemblePersistentClass$accessors(names(SowCatcherEnsemblePersistentClass$fields()));

if (!exists('SowReap_env__')) SowReap_env__ = new.env();
SowReap_env__ = new.env();

SowReapInit = function(ensembleClass = 'SowCatcherEnsemble', ...) {
	ensemble = getRefClass(ensembleClass)$new(...);
	assign('sowEnsemble', ensemble, envir = SowReap_env__);
	ensemble
}
SowReapConclude = function() {
	sowReapEnsemble()$conclude();
}
sowReapEnsemble = function() {
	if (!exists('sowEnsemble', envir = SowReap_env__)) SowReapInit();
	ensemble = get('sowEnsemble', envir = SowReap_env__);
	ensemble
}

SowReapCreateField = function(sow_field, sowCatcherClass = 'SowCatcher', ...) {
	e = sowReapEnsemble();
	for (sf in sow_field) {
		catcher = getRefClass(sowCatcherClass)$new();
		e$push(catcher, sow_field = sf, ...);
	}
	NULL
}
SowReapReapField = function(sow_field) {
	e = sowReapEnsemble();
	e$pop(sow_field)$getSeeds();
}

Sow = function(..., sow_field = 'default') {
	catcher = sowReapEnsemble()$sowCatcher(sow_field = sow_field);
	catcher$sow(...)
}

Reap = function(expr, sow_field = 'default', fields = NULL, envir = parent.frame(), auto_unlist = T,
	vivify = F) {
	e = sowReapEnsemble();
	r = if (missing(expr)) {
		r = e$reap(sow_field, fields = fields);
		if (vivify) {
			r = lapply(r, function(e) {
				tbVivified = setdiff(fields, names(e));
				e = c(e, unlist.n(lapply(tbVivified, function(n)List(NULL, names_ = n)), 1));
				e
			});
		}
		if (auto_unlist && length(r) == 1) r = r[[1]];
		r
	} else {
		catcher = getRefClass(e$getSowCatcherClass())$new();
		e$push(catcher, sow_field = sow_field);
			eval(expr, envir = envir);
		e$pop(sow_field)$Seeds(fields);
	}
	r
}

ReapFromDisk = function(path, sow_field = 'default', fields = NULL, auto_unlist = T,
	ensembleClass = 'SowCatcherEnsemblePersistent', vivify = F) {
	e = getRefClass(ensembleClass)$new(path = path);
	e = e$thaw();

	r = e$reap(sow_field, fields = fields);
	if (vivify) {
		r = lapply(r, function(e) {
			tbVivified = setdiff(fields, names(e));
			e = c(e, lapply(tbVivified, function(n)List(NULL, names_ = n)));
			e
		});
	}
	if (auto_unlist && length(r) == 1) r = r[[1]];
	r
	
}
