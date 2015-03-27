#
#	RparallelTools.R
#Fri Jul 26 09:13:16 2013

#
#	<p> interface functions
#

Env.new = function(hash = T, parent = parent.frame(), size = 29L, content = list()) {
	e = new.env(hash = hash, parent = parent, size = size);
	nlapply(content, function(n) {
		assign(n, content[[n]], envir = e);
		NULL
	});
	e
}

#' Create a placeholder for an object to be loaded later
#'
#' @param path File system path to the file containing a saved R data structure
#'
delayed_load = function(path) {
	new('ParallelizeDelayedLoad', path)
}

delayed_load_dummy = function(path) get(load(path)[1])
