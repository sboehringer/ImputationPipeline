#
#	Rdataset.R
#Tue Sep 28 14:53:47 2010

#	a dataset is a list with two data.frames
#	data: contains "data"
#	meta: contains meta information about "data"

# meta data frame
#	name	string/re to describe variable
#	type	(admin|var|unknown)
#	fullType	(admin:cluster|id|idM|idF)
#	index		index of column

metaData = function(d, metaTemplate, ignore.case = T) {
	ns = names(d);
	dm = listOfLists2data.frame(lapply(1:length(ns), function(i) {
		n = ns[i];
		m = sapply(metaTemplate, function(mt)(length(grep(mt$name, n, ignore.case = ignore.case)) > 0));
		r = metaTemplate[m];
		r = if (length(r) != 1) list(name = n, type = 'unknown', fullType = 'unknown') else
			merge.lists(r[[1]], list(name = n))[c('name', 'type', 'fullType')];
		r = c(r, list(index = i));
		r
	}), idColumn = NULL);
	dm
}

transformData = function(d, metaTemplate, ..., ignore.case = T) {
	ns = names(d);
	for (n in ns) {
		m = sapply(metaTemplate, function(mt)(length(grep(mt$name, n, ignore.case = ignore.case)) > 0));
		if (sum(m) == 1) {
			mt = metaTemplate[m][[1]];
			if (!is.null(mt$transf)) d[[n]] = mt$transf(d[[n]]);
		}
	}
	d
}

columnsOfType = function(d, type)d$meta$name[d$meta$fullType == type];
