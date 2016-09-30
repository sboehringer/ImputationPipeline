#
#	Rlibraries.R
#Wed Oct 31 19:00:40 CET 2012

loadLibraries = function() {
	require('geepack');
	require('glmnet');
	require('ggplot2');
	#library('foreign');
}
#
#	Rdata.R
#Mon 27 Jun 2005 10:49:06 AM CEST
#system("cd ~/src/Rprivate ; ./exportR.sh");
#system("cd ~/src/Rprivate ; ./exportR.sh"); source("RgenericAll.R"); source("Rgenetics.R"); loadLibraries();
#system('WD=`pwd` ; cd ~/src/Rprivate ; ./exportR.sh ; cp RgenericAllRaw.R $WD ; cd $WD');

#
#	<§> abstract data functions
#

defined = function(x) exists(as.character(substitute(x)));
defined.by.name = function(name) { class(try(get(name), silent = T)) != 'try-error' }
# equivalent to i %in % v
is.in = function(i, v)(length((1:length(v))[v == i])>0)
rget = function(name, default = NULL, ..., pos = -1, envir = as.environment(pos)) {
	#obj = try(get(name, ...), silent = T);
	#r = if(class(obj) == 'try-error') default else obj;
	#r = if (exists(name, where = pos, envir = envir)) get(name, ..., pos = pos, envir = envir) else default;
	r = if (exists(name, envir = envir)) get(name, ..., envir = envir) else default;
	r
}
firstDef = function(..., .fdInterpolate = F, .fdIgnoreErrors = F) {
	l = if (.fdInterpolate) c(...) else list(...);
	for (i in l) { if (!is.null(i) && (!.fdIgnoreErrors || class(i) != 'try-error')) return(i)};
	NULL
}
firstDefNA = function(..., .fdInterpolate = F){
	l = if (.fdInterpolate) c(...) else list(...);
	for (i in l) { if (!is.na(i)) return(i)};
	NULL
}
# <N> NULL behaviour
to.list = function(..., .remove.factors = T){
	r = if(is.null(...)) NULL else if (is.list(...)) c(...) else list(...);
	if (.remove.factors) {
		r = sapply(r, function(e)ifelse(is.factor(e), levels(e)[e], e));
	}
	r
}
# pretty much force to vector
#avu = function(v)as.vector(unlist(v))
avu = function(v, recursive = T, toNA = T) {
	transform = if (toNA)
		function(e, condition)(if (condition) NA else avu(e, toNA = T, recursive = T)) else
		function(e, ...)avu(e, toNA = F, recursive = T);

	r = if (is.list(v)) {
		nls = sapply(v, is.null);	# detects nulls
		# unlist removes NULL values -> NA
		unlist(sapply(seq_along(v), function(i)transform(v[[i]], nls[i])));
	} else as.vector(v);
	if (!length(r)) return(NULL);
	r
}
pop = function(v)rev(rev(v)[-1]);

assign.list = function(l, pos = -1, envir = as.environment(pos), inherits = FALSE, immediate = TRUE) {
	for (n in names(l)) {
		assign(n, l[[n]], pos, envir, inherits, immediate);
	}
}
eval.text = function(text, envir = parent.frame())eval(parse(text = c[1]), envir= envir);


# replace elements base on list
# l may be a list of lists with elements f (from) and t (to), when f is replaced with t
# if both, f and t arguments are not NULL, l will be ignored and f is replaced with t
vector.replace = function(v, l, regex = F, ..., f = NULL, t = NULL) {
# 	if (!is.null(f) & !is.null(t)) l = list(list(f = f, t = t));
# 	# replacments are given in f/t pairs
# 	if (all(sapply(l, length) == 2)) {
# 		from = list.key(l, "f");
# 		to = list.key(l, "t");
# 	} else {
# 		from = names(l);
# 		to = unlist(l);
# 	}
# 	for (i in 1:length(from)) {
# 		if (regex) {
# 			idcs = which(sapply(v, function(e)(length(fetchRegexpr(from[i], e, ...)) > 0)));
# 			v[idcs] = sapply(v[idcs], function(e)gsub(from[i], to[i], e));
# 		} else v[which(v == from[i])] = to[i];
# 	}
 	repl = if (!is.null(f) & !is.null(t)) listKeyValue(f, t) else l;
	# <!> tb tested
 	v = if (!regex) {
		raw = repl[v];
		unlist(ifelse(sapply(repl[v], is.null), v, raw))
	} else {
		sapply(v, function(e){
			# first match takes precedent
			j = which(sapply(names(repl), function(f)length(fetchRegexpr(f, e, ...)) > 0))[1];
			if (is.na(j)) e else gsub(names(repl)[j], repl[[j]], e)
		})
	}
	v
}

vector.with.names = function(v, all_names, default = 0) {
	r = rep(default, length(all_names));
	names(r) = all_names;
	is = which.indeces(names(v), all_names, ret.na = T);
	r[is[!is.na(is)]] = v[!is.na(is)];
	r
}

# dir: direction of selection: 1: select rows, 2: select columns
mat.sel = function(m, v, dir = 1) {
	r = if (dir == 1)
		sapply(1:length(v), function(i)m[v[i], i]) else
		sapply(1:length(v), function(i)m[i, v[i]]);
	r
}

# rbind on list
sapplyId = function(l)sapply(l, identity);

listFind = function(lsed, lsee) {
	values = sapply(names(lsee), function(n)list.key(lsed, n), simplify = F, USE.NAMES = F);
	values = sapply(values, identity);
	found = apply(values, 1, function(r) all(r == lsee));
	r = unlist.n(lsed[found], 1);
	r
}

same.vector = function(v)all(v == v[1])

#
#	<§> string manipulation
#

say = function(...)cat(..., "\n");
printf = function(fmt, ...)cat(sprintf(fmt, ...));
join = function(v, sep = " ")paste(v, collapse = sep);
con = function(...)paste(..., sep="");
# pastem = function(a, b, ..., revsort = T) {
# 	if (revsort)
# 		as.vector(apply(merge(data.frame(a = b), data.frame(b = a), sort = F), 1,
# 			function(e)paste(e[2], e[1], ...))) else
# 		as.vector(apply(merge(data.frame(a = a), data.frame(b = b), sort = F), 1,
# 			function(e)paste(e[1], e[2], ...)))
# }
pastem = function(a, b, ..., revsort = T) {
	df = merge.multi.list(list(Df(a = a), Df(b = b)), .first.constant = revsort);
	paste(df[, 1], df[, 2], ...)
}

r.output.to.vector.int = function(s) {
	matches = gregexpr("(?<![\\[\\d])\\d+", s, perl=T);
	starts = as.vector(matches[[1]]);
	lengthes = attr(matches[[1]], "match.length");
	v = sapply(1:length(starts), function(i){ substr(s, starts[i], starts[i] + lengthes[i] -1) });
	as.integer(v)
}
r.output.to.vector.numeric = function(s) {
	matches = gregexpr("\\d*\\.\\d+", s, perl=T);
	starts = as.vector(matches[[1]]);
	lengthes = attr(matches[[1]], "match.length");
	v = sapply(1:length(starts), function(i){ substr(s, starts[i], starts[i] + lengthes[i] -1) });
	as.numeric(v)
}
readFile = function(path) { join(scan(path, what = "raw", sep = "\n", quiet = T), sep = "\n") };
circumfix = function(s, post = NULL, pre = NULL) {
	if (is.null(s) || length(s) == 0) return('');
	sapply(s, function(s)if (s == '') s else con(pre, s, post))
}
abbr = function(s, Nchar = 20, ellipsis = '...') {
	ifelse(nchar(s) > Nchar, paste(substr(s, 1, Nchar - nchar(ellipsis)), ellipsis, sep = ''), s)
}

Which.max = function(l, last.max = T, default = NA) {
	if (is.logical(l) && all(!l)) return(default);
	r = if (last.max) (length(l) - which.max(rev(l)) + 1) else which.max(l);
	r
}
Which.min = function(l, last.min = F, default = NA) {
	if (is.logical(l) && all(!l)) return(default);
	r = if (last.min) (length(l) - which.min(rev(l)) + 1) else which.min(l);
	r
}
# capturesN: named captures; for each name in captureN put the captured value assuming names to be ordered
# captures: fetch only first capture per match <!> deprecated
# capturesAll: fetch all caputers for each match
fetchRegexpr = function(re, str, ..., ret.all = F, globally = T, captures = F, captureN = c(),
	capturesAll = F, maxCaptures = 9, returnMatchPositions = F) {
	if (length(re) == 0) return(c());
	r = if (globally)
		gregexpr(re, str, perl = T, ...)[[1]] else
		regexpr(re, str, perl = T, ...);
	if (all(r < 0)) return(NULL);
	l = sapply(1:length(r), function(i)substr(str, r[i], r[i] + attr(r, "match.length")[i] - 1));
	if (captures) {
		l = sapply(l, function(e)gsub(re, '\\1', e, perl = T, fixed = F));
	} else if (length(captureN) > 0) {
		l = lapply(l, function(e) {
			r = sapply(1:length(captureN), function(i) {
				list(gsub(re, sprintf('\\%d', i), e, perl = T, fixed = F))
			});
			names(r) = captureN;
			r
		});
	} else if (capturesAll) {
		l = lapply(l, function(e) {
			cs = c();	# captures
			# <!> hack to remove zero-width assertions (no nested grouping!)
			#re = gsub('(\\(\\?<=.*?\\))|(\\(\\?=.*?\\))', '', re, perl = T, fixed = F);
			for (i in 1:maxCaptures) {
				n = gsub(re, sprintf('\\%d', i), e, perl = T, fixed = F);
				cs = c(cs, n);
			}
			cs
		});

		# trim list
		#maxEls = maxCaptures - min(c(maxCaptures + 1, sapply(l, function(e)Which.max(rev(e != ''))))
		#	, na.rm = T) + 1;
		maxEls = max(c(sapply(l, function(e)Which.max(e != '', default = 1)), 1));
		l = lapply(l, function(e)(if (maxEls > 0) e[1:maxEls] else NULL));
	}
	if (!ret.all) l = l[l != ""];
	ret = if (returnMatchPositions) list(match = l, positions = r) else l;
	ret
}
# improved multistring version
FetchRegexpr = function(re, str, ..., ret.all = F, globally = T, captures = F, captureN = c(),
	capturesAll = F, maxCaptures = 9, returnMatchPositions = F) {
	if (length(re) == 0) return(c());
	r = if (globally)
		gregexpr(re, str, perl = T, ...) else
		list(regexpr(re, str, perl = T, ...));
	if (all(unlist(r) < 0)) return(NULL);
	l = sapply(seq_along(r),
		function(j) {
			r0 = r[[j]];
			sapply(1:length(r0),
				function(i)substr(str[j], r0[i], r0[i] + attr(r0, "match.length")[i] - 1))
	});
	if (captures) {
		l = sapply(l, function(e)gsub(re, '\\1', e, perl = T, fixed = F));
		#print(l);
	} else if (length(captureN) > 0) {
		l = lapply(l, function(e) {
			r = sapply(1:length(captureN), function(i) {
				list(gsub(re, sprintf('\\%d', i), e, perl = T, fixed = F))
			});
			names(r) = captureN;
			r
		});
	} else if (capturesAll) {
		l = lapply(l, function(e) {
			cs = c();	# captures
			# <!> hack to remove zero-width assertions (no nested grouping!)
			#re = gsub('(\\(\\?<=.*?\\))|(\\(\\?=.*?\\))', '', re, perl = T, fixed = F);
			for (i in 1:maxCaptures) {
				n = gsub(re, sprintf('\\%d', i), e, perl = T, fixed = F);
				cs = c(cs, n);
			}
			cs
		});

		# trim list
		#maxEls = maxCaptures - min(c(maxCaptures + 1, sapply(l, function(e)Which.max(rev(e != ''))))
		#	, na.rm = T) + 1;
		maxEls = max(c(sapply(l, function(e)Which.max(e != '', default = 1)), 1));
		l = lapply(l, function(e)(if (maxEls > 0) e[1:maxEls] else NULL));
	}
	if (!ret.all) l = l[l != ""];
	ret = if (returnMatchPositions) list(match = l, positions = r) else l;
	ret
}

regex = Vectorize(fetchRegexpr, 'str', SIMPLIFY = T, USE.NAMES = T);
Regex = Vectorize(FetchRegexpr, 're', SIMPLIFY = T, USE.NAMES = T);
regexIdcs = function(re, s, ...)vectorIdcs(regex(re, s, ...), is.null, not = T)

# unify capture extraction for gregexpr, regexpr
# pos == 0: grexepr, regexpr else by iterating pos as index into str
matchRegexCapture = function(reg, str, pos = NULL) {
	if (is.null(attr(reg, 'capture.start'))) return(NULL);
	if (!is.null(pos)) str = str[pos] else pos = seq_along(reg);
	captures = lapply(1:ncol(attr(reg, 'capture.start')), function(i) {
		sapply(pos, function(j)Substr(str,
			attr(reg, 'capture.start')[j, i], attr(reg, 'capture.length')[j, i]))
	});
	names(captures) = attr(reg, 'capture.names');
	captures
}
matchRegexExtract = function(reg, str, pos = NULL) {
	if (!is.null(pos)) str = str[pos] else pos = seq_along(reg);
	matches = ifelse(reg[pos] < 0, character(0),
		sapply(pos, function(i)Substr(str, reg[i], attr(reg, 'match.length')[i])));
	matches
}
# <i> re nested list with sub-res for named captures
# <!> globally == FALSE, removeNonMatch == FALSE
matchRegex = function(re, str, ..., globally = TRUE, simplify = TRUE,
	positions = FALSE, removeNonMatch = FALSE) {
	if (length(re) == 0) return(NULL);
	reg = if (globally) gregexpr(re, str, perl = T, ...) else regexpr(re, str, perl = T, ...);
	ms = if (globally)
		lapply(seq_along(reg), function(i)matchRegexExtract(reg[[i]], str[i])) else
		lapply(seq_along(str), function(i)matchRegexExtract(reg, str, pos = i));
	#	regmatches(str, reg);
	captures = if (globally)
		lapply(seq_along(reg), function(i)matchRegexCapture(reg[[i]], str[i])) else
		lapply(seq_along(str), function(i)matchRegexCapture(reg, str, pos = i));
	if (removeNonMatch) {
		nonmatch = sapply(ms, length) == 0 | is.na(ms);
		ms = ms[!nonmatch];
		captures = captures[!nonmatch];
		reg = reg[!nonmatch];
	}
	if (simplify && length(str) == 1) {
		ms = ms[[1]];
		captures = captures[[1]];
		reg = reg[[1]];
	}
	r = if(positions) list(match = ms, capture = captures, positions = reg) else
		list(match = ms, capture = captures);
	r
}

#
#	<p> final interface as of 2016/04
#
MatchRegex = function(re, str, mode = 'return') {
	r = regexpr(re, str);
	if (mode == 'return') {
		r = str[which(r > 0)];
	}
	r
}

splitString = function(re, str, ..., simplify = T) {
	l = lapply(str, function(str) {
		r = gregexpr(re, str, perl = T, ...)[[1]];
		if (r[1] < 0) return(str);
		l = sapply(1:(length(r) + 1), function(i) {
			substr(str, ifelse(i == 1, 1, r[i - 1] + attr(r, "match.length")[i - 1]),
				ifelse(i > length(r), nchar(str), r[i] - 1))
		});
	});
	if (length(l) == 1 && simplify) l = l[[1]];
	l
}
quoteString = function(s)sprintf('"%s"', s)
trimString = function(s) {
	sapply(s, function(e)
		if (is.na(e)) NA else FetchRegexpr('^\\s*(.*?)\\s*$', e, captures = T)
	)
}

# <N> maxIterations needs to be large as a new iteration is entered after each successful substitution
#	this is necessary, as 
mergeDictToString = function(d, s, valueMapper = function(s)
	ifelse(is.na(d[[n]]), '{\\bf Value missing}', d[[n]]),
	iterative = F, re = F, maxIterations = 1e4, doApplyValueMap = T, doOrderKeys = T, maxLength = 1e7) {
	ns = names(d);
	# proceed in order of decreasing key lengthes
	if (doOrderKeys) ns = ns[rev(order(sapply(ns, nchar)))];
	for (i in 1:maxIterations) {
		s0 = s;
		for (n in ns) {
			# counteract undocumented string interpolation
			subst = if (doApplyValueMap)
				gsub("[\\\\]", "\\\\\\\\", valueMapper(d[[n]]), perl = T)
				else d[[n]];
			# <!> quoting
			if (!re) n = sprintf("\\Q%s\\E", n);
			s = gsub(n, firstDef(subst, ""), s, perl = T, fixed = F);
			# <A> if any substitution was made, it is nescessary to reiterate ns to preserver order
			#	of substitutions
			if (iterative && s != s0) break;
		}
		if (!iterative || s == s0 || nchar(s) > maxLength) break;
	}
	s
}
mergeDictToStringV = Vectorize(mergeDictToString, 's', SIMPLIFY = T, USE.NAMES = T);

mergeDictToVector = function(d, v) { unlist(ifelse(is.na(names(d[v])), v, d[v])) }

mergeDictToDict = function(dMap, dValues, ..., recursive = T) {
	r = lapply(dValues, function(v) {
		r = if (class(v) == 'list') {
			if (recursive) mergeDictToDict(dMap, v, ...) else v
		} else if (class(v) == 'character') mergeDictToString(dMap, v, ...) else v;
		r
	});
	r
}

# double quote if needed
qsSingle = function(s, force = F) {
	# <N> better implementation possible: detect unquoted white-space
	if (force || length(fetchRegexpr('[ \t"()\\[\\]:,]', s)) > 0) {
		s = gsub('([\\"])', '\\\\\\1', s);
		s = sprintf('"%s"', s);
	} else {
		s0 = gsub("([\\'])", '\\\\\\1', s);
		if (s0 != s) s = sprintf("$'%s'", s0);
	}
	s
}
qs = function(s, ...)sapply(s, qsSingle, ...)
# single quote if needed
qssSingle = function(s, force = F) {
	# <N> better implementation possible: detect unquoted white-space
	if (force || length(fetchRegexpr("[ \t'()\\[\\]:,]", s)) > 0) {
		s = gsub("(['])", "'\"'\"'", s);
		s = sprintf("'%s'", s);
	}
	s
}
qss = function(s, ...)sapply(s, qssSingle, ...)

#' Return sub-strings indicated by positions or produce a string by substituting those strings with
#'	replacements
#'
#' The function behaves similar to sprintf, except that character sequences to be substituted are
#' indicated by name.
#'
#' @param s template string
#' @param start vector of start positions of substrings to substitute
#' @param length vector of lengthes of substrings to substitute
#' @param replacement vector of strings to subsitute. If missing, \code{Substr} returns sub-strings indicated
#'	by start/length
#'
#' @examples
#' print(Substr("abc", c(2, 3), c(1, 1), c("def", 'jkl')));
#' print(Substr("abcdef", c(2, 3, 5), c(1, 1, 1), c("123", '456', '789')));
#' print(Substr("abcdef", c(1, 3, 5), c(1, 1, 1), c("123", '456', '789')));
#' print(Substr("abcdef", c(1, 3, 5), c(0, 1, 0), c("123", '456', '789')));
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

# <!> quoting
#'	Produce string by substituting placeholders
#'
#' The function behaves similar to sprintf, except that character sequences to be substituted are
#' indicated by name. To be implemented: *-specifications
#'
#' @param s template string
#' @param d values to substitute into \code{s}
#' @param template template for substitution pattern. Within this pattern \code{__DICT_KEY__} is
#'  substituted for a key in \code{d}. This string \code{k} is substituted in \code{s} with \code{d[[k]]}.
#'
#' @examples
#' Sprintf('These are N %{N} characters.', list(N = 10));
#' Sprintf('These are N %{N}d characters.', list(N = 10));
#' Sprintf('These are N %{N}02d characters.', list(N = 10));
Sprintfl = function(fmt, values, sprintf_cartesian = FALSE, envir = parent.frame()) {
	dict = extraValues = list();
	for (i in seq_along(values)) {
		if (is.list(values[[i]]))
			dict = merge.lists(dict, values[[i]]) else
		if (!is.null(names(values)[i]) && names(values)[i] != '')
			dict = merge.lists(dict, values[i]) else
			extraValues = c(extraValues, values[i]);
	}
# 	re = '(?x)(?:
# 		(?:^|[^%]|(?:%%)+)\\K
# 		[%]
# 			(?:[{]([^{}\\*\'"]*)[}])?
# 		((?:[-]?[*\\d]*[.]?[*\\d]*)?(?:[sdfegG]|))(?=[^%sdfegG]|$)
# 	)';
	# <!> new, untested regexpr as of 22.5.2014
	# un-interpolated formats do no longer work
	re = '(?x)(?:
		(?:[^%]+|(?:%%)+)*\\K
		[%]
			(?:[{]([^{}\\*\'"]*)[}])?
		((?:[-]?[*\\d]*[.]?[*\\d]*)?(?:[sdfegGDQqu]|))(?=[^sdfegGDQqu]|$)
	)';
	r = fetchRegexpr(re, fmt, capturesAll = T, returnMatchPositions = T);
	# <p> nothing to format
	if (length(r$match) == 0) return(fmt);
	typesRaw = sapply(r$match, function(m)ifelse(m[2] == '', 's', m[2]));
	types = ifelse(typesRaw %in% c('D', 'Q'), 's', typesRaw);
	fmts = sapply(r$match, function(m)sprintf('%%%s',
		ifelse(m[2] %in% c('', 'D', 'Q', 'q', 'u'), 's', m[2])));
	fmt1 = Substr(fmt, r$positions, attr(r$positions, 'match.length'), fmts);

	keys = sapply(r$match, function(i)i[1]);
	nonKeysI = cumsum(keys == '');	# indeces of values not passed by name
	nonKeysIdcs = which(keys == '');

	# <p> collect all values
	allValues = c(extraValues, dict);
	# get interpolation variables
	interpolation = nlapply(keys[keys != ''], function(k)
		if (!is.null(allValues[[k]])) NULL else rget(k, default = NA, envir = envir)
	);
	# <p> handle %D: current day
	keys[typesRaw == 'D'] = '..Sprintf.date..';
	dateValue = if (sum(typesRaw == 'D'))
		list(`..Sprintf.date..` = format(Sys.time(), '%Y%d%m')) else
		list();
	allValues = c(allValues, dateValue, List_(interpolation, rm.null = T));

	# 14.9.2015 -> convert to indeces
	# build value combinations
	listedValues = lapply(keys, function(k)allValues[[k]]);
	dictDf = if (!sprintf_cartesian) Df_(listedValues) else merge.multi.list(listedValues);
#if (substr(fmt, 0, 5) == '%{wel') browser();
	# fill names of anonymous formats
	keys[keys == ''] = names(dictDf)[Seq(1, sum(nonKeysI != 0))];
	# due to repeat rules of R vectors might have been converted to factors
	#dictDf = Df_(dictDf, as_character = unique(keys[types == 's']));
	dictDf = Df_(dictDf, as_character = which(types == 's'));
	
	# <p> conversion <i>: new function
	#colsQ = keys[typesRaw == 'Q'];
	# <!> switch to index based transformation on account of duplicate keys
	colsQ = which(typesRaw == 'Q');
	dictDf[, colsQ] = apply(dictDf[, colsQ, drop = F], 2, qs);
	#colsq = keys[typesRaw == 'q'];
	colsq = which(typesRaw == 'q');;
	dictDf[, colsq] = apply(dictDf[, colsq, drop = F], 2, qss);

	colsu = which(typesRaw == 'u');;
	dictDf[, colsu] = apply(dictDf[, colsu, drop = F], 2, uc.first);

	colsd = which(typesRaw == 'd');;
	dictDf[, colsd] = apply(dictDf[, colsd, drop = F], 2, as.integer);
	s = sapply(1:nrow(dictDf), function(i) {
		valueDict = as.list(dictDf[i, , drop = F]);
# 		sprintfValues = lapply(seq_along(keys), function(i)
# 			ifelse(keys[i] == '', extraValues[[nonKeysI[i]]],
# 				firstDef(valueDict[[keys[i]]], rget(keys[i], default = '__no value__'), pos = -2)));
# 		sprintfValues = lapply(seq_along(keys), function(i)
# 			firstDef(valueDict[[keys[i]]], rget(keys[i], default = '__no value__', envir = envir)));
		#sprintfValues = lapply(seq_along(keys), function(i)valueDict[[keys[i]]]);
		#do.call(sprintf, c(list(fmt = fmt1), sprintfValues))
		# <!> simplify above two lines, now robust against duplicated entries -> <i> needs unit tests
		names(valueDict) = NULL;
		do.call(sprintf, c(list(fmt = fmt1), valueDict))
	});
	s
}

Sprintf = sprintd = function(fmt, ..., sprintf_cartesian = FALSE, envir = parent.frame()) {
	Sprintfl(fmt, list(...), sprintf_cartesian = sprintf_cartesian, envir = envir);
}

#r = getPatternFromStrings(DOC, '(?:\\nDOCUMENTATION_BEGIN:)([^\\n]+)\\n(.*?)(?:\\nDOCUMENTATION_END\\n)');
getPatternFromStrings = function(strings, pattern, keyIndex = 1) {
	r = lapply(strings, function(s) {
		ps = fetchRegexpr(pattern, s, capturesAll = T);
		listKeyValue(sapply(ps, function(e)e[[keyIndex]]), sapply(ps, function(e)e[-keyIndex]));
	});
	r
}

getPatternFromFiles = function(files, locations = NULL, ...) {
	strings = sapply(files, function(f)readFile(f, prefixes = locations));
	getPatternFromStrings(strings, ...);
}

#
#	hex strings
#

asc = function(x)strtoi(charToRaw(x), 16L);
character.as.characters = function(str) {
	sapply(str, function(s) sapply(1:nchar(s), function(i)substr(str, i, i)));
}

# bit_most_sig in bits
hex2int = function(str, bit_most_sig = 32) {
	cs = rev(sapply(character.as.characters(tolower(str)), asc));
	cms = bit_most_sig / 4;	# character containing most significant bit
	is = ifelse(cs >= asc('a'), cs - asc('a') + 10, cs - asc('0'));
	flipSign = (length(is) >= cms && is[cms] >= 8);
	if (flipSign) is[cms] = is[cms] - 8;
	r = sum(sapply(1:length(is), function(i)(is[i] * 16^(i-1))));
	if (flipSign) r = r - 2^(bit_most_sig - 1);
	r = if (r == - 2^(bit_most_sig - 1)) NA else as.integer(r);
	r
}

# chunk_size in bits
hex2ints = function(str, chunk_size = 32) {
	l = nchar(str);
	csc = chunk_size / 4;	# chunk_size in characters
	chunks = (l + csc - 1) %/% csc;
	r = sapply(1:chunks, function(i)hex2int(substr(str, (i - 1)*csc + 1, min(l, i*csc))));
	r
}

#
#	<§> binary numbers/n-adic numbers
#

ord2base = dec2base = function(o, digits = 5, base = 2) {
	sapply(1:digits, function(i){(o %/% base^(i-1)) %% base})
}
base2ord = base2dec = function(v, base = 2) {
	sum(sapply(1:length(v), function(i)v[i] * base^(i-1)))
}

ord2bin = dec.to.bin = function(number, digits = 5) ord2base(number, digits, base = 2);
bin2ord = bin.to.dec = function(bin) base2ord(bin, base = 2);

#
#	<Par> sequences
#

#'	Produce constrained sequences
#'
#' This is a wrapper around seq that adds constraints. Setting ascending, descending to NA reverts to
#' standard \code{seq} behaviour.
#'
#' @param ascending restrict sequences to be ascending; return empty list if to < from
#' @param descending restrict sequences to be descending; return empty list if from < to
#' @examples
#' Seq(1, 10, ascending = T)
#' Seq(1, 10, descending = T)
#' Seq(10, 1, ascending = NA)
Seq = function(from, to, ..., ascending = T, descending = !ascending, neg = F) {
	# <!> order matters: if called with only descending == T
	if (nif(descending) && to > from) return(if (neg) T else c()) else
	if (nif(ascending) && from > to) return(if (neg) T else c());
	s = seq(from, to, ...);
	r = if (neg) -s else s;
	r
}

#' Produce index pairs for vector of counts
#'
#' @param counts vector of integers specifying counts
#' @return vector of pairs of indeces indicating the first and last element in a vector for the blocks 
#'  specified by \code{counts}
#' @examples
#' count2blocks(c(1, 5, 3))
count2blocks = function(counts) {
	ccts = cumsum(counts);
	fidcs = c(1, ccts[-length(ccts)] + 1);
	blks = as.vector(rbind(fidcs, fidcs + counts - 1));
	blks
}

#
#	expand a block list - for example as from count2blocks - to a list of integers
#
expandBlocks = function(blks) {
	apply(matrix(blks, ncol = 2, byrow = T), 1, function(r) { r[1]:r[2] } )
}


splitListIndcs = function(M, N = 1, .compact = F, .truncate = T) {
	if (.truncate & M < N) N = M;
	if (.compact) {
		n = rep(ceiling(M / N), N);	# size of parts
		idcs = c(0, cumsum(n));
		idcs = idcs[idcs < M];
		idcs = c(idcs, M);
	} else {
		n = rep(floor(M / N), N);		# size of parts
		R = M - n[1] * N;
		n = n + c(rep(1, R), rep(0, N - R));
		idcs = c(0, cumsum(n));
	}
	idcs = cbind(idcs + 1, c(idcs[-1], 0))[-length(idcs), ];	# from:to in a row
	# <!> usual R degeneracy
	if (!is.matrix(idcs)) idcs = matrix(idcs, nrow = 1);
	idcs
}
splitListEls = function(l, N, returnElements = FALSE) {
	idcs = splitListIndcs(length(l), N);
	li = apply(idcs, 1, function(r)(if (returnElements) l[r[1]:r[2]] else r[1]:r[2]));
	# <!> R ambiguity of apply return type
	if (is.matrix(li)) li = lapply(1:(dim(li)[2]), function(i)li[, i]);
	if (is.vector(li)) li = as.list(li);;
	li
}

# @arg l list of index positions from another object
# @return return vector indicating to which list element an index was assigned
# Example: glmnet accepts fold numbers per index (as opposed to a partitioning of elements)
index2listPosition = function(l) {
	N = sum(sapply(l, length));
	na = rep(NA, N);
	m = sapply(1:length(l), function(i)vector.assign(na, l[[i]], i, na.rm = NA));
	r = apply(m, 1, na.omit);
	r
}

# splitting based on fractions
# voting percentages to seats
#	simple algorithm based on size of residuals
splitSeatsForFractions = function(Nseats, fractions) {
	# number of parties
	Nparties = length(fractions);
	# fractional seats
	Nseats0 = fractions * Nseats;
	# garuantee one seat, otherwise round to nearest
	Nseats1 = ifelse (Nseats0 < 1, 1, round(Nseats0));
	# mismatch
	diff = sum(Nseats1) - Nseats;
	# redistribute deficit/overshoot
	if (diff != 0) {
		Nresid = sapply(Nseats0 - Nseats1, function(i)ifelse(i < 0, 1, i));
		subtr = order(Nresid, decreasing = diff < 0)[1:abs(diff)];
		# assume one round of correction is always sufficient <!>
		Nseats1[subtr] = Nseats1[subtr] - sign(diff);
	}
	Nseats1
}

# tranform number of elements (as from splitSeatsForFractions) into from:to per row in a matrix
counts2idcs = function(counts) {
	idcs = c(0, cumsum(counts));
	idcs = cbind(idcs + 1, c(idcs[-1], 0))[-length(idcs), ];
	idcs
}

# N is partitioned into fractions from p, where each element of p partitions the remaining part of N
# procedure makes sure to leave space for length(p) elements
cumpartition = function(N, p) {
	I = c();	# indeces within 1:N
	for (i in 1:length(p)) {
		# partition remaining space (ifelse), leave room for subsequent indeces
		Ii = floor(p[i] * (ifelse(i == 1, N, N - I[i - 1]) - (length(p) - i))) + 1;
		I = c(I, ifelse(i == 1, Ii, I[i - 1] + Ii));
	}
	as.integer(I)
}

#' Extract parts of a nested structure based on the range from..to
#'
#'
#' @param Ns Vector of integers that specify the size of the substructure
#' @return Return list of list, where each basic list contains key \code{segment}
#'  (which of the elements of Ns) and key \code{range}, a list with elements \code{from} and \code{to},
#'  specifying which elements to use from
#'  that segment.
subListFromRaggedIdcs = function(Ns, from = 1, to = sum(segments)) {
	NsCS = cumsum(Ns);
	NsCSs = c(0, pop(NsCS));	# shifted cumsum
	segments = which(from <= NsCS & to > NsCSs);
	r = lapply(segments, function(segment){
		N = Ns[segment];	# list-call
		from_ = 1;
		to_ = N;
		if (segment == segments[1]) from_ = from - NsCSs[segment];
		if (segment == rev(segments)[1]) to_ = to - NsCSs[segment];
		r = list(segment = segment, range = list(from = from_, to = to_));
		r
	});
	r
}

#' Extract parts of nested lists based on the range from..to
#'
#'
#' @param ls nested list structure (currently only two levels supported)
#' @return Return list of list, where each basic list contains key \code{segment}
#'  (which of the elements of Ns) and key \code{range}, a list with elements \code{from} and \code{to},
#'  specifying which elements to use from
#'  that segment.
subListFromRaggedLists = function(ls, from = 1, to = sum(sapply(ls, length))) {
	sl = subListFromRaggedIdcs(sapply(ls, length), from = from, to = to);
	r = lapply(sl, function(s) with(s, {
		r = ls[[segment]][range$from: range$to];
		r
	}));
	r = unlist.n(r, 1);
	r
}


#
#	<§> vector functions
#

# does the position exists in vector v
exists.pos = function(v, i)(is.vector(v) && !is.na(v[i]))

#
#	<par> lists
#

merge.lists = function(..., ignore.nulls = TRUE, listOfLists = FALSE, concat = FALSE, useIndeces = FALSE) {
	lists = if (listOfLists) c(...) else list(...);
	l1 = lists[[1]];
	if (length(lists) > 1) for (i in 2:length(lists)) {
		l2 = lists[[i]];
		ns = if (useIndeces) 1L:length(l2) else names(l2);
		for(n in ns) {
			if (is.null(n)) print("Warning: tried to merge NULL key");
			if (!is.null(n) & (!ignore.nulls | !is.null(l2[[n]]))) {
				if (concat) l1[[n]] = c(l1[[n]], l2[[n]]) else l1[[n]] = l2[[n]];
			}
		}
	}
	l1
}

merge.lists.recursive = function(..., ignore.nulls = TRUE, listOfLists = F) {
	lists = if (listOfLists) c(...) else list(...);
	l1 = lists[[1]];
	if (length(lists) > 1) for (i in 2:length(lists)) {
		l2 = lists[[i]];
		for(n in names(l2)) {
			if (is.null(n)) print("Warning: tried to merge NULL key");
			if (!is.null(n) & (!ignore.nulls | !is.null(l2[[n]])))
				l1[[n]] = if (is.list(l1[[n]]))
					merge.lists.recursive(l1[[n]], l2[[n]]) else
					l2[[n]]
		}
	}
	l1
}

unshift = function(l, listOfList = T) {
	if (!listOfList) l = list(l);
	e1 = lapply(l, function(l0)if (is.list(l0)) l0[[1]] else l0[1]);
	r1 = lapply(l, function(l0)l0[-1]);
	r = list(elements = e1, remainder = r1);
	r
}

Merge.lists.raw = function(lists, ignore.nulls = TRUE, recursive = FALSE, keys = NULL) {
	if (!is.null(keys)) keys = unshift(keys);

	l1 = lists[[1]];
	if (length(lists) > 1) for (i in 2:length(lists)) {
		l2 = lists[[i]];
		for(n in names(l2)) {
			if (is.null(n)) print("Warning: tried to merge NULL key");
			if (!is.null(n) & (!ignore.nulls | !is.null(l2[[n]])))
				l1[[n]] = if (recursive && is.list(l1[[n]]) && (is.null(keys) || n %in% keys$elements))
					Merge.lists.raw(list(l1[[n]], l2[[n]]), ignore.nulls, recursive,
						if (is.null(keys)) NULL else keys$remainder) else
					l2[[n]]
		}
	}
	l1
}

Merge.lists = function(..., ignore.nulls = TRUE, listOfLists = F, recursive = F, keyPathes = NULL) {
	lists = if (listOfLists) c(...) else list(...);
	keys = if (!is.null(keyPathes)) splitString("[$]", keyPathes, simplify = F) else NULL; 
	l = Merge.lists.raw(lists, ignore.nulls = ignore.nulls, recursive = recursive, keys = keys);
	l
}

compare_print = function(r, e) {
	require('compare');
	cmp = compare(model = r, comparison = e);
	if (!cmp$result) {
		print("Expectation not met (result != expectation):");
		print(r);
		print(e);
	}
	cmp$result
}


# use.names preserves names and concatenates with lower level names
# reset sets names to top level names
unlist.n = function(l, n = 1, use.names = T, reset = F) {
	if (n > 0) for (i in 1:n) {
		ns = names(l);
		#names(l) = rep(NULL, length(l));	# <!> untested removal Tue Oct 19 17:11:53 2010
		l = unlist(l, recursive = F, use.names = use.names);
		if (reset) names(l) = ns;
	}
	l
}

# <N> obsolete, better: with(l, { ...})
instantiate.list = function(l, n = 1) {
	for (nm in names(l)) {
 		eval.parent(parse(file = "", text = sprintf("%s = %s", nm, deparse(l[[nm]]))), n = n);
# 		if (is.integer(l[[nm]])) {
# 			eval.parent(parse(file = "", text = sprintf("%s = %d", nm, l[[nm]])), n = n);
# 		} else if (is.numeric(l[[nm]])) {
# 			eval.parent(parse(file = "", text = sprintf("%s = %f", nm, l[[nm]])), n = n);
# 		} else {
# 			eval.parent(parse(file = "", text = sprintf("%s = \"%s\"", nm, l[[nm]])), n = n);
# 		};
	}
}
# for use in testing code
instantiate = function(l, ..., envir = parent.frame()) {
	l0 = c(l, list(...));
	for (i in seq_along(l0)) assign(names(l0)[i], l0[[i]], envir = envir);
	invisible(l0)
}

# assume a list of lists (aka vector of dicts) and extract a certain key from each of the lists
list.key = function(v, key, unlist = T, template = NULL, null2na = F) {
	l = lapply(v, function(i){
		if (is.list(i)) {
			if (is.null(i[[key]])) { if (null2na) NA else NULL } else i[[key]]
		} else template});
	if (unlist) l = unlist(l);
	l
}
# extract key path from list, general, recursive version
#	key path recursive worker
list.kprw = function(l, keys, unlist.pats, template, null2na, carryNames, test) {
	key = keys[1];
	# <p> extract key
	r = if (key != "*") {
		index = fetchRegexpr("\\A\\[\\[(\\d+)\\]\\]\\Z", key, captures = T);
		if (length(index) > 0) key = as.integer(index[[1]]);
		if (is.list(l)) {
			r = if (is.null(l[[key]])) {
					if (null2na) NA else NULL
				} else l[[key]];
			if (length(keys) > 1)
				list.kprw(r, keys[-1], unlist.pats[-1], template, null2na, carryNames, test) else
				if (test) !(is.null(r) || all(is.na(r))) else r;
		} else if (class(l) %in% c('character')) {
			l[names(l) %in% key];
		} else if (class(l) %in% c('data.frame', 'matrix')) {
			l[, key]
		} else return(template)
	} else {
		if (length(keys) > 1)
			lapply(l, function(sl)
				list.kprw(sl, keys[-1], unlist.pats[-1], template, null2na, carryNames, test)
			) else l;
	}
	# <p> unlisting
	if (!is.null(unlist.pats)) if (unlist.pats[1]) r = unlist.n(r, 1, reset = carryNames);
	r
}
# wrapper for list.kprw
# keyPath obeys EL1 $ EL2 $ ..., where ELn is '*' or a literal
# unlist.pat is pattern of truth values TR1 $ TR2 $..., where TRn is in 'T|F' and specifies unlist actions
# carryNames determines names to be carried over from the top level in case of unlist
list.kpr = function(l, keyPath, do.unlist = F, template = NULL,
	null2na = F, unlist.pat = NULL, carryNames = T, as.matrix = F, test = F) {
	keys = fetchRegexpr("[^$]+", keyPath);
	unlist.pats = if (!is.null(unlist.pat)) as.logical(fetchRegexpr("[^$]+", unlist.pat)) else NULL;

	r = list.kprw(l, keys, unlist.pats, template, null2na, carryNames, test = test);
	if (do.unlist) { r = unlist(r); }
	if (as.matrix) r = t(sapply(r, function(e)e));
	r
}
# extract key path from list
# <!> interface change: unlist -> do.unlist (Wed Sep 29 18:16:05 2010)
# test: test existance instead of returning value
list.kp = function(l, keyPath, do.unlist = F, template = NULL, null2na = F, test = F) {
	r = list.kpr(l, sprintf("*$%s", keyPath), do.unlist = do.unlist, template, null2na = null2na, test = test);
	r
}

list.keys = function(l, keys, default = NA) {
	l = as.list(l);
	r = lapply(unlist(keys), function(key) if (is.null(l[[key]])) default else l[[key]]);
	r
}


# return list without listed keys
list.min  = function(l, keys) {
	l[-which.indeces(keys, names(l))]
}
# list generation on steroids (wraps other functions)
.list = function(l, .min = NULL) {
	if (!is.null(.min)) l = list.min(l, .min);
	l
}
# get apply
gapply = function(l, key, unlist = F)list.key(l, key, unlist)
# construct list as a dictionary for given keys and values
listKeyValue = function(keys, values) {
	if (length(keys) != length(values))
		stop("listKeyValue: number of provided keys does not match that of values");

	l = as.list(values);
	names(l) = keys;
	l
}
vectorNamed = function(v, names) {
	if (length(names) > length(v)) stop("vectorNamed: more names than vector elements");
	names(v) = names;
	v
}
#listInverse = function(l)listKeyValue(avu(l), names(l));
listInverse = function(l, toNA = F) {
	n = sapply(l, length);
	# <p> values of inverse map
	vs = rep.each(names(l), n);
	# <p> construct list
	r = listKeyValue(avu(l, recursive = F, toNA = toNA), vs);
	r
}

# name the list elements by the iterated vector elements ns (names)
nlapply = function(ns, f, ...) {
	if (is.list(ns)) ns = names(ns);
	r = lapply(ns, f, ...);
	names(r) = ns;
	r
}
nelapply = function(l, f, ...) {
	ns = names(l);
	r = lapply(ns, function(n, ...)f(n, l[[n]]), ...);
	names(r) = ns;
	r
}

ilapply = function(l, f, ...) {
	r = lapply(1:length(l), function(i)f(l[[i]], i, ...));
	if (!is.null(names(l))) names(r) = names(l);
	r
}
# pass element, index, name
einlapply = function(l, f, ...) {
	ns = names(l);
	r = lapply(1:length(l), function(i)f(l[[i]], i, ns[i], ...));
	names(r) = ns;
	r
}

kvlapply = function(l, f, ...) {
	ns = names(l);
	r = lapply(1:length(l), function(i)f(ns[i], l[[i]], ...));
	names(r) = ns;
	r
}
getElement = function(v, i)if (is.list(v)) v[[i]] else v[i];
# unify w/ list.takenFrom -> tests
List.takenFrom = function(listOfLists, v)
	lapply(1:length(listOfLists), function(j)getElement(listOfLists[[j]], v[j]));
# tuple-apply
tuapply = function(..., fct = Identity, args = list(), names = NULL) {
	tupels = list(...);
	M = length(tupels);
	Ns = sapply(tupels, length);
	N = Ns[1];
	if (any(Ns != N)) stop('Indexable elements not of same length');
	r = lapply(1:N, function(i)do.call(fct, c(List.takenFrom(tupels, rep(i, M)), args)));
	if (is.null(names) && !is.null(base::names(tupels[[1]]))) names = base::names(tupels[[1]]);
	if (!is.null(names)) base::names(r) = names;
	r
}


# USE.NAMES logic reversed for sapply
sapplyn = function(l, f, ...)sapply(l, f, ..., USE.NAMES = F);
list.with.names = function(..., .key = 'name') {
	l = list(...);
	ns = names(l);
	r = nlapply(l, function(n) c(l[[n]], listKeyValue(.key, n)));
	r
}

#
#	<par> data type conversions
#

# assure m has at least 1 column
to.col = function(m) { if (is.null(dim(m))) t(t(m)) else m }
col.frame = function(l, col.name = 'value', minus = NULL, ignore.null = TRUE,
	do.paste = NULL, do.format = T, digits = 3, plus = NULL) {
	if (ignore.null) { for (n in names(l)) { if (is.null(l[[n]])) l[[n]] = NULL; } }
	if (!is.null(minus)) { for (n in minus) { l[[n]] = NULL; } }
	my.names = if (!is.null(plus)) plus else names(l);
	digits = if (length(digits) > 1) digits else rep(digits, length(l));
	if (!is.null(do.paste)) {
		if (do.format) {
			i = 1;
			for (n in my.names) { if (is.vector(l[[n]])) {
				l[[n]] = paste(sapply(l[[n]],
						function(e){if (is.numeric(e)) sprintf("%.*f", digits[i], e) else e}
					), collapse = do.paste)
				i = i + 1;
			}}
		} else {
			for (n in my.names) { if (is.vector(l[[n]])) l[[n]] = paste(l[[n]], collapse = do.paste) }
		}
	}
	f = as.data.frame(l);
	if (dim(f)[2] > length(col.name) && length(col.name) == 1)
		row.names(f) = paste(col.name, 1:dim(f)[1], sep = "")
	else row.names(f) = c(col.name);
	t(f)
}

# <i> collect recursively until list or data.frame
# convert list of lists to data frame (assuming identical keys for each sub list)
#	also works on list of vectors
listOfLists2data.frame = function(l, idColumn = "id", .names = NULL) {
	# collect keys
	keys = if (is.list(l[[1]]))
		sort(unique(as.vector(unlist(sapply(l, function(e)names(e)))))) else 1:length(l[[1]]);
	if (is.null(.names)) .names = keys;
	# row names
	rows = names(l);
	if (is.null(rows)) rows = 1:length(l);
	# build df

	#df = t(sapply(rows, function(r) { unlist(l[[r]][keys]) }));
	df = t(sapply(rows, function(r)list2df(l[[r]], keys)));
	df = if (!is.null(idColumn)) {
		data.frame.types(data.frame(..idColumn.. = rows, df),
			row.names = 1:length(rows), names = c(idColumn, .names));
	} else {
		data.frame.types(df, row.names = rows, names = .names);
	}
	df
}

# resetColNames: reset column names to names of first data frame
# colsFromFirstDf: take columns from the first data frame
# <i> improved algorithm: unlist everything, bind together: cave: data types,
#	strictly valid only for matrices
# Use cases:
#	list with named vectors: get data frame that contains all vectors with all possible names represented
#		listOfDataFrames2data.frame(cfs, colsFromUnion = T, do.transpose = T, idColumn = NULL);
listOfDataFrames2data.frame = function(l, idColumn = "id", do.unlist = T, direction = rbind,
	resetColNames = T, colsFromFirstDf = F, colsFromUnion = F, do.transpose = F, idAsFactor = F) {
	# row names
	# <!> 2009-11-20 changed from: rows = firstDef(names(l), list(1:length(l)));
	rows = firstDef(names(l), 1:length(l));
	# columns
	ns = NULL;
	if (colsFromUnion) {
		ns = unique(unlist(lapply(l, names)));
		# get data.frame names
		ns = names(do.call(data.frame, listKeyValue(ns, rep(NA, length(ns)))));
		resetColNames = F;	# <!> mutually exclusive
	}
	# build df
	df = NULL;
	for (i in 1:length(rows)) {
		if (is.null(l[[i]])) next;	# ignore empty entries
		# <p> force to data frame
		df0 = if (do.transpose) as.data.frame(t(l[[i]])) else as.data.frame(l[[i]]);
		# <p> homogenize columns
		if (colsFromUnion) {
			# add missing columns
			ns0 = setdiff(ns, names(df0));
			df0 = do.call(data.frame, c(list(df0), listKeyValue(ns0, rep(NA, length(ns0)))));
			# correct order of columns
			df0 = df0[, ns];
		}
		if (!is.null(df)) {
			if (colsFromFirstDf) df0 = df0[, names(df)] else
			if (resetColNames) {
				names(df0) = if (is.null(idColumn)) names(df) else names(df)[-1];
			}
		}
		# <p> add id column
		df0 = if (is.null(idColumn)) df0 else cbind(rep(rows[i], dim(df0)[1]), df0);
		# <A> case differentiation should not me necessary
		df = if (i == 1) df0 else direction(df, df0);
	}
	if (!is.null(idColumn)) names(df)[1] = idColumn;
	if (do.unlist) for (n in names(df)) { df[[n]] = unlist(df[[n]]); }
	if (idAsFactor) df[[idColumn]] = as.factor(df[[idColumn]]);
	row.names(df) = NULL;
	df
}
cbindDataFrames = function(l, do.unlist = F) {
	listOfDataFrames2data.frame(l, idColumn = NULL, do.unlist = do.unlist, direction = cbind,
		resetColNames = F)
}
rbindDataFrames = function(l, do.unlist = F, useDisk = F, idColumn = NULL, transpose = F,
	resetColNames = F, colsFromFirstDf = F, idAsFactor = F) {
	r = if (useDisk) {
		tempTable = tempfile();
		for (i in 1:length(l)) {
			d0 = l[[i]];
			if (class(d0) != 'data.frame') d0 = as.data.frame(d0);
			if (transpose) d0 = t(d0);
			if (!is.null(idColumn)) {
				d0 = data.frame(idColumn = names(l)[i], d0);
				names(d0)[1] = idColumn;
			}
			write.table(d0, file = tempTable, col.names = i == 1, append = i != 1, row.names = F);
		}
		read.table(tempTable, header = T, as.is = T);
	} else {
		listOfDataFrames2data.frame(l, idColumn = idColumn, do.unlist = do.unlist,
			direction = rbind, resetColNames = resetColNames, colsFromFirstDf = colsFromFirstDf,
			idAsFactor = idAsFactor)
	}
	r
}

# names2col assigns names of the list to a column of the data frame and values to the valueCol
list2df = function(l, cols = names(l), row.name = NULL, names2col = NULL, valueCol = 'value') {
	idcs = if (is.null(cols)) 1:length(l) else
		if (all(is.integer(cols))) cols else which.indeces(names(l), cols);
	if (is.null(cols) || all(is.integer(cols))) cols = paste('C', 1:length(l), sep = '');
	r = as.list(rep(NA, length(cols)));
	names(r) = cols;
	r[idcs] = l;
	r = as.data.frame(r, stringsAsFactors = F);
	if (!is.null(row.name)) row.names(r)[1] = row.name;
	if (!is.null(names2col)) {
		r = data.frame(name = names(r), value = unlist(r[1, ]), row.names = NULL, stringsAsFactors = F);
		names(r) = c(names2col, valueCol);
	}
	r
}

be.numeric = function(v)
	sapply(v, function(e)grepl('^-?\\d*(\\.\\d+)?(e-?\\d+)?$', e, ignore.case = T, perl = T));

list2df.print = function(l, valueCol = 'value', names2col = NULL, ..., digits = 3, scientific = 3) {
	l1 = list2df(l, valueCol = valueCol, names2col = names2col, ...);
	numericRows = be.numeric(l1[[valueCol]]);
	numbers = as.numeric(l1[[valueCol]][numericRows]);
	log10range = max(floor(log10(numbers))) - min(floor(log10(numbers)));
	#fmt = if (log10range > digits + 1) '%.*e' else '%.*f';
	numbers = sprintf(ifelse(abs(floor(log10(numbers))) > scientific, '%.*e', '%.*f'), digits, numbers);
	#numbers = sapply(numbers, function(n)sprintf(fmt, digits, n));
	separators = as.vector(names(l) == '' & is.na(l));
	l1[separators, names2col] = '-';
	l1[separators, valueCol] = '';
	l1[numericRows, valueCol] = numbers;
	print(l1);
}


rbind.list2df = function(d, l, row.name = NULL) {
	d = as.data.frame(d);
	r = list2df(l, names(d), row.name);
	r0 = rbind(d, r);
	r0
}

# take list of lists
#	names of list elements become column-names
listOfLists2df = function(l, columnNames = names(l[[1]])) {
	colV = lapply(columnNames, function(n)Df_(list.kp(l, n, do.unlist = T)));
	r = Df_(do.call(cbind, colV), names = columnNames);
	r
}

# d: data frame, l: list with names corresponding to cols, values to be searched for in columns
searchDataFrame = function(d, l, .remove.factors = T) {
	ns = names(l);
	d = d[, ns, drop = F];
	if (.remove.factors) {
		l = sapply(l, function(e)ifelse(is.factor(e), levels(e)[e], e));
		#d = apply(d, 2, function(col)(if (is.factor(col)) levels(col)[col] else col));
	}
	rs = which(as.vector(apply(apply(d, 1, function(r)(r == l)), 2, all)));
	rs
}

.df.cols = which.cols = function(d, cols, regex = F) {
	cols[is.numeric(cols)] = as.integer(cols[is.numeric(cols)]);
	cols[is.character(cols)] = which.indeces(cols[is.character(cols)], names(d), regex = regex);
	as.integer(cols)
}
# select columns by name
.df = function(d, names, regex = T, as.matrix = F) {
	cols = which.indeces(names, names(d), regex = regex);
	d0 = d[, cols, drop = F];
	# <t> simpler version:
	# d0 = d[, .df.cols(d, names, regex)];
	if (as.matrix) d0 = as.matrix(d0);
	d0
}
.df.reorder = function(d, names, regex = T) {
	cols = .df.cols(d, names, regex);
	d0 = d[, c(cols, setdiff(1:dim(d)[2], cols))];
	d0
}
# remove columns by name
.dfm = function(d, names, regex = F, as.matrix = F) {
	cols = if (all(is.numeric(names))) as.integer(names) else which.indeces(names, names(d), regex = regex);
	d0 = d[, -cols, drop = F];
	if (as.matrix) d0 = as.matrix(d0);
	d0
}
# remove rows by name
.dfrmr = function(d, names, regex = F, as.matrix = F) {
	rows = if (all(is.numeric(names)))
		as.integer(names) else
		which.indeces(names, row.names(d), regex = regex);
	d0 = d[-rows, , drop = F];
	if (as.matrix) d0 = as.matrix(d0);
	d0
}

# remove rows/columns by name
.dfrm = function(d, rows = NULL, cols = NULL, regex = F, as.matrix = F) {
	d = as.data.frame(d);	# enforce data frame
	rows = if (is.null(rows)) 1:dim(d)[1] else
		-(if (all(is.numeric(rows))) as.integer(rows) else which.indeces(rows, row.names(d), regex = regex));
	cols = if (is.null(cols)) 1:dim(d)[2] else 
		-(if (all(is.numeric(cols))) as.integer(cols) else which.indeces(cols, names(d), regex = regex));
	d0 = d[rows, cols, drop = F];
	if (as.matrix) d0 = as.matrix(d0);
	d0
}

subsetTop = function(obj, sel, N = 1) {
	d0 = subset(obj, sel);
	d1 = d0[1:min(nrow(d0), N), ];
	d1
}

# convert strings to data frame names
#	<i> create a data frame and extract names
.dfns = function(ns)gsub(':', '.', ns);

# manipulate list of vectors
# vectors i = 1,.., n with entries v_ij are represented as vector v_11, ..., v_n1, v_21, ...
vector.intercalate = meshVectors = function(...) {
	l = list(...);
	if (length(l) == 1) l = l[[1]];
	v = as.vector(t(sapply(l, function(v)unlist(v))));
	# <N> preferred implementation
	# No unlist -> should be part of input sanitization
	# v = as.vector(do.call(rbind, l));
	v
}

is.sorted = function(...)(!is.unsorted(...))
is.ascending = function(v) {
	if (length(v) < 2) return(T);
	for (i in 2:length(v)) if (v[i] <= v[i - 1]) return(F);
	return(T);
}

# pad a vector to length N
pad = function(v, N, value = NA)c(v, rep(value, N - length(v)));

#
#	<par> number sequences
#

rep.each = function(l, n) {
	l = avu(l);
	if (length(n) == 1) as.vector(sapply(l, function(e)rep(e, n))) else
		avu(sapply(seq_along(l), function(i)rep(l[i], n[i])))
}
rep.each.row = function(m, n) {
	r = matrix(rep.each(m, n), ncol = ncol(m));
	if (class(m) == 'data.frame') r = Df_(r, names = names(m));
	r
}
#rep.list = function(l, n) lapply(1:length(l), function(e)l);
# <!> changed as of 23.8.2016; n not used before
rep.list = function(l, n) lapply(1:n, function(e)l);
matrix.intercalate = function(..., direction = 1) {
	l = list(...);
	# <!> assume same dimension
	d = dim(l[[1]]);
	N = prod(d);
	# <p> create new matrix
	v = c(if (direction == 1) sapply(l, as.vector) else sapply(sapply(l, t), as.vector) , recursive = T);
	vN = as.vector(matrix(v, ncol = N, byrow = T));
	r = if (direction == 1)
		matrix(vN, nrow = d[1] * length(l)) else
		matrix(vN, ncol = d[2] * length(l), byrow = T);
	# <p> return value
	if (class(l[[1]]) == 'data.frame') r = Df_(r, names = names(l[[1]]));
	r
}

data.frame.expandWeigths = function(data, weights = 'weights') {
	w = data[[weights]];
	weightsCol = which(names(data) == weights);
	df0 = lapply(1:length(w), function(i) {
		if (w[i] > 0) rep.each.row(data[i, -weightsCol], w[i]) else list();
	});
	df1 = rbindDataFrames(df0);
	df1
}

# spread/fill vector to indeces
vector.spread = function(v, idcs, N, default = 0) {
	r = rep(default, N);
	r[idcs] = v;
	r
}

# create new vector with length == length(v) + length(idcs)
# idcs are positions in the final vector
vector.embed = function(v, idcs, e, idcsResult = T) {
	if (!idcsResult) idcs = idcs + (1:length(idcs)) - 1;
	N = length(v) + length(idcs);
	r = rep(NA, N);
	r[setdiff(1:N, idcs)] = v;
	r[idcs] = e;
	r
}
# set values at idcs
vector.assign = function(v, idcs, e, na.rm = 0) {
	v[idcs] = e;
	if (!is.na(na.rm)) v[is.na(v)] = na.rm;
	v
}
matrix.assign = function(m, idcs, e, byrow = T) {
	if (length(dim(idcs)) > 1) {
		m[as.matrix(idcs)] = e
	} else if (byrow)
		m[idcs, ] = e else
		m[, idcs] = e
	m
}

# are columns/rows same values in matrix
matrix.same = function(m, direction = 1) {
	apply(m, direction, function(e)all(e[1] == e))
}

vectorIdcs = function(v, f, ..., not = F) {
	r = sapply(v, f, ...);
	which(if (not) !r else r)
}


# produce indeces for indeces positioned into blocks of blocksize of which count units exists
# example: expand.block(2, 10, 1:2) == c(1, 2, 11, 12)
expand.block = function(count, blocksize, indeces) {
	as.vector(apply(to.col(1:count), 1,
		function(i){ (i - 1) * blocksize + t(to.col(indeces)) }
	));
}

search.block = function(l, s) {
	b.sz = length(s);
	which(sapply(
		1:(length(l)/b.sz), function(i){all(l[((i - 1) * b.sz + 1):(i * b.sz)] == s)}
	));
}

#
#	<par> matrix functions
#

# <!> assumes same indeces for rows/columns
matrixFromIndexedDf = function(df, idx.r = 'idx.r', idx.c = 'idx.c', value = 'value', referenceOrder = NULL) {
	id = unique(c(df[[idx.r]], df[[idx.c]]));
	# matrix indeces
	# <A> canonical order is by repeating vector id for row index, constant for columns within repetition
	#	-> matrix filled by columns
	midcs = merge(data.frame(id = id), data.frame(id = id), by = NULL);
	midcs = data.frame(midcs, mfid.i = 1:nrow(midcs));
	map = merge(df[, c(idx.r, idx.c, value)], midcs,
		by.x = c(idx.r, idx.c), by.y = c('id.x', 'id.y'), all.y = T);
	# return to midcs order
	map = map[order(map$mfid.i), ];
	# filled by rows
	m = matrix(map[[value]], nrow = length(id));
	# reorder matrix
	o = order_align(firstDef(referenceOrder, id), id);
	# reorder in two steps -> out of mem otherwise
	m1 = m[o, ];
	m2 = m1[, o];
	m2
}

symmetrizeMatrix = function(m) {
	m[is.na(m)] = t(m)[is.na(m)];
	m
}

which.row = function(m, row) {
	cols = names(as.list(row));
	if (is.null(cols)) cols = 1:length(row);
	rows = 1:(dim(m)[1]);
	rows.found = rows[sapply(rows, function(i){ all(m[i, cols] == row) })];
	rows.found
}

# lsee:	list with searchees
# lsed:	list with searched objects
# inverse: lsed are regexes matched against lsee; pre-condition: length(lsee) == 1
# ret.list: for match.multi return list by lsee
# <!><t> cave: semantics changed as of 17.8.2009: return NA entries for unfound lsee-entries
# <!> match multi only implemented for merge = T
which.indeces = function(lsee, lsed, regex = F, ret.na = F, merge = T, match.multi = F, ...,
	inverse = F, ret.list = FALSE) {
	if (!length(lsed) || !length(lsee)) return(c());
	v = if (is.list(lsed)) names(lsed) else lsed;
	idcs = if (regex) {
		which(sapply(lsed, function(e)(
			if (inverse) length(fetchRegexpr(e, lsee, ...)) > 0 else
				any(sapply(lsee, function(see)(length(fetchRegexpr(see, e, ...)) > 0)))
		)))
	} else if (merge) {
		d0 = merge(
			data.frame(d = lsed, ix = 1:length(lsed)),
			data.frame(d = lsee, iy = 1:length(lsee)), all.y = T);
		d0 = d0[order(d0$iy), ];
		idcs = if (match.multi) {
				#d0$ix[unlist(sapply(lsee, function(e)which(d0$d == e)))]
				#na.omit(sort(d0$ix))
				r = if (ret.list)
					unlist.n(by(d0, d0$d, function(d)list(na.omit(d$ix)), simplify = FALSE)) else
					na.omit(d0$ix);
				r
			} else {
				d0$ix[pop(which(c(d0$iy, 0) - c(0, d0$iy) != 0))];
			}
		# less efficient version
#		} else d0$ix[unlist(sapply(lsee, function(e)which(d0$d == e)[1]))];
#		} else d0$ix[order(d0$iy)]
		if (!ret.na) idcs = idcs[!is.na(idcs)];
		idcs
	} else {
		unlist(as.vector(sapply(lsee, function(e){
			w = which(e == v);
			if (!ret.na) return(w);
			ifelse(length(w), w, NA)
		})))
	};
	r = if (ret.list) idcs else as.integer(idcs);
	r
}

grep.vector = function(lsee, lsed, regex = F, ret.na = F, merge = T, match.multi = F, ..., inverse = F) {
	lsed[which.indeces(lsee, lsed, regex, ret.na, merge, match.multi, ..., inverse = inverse)]
}
grep.infixes = function(lsee, lsed, ...) {
	r = grep.vector(sapply(lsee, function(v)sprintf('^%s.*', v)), lsed, regex = T, inverse = F, ... );
	r
}

# force structure to be matrix (arrange vector into a row)
MR = function(m) {
	if (!is.matrix(m)) m = matrix(m, byrow = T, ncol = length(m));
	m
}
# force structure to be matrix (arrange vector into a columns)
MC = function(m) {
	if (!is.matrix(m)) m = matrix(m, byrow = F, nrow = length(m));
	m
}

#
#	<par> data processing
#

# like table but produce columns for all numbers 1..n (not only for counts > 0)
# cats are the expected categories
table.n = function(v, n, min = 1, categories = NULL) {
	if (is.null(categories)) categories = min:n;
	t = as.vector(table(c(categories, v)) - rep(1, length(categories)));
	t
}
table.freq = function(v) {
	t0 = table(v);
	r = t0 / sum(t0);
	r
}
table.n.freq = function(...) {
	t0 = table.n(...);
	r = t0 / sum(t0);
	r
}

#
#	<par> data types
#

to.numeric = function(x) { suppressWarnings(as.numeric(x)) }

# set types for columns: numeric: as.numeric
data.frame.types = function(df, numeric = c(), character = c(), factor = c(), integer = c(),
	do.unlist = T, names = NULL, row.names = NULL, reset.row.names = F, do.rbind = F, do.transpose = F,
	stringsAsFactors = F) {
	if (do.rbind) {
		#old code: df = t(sapply(df, function(e)e));
		lengthes = sapply(df, length);
		maxL = max(lengthes);
		df = t(sapply(1:length(df), function(i)c(df[[i]], rep(NA, maxL - lengthes[i]))));
	}
	if (do.transpose) df = t(df);
	df = as.data.frame(df, stringsAsFactors = stringsAsFactors);
	# set or replace column names
	if (!is.null(names)) {
		if (class(names) == "character") names(df)[1:length(names)] = names;
		if (class(names) == "list") names(df) = vector.replace(names(df), names);
	}
	if (do.unlist) for (n in names(df)) { df[[n]] = unlist(df[[n]]); }
	for (n in numeric) { df[[n]] = as.numeric(df[[n]]); }
	for (n in integer) { df[[n]] = as.integer(df[[n]]); }
	for (n in character) { df[[n]] = as.character(df[[n]]); }
	for (n in factor) { df[[n]] = as.factor(df[[n]]); }
	if (reset.row.names) row.names(df) = NULL;
	if (length(row.names) > 0) row.names(df) = row.names;
	df
}

DfClasses = function(dataFrame)nlapply(dataFrame, function(n)class(dataFrame[[n]]));
DfAsInteger = function(dataFrame, as_integer) {
	#dfn = apply(dataFrame[, as_integer, drop = F], 2, function(col)as.integer(avu(col)));
	# <!> 6.6.2016 as.integer first needed to retain factor status on factors
	dfn = nlapply(as_integer, function(col)avu(as.integer(dataFrame[[col]])));
	dataFrame[, as_integer] = as.data.frame(do.call(cbind, dfn));
	dataFrame
}
DfAsCharacter = function(dataFrame, as_character) {
	#dfn = apply(dataFrame[, as_character, drop = F], 2, function(col)as.character(avu(col)));
	#dataFrame[, as_character] = as.data.frame(dfn, stringsAsFactors = FALSE);
	dfn = nlapply(as_character, function(col)avu(as.character(dataFrame[[col]])));
	dataFrame[, as_character] = as.data.frame(do.call(cbind, dfn), stringsAsFactors = FALSE);
	dataFrame
}

# as of 22.7.2013 <!>: min_ applied before names/headerMap
# as of 19.12.2013 <!>: as.numeric -> as_numeric
# as of 22.5.2014 <!>: t -> t_
# as of 13.11.2014 <!>: sapply -> simplify_
#' Create data frames with more options than \code{data.frame}
Df_ = function(df0, headerMap = NULL, names = NULL, min_ = NULL,
	as_numeric = NULL, as_character = NULL, as_factor = NULL, as_integer = NULL,
	row.names = NA, valueMap = NULL, Df_as_is = TRUE, simplify_ = FALSE,
	deep_simplify_ = FALSE, t_ = FALSE, unlist_cols = F, transf_log = NULL, transf_m1 = NULL,
	Df_doTrimValues = FALSE, Df_mapping_value = '__df_mapping_value__',
	Df_mapping_empty = '__DF_EMPTY__', Do_Df_mapping_empty = TRUE) {
	#r = as.data.frame(df0);
	if (t_) df0 = t(df0);
	r = data.frame(df0, stringsAsFactors = !Df_as_is);
	if (notE(min_)) {
		is = which.indeces(min_, names(r));
		if (length(is) > 0) r = r[, -is, drop = F];
	}
	if (simplify_) r = sapply(r, identity);
	if (deep_simplify_) r = as.data.frame(
		nlapply(r, function(col)sapply(r[[col]], unlist)), stringsAsFactors = !Df_as_is
	);
	if (notE(names)) {
		if (class(names) == 'character') names(r)[1:length(names)] = names;
		if (class(names) == 'list') names(r) = vector.replace(names(r), names);
	}
	if (notE(headerMap)) names(r) = vector.replace(names(r), headerMap);
	if (notE(valueMap)) {
		for (n in names(valueMap)) {
			vs = if (Df_doTrimValues)
				nina(trimString(as.character(r[[n]])), Df_mapping_value) else
				as.character(r[[n]]);
			if (Do_Df_mapping_empty) vs = ifelse(nit(vs == ''), Df_mapping_empty, vs)
			vs = nina(valueMap[[n]][vs], Df_mapping_value);
			vs = ifelse(vs == Df_mapping_value, as.character(r[[n]]), vs)
			r[[n]] = vs;
		}
	}
	if (notE(as_numeric)) {
		dfn = apply(r[, as_numeric, drop = F], 2, function(col)as.numeric(avu(col)));
		r[, as_numeric] = as.data.frame(dfn);
	}
	if (notE(as_integer)) r = DfAsInteger(r, as_integer);
	if (notE(as_character)) r = DfAsCharacter(r, as_character);
	if (notE(as_factor)) {
		# <N> does not work
		#dfn = apply(r[, as_factor, drop = F], 2, function(col)as.factor(col));
		#r[, as_factor] = dfn;
		for (f in as_factor) r[, f] = as.factor(r[[f]]);
	}
	#
	#	<p> transformations
	#
	if (notE(transf_log)) r[, transf_log] = log(r[, transf_log, drop = F]);
	if (notE(transf_m1)) r[, transf_m1] = r[, transf_m1, drop = F] - 1;

	if (!all(is.na(row.names))) row.names(r) = row.names;
	if (unlist_cols) for (n in names(r)) r[[n]] = avu(r[[n]]);
	r
}

Df = function(..., headerMap = NULL, names = NULL, min_ = NULL, row.names = NA, Df_as_is = TRUE,
	as_numeric = NULL, as_character = NULL, as_factor = NULL, t_ = F, unlist_cols = F) {
	r = data.frame(...);
	Df_(r, headerMap = headerMap, names = names, min_ = min_, row.names = row.names,
		as_numeric = as_numeric,
		as_character = as_character,
		as_factor = as_factor,
		Df_as_is = Df_as_is,
		t_ = t_,
		unlist_cols = unlist_cols
	);
}
Df2list = function(df) {
	df = as.data.frame(df);
	nlapply(names(df), function(n)df[[n]]);
}
Dfselect = function(data, l, na.rm = nif) {
	sel = apply(sapply(nlapply(l, function(n)data[[n]] == l[[n]]), identity), 1, all);
	r = data[na.rm(sel), ];
	r
}


List_ = .List = function(l, min_ = NULL, sel_ = NULL,
	rm.null = F, names_ = NULL, null2na = F, simplify_ = F, rm.na = F) {
	if (!is.null(min_)) {
		i = which.indeces(min_, names(l));
		if (length(i) > 0) l = l[-i];
	}
	if (!is.null(sel_)) {
		i = which.indeces(sel_, names(l));
		if (length(i) > 0) l = l[i];
	}
	if (rm.null) {
		remove = -which(sapply(l, is.null));
		if (length(remove) > 0) l = l[remove];
	}
	if (null2na) {
		nullI = which(sapply(l, is.null));
		l[nullI] = NA;
	}
	if (rm.na) {
		l = l[!is.na(l)];
	}
	if (!is.null(names_)) names(l)[Seq(1, length(names_))] = names_;
	if (simplify_) l = sapply(l, identity);
	l
}
List = function(..., min_ = NULL, envir = parent.frame(), names_ = NULL) {
	l = eval(list(...), envir = envir);
	.List(l, min_ = min_, names_ = names_);
}

Unlist = function(l, ..., null2na_ = FALSE) {
	if (null2na_) l[sapply(l, is.null)] = NA;
	unlist(l, ...)
}

last = function(v)(v[length(v)])
pop = function(v)(v[-length(v)])
# differences between successive elements, first diff is first element with start
vectorLag = function(v, start = 0)pop(c(v, start) - c(start, v))
splitN = function(N, by = 4) vectorLag(round(cumsum(rep(N/by, by))));
splitToMax = function(N, max = 4) vectorLag(round(cumsum(rep(N/ceiling(N/max), ceiling(N/max)))));

# cumsum returning indeces for numbers given in Ns
cumsumI = function(Ns, offset = 1, do.pop = FALSE) {
	cs = vectorNamed(c(0, cumsum(Ns)) + offset, c(names(Ns), 'N'));
	if (do.pop) cs = pop(cs);
	cs
}
# recursive cumsum (one level)
cumsumR = function(l, offset = 1) {
	cs0 = if (is.list(l)) lapply(l, cumsumR, offset = 0) else rev(cumsum(l))[1];
	cs = vectorNamed(c(0, pop(unlist(cs0))) + offset, names(cs0));
	cs
}

#
#	<par> sets and permutations
#

#' @title wrapper for order to allow multivariate ordering
Order = function(v, ...) {
	if (is.data.frame(v)) do.call(order, lapply(v, identity), ...) else
	if (is.list(v)) do.call(order, v, ...) else
	order(v, ...)
}

#' @title Return all value combinations appearing in a data frame
#'
#' @examples
#' combs = valueCombinations(iris);
valueCombinations = function(d) merge.multi.list(dimnames(table(d)));

#' @title Computes order so that inverseOrder after order is the identity
#'
#' @examples
#' v = runif(1e2);
#' print(all(sort(v)[inverseOrder(v)] == v))
Rank = inverseOrder = inversePermutation = function(p) {
	## <p> naive version
	# 	o = order(p);
	# 	i = rep(NA, length(o));
	# 	for (j in 1:length(o)) { i[o[j]] = j};
	# 	i
	## <p> build-in version (not working for multivariate case)
	#rank(v, ties.method = 'first')
	## <p> better version
	which.indeces(1:(if (class(p) == 'data.frame') nrow(p) else length(p)), Order(p))
}

#' @title Calculates inverseOrder, assuming that the argument is already an \code{order}-vector.
inverseOrder_fromOrder = function(p)which.indeces(1:length(p), p)

#' @title Return vector that reorders v to equal reference.
#'
#' Assuming that two arguments are permutaions of each other, return a vector of indeces such that \code{all(reference == v[order_align(reference, v)]) == T} for all vectors \code{reference, v}.
#'
#' @examples
#' sapply(1:10, function(i){v = sample(1:5); v[order_align(5:1, v)]})
#' sapply(1:10, function(i){v = runif(1e2); v1 = sample(v, length(v)); all(v1[order_align(v, v1)] == v)})
order_align = function(reference, v)Order(v)[inverseOrder(reference)];

#' Calculates \code{order_align}, assuming that the both arguments are already orders.
#' sapply(1:40, function(i){v = runif(1e2); v1 = sample(v, length(v)); all(v1[order_align_fromOrder(order(v), order(v1))] == v)})
order_align_fromOrder = function(reference, v)v[inverseOrder_fromOrder(reference)];

# permutation is in terms of elements of l (not indeces)

applyPermutation = function(l, perm, from = 'from', to = 'to', returnIndeces = T) {
	# 1. bring perm[[from]] in the same order as l
	# 2. apply this order to perm[[to]]
	r0 = perm[[to]][order(perm[[from]])[inverseOrder(l)]];
	# 3. determine permutation going from l to r0
	r = order(l)[inverseOrder(r0)]
	if (!returnIndeces) r = l[r];
	r
}

order.df = function(df, cols = NULL, decreasing = F, na.last = F) {
	if (is.null(cols)) cols = 1:ncol(df);
	if (!is.numeric(cols)) cols = which.indeces(cols, names(df));
	orderText = sprintf("order(%s, decreasing = %s, na.last = %s)",
		paste(sapply(cols, function(i) { sprintf("df[, %d]", i) }), collapse = ", "
		), as.character(decreasing), as.character(na.last)
#		paste(sapply(cols, function(i) {
#			if (is.numeric(i)) sprintf("df[, %d]", i) else sprintf("df$%s", i) }), collapse = ", "
#		), as.character(decreasing), as.character(na.last)
	);
	o = eval(parse(text = orderText));
	#print(list(text = orderText, order = o, df=df));
	o
}

order.df.maps = function(d, maps, ..., regex = F) {
	cols = NULL;
	for (i in 1:length(maps)) {
		m = names(maps)[i];
		map = maps[[i]];
		keys = names(map);
		cols = c(cols, if (is.list(map)) {
			tempColName = sprintf("..order.df.maps.%04d", i);
			col = if (regex)
				sapply(d[[m]], function(e){ j = which.indeces(e, keys, regex = T, inverse = T)
					if (length(j) == 0) NA else map[[j]]
				}) else	as.character(map[d[[m]]]);
			col[col == "NULL"] = NA;
			d = data.frame(col, d, stringsAsFactors = F);
			names(d)[1] = tempColName;
		} else { m });
	}
	o = order.df(d, cols, ...);
	o
}

data.frame.union = function(l) {
	dfu = NULL;
	for (n in names(l)) {
		df = l[[n]];
		factor = rep(n, dim(df)[1]);
		dfu = rbind(dfu, cbind(df, factor));
	}
	dfu
}

# levels: take levels in that order, unmentioned levels are appended
# setLevels: restrict to these levels, else set to NA
# setLevelsTo: set names of levels to argument, set excess levels to NA
recodeLevels = function(f, map = NULL, others2na = TRUE, levels = NULL, setLevels = NULL,
	setLevelsTo = NULL) {
	r = f;
	if (!is.null(map)) {
		# map others to NA
		if (others2na) {
			nonmentioned = setdiff(if (is.factor(f)) levels(f) else unique(f), names(map));
			map = c(map, listKeyValue(nonmentioned, rep(NA, length(nonmentioned))));
		}
		v = vector.replace(as.character(f), map);
		if (is.integer(f)) v = as.integer(v);
		if (is.factor(f)) v = as.factor(v);
		r = v;
	}
	if (!is.null(levels) || !is.null(setLevels)) {
		# <p> preparation
		fact0 = as.factor(r);
		levls = levels(fact0);
		r = levls[fact0];

		# <p> new levels
		levlsN0 = firstDef(setLevels, levels, levls);
		levlsN = c(levlsN0, setdiff(levls, levlsN0));

		# <p> remove unwanted levels
		if (!is.null(setLevels)) r = ifelse(r %in% setLevels, r, NA);
		# <p> rename levels
		if (!is.null(setLevelsTo)) {
			r = drop.levels(ifelse(as.integer(r) <= length(setLevels), r, NA));
			levels(r) = setLevelsTo;
		}
		r = factor(r, levels = if (!is.null(setLevels)) levlsN0 else levlsN);
	}
	r
}

Union = function(..., .drop = T, as.list = FALSE) {
	l = if (as.list) list(...)[[1]] else list(...);
	l = list(...);
	# auto-detect list of values
	if (.drop && length(l) == 1 && is.list(l[[1]])) l = l[[1]];
	r = NULL;
	for (e in l) { r = union(r, e); }
	r
}
Intersect = function(..., .drop = T, as.list = FALSE) {
	l = if (as.list) list(...)[[1]] else list(...);
	# auto-detect list of values
	if (.drop && length(l) == 1 && is.list(l[[1]])) l = l[[1]];
	r = l[[1]];
	for (e in l[-1]) { r = intersect(r, e); }
	r
}

intersectSetsCount = function(sets) {
	i = iterateModels(list(s1 = names(sets), s2 = names(sets)), function(s1, s2) {
		length(intersect(sets[[s1]], sets[[s2]]))
	}, lapply__ = lapply);
	#r = reshape.wide(Df(i$models_symbolic, count = unlist(i$results)), 's1', 's2');
	rM = matrix(i$results, nrow = length(sets), byrow = T);
	dimnames(rM) = list(names(sets), names(sets));
	rM
}
unionCum = function(..., .drop = T) {
	l = list(...);
	# auto-detect list of values
	if (.drop && length(l) == 1 && is.list(l[[1]])) l = l[[1]];
	r = l[1];
	if (length(l) > 1)
		for (n in names(l)[-1]) { r = c(r, List(union(r[[length(r)]], l[[n]]), names_ = n)); }
	r
}

# row bind of data.frames/matrices with equal number of cols
lrbind = function(l, as.data.frame = F, names = NULL) {
	d = dim(l[[1]])[2];
	v = unlist(sapply(l, function(m) unlist(t(m))));
	m = matrix(v, byrow = T, ncol = d);
	dimnames(m) = list(NULL, names(l[[1]]));
	if (as.data.frame) {
		m = data.frame(m);
		if (!is.null(names)) names(m) = names;
	}
	m
}

#
#	logic arrays/function on list properties
#

# old versions:
#	if (na.rm) v = v[!is.na(v)];
#	sum(v)	# old version: length((1:length(v))[v])
# same as in Rlab
count = function(v, na.rm = T)sum(v, na.rm = na.rm)
# old versions:
#	if (na.rm) v = v[!is.na(v)]; (sum(v)/length(v))
#	{ length(v[v]) / length(v) }
# v assumed to be logical
fraction = function(v, na.rm = T)mean(v, na.rm = na.rm);
# treat v as set
set.card = function(v)count(unique(v))
# cardinality of a set
size = function(set)length(unique(set));

# null is false
#nif = function(b)(!(is.null(b) | is.na(b) | !b))
#nif = function(b)sapply(b, function(b)(!(is.null(b) || is.na(b) || !b)))
nif = function(b) {
	if (length(b) == 0) return(F);
	!(is.null(b) | is.na(b) | !b)
}
Nif = function(b, allnif = T, nonLogicalIsTrue = T) {
	bLog = sapply(b, as.logical);
	b = ifelse(is.na(b) | sapply(b, class) == 'logical', bLog, nonLogicalIsTrue);
	summ = (if (allnif) all else any);
	r = summ(sapply(b, nif));
	r
}
# null is true
#nit = function(b)(is.null(b) | is.na (b) | b)
#nit = function(b)sapply(b, function(b)(is.null(b) || is.na (b) || b))
nit = function(b) {
	if (length(b) == 0) return(T);
	is.null(b) | is.na (b) | b
}
# null is zero
#niz = function(e)ifelse(is.null(e) | is.na(e), 0, e)
niz = function(e)ifelse(is.null(e) | is.na(e), 0, e)

# null is na (or other special value
#niz = function(e)ifelse(is.null(e) | is.na(e), 0, e)
nina = function(e, value = NA)sapply(e, function(e)ifelse(is.null(e), value, e))
Nina = function(e, value = NA)if (length(e) == 0) value else nina(e, value);

# not empty
notE = function(e)(length(e) > 0);

#
#	<p> complex structures
#

#
# Averaging a list of data frames per entry over list elements
#

# meanMatrices = function(d) {
# 	df = as.data.frame(d[[1]]);
# 	ns = names(df);
# 	# iterate columns
# 	dfMean = sapply(ns, function(n) {
# 		m = sapply(d, function(e)as.numeric(as.data.frame(e)[[n]]));
# 		mn = apply(as.matrix(m), 1, mean, na.rm = T);
# 		mn
# 	});
# 	dfMean
# }
meanMatrices = function(d) {
	dm = dim(d[[1]]);
	good = sapply(d, function(m)(length(dim(m)) == 2 && all(dim(m) == dm)));
	if (any(!good)) warning('meanMatrices: malformed/incompatible matrices in list, ignored');
	d = d[good];
	m0 = sapply(d, function(e)avu(e));
	m1 = apply(m0, 1, mean, na.rm = T);
	r = matrix(m1, ncol = dm[2], dimnames = dimnames(d[[1]]));
	r
}
meanVectors = function(d) {
	ns = names(d[[1]]);
	mn = apply(as.matrix(sapply(d, function(e)e)), 1, mean, na.rm = T);
	mn
}
meanList = function(l)mean(as.numeric(l));

meanStructure = function(l) {
	r = nlapply(names(l[[1]]), function(n) {
		meanFct =
			if (is.matrix(l[[1]][[n]])) meanMatrices else
			if (length(l[[1]][[n]]) > 1) meanVectors else
				meanList;
		meanFct(list.key(l, n, unlist = F));
	});
	r
}

matrixCenter = function(m, direction = 2, centerBy = median) {
	center = apply(m, direction, centerBy, na.rm = T);
	m = if (direction == 1) (m - center) else t(t(m) - center);
	list(matrix = m, center = center)
}

matrixDeCenter = function(m, center, direction = 2) {
	m = if (direction == 1) t(t(m) + center) else (m + center);
	m
}


#
#	<p> combinatorial functions
#

# form all combinations of input arguments as after being constraint to lists
# .first.constant designates whether the first list changes slowest (T) or fastest (F)
#	in the resulting data frame,
#	i.e. all other factors are iterated for a fixed value of l[[1]] (T) or not
# .constraint provides a function to filter the resulting data frame
merge.multi.list = function(l, .col.names = NULL, .col.names.prefix = "X",
	.return.lists = F, .first.constant = T, stringsAsFactors = F, .cols.asAre = F, .constraint = NULL, ...) {
	# <p> determine column names of final data frame
	.col.names.generic = paste(.col.names.prefix, 1:length(l), sep = "");
	if (is.null(.col.names)) .col.names = names(l);
	if (is.null(.col.names)) .col.names = .col.names.generic;
	.col.names[.col.names == ""] = .col.names.generic[.col.names == ""];
	names(l) = .col.names;		# overwrite names
	# <p> construct combinations
	if (.first.constant) l = rev(l);
	df0 = data.frame();
	if (length(l) >= 1) for (i in 1:length(l)) {
		newNames = if (.cols.asAre) names(l[[i]]) else names(l)[i];
		# <p> prepare data.frame: handle lists as well as data.frames
		# <!> changed 22.3.2016
		#dfi = if (is.list(l[[i]])) unlist(l[[i]]) else l[[i]];
		dfi = if (!is.data.frame(l[[i]])) unlist(l[[i]]) else l[[i]];
		df1 = data.frame.types(dfi, names = newNames, stringsAsFactors = stringsAsFactors);
		# <p> perform merge
		df0 = if (i > 1) merge(df0, df1, ...) else df1;
	}
	if (.first.constant) df0 = df0[, rev(names(df0)), drop = F];
	if (.return.lists) df0 = apply(df0, 1, as.list);
	if (!is.null(.constraint)) {
		df0 = df0[apply(df0, 1, function(r).do.call(.constraint, as.list(r))), ];
	}
	df0
}

# analysis pattern using merge.multi.list
# i needs not to be an argument to f as .do.call strips excess arguments
iterateModels_old = function(modelList, f, ...,
	.constraint = NULL, .clRunLocal = T, .resultsOnly = F, .unlist = 0, lapply__ = clapply) {
	models = merge.multi.list(modelList, .constraint = .constraint);

	r = lapply__(1:dim(models)[1], function(i, ..., f__, models__) {
		args = c(list(i = i), as.list(models__[i, , drop = F]), list(...));
		.do.call(f__, args)
	}, ..., f__ = f, models__ = models);
	r = if (.resultsOnly) r else list(models = models, results = r);
	r = unlist.n(r, .unlist);
	r
}

# list of list, vector contains index for each of these lists to select elements from
#	these elements are merged and return
#	if sub-element is not a list, take name of sub-element and contruct list therefrom
#	namesOfLists controls whether, if a selected element is a list, its name is used instead
#		can be used to produce printable summaries
list.takenFrom = function(listOfLists, v) {
	ns = names(listOfLists);
	if (any(ns != names(v))) v = v[order_align(ns, names(v))];
	l = lapply(1:length(v), function(i) {
		new = if (!is.list(listOfLists[[i]]))
			listKeyValue(ns[i], listOfLists[[i]][v[i]]) else {
				t = listOfLists[[i]][[v[i]]];
				# list of vectors
				t = (if (!is.list(t)) {
					# define name from higher level
					listKeyValue(firstDef(
						names(listOfLists[[i]])[v[i]], ns[i]
					), list(t))
					# <A> probably better and correct
					#listKeyValue(ns[i], list(t))
				} else if (is.null(names(t))) listKeyValue(ns[i], t) else t);
				t
			}
	});
	names(l) = names(v);
	l
}

merge.lists.takenFrom = function(listOfLists, v) {
	merge.lists(list.takenFrom(listOfLists, v), listOfLists = TRUE);
}

merge.lists.takenFrom_old = function(listOfLists, v) {
	l = list();
	ns = names(listOfLists);
	if (any(ns != names(v))) v = v[order_align(ns, names(v))];
	for (i in 1:length(v)) {
		new = if (!is.list(listOfLists[[i]]))
			listKeyValue(ns[i], listOfLists[[i]][v[i]]) else {
				t = listOfLists[[i]][[v[i]]];
				# list of vectors
				t = (if (!is.list(t)) {
					# define name from higher level
					listKeyValue(firstDef(
						names(listOfLists[[i]])[v[i]], ns[i]
					), list(t))
					# <A> probably better and correct
					#listKeyValue(ns[i], list(t))
				} else if (is.null(names(t))) listKeyValue(ns[i], t) else t);
				t
			}
		l = merge.lists(l, new);
	}
	l
}

# take indeces given by v from a nested list
# namesOfLists: take the name of the list at the position in v
#	if null, take first element or leave aggregation to the function aggregator
# aggregator: called with the final result, should flatten existing lists into characters
lists.splice = function(listOfLists, v, namesOfLists = F, aggregator = NULL, null2na = T) {
	ns = names(listOfLists);
	l = lapply(1:length(ns), function(i) {
		name = ns[i];
		e = listOfLists[[i]][v[i]];
		r = if (!is.list(e)) e else {
			f = if (namesOfLists) {
				g = names(e)[1];
				# handle name == NULL
				if (is.null(g)) {
					# make an attempt later to print element
					#if (!is.null(aggregator)) e[[1]] else e[[1]][[1]]
					if (!is.null(aggregator))
						e[[1]] else
						join(as.character(e[[1]][[1]]), ", ")
				} else g
			} else e[[1]];
		}
		r
	});
	if (null2na) l = lapply(l, function(e)ifelse(is.null(e), NA, e));
	if (!is.null(aggregator)) l = aggregator(listKeyValue(ns, l), v, l);
	l
}

# dictionary produced by lists.splice, v: splice vector, l: aggregated list (w/o names)
merge.multi.symbolizer = function(d, v, l)unlist.n(d, 1);

merge.multi.list.symbolic = function(modelList, ..., symbolizer = NULL) {
	modelSize = lapply(modelList, function(m)1:length(m));
	models = merge.multi.list(modelSize, ...);
	namesDf = if (is.null(symbolizer)) names(modelList) else NULL;
	df0 = sapply(1:nrow(models), function(i, ...) {
		r = lists.splice(modelList, unlist(models[i, ]),
			namesOfLists = T, aggregator = symbolizer);
		r
	});
	r = Df_(df0, t_ = T, names = namesDf);
	r
}

inlist = function(l)lapply(l, function(e)list(e));
Inlist = function(...)inlist(list(...));

Do.callIm = function(im__f, args, ..., restrictArgs = TRUE, callMode = 'inline') {
	if (callMode == 'inlist') {
		.do.call(im__f, c(args, list(...)), restrictArgs = restrictArgs)
	} else if (callMode == 'list') {
		im__f(unlist.n(args, 1, reset = T), ...)
	} else if (callMode == 'inline') {
		args = c(merge.lists(args, listOfLists = TRUE), list(...));
		.do.call(im__f, args, restrictArgs = restrictArgs)
	} else stop('Unknown call mode');
}

# <!> should be backwards compatible with iterateModels_old, not tested
# modelList: list of lists/vectors; encapuslate blocks of parameters in another level of lists
# Example:
#
#' Iterate combinations of parameters
#'
#' This function takes a list of parameters for which several values are to be evaluated. These values can be vectors of numbers or lists that contain blocks of parameters. All combinations are formed and passed to a user supplied function \code{f}. This functions takes an index of the combination together with parameter values. Argument \code{callWithList} controls whether there is exactly one argument per parameter position or wether one more step of unlisting takes place. In case that a block of parameters is supplied, all values of the block are passed as individual arguments to \code{f} in case \code{callWithList == F}.
#'
#' @param selectIdcs restrict models to the given indeces
#'
#' @examples
#' modelList = list(global = list(list(a=1, b=2)), N = c(1, 2, 3));
#' print(iterateModels(modelList));
#' modelList = list(N = c(1, 2, 3), parsAsBlock = list(list(list(c = 1, d = 2)), list(list(c = 3, d = 4))));
#' print(iterateModels(modelList));
#' # ensure elements on A are given as a block (list)
#' A = list(list(a = 1, b = 2), list(a = 3, b = 5));
#' modelList = list(N = inlist(A), parsAsBlock = list(list(list(c = 1, d = 2)), list(list(c = 3, d = 4))));
#' print(iterateModels(modelList));
#' # shorter version of the above
#' modelList = list(N = Inlist(list(a = 1, b = 2), list(a = 3, b = 5)), parsAsBlock = Inlist(list(c = 1, d = 2), list(c = 3, d = 4)));
#' print(iterateModels(modelList));
#' # inline calling
#' modelList = list(N = list(list(a = 1, b = 2), list(a = 3, b = 5)), parsAsBlock = list(list(c = 1, d = 2), list(c = 3, d = 4)));
#' print(iterateModels(modelList));
#'
#'
#'
#' callMode: 'inline', 'list', 'inlist'
iterateModels_raw = function(modelList, models, f_iterate = function(...)list(...), ...,
	callWithList = F, callMode = NULL, restrictArgs = T, parallel = F, lapply__) {
	if (!parallel) Lapply = lapply;
	if (is.null(callMode)) callMode = if (callWithList) 'list' else 'inline';
	# model indeces contains the original positions in models
	# this allows reordering of execution, eg with reverseEvaluationOrder
	r = Lapply(1:nrow(models), function(i, ..., im__f, im__model_idcs) {
		args = c(list(i = list(i = im__model_idcs[i])), list.takenFrom(modelList, unlist(models[i, ])));
#if (callMode == 'list') browser();
		Do.callIm(im__f, args, ..., restrictArgs = restrictArgs, callMode = callMode);
	}, ..., im__f = f_iterate, im__model_idcs = as.integer(row.names(models)));
	r
}

# <i> refactor iterateModels to use iterateModels_prepare
iterateModels_prepare = function(modelList, .constraint = NULL,
	callWithList = FALSE, callMode = NULL, restrictArgs = T, selectIdcs = NULL, .first.constant = T) {
	# <p> preparation
	if (is.null(callMode)) callMode = if (callWithList) 'list' else 'inline';

	modelSize = lapply(modelList, function(m)1:length(m));
	models = merge.multi.list(modelSize, .first.constant = .first.constant);

	# <p> handle constraints
	selC = if (is.null(.constraint)) T else
		unlist(iterateModels_raw(modelList, models, f_iterate = .constraint,
			parallel = FALSE, callMode = callMode, restrictArgs = restrictArgs, ...));
	selI = if (is.null(selectIdcs)) T else 1:nrow(models) %in% selectIdcs;
	#	apply constraints
	models = models[selC & selI, , drop = F];
	r = list(
		modelsRaw = models,
		selection = selC & selI,
		models = models
	);
	r
}

iterateModelsDefaultSymbolizer = function(i, ...) {
	l = list(...);
	r = lapply(l, function(e)unlist(as.character(unlist(e)[1])));
	r
}
iterateModelsSymbolizer = function(i, ..., im_symbolizer, im_symbolizerMode) {
	l = list(...);
	l0 = iterateModelsDefaultSymbolizer(i, ...);
	l1 = .do.call(im_symbolizer, c(list(i = i), list(...)), restrictArgs = TRUE);
	r = merge.lists(l0, l1);
	r
}

# <i> make name of supplied model index, currently 'i', configurable
iterateModels = function(modelList, f = function(...)list(...), ...,
	.constraint = NULL, .clRunLocal = TRUE, .resultsOnly = FALSE, .unlist = 0,
	callWithList = FALSE, callMode = NULL,
	symbolizer = iterateModelsDefaultSymbolizer, symbolizerMode = 'inlist',
	restrictArgs = T, selectIdcs = NULL,
	.first.constant = TRUE, parallel = FALSE, lapply__, reverseEvaluationOrder = TRUE) {
	# <p> pre-conditions
	nsDupl = duplicated(names(modelList));
	if (any(nsDupl))
		stop(con('iterateModels: duplicated modelList entries: ', join(names(modelList)[nsDupl], ', ')));

	# <p> preparation
	if (is.null(callMode)) callMode = if (callWithList) 'list' else 'inline';

	# <p> produce raw combinations
	modelSize = lapply(modelList, function(m)1:length(m));
	models = merge.multi.list(modelSize, .first.constant = .first.constant);
# 	models_symbolic = merge.multi.list.symbolic(modelList,
# 		symbolizer = symbolizer, .first.constant = .first.constant);
	models_symbolic = do.call(rbind, iterateModels_raw(modelList, models, iterateModelsSymbolizer,
		callMode = 'inlist', parallel = F,
		im_symbolizerMode = symbolizerMode, im_symbolizer = symbolizer));

	# <p> handle constraints
	selC = if (is.null(.constraint)) T else
		unlist(iterateModels_raw(modelList, models, f_iterate = .constraint,
			callMode = callMode, restrictArgs = restrictArgs, ..., parallel = F));
	selI = if (is.null(selectIdcs)) T else 1:nrow(models) %in% selectIdcs;
	# <p> apply constraints
	models = models[selC & selI, , drop = F];
	models_symbolic = models_symbolic[selC & selI, , drop = F];

	# <p> models to be iterated
	modelsIt = if (reverseEvaluationOrder) models[rev(1:nrow(models)), , drop = F] else models;
	r = iterateModels_raw(modelList, modelsIt, f_iterate = f,
		callMode = callMode, restrictArgs = restrictArgs, ..., parallel = parallel);
	if (reverseEvaluationOrder) r = rev(r);
	r = if (.resultsOnly) r else list(
		models = models,
		results = r,
		models_symbolic = models_symbolic
	);
	r = unlist.n(r, .unlist);
	r
}

iterateModelsExpand = function(modelList, .constraint = NULL) {
	modelSize = lapply(modelList, function(m)1:length(m));
	models = merge.multi.list(modelSize, .constraint = .constraint);
	r = list(
		models = models,
		models_symbolic = merge.multi.list.symbolic(modelList, .constraint = .constraint)
	);
	r
}

IterateModelsExpand = function(modelList, .constraint = NULL) {
	iterateModels(modelList, identity, .constraint = .constraint, callWithList = TRUE)$results
}

# reverse effect of .retern.lists = T
#	list.to.df(merge.multi.list(..., .return.lists = T)) === merge.multi.list(..., .return.lists = F)
list.to.df = function(l)t(sapply(l, function(e)e))

merge.multi = function(..., .col.names = NULL, .col.names.prefix = "X",
	.return.lists = F, stringsAsFactors = F, .constraint = NULL, .first.constant = T) {
	merge.multi.list(list(...), .col.names = .col.names, .return.lists = .return.lists,
		stringsAsFactors = stringsAsFactors, .constraint = .constraint, .first.constant = .first.constant)
}

merge.multi.dfs = function(l, .first.constant = T, all = T, stringsAsFactors = F, ...) {
	if (.first.constant) l = rev(l);
	if (length(l) >= 1) for (i in 1:length(l)) {
		df1 = data.frame.types(l[[i]], stringsAsFactors = stringsAsFactors);
		df0 = if (i > 1) merge(df0, df1, all = all, ...) else df1;
	}
	if (.first.constant) df0 = df0[, rev(names(df0)), drop = F];
	df0
}

Merge = function(x, y, by = intersect(names(x), names(y)), ..., safemerge = T, stableByX = FALSE) {
	if (stableByX) x = data.frame(x, MergeStableByX = 1:nrow(x));
	if (safemerge && length(by) == 0) {
		stop(sprintf('Merge: safemerge triggered. No common columns between "%s" and "%s"',
			join(names(x), sep = ','), join(names(y), sep = ',')))
	}
	r = merge(x = x, y = y, by = by, ...);
	if (stableByX) {
		indexCol = which(names(r) == 'MergeStableByX');
		r = r[order(r$MergeStableByX), -indexCol, drop = FALSE];
	}
	r
}

# ids: variables identifying rows in final table
# vars: each combination of vars gets transformed to an own column
# <!> not tested for length(ids) > 1 || ength(rvars) > 1
# blockVars: should the repeated vars go in blocks or be meshed for vars
#
# Examples:
# intersection table
# i = intersectSetsCount(sets);
# reshape.wide(Df(i$models_symbolic, count = unlist(i$results)), 's1', 's2');
reshape.wide = function(d, ids, vars, blockVars = F, reverseNames = F, sort.by.ids = T) {
	# remaining vars
	rvars = setdiff(names(d), union(ids, vars));
	# levels of variables used in the long expansion
	levls = lapply(vars, function(v)unique(as.character(d[[v]])));
	# combinations at the varying vars as passed to vars
	cbs = merge.multi.list(levls, .col.names = vars, .first.constant = !blockVars);
	# repvars: repeated variables
	repvars = merge.multi.list(c(list(rvars), levls),
		.first.constant = !blockVars, .col.names = c("..var", vars));
	varnames = apply(repvars, 1, function(r)join(if (reverseNames) rev(r) else r, "."));

	r0 = data.frame.types(unique(d[, ids], drop = F), names = ids);
	r1 = data.frame.types(apply(r0, 1, function(r) {
		# <p> isolate rows which match to current id columns
		ids = which(apply(d[, ids, drop = F], 1, function(id)all(id == r)));
		d1 = d[ids, ];
		# <p> construct vector of repeated values
		vs = sapply(1:dim(cbs)[1], function(i) {
			# <A> should be equal to one
			row = which(apply(d1[, vars, drop = F], 1, function(r)all(r == cbs[i, ])));
			v = if (length(row) != 1) rep(NA, length(rvars)) else d1[row, rvars];
			v
		});
		# heed blockVars
		vs = as.vector(unlist(if (!blockVars) t(vs) else vs));
		vs
	}), do.transpose = T, names = varnames);
	r = data.frame(r0, r1);
	if (sort.by.ids) r = r[order.df(r, ids), ];
	row.names(r) = NULL;
	r
}

#' Convert data in wide format to long format
#' 
#' Long format duplicates certain columns and adds rows for which one new column hold values coming
#' from a set of columns in wide format.
#'
#' @param d data frame with columns in wide format
#' @param vars columns in wide format by name or index
#' @param factors \code{vars} can be grouped. For each level of \code{factor} a new row is created. Implies
#'			that \code{length(vars)} is a multiple of \code{length(levels(factor))}
#' @param factorColumn name of the column to be created for the factor
#' @param valueColumn name of the new column of values that were in wide format
# factors: provide factor combinations explicitly for vars (otherwise split by '.', <i>)
#' @examples
#'	#reshape variables 2:9 (forming two groups: case/ctr), value of which is named 'group'
#'	# the shortened columns will get names valueColumn
#'	d0 = reshape.long(d, vars = 2:9, factors = c('case', 'ctr'), factorColumn = 'group',
#'		valueColumn = c('AA', 'AG', 'GG', 'tot'));
reshape.long = function(d, vars = NULL, factorColumn = 'factor', valueColumn = 'value',
	factors = as.factor(vars), useDisk = F, rowNamesAs = NULL) {
	if (is.null(vars)) vars = names(d);
	# make rownames an extra column
	if (!is.null(rowNamesAs)) {
		d = data.frame(reshape_row_names__ = rownames(d), d);
		names(d)[1] = rowNamesAs;
	}
	# indeces of columns vars
	Ivars = .df.cols(d, vars);
	# remaining vars
	rvars = setdiff(1:length(names(d)), Ivars);
	# names thereof
	Nrvars = names(d)[rvars];

	# how wide are the blocks?
	S = length(vars) / length(factors);
	# columns of intermediate data.frame
	N = length(rvars);
	# create list of data frames
	dfs = lapply(1:nrow(d), function(i) {
		st = d[i, rvars];	# start of the new row
		df0 = data.frame(factors, value =  matrix(d[i, vars], nrow = length(factors), byrow = T));
		df1 = data.frame(st, df0, row.names = NULL);
		names(df1) = c(Nrvars, factorColumn, valueColumn);
		df1
	});
	r = rbindDataFrames(dfs, do.unlist = T, useDisk = useDisk);
	r
}

#' Reduce data frame by picking the first row of blocks for which \code{cols} has the same values
uniqueByCols = function(d, cols) {
	row.names(d) = NULL;
	d[as.integer(row.names(unique(d[, cols, drop = F]))), ]
}


#
# <p> string functions
#

uc.first = firstUpper = function(s) {
	paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = "");
}

#
#	<p> factor transformations for data frames
#

dataExpandedNames = function(data) {
	dnames = unlist(lapply(names(data), function(v){
		if (is.factor(data[[v]])) paste(v, 1:(length(levels(data[[v]])) - 1), sep = "") else v;
	}));
	dnames
}
# model.matrix removes missing columns and could not be tweaked into working
dataExpandFactors = function(data, vars =  NULL) {
	if (is.null(vars)) vars = names(data);
	d0 = lapply(vars, function(v) {
		if (is.factor(data[[v]])) {
			ls = levels(data[[v]]);
			dcNa = rep(NA, length(ls) - 1);	# missing data coding
			dc = rep(0, length(ls) - 1);	# dummy coding
			sapply(data[[v]], function(e) {
				if (is.na(e)) return(dcNa);
				i = which(e == ls);
				if (i == 1) return(dc);
				dc[i - 1] = 1;
				return(dc);
			});
		} else data[[v]];
	});
	d0names = dataExpandedNames(data[, vars]);
	# re-transform data
	d1 = data.frame(matrix(unlist(lapply(d0, function(e)t(e))), ncol = length(d0names), byrow = F));
	names(d1) = d0names;
	d1
}
coefficientNamesForData = function(vars, data) {
	lnames = dataExpandedNames(data);	# names of levels of factors
	cnames = lnames[unlist(sapply(vars, function(v)which.indeces(v, lnames, regex = T)))];
	cnames
}

#
# <p> statistic oriented data frame manipulation
#

variableIndecesForData = function(d, vars, varsArePrefixes = T) {
	if (varsArePrefixes) vars = sapply(vars, function(e)sprintf('%s.*', e));
	which.indeces(vars, names(d), regex = T, match.multi = T)
}
variablesForData = function(d, vars, varsArePrefixes = T) {
	names(d)[variableIndecesForData(d, vars, varsArePrefixes)]
}

subData = function(d, vars, varsArePrefixes = T) {
	dfr = d[, variableIndecesForData(d, vars, varsArePrefixes), drop = F];
	dfr
}

subDataFromFormula = function(d, formula, responseIsPrefix = T, covariateIsPrefix = T) {
	resp = formula.response(formula);
	cov = formula.covariates(formula);
	ns = names(d);
	r = list(
		response = subData(d, resp, responseIsPrefix),
		covariate = subData(d, cov, covariateIsPrefix)
	);
	r
}

#
#	<p> graph functions
#

sub.graph.merge = function(df, leader, follower) {
	# next transitive step
	r0 = merge(df, data.frame(leader = leader, follower = follower), by = 'follower');
	# add new connections
	r1 = rbind(df, data.frame(follower = r0$leader.y, leader = r0$leader.x, cluster = r0$cluster));
	# symmetric closure
	r1 = rbind(r1, data.frame(follower = r1$leader, leader = r1$follower, cluster = r1$cluster))
	# form clusters by selecting min cluster number per connection
	r1 = r1[order(r1$cluster), ];
	row.names(r1) = 1:dim(r1)[1];
	r2 = unique(r1[, c('leader', 'follower')]);
	# select unique rows (first occurunce selects cluster)
	r = r1[as.integer(row.names(r2)), ];
	# pretty sort data frame
	r = r[order(r$cluster), ];
	r
}
# form clusters from a relationally defined hierarchy
sub.graph = function(df) {
	df = as.data.frame(df);
	names(df)[1:2] = c('follower', 'leader');
	df = df[order(df$follower), ];
	# seed clusters
	ids = sort(unique(df$follower));
	idsC = as.character(ids);
	counts = lapply(ids, function(id)sum(df$follower == id));
	names(counts) = idsC;
	clusters = unlist(sapply(idsC, function(id){ rep(as.integer(id), counts[[id]]) }));

	df = cbind(df, data.frame(cluster = rep(clusters, 2)));
	df = unique(rbind(df, data.frame(follower = df$leader, leader = df$follower, cluster = df$cluster)));
	# receiving frame
	df0 = df;
	# results with clusters
	i = 1;
	repeat {
		Nrows = dim(df0)[1];
		cls = df0$clusters;
		# add transitive connections
		df0 = sub.graph.merge(df0, follower = df0$leader, leader = df0$follower);
		if (dim(df0)[1] == Nrows && all(cls == df0$clusters)) break();
	}
	df0 = df0[order(df0$cluster), ];
	cIds = unique(df0$cluster);
	cls = lapply(cIds, function(id)unique(avu(df0[df0$cluster == id, c('follower', 'leader')])));
	cls
}

#
#	<p> formulas
#

# formula: formula as a character string with wildcard character '%'
# 	<!>: assume whitespace separation in formula between terms
#	<!>: write interaction with spaces <!> such as in:
#		f = 'MTOTLOS_binair ~ ZRES% + sq(ZRes%) + ( ZRES% )^2';
formula.re = function(formula, data, ignore.case = F, re.string = '.*') {
	vars = names(data);
	#regex = '(?:([A-Za-z_.]+[A-Za-z0-9_.]*)[(])?([A-Za-z.]+[%][A-Za-z0-9.%_]*)(?:[)])?';
	#			function names				(    regex						   )
	#regex = '(?:([A-Za-z_.]+[A-Za-z0-9_.]*)[(])?([A-Za-z%.]+[A-Za-z0-9.%_]*)(?:[)])?';
	# allow backslash quoting
	regex = '(?:([A-Za-z_.\\\\]+[A-Za-z0-9_.\\\\]*)[(])?([A-Za-z%.\\\\]+[A-Za-z0-9.%_\\\\]*)(?:[)])?';
	patterns = unique(fetchRegexpr(regex, formula, ignore.case = ignore.case));
	subst = nlapply(patterns, function(p) {
		comps = fetchRegexpr(regex, p, captureN = c('fct', 'var'), ignore.case = ignore.case)[[1]];
		p = sprintf("^%s$", gsub('%', re.string, comps$var));
		mvars = vars[sapply(vars, function(v)regexpr(p, v, perl = T, ignore.case = ignore.case)>=0)];
		if (comps$fct != '') {
			varf = sprintf('%s', paste(sapply(mvars, function(v)sprintf('%s(%s)', comps$fct, v)),
				collapse = " + "));
		} else {
			varf = sprintf('%s', paste(mvars, collapse = " + "));
		}
		varf
	});
	formulaExp = as.formula(mergeDictToString(subst, formula));
	formulaExp
}

formula.response = function(f) {
	#r = fetchRegexpr('[^\\s~][^~]*?(?=\\s*~)', if (is.formula(f)) deparse(f) else f);
	f = if (class(f) == 'formula') join(deparse(f), '') else f;
	r = as.character(fetchRegexpr('^\\s*([^~]*?)(?:\\s*~)', f, captures = T));
	# <p> version 2
	#fs = as.character(as.formula(as.character(f)));	# "~" "response" "covs"
	#r = fs[2];
	# <p> version 1
	#f = as.formula(f);
	#r = all.vars(f)[attr(terms(f), "response")];	# fails to work on 'response ~ .'
	r
}
formula.rhs = function(f, noTilde = FALSE, as_character = FALSE) {
	rhs = fetchRegexpr('[~](.*)', if (!is.character(f)) formula.to.character(f) else f, captures = T);
	r = if (noTilde) rhs else con('~', rhs);
	r = if (as_character) r else as.formula(r);
	r
}
formula.covariates = function(f) {
	covs = all.vars(formula.rhs(f));
	#covs = setdiff(all.vars(as.formula(f)), formula.response(f));
	covs
}
formula.vars = function(f)union(formula.response(f), formula.covariates(f));

formula.nullModel = function(f) {
	r = formula.response(f);
	fn = as.formula(sprintf("%s ~ 1", r));
	fn
}
formula.to.character = function(f)join(deparse(as.formula(f)), '');
Formula.to.character = function(f)ifelse(is.character(f), f, formula.to.character(f));

formula2filename = function(f) {
	fs = join(f, sep = '');
	filename = mergeDictToString(list(
		`\\s+` = '',
		`_` = '-',
		`Surv\\(.*\\)` = 'surv',
		MARKER = 'snp'
		# other components
	), fs, re = T, doApplyValueMap = F, doOrderKeys = F);
	filename
}
data.vars = function(data, formula, re.string = '.*', ignore.case = F) {
	all.vars(formula.re(formula = formula, data = data, re.string = re.string, ignore.case = ignore.case));
}
formula.add.rhs = function(f0, f1) {
	as.formula(join(c(formula.to.character(f0), formula.rhs(f1, noTilde = TRUE)), '+'))
}
formula.add.response = function(f0, f1, envir = parent.frame()) {
	formula = join(c(formula.response(f1), formula.rhs(f0, noTilde = FALSE)), ' ');
	as.formula(formula, env = envir)
}
formula.predictors = function(f, data, dataFrameNames = TRUE) {
	if (formula.rhs(f) == ~ 1) return('(Intercept)');
	mm = model.matrix(model.frame(formula.rhs(f), data), data);
	ns = dimnames(mm)[[2]];

	# <p> create data frame to extract proper names
# 	if (dataFrameNames) {
# 		df0 = as.data.frame(t(rep(1, length(ns))));
# 		names(df0) = ns;
#		ns = names(df0);
# 	}
	ns
}

# <!> cave survival
formulaRemoveTransformation = function(model) {
	respVar = setdiff(all.vars(model), all.vars(formula.rhs(model)));
	formula.add.response(formula.rhs(model), as.formula(Sprintf('%{respVar}s ~ 1')))
}

formulas.free = function(f1, f0, data) {
	setdiff(formula.predictors(f1, data), formula.predictors(f0, data))
}


# <i> use terms.formula from a (a + ... + z)^2 formula
# <i> merge.multi.list(rep.list(covs, 2), .constraint = is.ascending)
covariatePairs = function(covs) {
	pairs = merge(data.frame(c1 = 1:length(covs)), data.frame(c2 = 1:length(covs)));
	pairs = pairs[pairs[, 1] > pairs[ ,2], ];
	df = data.frame(c1 = covs[pairs[, 1]], c2 = covs[pairs[, 2]]);
	df
}

formulaWith = function(repsonse = "y", covariates = "x")
	as.formula(sprintf("%s ~ %s", repsonse,  paste(covariates, collapse = "+")))

#
#	<p> set operations
#

minimax = function(v, min = -Inf, max = Inf) {
	r = ifelse(v < min, min, ifelse(v > max, max, v));
	r
}
#
#	Rsystem.R
#Mon 27 Jun 2005 10:51:30 AM CEST 

#
#	<par> file handling
#

# <!><N> works only on atomic path
# <!> 5.1.2016: trailing slash leads to basename of ""
splitPath = function(path, removeQualifier = T, ssh = F, skipExists = F) {
	if (is.null(path)) return(NULL);
	if (removeQualifier) {
		q = fetchRegexpr('(?<=^\\[).*?(?=\\]:)', path);
		if (length(q) > 0) path = substr(path, nchar(q) + 4, nchar(path));
	}
	sshm = list(user = '', host = '', userhost = '');
	if (ssh) {
		sshm = fetchRegexpr('^(?:(?:([a-z]\\w*)(?:@))?([a-z][\\w.]*):)?(.*)', path,
			ignore.case = T, captureN = c('user', 'host', 'path'))[[1]];
		sshm$userhost = if (sshm$user != '') sprintf('%s@%s', sshm$user, sshm$host) else sshm$host;
		path = sshm$path;
	}

	#path = "abc/def.ext";
	#r.base = basename(path);
	#re = "([^.]*$)";
	#r = gregexpr(re, r.base)[[1]];
	#ext = substr(r.base, r[1], r[1] + attr(r, "match.length")[1] - 1);
	#ext = firstDef(fetchRegexpr('(?<=\\.)[^/.]+\\Z', path), '');
	ext = fetchRegexpr('(?<=\\.)[^/.]+\\Z', path);
	# take everything before ext and handle possible absence of '.'
	#base = substr(r.base, 1, r[1] - 1 - (ifelse(substr(r.base, r[1] - 1, r[1] - 1) == '.', 1, 0)));
	# reduce to file.ext
	Nchar = nchar(path);
	if (Nchar != 0 && substr(path, Nchar, Nchar) == '/') {
		base = '';
		dir = substr(path, 1, Nchar - 1);
	} else {
		base = basename(path);
		dir = dirname(path);
	}
	# base as yet still contains the file extension
	file = base;
	# chop off extension if present
	if (length(fetchRegexpr('\\.', base)) > 0) base = fetchRegexpr('\\A.*(?=\\.)', base);
	
	#pieces = regexpr(re, path, perl = T);
	pieces = fetchRegexpr('([^.]+)', path);
	isAbsolute = Nchar != 0 && substr(path, 1, 1) == '/';
	# <N> disk is accessed
	exists = if (!skipExists) File.exists(path, host = sshm$userhost, ssh = F) else NA;
	nonempty = exists && (file.info(path)$size > 0);
	ret = list(
		dir = dir,
		base = base,
		path = path,
		fullbase = sprintf("%s/%s", dir, base),
		ext = ext,
		file = file,
		isAbsolute = isAbsolute,
		absolute = if (isAbsolute) path else sprintf('%s/%s', getwd(), path),
		# fs properties
		exists = exists, nonempty = nonempty,
		# remote
		is.remote = !(sshm$user == '' && sshm$host == ''),
			user = sshm$user, host = sshm$host, userhost = sshm$userhost
	);
	ret
}
path.absolute = absolutePath = function(path, home.dir = T, ssh = T) {
	path = splitPath(path, ssh = ssh)$path;
	if (home.dir && nchar(path) >= 2 && substr(path, 1, 2) == "~/")
		path = sprintf("%s/%s", Sys.getenv('HOME'), substr(path, 3, nchar(path)));
	if (nchar(path) > 0 && substr(path, 1, 1) == "/") path else sprintf("%s/%s", getwd(), path)
}
tempFileName = function(prefix, extension = NULL, digits = 6, retries = 5, inRtmp = F,
	createDir = F, home.dir = T, doNotTouch = F) {
	ext = if (is.null(extension)) '' else sprintf('.%s', extension);
	path = NULL;
	if (inRtmp) prefix = sprintf('%s/%s', tempdir(), prefix);
	if (home.dir) prefix = path.absolute(prefix, home.dir = home.dir);
	for (i in 1:retries) {
		path = sprintf('%s%0*d%s', prefix, digits, floor(runif(1) * 10^digits), ext);
		if (!File.exists(path)) break;
	}
	if (File.exists(path))
		stop(sprintf('Could not create tempfile with prefix "%s" after %d retries', prefix, retries));
	# potential race condition <N>
	if (createDir)
		Dir.create(path, recursive = T) else
		if (!doNotTouch) writeFile(path, '', mkpath = T, ssh = T);
	# # old implementation
	#path = tempfile(prefix);
	#cat('', file = path);	# touch path to lock name
	#path = sprintf("%s%s%s", path, ifelse(is.null(extension), "", "."),
	#	ifelse(is.null(extension), "", extension));
	Log(sprintf('Tempfilename:%s', path), 5);
	path
}
dirList = function(dir, regex = T, case = T) {
	base = splitPath(dir)$dir;
	files = list.files(base);
	if (regex) {
		re = splitPath(dir)$file;
		files = files[grep(re, files, perl = T, ignore.case = !case)];
	}
	files
}
list_files_with_exts = function(path, exts, full.names = T)
	list.files(path, pattern = Sprintf('.(%{Exts}s)$', Exts = join(exts, '|')), full.names = full.names);

write.csvs = function(t, path, semAppend = "-sem", ...) {
	s = splitPath(path);
	write.csv(t, path);
	pathSem = sprintf("%s%s.%s", s$fullbase, semAppend, s$ext);
	# make sure t is a data.frame or dec option will not take effect <A>
	#write.csv2(t, pathSem);
	write.table(t, file = pathSem, row.names = F, col.names = T, dec = ",", sep = ";");
}

#
#	<p> file manipulation
#

File.exists = function(path, host = '', agent = 'ssh', ssh = T) {
	if (ssh) {
		sp = splitPath(path, skipExists = T, ssh = T);
		host = sp$userhost;
		path = sp$path;
	}
	r = if (!is.null(host) && host != '') {
		ret = system(sprintf('%s %s stat %s >/dev/null 2>&1', agent, host, qs(path)));
		ret == 0
	} else file.exists(path);
	r
}

File.copy_raw = function(from, to, ..., recursive = F, agent = 'scp', logLevel = 6, ignore.shell = T,
	symbolicLinkIfLocal = T) {
	spF = splitPath(from, ssh = T);
	spT = splitPath(to, ssh = T);
	is.remote.f = !spF$is.remote || spF$host == 'localhost';
	is.remote.t = !spT$is.remote || spT$host == 'localhost';

	r = if (!is.remote.f && !is.remote.t) {
		if (symbolicLinkIfLocal) {
			file.symlink(spF$path, spT$path, ...);
		} else file.copy(spF$path, spT$path, recursive = recursive, ...);
	} else {
		# <A> assume 'to' to be atomic
		System(sprintf('%s %s %s %s %s',
			agent,
			ifelse(recursive, '-r', ''),
			paste(sapply(from, qs), collapse = ' '),
			qs(to),
			ifelse(ignore.shell, '>/dev/null', '')
		), logLevel);
	}
	r
}

File.copy = function(from, to, ..., recursive = F, agent = 'scp', logLevel = 6, ignore.shell = T,
	symbolicLinkIfLocal = T) {
	if (is.null(from)) return(NULL);
	pairs = cbind(from, to);
	r = apply(pairs, 1, function(r) {
		File.copy_raw(r[1], r[2], ...,
			recursive = recursive, agent = agent, logLevel = logLevel,
			ignore.shell = ignore.shell, symbolicLinkIfLocal = symbolicLinkIfLocal)
	})
	r
}

File.remove = function(path, ..., agent = 'ssh', ssh = T, logLevel = 6) {
	r = if (ssh) {
		sp = splitPath(path, skipExists = T, ssh = T);
		host = sp$userhost;
		rpath = sp$path;
		if (File.exists(path, ssh = T))
			System(sprintf('rm %s', join(sapply(rpath, qs))), pattern = agent,
				ssh_host = host, logLevel = logLevel);
	} else if (file.exists(path)) file.remove(path, ...);
	r
}

# <i> remote operations
File.symlink = function(from, to, replace = T, agent = 'ssh', ssh = F, logLevel = 6) {
	r = if (ssh) {
		sp = splitPath(from, skipExists = T, ssh = T);
		host = sp$userhost;
		rpath = sp$path;
		# <!><i>
		stop('not implmenented');
	} else {
		Log(sprintf('symlink %s -> %s', qs(from), qs(to)), logLevel);
		if (replace && file.exists(to)) file.remove(to);
		file.symlink(from, to);
	}
	r
}


# <!> only atomic path
#	treatAsFile: causes Dir.create to split off last path-component
Dir.create = function(path, ..., recursive = F, agent = 'ssh', logLevel = 6,
	ignore.shell = T, allow.exists = T, treatPathAsFile = F) {
	sp = splitPath(path, ssh = T);
	# ignore last path-component
	if (treatPathAsFile) {
		sp$path = sp$dir;
		Log(sprintf('creating path %s', sp$path), 4);
	}
	if (sp$is.remote) {
		System(sprintf('ssh %s mkdir %s %s %s',
			sp$userhost,
			if (recursive) '--parents' else '',
			paste(sapply(sp$path, qs), collapse = ' '),
			if (ignore.shell) '2>/dev/null' else ''
		), logLevel);
	} else {
		if (allow.exists && !file.exists(sp$path)) dir.create(sp$path, ..., recursive = recursive);
	}
}

Save = function(..., file = NULL, symbolsAsVectors = F, mkpath = T, envir = parent.frame(1)) {
	sp = splitPath(file, ssh = T);
	localPath = if (sp$is.remote) tempfile() else file;
	if (mkpath) { Dir.create(file, recursive = T, treatPathAsFile = T); }
	r = if (symbolsAsVectors) {
		do.call('save', c(as.list(c(...)), list(file = localPath)), envir = envir);
	} else save(..., file = localPath, envir = envir);
	if (sp$is.remote) File.copy(localPath, file);
	r
}
Load = function(..., file = NULL, Load_sleep = 0, Load_retries = 3, envir = parent.frame(1), logLevel = 6) {
	sp = splitPath(file, ssh = T);
	localPath = if (sp$is.remote) tempfile() else file;
	r = NULL;
	for (i in 1:Load_retries) {
		if (sp$is.remote) {
			if (!File.exists(file)) {
				Sys.sleep(Load_sleep);
				next;
			}
			File.copy(file, localPath, logLevel = logLevel);
		}
		r = try(load(..., file = localPath, envir = envir));
		if (class(r) == 'try-error' && Load_sleep > 0) Sys.sleep(Load_sleep) else break;
	}
	if (is.null(r)) stop(sprintf('could not Load %s', file));
	if (class(r) == 'try-error') stop(r[1]);
	r
}

#
#	create output file names
# output = list(prefix = "results/pch", extension = "pdf", tag = "20100727");
fileName = function(output, extension = NULL, subtype = NULL) {
	if (is.null(output)) return(NULL);
	if (is.null(output$prefix)) return(NULL);
	subtype = firstDef(subtype, output$subtype, "");
	if (subtype != "") subtype =  sprintf("%s-", subtype);
	r = sprintf("%s-%s%s.%s", output$prefix, subtype, output$tag,
		firstDef(extension, output$extension, ""));
	Log(r, 4);
	r
}
#.globalOutput = list(prefix = 'results/20120126-');
#save(r, file = .fn('simulation', 'RData'))
.globalOutputDefault = .globalOutput = list(prefix = '', tag = NULL, tagFirst = F);
GlobalOutput_env__ = new.env();
# .fn.set(prefix = 'results/predictionTesting-')
# @par prefix character, start path name with this character string
# @par tag character, add dashed string to all files (defaults to appending to filename)
# @par tagFirst boolean, put tag as a prefix to the file name instead
.fn.set = function(...) {
	.globalOutput = merge.lists(.globalOutputDefault, list(...));
	assign('.globalOutput', .globalOutput, envir = GlobalOutput_env__);
}
# create output file name on globalOptions
.fn = function(name, extension = '', options = NULL) {
	o = merge.lists(.globalOutputDefault, .globalOutput,
		get('.globalOutput', envir = GlobalOutput_env__), options);
	# construct plain filename
	pathes = sprintf('%s%s%s%s', o$prefix, name, ifelse(extension == '', '', '.'), extension);
	fn = sapply(pathes, function(path) {
		sp = splitPath(path);
		# <p> dir
		if (!file.exists(sp$dir)) dir.create(sp$dir);
		# <p> tag
		ext = firstDef(sp$ext, '');
		fn = if (!is.null(o$tag)) {
			if (o$tagFirst) {
				sprintf('%s/%s-%s%s%s', sp$dir, o$tag, sp$base, ifelse(ext == '', '', '.'), ext)
			} else { sprintf('%s/%s-%s%s%s', sp$dir, sp$base, o$tag, ifelse(ext == '', '', '.'), ext) };
		} else sprintf('%s/%s%s%s', sp$dir, sp$base, ifelse(ext == '', '', '.'), ext);
		fn
	});
	avu(fn)
}
.fn.pushPrefix = function(prefix) {
	output = merge.lists(.globalOutput, list(prefix = sprintf('%s%s', .globalOutput$prefix, prefix)));
	assign('.globalOutput', output, envir = GlobalOutput_env__);
	.globalOutput
}
.fn.popPrefix = function(prefix) {
	output = merge.lists(.globalOutput, list(prefix = sprintf('%s/', splitPath(.globalOutput$prefix)$dir)));
	assign('.globalOutput', output, envir = GlobalOutput_env__);
	.globalOutput
}

#
#	command argument handling
#

# default args: command line call minus command
evaluateArgs = function(c = commandArgs()[-1]) {
	is.no.option = is.na(as.integer(sapply(c, function(a)grep("^--", a))));
	#c = c[!(c == "--vanilla")];	# eliminate '--vanilla' arguments
	c = c[is.no.option];
	if (length(c) > 0) {
		eval.parent(parse(text = c[1]));
		argListString = gsub(";", ",", gsub(";$", "", c[1]));
		print(argListString);
		return(eval(parse(text = sprintf("list(%s)", argListString))));
	}
	return(NULL);
}

# default args: command line call minus command
getCommandOptions = function(c = commandArgs()[-1]) {
	is.no.option = is.na(as.integer(sapply(c, function(a)grep("^--", a))));
	#c = c[!(c == "--vanilla")];	# eliminate '--vanilla' arguments
	c = c[is.no.option];
	o = lapply(c, function(e) {
		eval(parse(text = e));
		nlapply(setdiff(ls(), 'e'), function(n)get(n))
	});
	o = unlist.n(o, 1);
	o
}

# R.pl interface

handleTriggers = function(o, triggerDefinition = NULL) {
	if (is.null(triggerDefinition)) triggerDefinition = rget('.globalTriggers');
	if (!is.list(o) || is.null(triggerDefinition)) return(NULL);
	for (n in names(triggerDefinition)) {
		if (!is.null(o[[n]])) triggerDefinition[[n]](o$args, o);
	}

}


#
#	level dependend logging
#
#Global..Log..Level = 4;
#Default..Log..Level = 4;
#assign(Default..Log..Level, 4, envir = .GlobalEnv);
Log_env__ <- new.env();
assign('DefaultLogLevel', 4, envir = Log_env__);

#' Log a message to stderr.
#' 
#' Log a message to stderr. Indicate a logging level to control verbosity.
#' 
#' This function prints a message to stderr if the condition is met that a
#' global log-level is set to greater or equal the value indicated by
#' \code{level}. \code{Log.level} returns the current logging level.
#' 
#' @aliases Log Log.setLevel Log.level
#' @param o Message to be printed.
#' @param level If \code{Log.setLevel} was called with this value, subsequent
#' calls to \code{Log} with values of \code{level} smaller or equal to this
#' value will be printed.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{Log.setLevel}}, ~~~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' 	Log.setLevel(4);
#' 	Log('hello world', 4);
#' 	Log.setLevel(3);
#' 	Log('hello world', 4);
#' 
Log = function(o, level = get('DefaultLogLevel', envir = Log_env__), doPrint = NULL) {
	if (level <= get('GlobalLogLevel', envir = Log_env__)) {
		cat(sprintf("R %s: %s\n", date(), as.character(o)));
		if (!is.null(doPrint)) print(doPrint);
	}
}
Logs = function(o, level = get('DefaultLogLevel', envir = Log_env__), ..., envir = parent.frame()) {
	Log(Sprintf(o, ..., envir = envir), level = level);
}

Log.level = function()get('GlobalLogLevel', envir = Log_env__);
Log.setLevel = function(level = get('GlobalLogLevel', envir = Log_env__)) {
	assign("GlobalLogLevel", level, envir = Log_env__);
}
Log.setLevel(4);	# default

.System.fileSystem = list(
	#tempfile = function(prefix, ...)tempfile(splitPath(prefix)$base, tmpdir = splitPath(prefix)$dir, ...),
	tempfile = function(prefix, ...)tempFileName(prefix, ...),
	readFile = function(...)readFile(...)
);
.System.patterns = list(
	default = list(pre = function(cmd, ...)cmd, post = function(spec, ret, ...)list()	),
	qsub = list(pre = function(cmd, spec,
		jidFile = spec$fs$tempfile(sprintf('/tmp/R_%s/qsub_pattern', Sys.getenv('USER'))),
		qsubOptions = '',
		waitForJids = NULL, ...) {
		Dir.create(jidFile, treatPathAsFile = TRUE);
		waitOption = if (is.null(waitForJids)) '' else
			sprintf('--waitForJids %s', join(waitForJids, sep = ','));
		print(cmd);
		ncmd = sprintf('qsub.pl --jidReplace %s %s --unquote %s -- %s',
			jidFile, waitOption, qsubOptions, qs(cmd));
		print(ncmd);
		spec = list(cmd = ncmd, jidFile = jidFile);
		spec
	},
	post = function(spec, ret, ...) { list(jid = as.integer(spec$fs$readFile(spec$jidFile))) }
	),
	
	cwd = list(pre = function(cmd, spec, cwd = '.', ...) {
		ncmd = sprintf('cd %s ; %s', qs(cwd), cmd);
		spec = list(cmd = ncmd);
		spec
	},
	post = function(spec, ret, ...) { list() }
	),
	# <i> stdout/stderr handling
	ssh = list(pre = function(cmd, spec, ssh_host = 'localhost', ssh_source_file = NULL, ...,
		ssh_single_quote = T) {
		if (!is.null(ssh_source_file)) {
			cmd = sprintf('%s ; %s',
				join(paste('source', qs(ssh_source_file), sep = ' '), ' ; '), cmd);
		}
		fmt = if (ssh_single_quote) 'ssh %{ssh_host}s %{cmd}q' else 'ssh %{ssh_host}s %{cmd}Q';
		spec = list(cmd = Sprintf(fmt));
		spec
	},
	fs = function(fs, ..., ssh_host) {
		list(
			tempfile = function(prefix, ...) {
				Log(sprintf('tempfile ssh:%s', prefix), 1);
				r = splitPath(tempFileName(sprintf('%s:%s', ssh_host, prefix), ...), ssh = T)$path;
				Log(sprintf('tempfile ssh-remote:%s', r), 1);
				r
			},
			readFile = function(path, ...)readFile(sprintf('%s:%s', ssh_host, path), ..., ssh = T)
		);
	},
	post = function(spec, ret, ...) { list() }
	)
);
#
#	a system call (c.f. privatePerl/TempFilenames::System)
#
System_env__ <- new.env();
assign(".system.doLogOnly", FALSE, envir = System_env__);

System = function(cmd, logLevel = get('DefaultLogLevel', envir = Log_env__),
	doLog = TRUE, printOnly = NULL, return.output = F,
	pattern = NULL, patterns = NULL, ..., return.cmd = F) {
	# prepare
	if (!exists(".system.doLogOnly", envir = System_env__))
		assign(".system.doLogOnly", F, envir = System_env__);
	doLogOnly = ifelse (!is.null(printOnly), printOnly, get('.system.doLogOnly', envir = System_env__));

	# pattern mapping
	fs = .System.fileSystem;
	if (!is.null(patterns)) {
		spec = list();
		# map file accesses
		for (pattern in rev(patterns)) {
			fsMapper = .System.patterns[[pattern]]$fs;
			if (!is.null(fsMapper)) fs = fsMapper(fs, ...);
			spec[[length(spec) + 1]] = list(fs = fs);
		}
		# wrap commands into each other
		for (i in 1:length(patterns)) {
			spec[[i]] = merge.lists(spec[[i]], .System.patterns[[patterns[[i]]]]$pre(cmd, spec[[i]], ...));
			cmd = spec[[i]]$cmd;
		}
	} else if (!is.null(pattern)) {
		spec = .System.patterns[[pattern]]$pre(cmd, list(fs = fs), ...);
		spec$fs = fs;	# manually install fs
		cmd = spec$cmd;
	}
	# redirection (after patterns) <A>
	if (return.output & !doLogOnly) {
		tmpOutput = tempfile();
		cmd = sprintf("%s > %s", cmd, tmpOutput);
	}
	# logging
	if (doLog){ Log(sprintf("system: %s", cmd), logLevel); }
	# system call
	ret = NULL;
	if (!doLogOnly) ret = system(cmd);
	# return value
	r = list(error = ret);
	if (return.output & !doLogOnly) {
		r = merge.lists(r, list(error = ret, output = readFile(tmpOutput)));
	}
	# postprocess
	if (!doLogOnly) if (!is.null(patterns)) {
		for (i in rev(1:length(patterns))) {
			r = merge.lists(r, .System.patterns[[patterns[[i]]]]$post(spec[[i]], ret, ...));
		}
	} else if (!is.null(pattern)) {
		r = merge.lists(r, .System.patterns[[pattern]]$post(spec, ret, ...));
	}
	if (return.cmd) r$command = cmd;
	# simplified output
	if (!return.output && !return.cmd && is.null(pattern)) r = r$error;
	r
}

# wait on job submitted by system
.System.wait.patterns = list(
	default = function(r, ...)(NULL),
	qsub = function(r, ...) {
		ids = if (is.list(r[[1]]) & !is.null(r[[1]]$jid)) list.kp(r, 'jid', do.unlist = T) else r$jid;
		idsS = if (length(ids) == 0) '' else paste(ids, collapse = ' ');
		System(sprintf('qwait.pl %s', idsS), ...);
	}
);
System.wait = function(rsystem, pattern = NULL, ...) {
	r = if (!is.null(pattern)) .System.wait.patterns[[pattern]](rsystem, ...) else NULL;
	r
}

System.SetDoLogOnly = function(doLogOnly = F) {
	assign(".system.doLogOnly", doLogOnly, envir = System_env__);
}

ipAddress = function(interface = "eth0") {
	o = System(sprintf("/sbin/ifconfig %s", interface), logLevel = 6, return.output = T);
	ip = fetchRegexpr("(?<=inet addr:)[^ ]+", o$output);
	ip
}


#
#	<p> cluster abstraction
#
# Example:
#specifyCluster(localNodes = 8, sourceFiles = c('RgenericAll.R', 'dataPreparation.R'));
#.clRunLocal = F;
#data.frame.types(clapply(l, f, arg1 = 1), rbind = T, do.transpose = T);

# default cluster configuration
.defaultClusterConfig = list(
	hosts = list(list(host = "localhost", count = 2, type = "PSOCK")), local = F,
	provideChunkArgument = F, reverseEvaluationOrder = T, splitN = 4, reuseCluster = F,
	nestingLevel = 0,	# records the nesting of clapply calls
	splittingLevel = 1,	# specifies at which level clapply should parallelize
	evalEnvironment = F	# call environment_eval on function before passing on
);
Snow_cluster_env__ = new.env();
specifyCluster = function(localNodes = 8, sourceFiles = NULL, cfgDict = list(), hosts = NULL,
	.doSourceLocally = F, .doCopy = T, splitN = NULL, reuseCluster = F, libraries = NULL,
	evalEnvironment = F) {
	#<!> might not be available/outdated
	require('parallel');
	cfg = merge.lists(.defaultClusterConfig,
		cfgDict,
		list(splitN = splitN, reuseCluster = reuseCluster, evalEnvironment = evalEnvironment),
		list(local = F, source = sourceFiles, libraries = libraries, hosts = (if(is.null(hosts))
			list(list(host = "localhost", count = localNodes, type = "PSOCK",
				environment = list(setwd = getwd()))) else
				hosts)
	));
	assign(".globalClusterSpecification", cfg, envir = Snow_cluster_env__);
	.globalClusterSpecification = get('.globalClusterSpecification', envir = Snow_cluster_env__);
	if (.doCopy) {
		for (h in .globalClusterSpecification$hosts) {
			if (h$host != "localhost" & !is.null(h$env$setwd)) {
				System(sprintf("ssh %s mkdir '%s' 2>/dev/null", h$host, h$env$setwd), 5);
				System(sprintf("scp '%s' %s:'%s' >/dev/null", paste(sourceFiles, collapse = "' '"),
					h$host, h$env$setwd), 5);
			}
		}
	}
	if (.doSourceLocally) {
		sourceFiles = setdiff(sourceFiles, "RgenericAll.R");	# assume we have been sourced
		eval(parse(text =
			paste(sapply(sourceFiles, function(s)sprintf("source('%s', chdir = TRUE);", s)), collapse = "")));
	}
}

# l: list, f: function, c: config
# <i><!> test clCfg$reverseEvaluationOrder before uncommenting
clapply_cluster = function(l, .f, ..., clCfg = NULL) {
	#if (clCfg$reverseEvaluationOrder) l = rev(l);

	# only support SOCK type right now <!><i>
	hosts = as.vector(unlist(sapply(clCfg$hosts, function(h){
		if (h$type == "PSOCK") rep(h$host, h$count) else NULL})));
	master = ifelse(all(hosts == "localhost"), "localhost", ipAddress("eth0"));
	establishEnvironment = T;
	cl = if (clCfg$reuseCluster) {
		if (!exists(".globalClusterObject")) {
			assign(".globalClusterObject", makeCluster(hosts, type = "PSOCK", master = master),
				envir = Snow_cluster_env__);
		} else establishEnvironment = FALSE;
		get('.globalClusterObject', envir = Snow_cluster_env__)
	} else makeCluster(hosts, type = "PSOCK", master = master);
	#clusterSetupRNG(cl);	# snow
	clusterSetRNGStream(cl, iseed = NULL);	# parallel

	clusterExport(cl, clCfg$vars);

	# <p> establish node environment
	envs = listKeyValue(list.key(clCfg$hosts, "host"), list.key(clCfg$hosts, "environment", unlist = F));
	if (Log.level() >= 7) print(clCfg);

	if (establishEnvironment) r = clusterApply(cl, hosts, function(host, environments, cfg){
		env = environments[[host]];
		if (!is.null(env$setwd)) setwd(env$setwd);
		if (!is.null(cfg$source)) for (s in cfg$source) source(s, chdir = TRUE);
		if (!is.null(cfg$libraries)) for (package in cfg$libraries) library(package, character.only = TRUE);
		# <!> as of 3.4.2013: stop support of exporting global variables to enable CRAN submission
		#if (!is.null(env$globalVars))
		#	for (n in names(env$globalVars)) assign(n, env$globalVars[[n]], pos = .GlobalEnv);
		#sprintf("%s - %s - %s", host, hapmap, getwd());
		NULL
	}, environments = envs, cfg = clCfg);

	# <p> iterate
	N = clCfg$splitN * length(hosts);	# No of splits
	idcs = splitListIndcs(length(l), N);
	exportNames = c();
	iterator__ = if (clCfg$provideChunkArgument) {
		function(.i, ...) {
			r = lapply(idcs[.i, 1]:idcs[.i, 2], function(j)try(.f(l[[j]], .i, ...)));
			if (class(r) == "try-error") r = NULL;
			r
		}
	} else {
		function(.i, ...){
			r = lapply(idcs[.i, 1]:idcs[.i, 2], function(j)try(.f(l[[j]], ...)));
			if (class(r) == "try-error") r = NULL;
			r
		}
	}
	if (clCfg$evalEnvironment) {
		iterator__ = environment_eval(iterator__, functions = T);
		#clusterExport(cl, varlist = names(as.list(environment(iterator__))), envir = environment(iterator__));
	}
	r = clusterApplyLB(cl, 1:dim(idcs)[1], iterator__, ...);
	# <p> finish up
	if (!clCfg$reuseCluster) stopCluster(cl)
	r = unlist(r, recursive = F);
	#if (clCfg$reverseEvaluationOrder) r = rev(r);
	r
}

# wrapper (as of 3.12.8: I seem to have lost a previous change)
clapply = function(l, .f, ..., clCfg = NULL, .clRunLocal = rget(".clRunLocal", F, envir = .GlobalEnv)) {
	# <p> get cluster specification
	clCfg = merge.lists(
		rget(".globalClusterSpecification", default = list(), envir = Snow_cluster_env__),
		firstDef(clCfg, list())
	);
	# <p> update cluster specification
	clCfg$nestingLevel = clCfg$nestingLevel + 1;
	assign(".globalClusterSpecification", clCfg, envir = Snow_cluster_env__);

	# <p> choose/decline parallelization
	r = if (firstDef(.clRunLocal, clCfg$local, F) || clCfg$nestingLevel != clCfg$splittingLevel) {
		if (clCfg$provideChunkArgument) lapply(X = l, FUN = .f, 1, ...)
		else lapply(X = l, FUN = .f, ...)
	} else {
		clapply_cluster(l, .f, ..., clCfg = clCfg);
	};

	# <p> update cluster specification
	clCfg$nestingLevel = clCfg$nestingLevel - 1;
	assign(".globalClusterSpecification", clCfg, envir = Snow_cluster_env__);
	r
}


evalCall = function(call) {
	call = callEvalArgs(call);
	do.call(call$f, call$args, envir = call$envir)
}

# envirArgs: non-functional, depracated
Do.call = function(what, args, quote = FALSE, envir = parent.frame(),
	defaultEnvir = .GlobalEnv, envirArgs = NULL, do_evaluate_args = F) {
	if (is.null(envir)) envir = defaultEnvir;
	if (do_evaluate_args) args = nlapply(args, function(e)eval(args[[e]], envir = envir));
	do.call(what = what, args = args, quote = quote, envir = envir)
}

#
#	<p> file operations
#

#' Return absolute path for name searched in search-pathes
#'
#' Search for pathes.
#'
#' @param as.dirs assume that prefixes are pathes, i.e. a slash will be put between path and prefix
#' @param force enforces that path and prefix are always joined, otherwise if path is absolute no prefixing is performed
file.locate = function(path, prefixes = NULL, normalize = T, as.dirs = T, force = F, home = T) {
	if (!force && substr(path, 1, 1) == '/') return(path);
	if (substr(path, 1, 1) == '~' && home) {
		path = path.absolute(path, home = TRUE);
		if (!force) return(path);
	}
	if (is.null(prefixes)) prefixes = if (as.dirs) '.' else '';
	sep = ifelse(as.dirs, '/', '');
	for (prefix in prefixes) {
		npath = sprintf('%s%s%s', prefix, sep, path);
		if (normalize) npath = path.absolute(npath);
		if (file.exists(npath)) return(npath);
	}
	NULL
}

#' Read content of file and return as character object.
#' 
#' Read content of file and return as character object.
#' 
#' Read content of file and return as character object.
#' 
#' @param path Path to the file to be read.
#' @param prefixes Search for file by prepending character strings from
#' prefixes.
#' @param normalize Standardize pathes.
#' @param ssh Allow pathes to remote files in \code{scp} notation.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#'   parallel8 = function(e) log(1:e) %*% log(1:e);
#'   cat(readFile(tempcodefile(parallel8)));
#' 
# prefixes only supported locally <!>
readFile = function(path, prefixes = NULL, normalize = T, ssh = F) {
	s = splitPath(path, ssh = ssh);
	r = if (s$is.remote) {
		tf = tempfile();
		File.copy(path, tf);
		readChar(tf, nchars = as.list(file.info(tf)[1,])$size);
	} else {
		if (!is.null(prefixes)) path = file.locate(path, prefixes, normalize);
		readChar(path, nchars = as.list(file.info(path)[1,])$size);
	}
	r
}

writeFile = function(path, str, mkpath = F, ssh = F) {
	s = splitPath(path, ssh = ssh);
	if (s$is.remote) {
		Dir.create(sprintf('%s:%s', s$userhost, s$dir), recursive = mkpath);
		tf = tempfile();
		out = file(description = tf, open = 'w', encoding='UTF-8');
			cat(str, file = out, sep = "");
		close(con = out);
		File.copy(tf, path);
	} else {
		if (mkpath) {
			if (!file.exists(s$dir)) dir.create(s$dir, recursive = T);
		}
		out = file(description = path, open = 'w', encoding='UTF-8');
			cat(str, file = out, sep = "");
		close(con = out);
	}
	path
}

isURL = function(path)(length(grep("^(ftp|http|https|file)://", path)) > 0L)

Source_url = function(url, ...) {
	require('RCurl');
	request = getURL(url, followlocation = TRUE,
		cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"));
	tf = tempfile();
	writeFile(tf, request);
    source(tf, ...)
}

# <!> local = T does not work
Source = function(file, ...,
	locations = c('', '.', sprintf('%s/src/Rscripts', Sys.getenv('HOME')))) {
	sapply(file, function(file) {
		if (isURL(file)) Source_url(file, ...) else {
		file0 = file.locate(file, prefixes = locations);
			source(file = file0, ...)
		}
	})
}

#
#	<p> helper functions readTable/writeTable
#

compressPathBz2 = function(pathRaw, path, doRemoveOrig = TRUE) {
	cmd = Sprintf("cat %{pathRaw}q | bzip2 -9 > %{path}q");
	r = System(cmd, 5);
	if (doRemoveOrig && !get('.system.doLogOnly', envir = System_env__)) file.remove(pathRaw);
	r
}
compressPath = function(pathRaw, path, extension = NULL, doRemoveOrig = TRUE) {
	if (is.null(extension)) return(path);
	compressor = get(Sprintf('compressPath%{extension}u'));
	r = compressor(pathRaw, path, doRemoveOrig = doRemoveOrig);
	r
}
decompressPathBz2 = function(path, pathTmp, doRemoveOrig = FALSE) {
	cmd = Sprintf("cat %{path}q | bunzip2 > %{pathTmp}q");
	r = System(cmd, 5);
	if (doRemoveOrig && !get('.system.doLogOnly', envir = System_env__)) file.remove(pathRaw);
	r
}
decompressPath = function(path, pathTmp, extension = NULL, doRemoveOrig = FALSE) {
	if (is.null(extension)) return(path);
	decompressor = get(Sprintf('decompressPath%{extension}u'));
	r0 = decompressor(path, pathTmp, doRemoveOrig = doRemoveOrig);
	r = list(destination = pathTmp, pathOrig = path, return = r0);
	r
}

compressedConnectionBz2 = function(path, mode = '') {
	#r = Sprintf('%{path}s.bz2');
	bzfile(path, open = mode)
}
compressedConnectionGz = function(path, mode = '') {
	gzfile(path, open = mode)
}
compressedConnection = function(path, extension = NULL, mode = '') {
	if (is.null(extension)) return(path);
	compressor = get(Sprintf('compressedConnection%{extension}u'));
	compressor(path, mode = mode)
}
compressedConnectionPath = function(conn) {
	if ('connection' %in% class(conn)) summary(conn)$description else conn
}

#
#	<p> readTable
#

# complete: return only complete data with respect to specified colums
# NA: specify 'NA'-values
readTableSepMap = list(T = "\t", S = ' ', C = ',', `;` = ';', `S+` = '');
optionParser = list(
	SEP = function(e)readTableSepMap[[e]],
	QUOTE = function(e)(if (e == 'F') '' else e),
	HEADER = function(e)list(T = T, F = F)[[e]],
	ROW.NAMES = function(e)list(T = T, F = F)[[e]],
	NAMES = function(e)splitString(';', e),
	FACTORS = function(e)splitString(';', e),
	PROJECT = function(e)splitString(';', e),
	`NA` = function(e)splitString(';', e),
	complete = function(e)splitString(';', e),
	CONST = function(e){ r = lapply(splitString(';', e), function(e){
			r = splitString(':', e);
			v = if (length(fetchRegexpr('^\\d+$', r[2])) > 0) r[2] = as.integer(r[2]) else r[2];
			listKeyValue(r[1], v)
		});
		unlist.n(r, 1)
	},
	HEADERMAP = function(e){ r = lapply(splitString(';', e), function(e){
			r = splitString(':', e);
			listKeyValue(r[1], r[2])
		});
		unlist.n(r, 1)
	},
	# tb implemented: <i>: merge.lists recursive
	VALUEMAP = function(e){ r = lapply(splitString(';', e), function(e){
			r = splitString(':', e);
			listKeyValue(r[1], r[2])
		});
		unlist.n(r, 1)
	},
	COLNAMESFILE = identity,
	SHEET = as.integer,
	SKIP = as.integer
);

splitExtendedPath = function(path) {
	q = fetchRegexpr('(?<=^\\[).*?(?=\\]:)', path);
	options = list();
	if (length(q) > 0 && nchar(q) > 0) {
		path = substr(path, nchar(q) + 4, nchar(path));
		os = sapply(splitString(',', q), function(e)splitString('=', e));
		os = listKeyValue(os[1, ], os[2, ]);
		os = nlapply(names(os), function(n)optionParser[[n]](os[[n]]));
		options = merge.lists(options, os);
	}
	r = list(path = path, options = options)
}

readTable.ods = function(path, options = NULL) {
	require('readODS');
	sheet = firstDef(options$SHEET, 1);
	read.ods(path)[[sheet]];
}

# <!> changed SEP default "\t" -> ",", 20.5.2015
#readTable.csv.defaults = list(HEADER = T, SEP = "\t", `NA` = c('NA'), QUOTE = '"');
readTable.csv.defaults = list(HEADER = T, SEP = ",", `NA` = c('NA'), QUOTE = '"', SKIP = 0);
readTable.txt = readTable.csv = function(
	path, options = readTable.csv.defaults, headerMap = NULL, setHeader = NULL, ...) {

	options = merge.lists(readTable.csv.defaults, options);
	t = read.table(path, header = options$HEADER, sep = options$SEP, as.is = T,
		na.strings = options$`NA`, comment.char = '', quote = options$QUOTE, skip = options$SKIP, ...);
	if (!is.null(options$NAMES)) names(t)[1:length(options$NAMES)] = options$NAMES;
	if (!is.null(headerMap)) names(t) = vector.replace(names(t), headerMap);
	if (!is.null(setHeader)) names(t) =  c(setHeader, names(t)[(length(setHeader)+1): length(names(t))]);
	if (!is.null(options$FACTORS)) t = Df_(t, as_factor = options$FACTORS);
	t
}

readTable.sav = function(path, options = NULL, headerMap = NULL, stringsAsFactors = F) {
	require('foreign');
	# read file
	r = read.spss(path);
	as.data.frame(r, stringsAsFactors = stringsAsFactors)
}

readTable.RData = function(path, options = NULL, headerMap = NULL) {
	t = as.data.frame(get(load(path)[1]), stringsAsFactors = F);
	#print(t);
	t
}

readTable.xls = function(path, options = NULL, ..., row.names = NULL, sheet = 1) {
	require('gdata');
	read.xls(path, sheet = sheet, ..., row.names = row.names, verbose = FALSE);
}

tableFunctionConnect = c('csv', 'RData');

tableFunctionForPathMeta = function(path, template = 'readTable.%{ext}s', default = readTable.csv,
	forceReader = NULL) {
	sp = splitPath(path);
	compression = NULL;
	tmpFile = NULL;
	if (firstDef(forceReader, sp$ext) %in% c('bz2', 'gz')) {
		compression = sp$ext;
		sp = splitPath(sp$fullbase);
		tmpFile = Sprintf('%{file}s.%{ext}s', file = tempfile(), ext = sp$ext);
	}
	name = Sprintf(template, ext = firstDef(forceReader, sp$ext));
	f = if (exists(name)) get(name) else default;
	r = list(
		fct = f, name = name, ext = sp$ext,
		compression = compression, tempfile = tmpFile, path = path
	);
	r
}
tableFunctionForPath = function(path, template = 'readTable.%{ext}s',
	default = readTable.csv, forceReader = NULL) {
	tableFunctionForPathMeta(path, template, default, forceReader)$fct
}

# forceReader: force readerFunction
tableFunctionForPathReader = function(path, template = 'readTable.%{ext}s', default = readTable.csv,
	forceReader = NULL) {
	m = m0 = tableFunctionForPathMeta(path, template = 'readTable.%{ext}s', default = default, forceReader);
	if (!is.null(m$compression)) {
		path = if (m0$compression %in% tableFunctionConnect)
			compressedConnection(m0$path, m0$compression) else
			decompressPath(m0$path, m0$tempfile, m0$compression)$destination
		m = merge.lists(m0, list(path = path));
	}
	m
}

# <!> as of 23.5.2014: headerMap after o$NAMES assignment
# <i> use tableFunctionForPath
readTable = function(path, autodetect = T, headerMap = NULL, extendedPath = T, colnamesFile = NULL, ...,
	as_factor = NULL, stringsAsFactors = F, defaultReader = readTable.csv, doRemoveTempFile = TRUE,
	forceReader = NULL) {
	# <p> preparation
	path = join(path, '');
	o = list();
	if (extendedPath) {
		r = splitExtendedPath(path);
		path = r$path;
		o = r$options;
	}

	# <p> read table raw
	sp = splitPath(path);
	reader = if (autodetect && !is.null(sp$ext)) 
		tableFunctionForPathReader(path, 'readTable.%{ext}s', readTable.csv, forceReader) else
		list(fct = defaultReader, path = path);
	r = reader$fct(reader$path, options = o, ...);

	# <p> cleanup
	if (doRemoveTempFile && !get('.system.doLogOnly', envir = System_env__) && !is.null(reader$tempfile))
		file.remove(reader$tempfile);

	# <p> table transformations
	if (!is.null(o$NAMES) && length(o$NAMES) <= ncol(r)) names(r)[1:length(o$NAMES)] = o$NAMES;
	colnamesFile = firstDef(o$COLNAMESFILE, colnamesFile);
	headerMap = c(headerMap, o$HEADERMAP);
	if (!is.null(headerMap)) names(r) = vector.replace(names(r), headerMap);
	if (!is.null(colnamesFile)) {
		ns = read.table(colnamesFile, header = F, as.is = T)[, 1];
		names(r)[1:length(ns)] = ns;
	}
	if (!is.null(o$PROJECT)) r = r[, o$PROJECT];
	if (!is.null(o$complete)) r = r[apply(r[, o$complete], 1, function(e)!any(is.na(e))), ];
	if (!is.null(o$CONST)) { for (n in names(o$CONST)) r[[n]] = o$CONST[[n]]; }
	if (!is.null(as_factor)) r = Df_(r, as_factor = as_factor);
	r
}

#
#	<p> writeTable
#

writeTable.defaults = list(
	SEP = ' ',
	ROW.NAMES = FALSE,
	HEADER = TRUE,
	QUOTE = TRUE
);
writeTable.table = function(dataFrame, path, ..., doCompress = NULL, row.names = TRUE, options = list()) {
	o = merge.lists(writeTable.defaults, list(ROW.NAMES = row.names), options);
	conn = compressedConnection(path, doCompress, mode = 'w');
	with(o, write.table(dataFrame, file = conn, ...,
		row.names = ROW.NAMES, col.names = HEADER, sep = SEP, quote = (QUOTE != '')));
}

writeTable.xls = function(object, path, doCompress = NULL, row.names = TRUE,
	doRemoveOrig = TRUE, options = list()) {
	require('WriteXLS');
	r = path;
	dataFrame = as.data.frame(object);
	pathRaw = if (!is.null(doCompress)) Sprintf('%{path}s_raw_') else path;
	r0 = WriteXLS(dataFrame, ExcelFileName = pathRaw, row.names = row.names);
	r1 = compressPath(pathRaw, path, doCompress, doRemoveOrig);
	r0
}

writeTable.csv = function(dataFrame, path, ..., doCompress = NULL, row.names = TRUE, options = list()) {
	conn = compressedConnection(path, doCompress, mode = 'w');
	write.csv(dataFrame, file = conn, ..., row.names = row.names);
}
# doCompress = 'bz2' to write bz2
# <i><!> determine from path
writeTableRaw = function(object, path, ..., doCompress = NULL, row.names = TRUE, autodetect = TRUE,
	defaultWriter = writeTable.csv, options = list()) {
	sp = splitPath(path);
	if (is.null(doCompress) && !is.null(sp$ext) && sp$ext %in% c('bz2', 'gz')) {
		doCompress = sp$ext;
	}
	writer = if (autodetect && !is.null(sp$ext)) 
		tableFunctionForPath(path, 'writeTable.%{ext}s', writeTable.csv) else defaultWriter;
	if (is.null(writer))
		stop(Sprintf("Writing table to extension '%{ext}s' not supported", ext = sp$ext));
	r0 = writer(object, path = path, ..., doCompress = doCompress, row.names = row.names, options = options);
	r = list(path = path, return = r0);
	r
}

writeTable = function(object, path, ..., doCompress = NULL, row.names = TRUE, autodetect = TRUE,
	defaultWriter = writeTable.csv, simplify = TRUE, extendedPath = TRUE) {
	o = list();
	if (extendedPath) {
		r = splitExtendedPath(path);
		path = r$path;
		o = r$options;
		defaultWriter = writeTable.table;
	}
	r = lapply(path, function(p)
		writeTableRaw(object, p, ...,
			doCompress = doCompress, row.names = row.names, autodetect = autodetect,
			defaultWriter = defaultWriter, options = o)
	);
	if (simplify && length(path) == 1) r = r[[1]];
	r
}

#
#	<p> swig
#

swigIt = function(interface, code, moduleName = NULL) {
	dir = tempdir();	# will be constant across calls
	if (is.null(moduleName)) {
		t = tempFileName("swig");
		moduleName = splitPath(t)$base;
	}

	ifile = sprintf("%s/%s.%s", dir, moduleName, "i");
	interface = sprintf("
		%%module %s
		%%inline %%{
			%s;
		%%}
	", moduleName, paste(interface, collapse = ";\n\t\t\t"));

	ifile = sprintf("%s/%s.%s", dir, moduleName, "i");
	base = splitPath(ifile)$fullbase;
	writeFile(ifile, interface);
	cfile = sprintf("%s.c", base);
	writeFile(cfile, code);
	#print(list(i = ifile, c = cfile, so = sprintf("%s.so", base)));
	system(sprintf("swig -r %s", ifile));
	#cat(code);

	system(sprintf("cd %s ; gcc -O2 -D__USE_BSD -D__USE_GNU -std=c99 -c -fpic %s.c %s_wrap.c -I/usr/local/lib64/R/include -lm ",
		splitPath(ifile)$dir, base, base));
	system(sprintf("cd %s ; gcc -shared %s.o %s_wrap.o -o %s.so", splitPath(ifile)$dir, base, base, base));
	#dyn.unload(sprintf("%s.so", base));
	dyn.load(sprintf("%s.so", base));
	source(sprintf("%s/%s.R", splitPath(ifile)$dir, moduleName));
}

#
#	<p> print
#

fprint = function(..., file = NULL, append = F) {
	if (!is.null(file)) sink(file = file, append = append);
	r = print(...);
	if (!is.null(file)) sink();
	r
}

stdOutFromCall = function(call_) {
	tf = tempfile();
	sink(tf);
		eval.parent(call_, n = 2);
	sink();
	readFile(tf)
}

#
#	crypotgraphy/checksumming
#

md5sumString = function(s, prefix = 'md5generator') {
	require('tools');
	path = tempfile('md5generator');
	writeFile(path, s);
	md5 = avu(md5sum(path));
	md5
}

#
#	<p> package documentation
#

#	docFile = sprintf('%s/tmp/docOut.Rd', Sys.getenv('HOME'));
#	docDir = sprintf('%s/src/Rpackages/parallelize.dynamic/parallelize.dynamic/man', Sys.getenv('HOME'));
#	docs = RdocumentationSkeleton('Rparallel.back.R', 'parallelize.dynamic', output = docFile);
#	writeRdocumentationToDir(docFile, docDir);

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

writeRdocumentationToDir = function(pathesIn, pathOut, cleanOut = F) {
	doc = sapply(pathesIn, readFile, USE.NAMES = F);
	r = unlist.n(getPatternFromStrings(doc, '(?s)(?:\\nDOCUMENTATION_BEGIN:)([^\\n]+)\\n(.*?)(?:\\nDOCUMENTATION_END\\n)'), 1);
	Dir.create(pathOut, recursive = T);
	if (cleanOut) {
		files = list_files_with_exts(pathOut, 'Rd');
		file.remove(files);
	}
	nlapply(r, function(n) {
		output = file.path(pathOut, sprintf('%s.Rd', n));
		Log(sprintf('Writing to %s', output), 3);
		writeFile(output, r[[n]]);
	});
	names(r)
}

reDoc = function(package = 'parallelize.dynamic',
	docFile = sprintf('./%s.doc.Rd', package), docDir = sprintf('./%s/man', package)) {
	writeRdocumentationToDir(docFile, docDir, cleanOut = T);
	install.packages(sprintf('./%s', package), repos = NULL);
	#detach(package);
	#library(package)
}

#
#	<p> Rcpp helpers
#

createModule = function(name, libpathes = c(), headers = c(), output = NULL) {
	require('Rcpp');
	require('inline');
	dirs = sapply(libpathes, function(e)splitPath(e)$dir);
	libs = sapply(libpathes, function(e)fetchRegexpr('(?<=lib)(.*)(?=.so)', splitPath(e)$file));
	.libPaths(c(.libPaths(), dirs));
	libincludes = join(sapply(seq_along(dirs), function(i)sprintf('-L"%s" -l%s', splitPath(dirs[i])$absolute, libs[i])), ' ');
	Sys.setenv(`PKG_LIBS` = sprintf('%s %s', Sys.getenv('PKG_LIBS'), libincludes));
	Sys.setenv(`PKG_CXXFLAGS` = sprintf('%s %s', Sys.getenv('PKG_LIBS'), stdOutFromCall(Rcpp:::CxxFlags())));


	for (lib in libpathes) { dyn.load(lib, local = F) }
	moduleRegex = '(?s:(?<=// -- begin inline Rcpp\n)(.*?)(?=// -- end inline Rcpp))';
	inc = join(sapply(headers, function(f) fetchRegexpr(moduleRegex, readFile(f))), "\n");

	rcpp = cxxfunction( signature(), '' , includes = inc, plugin = 'Rcpp', verbose = T );
	mod = Module( name,  getDynLib(rcpp) );
	if (!is.null(output)) {
		Dir.create(output, recursive = T);
		libfiles = sapply(libpathes, function(lib) {
			File.copy(lib, sprintf('%s/%s', output, splitPath(lib)$file));
			splitPath(lib)$file
		});
		glue = sprintf('%s/%s.so', output, name);
		File.copy(getDynLib(rcpp)[['path']], glue);
		module_descriptor = list(
			name = name,
			libs = c(libfiles, splitPath(glue)$file)
		);
		save(module_descriptor, file = sprintf('%s/module.RData', output));
	}
	mod
}

activateModule = function(path) {
	require('Rcpp');
	module_descriptor = get(load(sprintf('%s/module.RData', path))[1]);
	r = lapply(module_descriptor$libs, function(lib)try(dyn.unload(sprintf('%s/%s', path, lib)), silent = T));
	r = lapply(module_descriptor$libs, function(lib)dyn.load(sprintf('%s/%s', path, lib), local = F));
	mod = Module( module_descriptor$name, rev(r)[[1]] );
	mod
}

#
#	<p> sqlite
#

sqlCreateTable = function(columns, types = list, index = NULL, createAt = NULL) {
	# <p> create database
	types = merge.lists(listKeyValue(columns, rep('text', length(columns))), types);
	createDbSql = join(sep = "\n", c(
		sprintf('CREATE TABLE data (%s);',
			join(sep = ', ', sapply(columns, function(e)sprintf('%s %s', e, types[e])))),
		if (is.null(index)) c() else sapply(1:length(index), function(i)
			sprintf('CREATE INDEX index_%d ON data (%s);', i, join(index[[i]], sep = ', '))),
		'.quit', ''
	));
	if (!is.null(createAt)) System(sprintf('echo \'%s\' | sqlite3 %s', createDbSql, qs(createAt)), 1);
	createDbSql
}

# Create sqlite database with contents of csv-file
# @par index: list of columns to index
# @par type: sqlite types: integer, real, text, blob, not specified assumes text

csv2sqlitSepMap = readTableSepMap;
sepMap = list(T = '\\t', S = ' ', C = ',', `;` = ';', `S+` = '');
sepMapCut = list(T = '\\t', S = '" "', C = ',', `;` = ';', `S+` = '');
csv2sqlite = function(path, output = tempfile(),
	columnsNames = NULL, columnsSelect = NULL,
	index = NULL,
	inputSep = 'T', inputHeader = T, inputSkip = NULL,
	NULLs = NULL, types = list()) {

	# <!> cave: does not heed skip
	if (!inputHeader && is.null(columnsNames)) {
		columnsNames = read.table(path, header = F, nrows = 1, sep = csv2sqlitSepMap[[inputSep]]);
	}
	# <p> select columns
	cut = if (!is.null(columnsSelect)) {
		skipColumnsIds = which.indeces(columnsSelect, columnsNames);
		sprintf('| cut %s -f %s ',
			if (inputSep == 'T') '' else sprintf('-d %s', sepMapCut[[inputSep]]),
			join(skipColumnsIds, ',')
		)
	} else '';
	columns = if (is.null(columnsSelect)) columnsNames else columnsSelect;
	types = merge.lists(listKeyValue(columns, rep('text', length(columns))), types);
	sqlCreateTable(columns, types, index, createAt = output);

	# <p> import data
	skipCommand = if (is.null(inputSkip)) '' else sprintf('| tail -n +%d ', inputSkip + 1);
	reader = if (splitPath(path)$ext == 'gz') 'zcat' else 'cat';
	importSql = writeFile(tempfile(), join(sep = "\n", c(
		sprintf(".separator %s\n", sepMap[[inputSep]]),
		sprintf(".import \"/dev/stdin\" data")
	)));

	sepText = sepMap[[inputSep]];
	filter = if (is.null(NULLs)) '' else
		sprintf("| perl -pe 's/((?<=%s)(?:%s)(?=%s|$)|(?<=^)(?:%s)(?=%s|$))//g'",
			sepText, join(NULLs, '|'), sepText, sepText, sepText);
	cmd = Sprintf(con(
		"%{reader}s %{path}Q %{skipCommand}s %{cut}s %{filter}s",
		" | sqlite3 -init %{importSql}Q %{output}Q"));
	System(cmd, 1);
	output
}
# <!> unfinished, siphones code from old csv2sqlite function
url2sqlite = function(url, output = tempfile(), header = NULL, skip = NULL, selectColumns = NULL,
	index = NULL, sep = 'T',
	NULLs = NULL, types = list()) {
	
	# <p> determine header
 	tmp1 = tempfile();
 	ret = download.file(url, tmp1, method, quiet = FALSE, mode = "w", cacheOK = TRUE);
	#if (ret) stop(sprintf("Download of '%s' failed.", url));
	if (is.null(header)) {
		tmpHeader = tempfile();
	}
}

# <!> 7.1.2015: was qq, but conflicts with QQ-plot function
qquote = function(s)as.character(fetchRegexpr('([^ ]+)', s, captures = T))

sqlite2sqlite = function(dbS, dbD, query, cols, types = list(), index = NULL) {
	sqlCreateTable(cols, types, index, createAt = dbD);
	cmd = sprintf("echo %s | sqlite3 -init %s %s | sqlite3 -init %s %s",
		qs(query),
		qs(writeFile(tempfile(), ".mode csv")),
		qs(dbS),
		qs(writeFile(tempfile(), ".separator ,\n.import \"/dev/stdin\" data")),
		qs(dbD)
	);
	System(cmd, 1);
	dbD
}

sqliteOpen = function(path) {
	require('RSQLite');
	dbConnect(SQLite(), dbname = path);
}
sqliteQuery = function(db, query, table = NULL) {
	if (is.null(table)) table = dbListTables(db)[1];
	query = con(sapply(names(query), function(n)Sprintf('%{n}Q = %{v}s', v = qs(query[[n]], force = T))));
	query1 = Sprintf('SELECT * FROM %{table}Q WHERE %{query}s');
	Log(query1, 5);
	dbGetQuery(db, query1);
}

#
#	<p> publishing
#

# if (1) {
#	.fn.set(prefix = 'results/201404/expressionMonocytes-')
# 	initPublishing('expressionMonocytes201404', '201405');
# 	publishFile('results/expressionMonocytesReportGO.pdf');
# }
# # force as subdir in the reporting dir
# publishDir('results/BAP1IHCgekoppeldaanWelofGeenMonosomie3', asSubdir = T);

Publishing_env__ <- new.env();
initPublishing = function(project, currentIteration, publicationPath = '/home/Library/ProjectPublishing') {
	assign('project', project, Publishing_env__);
	assign('projectMd5', md5sumString(project), Publishing_env__);
	assign('currentIteration', currentIteration, Publishing_env__);
	assign('publicationPath', publicationPath, Publishing_env__);
}
publishFctEnv = function(path, into = NULL, as = NULL) with(as.list(Publishing_env__), {
	if (!exists('project')) stop('Publishing system not yet initialized.');

	projectFolder = Sprintf('%{publicationPath}s/%{projectMd5}s');
	prefix = if (is.null(into)) '' else Sprintf('%{into}s/');
	destinationPrefix = Sprintf('%{projectFolder}s/%{currentIteration}s/%{prefix}s');
	destination = Sprintf('%{destinationPrefix}s%{path}s',
		path = if (is.null(as)) splitPath(path)$file else as);
	r = list(projectFolder = projectFolder, prefix = prefix, destination = destination,
		destinationPrefix = destinationPrefix);
	r
})


publishFileRaw = function(file, into = NULL, as = NULL) with(publishFctEnv(file, into, as), {
	if (!is.null(into)) Dir.create(destination, treatPathAsFile = T, recursive = T);
	Logs('Publishing %{file} --> "%{destination}s', 3);
	Dir.create(splitPath(destination)$dir, recursive = T);
	System(Sprintf("chmod -R a+rX %{dir}s", dir = qs(projectFolder)), 4);
	r = file.copy(file, destination, overwrite = T);
	if (any(!r)) Logs("Copying of '%{file}s' failed.", 3);
	Sys.chmod(destination, mode = '0755', use_umask = F);
	destination
})

publishFiles = publishFile = function(files, into = NULL, as = NULL) {
	lapply(files, publishFileRaw, into = into, as = as)
}

publishCsv = function(table, as, ..., into = NULL) {
	file = tempfile('publish', fileext = 'csv');
	write.csv(table, file = file, ...);
	publishFile(file, into, as);
}

publishDir = function(dir, into = NULL, as = NULL, asSubdir = FALSE) with(publishFctEnv('', into, as), {
	sp = splitPath(dir);
	# if 'dir' is a slashed dir itself, use the last dir-component
	if (asSubdir) into = if (sp$file == '') splitPath(sp$dir)$file else sp$file;
	if (!is.null(into)) {
		destination = splitPath(Sprintf('%{destination}s/%{into}s/'))$fullbase;	# remove trailing slash
	}
	Dir.create(destination);
	Logs('Publishing %{dir} --> %{destination}s', 3);
	Dir.create(destination, recursive = T);
	System(Sprintf("chmod -R a+rX %{projectFolder}Q"), 4);
	System(Sprintf("cp -r %{dir}Q/* %{destination}Q"), 4);
	System(Sprintf("chmod -R a+rX %{projectFolder}Q"), 4);
	destination
})

publishAsZip = function(files, as, into = NULL, recursive = FALSE) {
	tmp = tempFileName('publishAsZip', createDir = T, inRtmp = T);
	output = tempFileName('publishAsZip', 'zip', inRtmp = T, doNotTouch = T);
	sapply(files, function(file) {
		File.symlink(splitPath(file)$absolute, Sprintf("%{tmp}s"), replace = F);
		NULL
	});
	recursiveOption = ifelse(recursive, '-r', '');
	System(Sprintf("zip -j %{recursiveOption}s %{output}s %{tmp}s/*"), 2);
	publishFile(output, into = into, as = as);
}


#
#	<p> quick pdf generation
#

print2pdf = function(elements, file) {
	es = elements;
	tf = tempfile();
	sink(tf);
		nlapply(es, function(n) {
			cat(n);
			cat('\n-------------------------------------------\n');
			print(es[[n]]);
			cat('\n\n');
		})
	sink();
	System(Sprintf('a2ps %{tf}s --columns 1 --portrait --o - | ps2pdf - - > %{output}s', output = qs(file)));
}

#
#	<p> workarounds
#

# fix broken install from dir: create tarball -> install_local
Install_local = function(path, ...) {
	pkgPath = Sprintf('%{dir}Q/%{base}Q.tgz', dir = tempdir(), base = splitPath(path)$base);
	System(Sprintf('tar czf %{pkgPath}Q %{path}Q'), 2);
	install_local(pkgPath, ...);
}

# misc

clearWarnings = function()assign('last.warning', NULL, envir = baseenv())

#
#	<p> packages
#

Library = function(name, ...) {
	if (!require(name, ...)) {
		r = install.packages(name);
		# if installation from CRAN fails, try bioconductor
		if (is.null(r)) {
			if (!exists('biocLite')) source("http://bioconductor.org/biocLite.R");
			biocLite(name)
		}
	}
	library(name, ...)
}
#
#	Rmeta.R
#Wed Jun  3 15:11:27 CEST 2015


#
#	<p> Meta-functions
#

#
#		Environments
#

# copy functions code adapted from restorepoint R package
object.copy = function(obj) {
	# Dealing with missing values
	if (is.name(obj)) return(obj);
	obj_class = class(obj);

	copy =
		if ('environment' %in% obj_class) environment.copy(obj) else
		if (all('list' == class(obj))) list.copy(obj) else
		#if (is.list(obj) && !(is.data.frame(obj))) list.copy(obj) else
		obj;
	return(copy)
}
list.copy = function(l)lapply(l, object.copy);
environment.restrict = function(envir__, restrict__= NULL) {
	if (!is.null(restrict__)) {
		envir__ = as.environment(List_(as.list(envir__), min_ = restrict__));
	}
	envir__
}
environment.copy = function(envir__, restrict__= NULL) {
	as.environment(eapply(environment.restrict(envir__, restrict__), object.copy));
}

bound_vars = function(f, functions = F) {
	fms = formals(f);
	# variables bound in default arguments
	vars_defaults = unique(unlist(sapply(fms, function(e)all.vars(as.expression(e)))));
	# variables used in the body
	vars_body = setdiff(all.vars(body(f)), names(fms));
	vars = setdiff(unique(c(vars_defaults, vars_body)), c('...', '', '.GlobalEnv'));
	if (functions) {
		vars = vars[!sapply(vars, function(v)is.function(rget(v, envir = environment(f))))];
	}
	vars
}
bound_fcts_std_exceptions = c('Lapply', 'Sapply', 'Apply');
bound_fcts = function(f, functions = F, exceptions = bound_fcts_std_exceptions) {
	fms = formals(f);
	# functions bound in default arguments
	fcts_defaults = unique(unlist(sapply(fms, function(e)all.vars(as.expression(e), functions = T))));
	# functions bound in body
	fcts = union(fcts_defaults, all.vars(body(f), functions = T));
	# remove variables
	#fcts = setdiff(fcts, c(bound_vars(f, functions), names(fms), '.GlobalEnv', '...'));
	fcts = setdiff(fcts, c(bound_vars(f, functions = functions), names(fms), '.GlobalEnv', '...'));
	# remove functions from packages
	fcts = fcts[
		sapply(fcts, function(e) {
			f_e = rget(e, envir = environment(f));
			!is.null(f_e) && environmentName(environment(f_e)) %in% c('R_GlobalEnv', '') && !is.primitive(f_e)
	})];
	fcts = setdiff(fcts, exceptions);
	fcts
}


environment_evaled = function(f, functions = FALSE, recursive = FALSE) {
	vars = bound_vars(f, functions);
	e = nlapply(vars, function(v) rget(v, envir = environment(f)));
	#Log(sprintf('environment_evaled: vars: %s', join(vars, ', ')), 7);
	#Log(sprintf('environment_evaled: functions: %s', functions), 7);
	if (functions) {
		fcts = bound_fcts(f, functions = TRUE);
		fcts_e = nlapply(fcts, function(v){
			#Log(sprintf('environment_evaled: fct: %s', v), 7);
			v = rget(v, envir = environment(f));
			#if (!(environmentName(environment(v)) %in% c('R_GlobalEnv')))
			v = environment_eval(v, functions = TRUE);
		});
		#Log(sprintf('fcts: %s', join(names(fcts_e))));
		e = c(e, fcts_e);
	}
	#Log(sprintf('evaled: %s', join(names(e))));
	r = new.env();
	lapply(names(e), function(n)assign(n, e[[n]], envir = r));
	#r = if (!length(e)) new.env() else as.environment(e);
	parent.env(r) = .GlobalEnv;
	#Log(sprintf('evaled: %s', join(names(as.list(r)))));
	r
}
environment_eval = function(f, functions = FALSE, recursive = FALSE) {
	environment(f) = environment_evaled(f, functions = functions, recursive = recursive);
	f
}

#
#	Parsing, evaluation
#

Parse = function(text, ...) {
	parse(text = text, ...)
}
Eval = function(e, ..., envir = parent.frame(), autoParse = T) {
	if (autoParse && is.character(e)) e = Parse(e, ...);
	eval(e, envir = envir)
	
}

#
#		Freeze/thaw
#

delayed_objects_env = new.env();
delayed_objects_attach = function() {
	attach(delayed_objects_env);
}
delayed_objects_detach = function() {
	detach(delayed_objects_env);
}

thaw_list = function(l)lapply(l, thaw_object, recursive = T);
thaw_environment = function(e) {
	p = parent.env(e);
	r = as.environment(thaw_list(as.list(e)));
	parent.env(r) = p;
	r
}

# <i> sapply
thaw_object_internal = function(o, recursive = T, envir = parent.frame()) {
	r = 		 if (class(o) == 'ParallelizeDelayedLoad') thaw(o) else
	#if (recursive && class(o) == 'environment') thaw_environment(o) else
	if (recursive && class(o) == 'list') thaw_list(o) else o;
	r
}

thaw_object = function(o, recursive = T, envir = parent.frame()) {
	if (all(search() != 'delayed_objects_env')) delayed_objects_attach();
	thaw_object_internal(o, recursive = recursive, envir = envir);
}

#
#	<p> backend classes
#

setGeneric('thaw', function(self, which = NA) standardGeneric('thaw'));

setClass('ParallelizeDelayedLoad',
	representation = list(
		path = 'character'
	),
	prototype = list(path = NULL)
);
setMethod('initialize', 'ParallelizeDelayedLoad', function(.Object, path) {
	.Object@path = path;
	.Object
});

setMethod('thaw', 'ParallelizeDelayedLoad', function(self, which = NA) {
	if (0) {
	key = sprintf('%s%s', self@path, ifelse(is.na(which), '', which));
	if (!exists(key, envir = delayed_objects_env)) {
		Log(sprintf('Loading: %s; key: %s', self@path, key), 4);
		ns = load(self@path);
		object = get(if (is.na(which)) ns[1] else which);
		assign(key, object, envir = delayed_objects_env);
		gc();
	} else {
		#Log(sprintf('Returning existing object: %s', key), 4);
	}
	#return(get(key, envir = delayed_objects_env));
	# assume delayed_objects_env to be attached
	return(as.symbol(key));
	}

	delayedAssign('r', {
		gc();
		ns = load(self@path);
		object = get(if (is.na(which)) ns[1] else which);
		object
	});
	return(r);
});

RNGuniqueSeed = function(tag) {
	if (exists('.Random.seed')) tag = c(.Random.seed, tag);
	md5 = md5sumString(join(tag, ''));
	r = list(
		kind = RNGkind(),
		seed = hex2int(substr(md5, 1, 8))
	);
	r
}

RNGuniqueSeedSet = function(seed) {
	RNGkind(seed$kind[1], seed$kind[2]);
	#.Random.seed = freeze_control$rng$seed;
	set.seed(seed$seed);
}

FreezeThawControlDefaults = list(
	dir = '.', sourceFiles = c(), libraries = c(), objects = c(), saveResult = T,
	freeze_relative = F, freeze_ssh = T, logLevel = Log.level()
);

thawCall = function(
	freeze_control = FreezeThawControlDefaults,
	freeze_tag = 'frozenFunction', freeze_file = sprintf('%s/%s.RData', freeze_control$dir, freeze_tag)) {

	load(freeze_file, envir = .GlobalEnv);
	r = with(callSpecification, {
		for (library in freeze_control$libraries) {
			eval(parse(text = sprintf('library(%s)', library)));
		}
		for (s in freeze_control$sourceFiles) source(s, chdir = T);
		Log.setLevel(freeze_control$logLevel);
		if (!is.null(freeze_control$rng)) RNGuniqueSeed(freeze_control$rng);

		if (is.null(callSpecification$freeze_envir)) freeze_envir = .GlobalEnv;
		# <!> freeze_transformation must be defined by the previous source/library calls
		transformation = eval(parse(text = freeze_control$thaw_transformation));
		r = do.call(eval(parse(text = f)), transformation(args), envir = freeze_envir);
		#r = do.call(f, args);
		if (!is.null(freeze_control$output)) save(r, file = freeze_control$output);
		r
	});
	r
}

frozenCallWrap = function(freeze_file, freeze_control = FreezeThawControlDefaults,
	logLevel = Log.level(), remoteLogLevel = logLevel)
	with(merge.lists(FreezeThawControlDefaults, freeze_control), {
	sp = splitPath(freeze_file, ssh = freeze_ssh);
	file = if (freeze_relative) sp$file else sp$path;
browser();
	#wrapperPath = sprintf("%s-wrapper.RData", splitPath(file)$fullbase);
	r = sprintf("R.pl --template raw --no-quiet --loglevel %d --code 'eval(get(load(\"%s\")[[1]]))' --",
		logLevel, file);
	r
})

frozenCallResults = function(file) {
	callSpecification = NULL;	# define callSpecification
	load(file);
	get(load(callSpecification$freeze_control$output)[[1]]);
}

freezeCallEncapsulated = function(call_,
	freeze_control = FreezeThawControlDefaults,
	freeze_tag = 'frozenFunction', freeze_file = sprintf('%s/%s.RData', freeze_control$dir, freeze_tag),
	freeze_save_output = F, freeze_objects = NULL, thaw_transformation = identity)
	with(merge.lists(FreezeThawControlDefaults, freeze_control), {

	sp = splitPath(freeze_file, ssh = freeze_ssh);
	outputFile = if (freeze_save_output)
		sprintf("%s_result.RData", if (freeze_relative) sp$base else sp$fullbase) else
		NULL;

	callSpecification = list(
		f = deparse(call_$fct),
		#f = freeze_f,
		args = call_$args,
		freeze_envir = if (is.null(call_$envir)) new.env() else call_$envir,
		freeze_control = list(
			sourceFiles = sourceFiles,
			libraries = libraries,
			output = outputFile,
			rng = freeze_control$rng,
			logLevel = freeze_control$logLevel,
			thaw_transformation = deparse(thaw_transformation)
		)
	);
	thawFile = if (freeze_relative) sp$file else sp$path;
	callWrapper = call('thawCall', freeze_file = thawFile);
	#Save(callWrapper, callSpecification, thawCall, file = file);
	#Save(c('callWrapper', 'callSpecification', 'thawCall', objects),
	#	file = freeze_file, symbolsAsVectors = T);
	#Save(c(c('callWrapper', 'callSpecification', 'thawCall'), objects),
	Save(c('callWrapper', 'callSpecification', 'thawCall', freeze_objects),
		file = freeze_file, symbolsAsVectors = T);
	freeze_file
})

# <!> assume matched call
# <A> we only evaluate named args
callEvalArgs = function(call_, env_eval = FALSE) {
	#if (is.null(call_$envir__) || is.null(names(call_$args))) return(call_);
	#if (is.null(call_$envir) || !length(call_$args)) return(call_);

	# <p> evaluate args
	if (length(call_$args)) {
		args = call_$args;
		callArgs = lapply(1:length(args), function(i)eval(args[[i]], envir = call_$envir));
		# <i> use match.call instead
		names(callArgs) = setdiff(names(call_$args), '...');
		call_$args = callArgs;
	}

	if (env_eval) {
		call_$fct = environment_eval(call_$fct, functions = FALSE, recursive = FALSE);
	}
	# <p> construct return value
	#callArgs = lapply(call_$args, function(e){eval(as.expression(e), call_$envir)});
	call_
}

#callWithFunctionArgs = function(f, args, envir__ = parent.frame(), name = NULL) {
callWithFunctionArgs = function(f__, args__, envir__ = environment(f__), name = NULL, env_eval = FALSE) {
	if (env_eval) f = environment_eval(f__, functions = FALSE, recursive = FALSE);
	call_ = list(
		fct = f__,
		envir = environment(f__),
		args = args__,
		name = name
	);
	call_
}

freezeCall = function(freeze_f, ...,
	freeze_control = FreezeThawControlDefaults,
	freeze_tag = 'frozenFunction', freeze_file = sprintf('%s/%s.RData', freeze_control$dir, freeze_tag),
	freeze_save_output = F, freeze_envir = parent.frame(), freeze_objects = NULL, freeze_env_eval = F,
	thaw_transformation = identity) {

	# args = eval(list(...), envir = freeze_envir)
	call_ = callWithFunctionArgs(f = freeze_f, args = list(...),
		envir__ = freeze_envir, name = as.character(sys.call()[[2]]), env_eval = freeze_env_eval);

	freezeCallEncapsulated(call_,
		freeze_control = freeze_control, freeze_tag = freeze_tag,
		freeze_file = freeze_file, freeze_save_output = freeze_save_output, freeze_objects = freeze_objects,
		thaw_transformation = thaw_transformation
	);
}


encapsulateCall = function(.call, ..., envir__ = environment(.call), do_evaluate_args__ = FALSE,
	unbound_functions = F) {
	# function body of call
	name = as.character(.call[[1]]);
	fct = get(name);
	callm = if (!is.primitive(fct)) {
		callm = match.call(definition = fct, call = .call);
		as.list(callm)[-1]
	} else as.list(.call)[-1];
	args = if (do_evaluate_args__) {
		nlapply(callm, function(e)eval(callm[[e]], envir = envir__))
	} else nlapply(callm, function(e)callm[[e]])
	# unbound variables in body fct
	#unbound_vars = 

	call_ = list(
		fct = fct,
		envir = envir__,

		#args = as.list(sys.call()[[2]])[-1],
		args = args,

		name = name
	);
	call_
}


#
#	</p> freeze/thaw functions
#

Deparse = function(o)join(deparse(o));
#
#	Rgraphics.R
#Mon 27 Jun 2005 10:52:17 AM CEST

require('grid');
#
#	<p> unit model
#
#	base unit is cm
setGeneric("factorToBase", function(this) standardGeneric("factorToBase"));
setGeneric("fromUnitToUnit", function(thisA, thisB) standardGeneric("fromUnitToUnit"));
setClass('unitGeneric', representation = list(value = 'numeric'), prototype = list(value = as.numeric(NA)));
setMethod('initialize', 'unitGeneric', function(.Object, value = as.numeric(NA)) {
	.Object@value = value;
	.Object
});
setMethod('fromUnitToUnit', c('unitGeneric', 'unitGeneric'), function(thisA, thisB)
	new(class(thisB), value = thisA@value * factorToBase(thisA) / factorToBase(thisB)));

setClass('unitCm', contains = 'unitGeneric');
setMethod('initialize', 'unitCm', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitCm', function(this)1);

setClass('unitInch', contains = 'unitGeneric');
setMethod('initialize', 'unitInch', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitInch', function(this)cm(1));

setClass('unitDpi150', contains = 'unitGeneric');
setMethod('initialize', 'unitDpi150', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitDpi150', function(this)cm(1)/150);

setClass('unitDpi200', contains = 'unitGeneric');
setMethod('initialize', 'unitDpi200', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitDpi200', function(this)cm(1)/200);

setClass('unitDpi300', contains = 'unitGeneric');
setMethod('initialize', 'unitDpi300', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitDpi300', function(this)cm(1)/300);

setClass('unitPoints', contains = 'unitGeneric');
setMethod('initialize', 'unitPoints', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitPoints', function(this)cm(1)/72);

valueU = valueUnited = function(value, unit) {
	class = getClass(Sprintf('unit%{unit}u'));
	new(class, value = value)
}
toUnit = function(value, unit)fromUnitToUnit(value, valueU(as.numeric(NA), unit));
ToUnit = function(value, unit)toUnit(value, unit)@value;

#
#	</p> unit model
#

cm2in = function(i) (i/2.54)

plotPoints = function(f=sin, interval=c(0,1), count = 1e2, steps = NULL, ...) {
	if (!is.null(steps))
		count = as.integer((interval[2] - interval[1]) / steps) else
		steps = (interval[2] - interval[1]) / (count + 1);
	xs = c(interval[1] + (0:(count - 1)) * steps, interval[2]);
	#ys = apply(t(xs), 2, function(x)(f(x)));
	#ys = Vectorize(function(x)f(x, ...))(xs);
	ys = Vectorize(function(x)f(x))(xs);
	data.frame(x = xs, y = ys)
}

plotRobust = function(f=sin, interval=c(0,1), count = 1e2, steps = NULL, points = F, ...) {
	pts = plotPoints(f, interval, count, steps, points, ...);
	if (points) {
		points(pts$x, pts$y, type="l");
	} else {
		plot(pts$x, pts$y, type="l");
	}
}

robustPlot = function(f=sin, interval=c(0,1), steps = 0.05, points = F, ...) {
	plotRobust(f, interval, steps = steps, points = points, ...);
}

#
# <p> vector functions
#

vNorm = function(v)sqrt(sum(v^2))
vToNorm = toNorm = function(v) {
	l = vNorm(v);
	if (l == 0) NA else v/l
}

# orthogonal vector in 2D
vPerp = function(v)rev(v) * c(-1, 1)

# the normal of a vector (in 2D), i.e. the perpendicular unit vector
vNormal = function(v)vToNorm(vPerp(v))

#
#	<p> graph drawing
#

# draw wedges
# x: x-coordinates
# y: y-coordinates
# w: widthes
wedge = function(x0, y0 = NULL, x1 = NULL, y1 = NULL, width = NULL, col = "black", ..., defaultWidth = .1) {
	d = if (!is.null(y0)) data.frame(x0, y0, x1, y1) else x0;
	if (is.null(width)) width = matrix(defaultWidth, ncol = 2, nrow = dim(x0)[1]);

	pts = matrix(sapply(1:dim(d)[1], function(i) {
		p1 = d[i, c("x0", "y0")];
		p2 = d[i, c("x1", "y1")];
		w = width[i, ];

		n = vNormal(p2 - p1); # normal of line
		c(p1 + n * w[1]/2, p1 - n * w[1]/2, p2 - n * w[2]/2, p2 + n * w[2]/2)
	}), ncol = 2, byrow = T);
	grid.polygon(x = pts[, 1], y = pts[, 2], id.lengths = rep(4, dim(d)[1]), gp = gpar(fill=1, col = col))
}

#
#	<p> ggplot2
#

#library('ggplot2');

qplotFaceted = function(f, from = 0, to = 1, data, facets, geom = 'line', ..., by = 0.02) {
	qplot.call = match.call(qplot);
	vars = formula.vars(facets);
	varLevels = unique(data[, vars, drop = F]);
	print(varLevels);
	xs = seq(from, to, by = by);
	r = apply(varLevels, 1, function(r) {
		environment(f) = f.env = new.env(parent = environment(f));
		fl = as.list(r);
		for (n in names(fl)) assign(n, fl[[n]], envir = f.env);
		ys = f(xs);
		d = data.frame(x = xs, y = ys, fl);
		d
	});
	d = rbindDataFrames(r);
	qplotArgs = c(as.list(qplot.call[-1]));
	p = qplot(x, y, data = d, facets = facets, geom = geom, ...);
	p
}

#
#	plot to file
#

plot_file_DefaultOptions = list(width = 12, height = 12, dpi = 200);

plot_file = function(code_or_object, file = NULL, options = list(), ..., envir = parent.frame()) {
	call = sys.call()[[2]];
	if (is.null(file)) file = tempFileName('plot_file', 'pdf', inRtmp = T);
	p = if (any(class(code_or_object) == 'ggplot')) {
		o = merge.lists(plot_file_DefaultOptions, options, list(...));
		with(o, { ggsave(code_or_object, file = file, width = width, height = height, dpi = dpi) });
		code_or_object
	} else {
		device = get(splitPath(file)$ext);
		device(file, ...);
			eval(call, envir = envir);
		dev.off();
		encapsulateCall(call, envir__ = envir);
	}
	p
}

#
#	<p> special plots
#

ggplot_qqunif = function(p.values, alpha = .05, fontsize = 6,
	tr = function(x)-log(x, 10), trName = '-log10(P-value)', colorCI = "#000099") {

	# <p> preparation
	if (any(p.values < 0 | p.values > 1)) stop("P.values not in interval (0, 1)");
	p.values = tr(sort(na.omit(p.values)));

	N = length(p.values);
	Ns = 1:N;
	# j-th order statistic from a uniform(0,1) sample has beta(j,n-j+1) distribution
	# (Casella & Berger, 2002, 2nd edition, pg 230, Duxbury)
	ciU = tr(qbeta(1 - alpha/2, Ns, N - Ns + 1));
	ciL = tr(qbeta(    alpha/2, Ns, N - Ns + 1));
	d = data.frame(theoretical = tr(Ns/N), ciU = ciU, ciL = ciL, p.value = p.values, colorCI = colorCI);
	p = ggplot(d) +
		geom_line(aes(x = theoretical, y = ciU, colour = colorCI)) +
		geom_line(aes(x = theoretical, y = ciL, colour = colorCI)) +
		geom_point(aes(x = theoretical, y = p.value), size = 1) +
		theme_bw() + theme(legend.position = 'none') + coord_cartesian(ylim = c(0, max(p.values)*1.1)) +
		scale_y_continuous(name = trName) +
		theme(text = element_text(size = fontsize));
	p
}
#ggplot_qqunif(seq(1e-2, 3e-2, length.out = 1e2))
QQunif = Qqunif = function(p.values, alpha = .05, fontsize = 12,
	tr = function(P)-log10(P), trName = Deparse(body(tr)), colorCI = "#000099",
	bins = c(2e2, 2e2), Nrep = 10) {

	# <p> preparation
	if (any(p.values <= 0 | p.values > 1)) stop("P.values not in interval (0, 1)");
	p.values = tr(sort(na.omit(p.values)));
	N = length(p.values);
	d = data.frame(theoretical = tr((1:N)/N), p.value = p.values);

	# <p> binning
	if (!is.null(bins)) {
		Ns = binPlot(d, bins = bins, Nrep = Nrep, returnIdcs = T);
		d = d[Ns, ];
	} else Ns = 1:N;

	# j-th order statistic from a uniform(0,1) sample has beta(j,n-j+1) distribution
	# (Casella & Berger, 2002, 2nd edition, pg 230, Duxbury)
	dPlot = data.frame(d,
		ciU = tr(qbeta(1 - alpha/2, Ns, N - Ns + 1)), ciL = tr(qbeta(    alpha/2, Ns, N - Ns + 1)),
		colorCI = colorCI);
	p = ggplot(dPlot) +
		geom_line(aes(x = theoretical, y = ciU, colour = colorCI)) +
		geom_line(aes(x = theoretical, y = ciL, colour = colorCI)) +
		geom_point(aes(x = theoretical, y = p.value), size = 1) +
		theme_bw() + theme(legend.position = 'none') + coord_cartesian(ylim = c(0, max(p.values)*1.1)) +
		scale_y_continuous(name = trName) +
		theme(text = element_text(size = fontsize));
	p
}


vp_at = function(x, y)viewport(layout.pos.row = x, layout.pos.col = y);
plot_grid_grid = function(plots, coords) {
	# <p> do plotting
	grid.newpage();
	# <!> layout might not be respected
	nrow = max(coords[, 1]);
	ncol = max(coords[, 2]);
	pushViewport(viewport(layout = grid.layout(nrow, ncol)));

	sapply(1:length(plots), function(i) {
		print(plots[[i]], vp = vp_at(coords[i, 1], coords[i, 2]));
	});
}

plot_grid_base = function(plots, coords, envirPlotVar = 'plot') {
	# <p> do plotting
	coordMat0 = matrix(0, nrow = max(coords[, 1]), ncol = max(coords[, 2]));
	coordMat = matrix.assign(coordMat0, coords, 1:length(plots));
	layout(coordMat);

	sapply(1:length(plots), function(i) {
		eval(get(envirPlotVar, plots[[i]]));
	});
# 			if (is.environment(plots[[i]])) eval(get(envirPlotVar, plots[[i]])) else print(plots[[i]]);
}


plot_grid = function(plots, nrow, ncol, byrow = T, mapper = NULL, envirPlotVar = 'plot') {
	if (missing(nrow)) {
		if (missing(ncol)) {
			ncol = 1;
			nrow = length(plots);
		} else {
			nrow = ceiling(length(plots) / ncol);
		}
	} else if (missing(ncol)) ncol = ceiling(length(plots) / nrow);

	coords = if (is.null(mapper))
		merge.multi(1:nrow, 1:ncol, .first.constant = byrow) else
		mapper(1:length(plots));
		
	if (is.environment(plots[[1]]))
		plot_grid_base(plots, coords, envirPlotVar) else
		plot_grid_grid(plots, coords)
}

plot_grid_to_path =  function(plots, ..., path,
	width = valueU(21, 'cm'), height = valueU(29.7, 'cm'), NperPage = NULL, pdfOptions = list(paper = 'a4')) {

	if (class(width) == 'numeric') width = valueU(width, 'inch');
	if (class(height) == 'numeric') height = valueU(height, 'inch');
	Nplots = length(plots);

	pages = if (!is.null(NperPage)) {
		Npages = ceiling(Nplots / NperPage);
		lapply(1:Npages, function(i) {
			Istrt = (i - 1) * NperPage + 1;
			Istop = min(i * NperPage, Nplots);
			Istrt:Istop
		})
	} else list(1:length(plots));

	pdfArgs = c(list(
		file = path, onefile = TRUE, width = ToUnit(width, 'inch'), height = ToUnit(height, 'inch')
	), pdfOptions);
	do.call(pdf, pdfArgs);

	lapply(pages, function(plotIdcs) {
		plot_grid(plots[plotIdcs], ...);
	});

	dev.off();
}

plot_adjacent = function(fts, factor, N = ncol(fts)) {
	ns = names(fts);
	ps = lapply(1:(N - 1), function(i){
		x = eval({fts[, i]});
		y = eval({fts[, i + 1]});
		qplot(x, y, color = as.factor(factor), xlab = ns[i], ylab = ns[i + 1]);
	});
}

plot_grid_pdf = function(plots, file, nrow, ncol, NperPage, byrow = T, mapper = NULL,
	pdfOptions = list(paper = 'a4')) {
	Nplots = length(plots);
	if (missing(nrow)) nrow = NperPage / ncol;
	if (missing(ncol)) ncol = NperPage / nrow;
	if (missing(NperPage)) NperPage = ncol * nrow;
	Npages = ceiling(Nplots / NperPage);

	do.call(pdf, c(list(file = file), pdfOptions));
	sapply(1:Npages, function(i) {
		Istrt = (i - 1) * NperPage + 1;
		Istop = min(i * NperPage, Nplots);
		plot_grid(plots[Istrt:Istop], nrow, ncol, byrow = byrow, mapper = mapper);
	});
	dev.off();
}

#
#	<p> Kaplan-Meier with ggplot
#

# stolen from the internet
createSurvivalFrame <- function(f.survfit){
	# initialise frame variable
	f.frame <- NULL
	# check if more then one strata
	if(length(names(f.survfit$strata)) == 0){
		# create data.frame with data from survﬁt
		f.frame <- data.frame(time=f.survfit$time, n.risk=f.survfit$n.risk, n.event=f.survfit$n.event,
			n.censor = f.survfit$n.censor, surv=f.survfit$surv, upper=f.survfit$upper, lower=f.survfit$lower)
		# create ﬁrst two rows (start at 1)
		f.start <- data.frame(time=c(0, f.frame$time[1]), n.risk=c(f.survfit$n, f.survfit$n), n.event=c(0,0),
		n.censor=c(0,0), surv=c(1,1), upper=c(1,1), lower=c(1,1))
		# add ﬁrst row to dataset
		f.frame <- rbind(f.start, f.frame)
		# remove temporary data
		rm(f.start)
	} else {
		# create vector for strata identiﬁcation
		f.strata <- NULL
		for(f.i in 1:length(f.survfit$strata)){
			# add vector for one strata according to number of rows of strata
			f.strata <- c(f.strata, rep(names(f.survfit$strata)[f.i], f.survfit$strata[f.i]))
		}
		# create data.frame with data from survﬁt (create column for strata)
		f.frame <- data.frame(time=f.survfit$time, n.risk=f.survfit$n.risk, n.event=f.survfit$n.event, n.censor = f.survfit
		$n.censor, surv=f.survfit$surv, upper=f.survfit$upper, lower=f.survfit$lower, strata=factor(f.strata))
		# remove temporary data
		rm(f.strata)
		# create ﬁrst two rows (start at 1) for each strata
		for(f.i in 1:length(f.survfit$strata)){
			# take only subset for this strata from data
			f.subset <- subset(f.frame, strata==names(f.survfit$strata)[f.i])
			f.start <- data.frame(time=c(0, f.subset$time[1]), n.risk=rep(f.survfit[f.i]$n, 2), n.event=c(0,0), n.censor=c(0,0), surv=c(1,1), upper=c(1,1), lower=c(1,1), strata=rep(names(f.survfit$strata)[f.i], 2))	
			# add ﬁrst two rows to dataset
			f.frame <- rbind(f.start, f.frame)
			# remove temporary data
			rm(f.start, f.subset)
		}
		# reorder data
		f.frame <- f.frame[order(f.frame$strata, f.frame$time), ]
		# rename row.names
		rownames(f.frame) <- NULL
	}
	# return frame
	return(f.frame)
}

# deﬁne custom function to draw kaplan-meier curve with ggplot
qplot_survival = function(f.frame, f.CI = "default", f.shape = 3, ..., title = NULL, layers = NULL){
	# use different plotting commands dependig whether or not strata's are given
	p = if("strata" %in% names(f.frame) == FALSE) {
		# conﬁdence intervals are drawn if not speciﬁed otherwise
		if(f.CI == "default" | f.CI == TRUE ){
			# create plot with 4 layers (ﬁrst 3 layers only events, last layer only censored)
			# hint: censoring data for multiple censoring events at timepoint are overplotted
			# (unlike in plot.survﬁt in survival package)
			ggplot(data=f.frame, ...) +
			geom_step(aes(x=time, y=surv), direction="hv") +
			geom_step(aes(x=time, y=upper), directions="hv", linetype=2) +
			geom_step(aes(x=time,y=lower), direction="hv", linetype=2) +
			geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape)
		} else {
			# create plot without conﬁdence intervalls
			ggplot(data=f.frame) +
			geom_step(aes(x=time, y=surv), direction="hv") +
			geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape)
		}
	} else {
		# without CI
		if(f.CI == "default" | f.CI == FALSE){
			ggplot(data=f.frame, aes(group=strata, colour=strata), ...) +
			geom_step(aes(x=time, y=surv), direction="hv") +
			geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape)
		} else {
			ggplot(data=f.frame, aes(colour=strata, group=strata), ...) +
			geom_step(aes(x=time, y=surv), direction="hv") +
			geom_step(aes(x=time, y=upper), directions="hv", linetype=2, alpha=0.5) +
			geom_step(aes(x=time,y=lower), direction="hv", linetype=2, alpha=0.5) +
			geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape)
		}
	}
	if (!is.null(title)) p = p + labs(title = title);
	if (!is.null(layers)) p = p + layers;
	p
}

quantileBinning = function(x, Nbins) {
	cut(x, quantile(x, seq(0, 1, length = Nbins + 1)), labels = seq_len(Nbins), include.lowest = TRUE)
}

kaplanMeierStrat = function(d1, f1, levels = NULL, title = NULL) {
	# <i> only allow one covariate
	stratVar = all.vars(formula.rhs(f1))[1];
	if (!is.null(levels)) {
		d1[[stratVar]] = as.factor(quantileBinning(d1[[stratVar]], levels));
	}
	stratValue = levels(drop.levels(d1[[stratVar]]));
	# <p> log-rank test
	lr = survdiff(as.formula(f1), data = d1);
	p.lr = pchisq(lr$chisq, df = dim(lr$n) - 1, lower.tail = F)
	# <p> kaplan-meyer
	fit = survfit(as.formula(f1), data = d1);
	fit.frame = createSurvivalFrame(fit);
	titleCooked = if (is.null(title))
		sprintf('%s, [P = %.2e]', stratVar, p.lr) else
		Sprintf('%{title}s, [P = %{p}.2e]', p = p.lr)
	p = qplot_survival(fit.frame, F, 20, title = titleCooked,
		layers = theme_bw());
	list(plot = p, level = stratValue)
}

kaplanMeierNested = function(d, f1, strata, combine = FALSE) {
	require('gdata');
	# <!><b> fix special case of length(strata) == 1
	d = Df(d, dummy_comb__kaplanMeierNested = 1);
	dStrat = d[, c(strata, 'dummy_comb__kaplanMeierNested'), drop = F];
	cbs = Df_(valueCombinations(dStrat), min_ = 'dummy_comb__kaplanMeierNested');
	dStrat = Df_(dStrat, min_ = 'dummy_comb__kaplanMeierNested');

	plots = apply(cbs, 1, function(r) {
		sel1 = nif(apply(dStrat == r, 1, all));
		sel = nif(sapply(1:nrow(d), function(i)all(dStrat[i, ] == r)));
		if (sum(sel) == 0) {
			warning('empty group selected');
			return(NULL);
		}
		#if (sum(sel) == 0) return(NULL);
		dSel = d[sel, , drop = F];
		N = sum(sel);
		title = Sprintf('Stratum: %{stratum}s, N = %{N}d',
			stratum = paste(names(r), r, sep = '=', collapse = ', '));
		kaplanMeierStrat(dSel, f1, title = title);
	});
	plots
}

#
#	<p> histograms
#


histogram_colors = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7");
histogram_colors = c('red', 'blue', 'green', 'yellow');
#dayColor = list(`0` = 'red', `1` = 'blue', `3` = 'green', `8` = 'yellow');

histogram_overlayed = function(data, f1,
	groupNames = levels(groups), palette = histogram_colors, log10 = T,
	x_lab = formula.response(f1), title = 'histogram', alpha = .3, breaks = 30) {

	# <p> column names, range
	xn = formula.response(f1);
	gn = formula.covariates(f1);
	lvls = levels(data[[gn]]);
	tab = table(cut(data[[xn]], breaks));
	#mx = if (log10) 10^ceiling(log10(max(tab))) else max(tab);
	mx = max(tab);

	# <p>  create legend using pseudo data (shifted out of view)
	dp = Df(x = rep(0, length(lvls)), y = rep(mx + 1, length(lvls)), group = lvls);
	p = ggplot(dp, aes(x = x)) +
		geom_rect(data = dp, aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 1, fill = group)) +
		scale_fill_manual(name = gn, values = palette);

	# <p> histograms
	for (i in 1:length(lvls)) {
		p = p + geom_histogram(data = data.frame(x = data[[xn]][data[[gn]] == lvls[i]]),
			fill = palette[i], alpha = alpha);
	}

	# <p> log transform
	if (log10) p = p + scale_y_continuous(trans = 'log10') + coord_cartesian(ylim = c(1, mx));

	# <p> final formatting
	p = p + ggtitle(title) + xlab(x_lab);
	p

}

#'@param data:	data frame or list
histograms_alpha = function(data, palette = histogram_colors, log10 = F,
	x_lab = '', title = 'histogram', alpha = .3, origin = NULL, binwidth = NULL, relative = FALSE,
	textsize = 20) {
	# <p> preparation
	N = length(as.list(data));
	columns = names(data);
	mx = max(unlist(as.list(data)), na.rm = T);
	mn = min(unlist(as.list(data)), na.rm = T);

	# <p>  create legend using pseudo data (shifted out of view)
	dp = Df(x = rep(2*mx + 2, N), y = rep(0, N), group = columns);
	p = ggplot(dp, aes(x = x)) +
		geom_rect(data = dp, aes(xmin = x, xmax = x + .01, ymin = y, ymax = y + .01, fill = group)) +
		scale_fill_manual(name = dp$group, values = palette);

	# <p> histograms
	for (i in 1:N) {
		col = columns[i];
		dfH = data.frame(x = data[[col]]);
		p = p + if (relative)
			geom_histogram(data = dfH, aes(y=..count../sum(..count..)),
				fill = palette[i], alpha = alpha, binwidth = binwidth, origin = origin
			) else
			geom_histogram(data = dfH, fill = palette[i], alpha = alpha, binwidth = binwidth, origin = origin)
	}

	# <p> log transform
	if (log10) p = p + scale_y_continuous(trans = 'log10') + coord_cartesian(ylim = c(1, mx));

	# <p> final formatting
	p = p + coord_cartesian(xlim = c(mn - 1, mx + 1)) + ggtitle(title) + xlab(x_lab) + theme_bw() +
		theme(text = element_text(size = textsize));
	if (relative) p = p + ylab('percentage');
	p

}

#
#	<p> saving of plots
#


# base unit is 600dpi
units_conv = list(
	cm = list(from = function(cm)(cm/2.54*600), to = function(b)(b/600*2.54)),
	points = list(from = function(points)(points/72*600), to = function(b)(b/600*72)),
	inch = list(from = function(i)(i*600), to = function(b)(b/600)),
	dpi150 = list(from = function(dpi)(dpi/150*600), to = function(b)(b*150/600)),
	dpi300 = list(from = function(dpi)(dpi/300*600), to = function(b)(b*300/600))
);
units_default = list(jpeg = 'dpi150', pdf = 'cm', png = 'points');

# autoscale scales canvas sizes to make scaling uniform between devices
#	increases the pixel resolution for pixel devices
deviceTypes = list(
	jpeg = list(hasDpi = TRUE, autoScale = 1),
	png = list(hasDpi = TRUE, autoScale = 1),
	pdf = list(forceUnit = 'inch', autoScale = 1)
);

Device = function(type, plot_path, width, height, ..., units = 'cm', dpi = NA, autoScale = FALSE) {
	device = get(type);
	o = deviceTypes[[type]];
	if (!is.null(o$forceUnit)) {
		width = toUnit(width, o$forceUnit);
		height = toUnit(height, o$forceUnit);
	}
	width = width@value;
	height = height@value;
	if (autoScale && !is.null(o$autoScale)) {
		width = width * o$autoScale;
		height = height * o$autoScale;
	}
	if (nif(o$hasDpi))
		device(plot_path, width = width, height = height, units = units, res = dpi, ...) else
		device(plot_path, width = width, height = height, ...);
}

# use 'cm' as output unit
# if unit_out is from a class beginning with unitDpi, produce a resolution argument correpsonding
#	to that many dpi

plot_save_raw = function(object, ..., width = 20, height = 20, plot_path = NULL,
	type = NULL, options = list(), unit = 'cm', unit_out = NULL, autoScale = FALSE, envir = parent.frame()) {

	if (is.null(unit_out)) unit_out = units_default[[type]];
	#width = toUnit(width, unit_out)@value;
	#height = toUnit(height, unit_out)@value;
	width = toUnit(width, 'cm');
	height = toUnit(height, 'cm');
	dpi = Nina(as.integer(FetchRegexpr('dpi(\\d+)', unit_out, captures = T)));
 	Logs('Saving %{type}s to "%{plot_path}s"  [width: %{w}f %{h}f dpi: %{dpi}d]',
		w = width@value, h = height@value, logLevel = 5);
	Device(type, plot_path, width = width, height = height, units = 'cm', dpi = dpi, ...,
		autoScale = autoScale);
		#ret = eval(object, envir = envir);
		ret = if (any(class(object) %in% c('ggplot', 'plot'))) {
			print(object)
		} else {
			eval(object, envir = envir);
		}
	dev.off();
}

plotSizes = list(
	a4 = list(width = valueU(21, 'cm'), height = valueU(21 * sqrt(2), 'cm')),
	a4R = list(width = valueU(21*sqrt(2), 'cm'), height = valueU(21, 'cm')),
	a4_15 = list(width = valueU(1.5*21, 'cm'), height = valueU(1.5 * 21 * sqrt(2), 'cm')),
	a4R_15 = list(width = valueU(1.5*21*sqrt(2), 'cm'), height = valueU(1.5 * 21, 'cm'))
);
#
#	Examples:
#	plot_save(c('a.jpeg', 'a.png'), options = list(jpeg = list(unit_out = 'dpi300')));
plot_typeMap = list(jpg = 'jpeg');
plot_save = function(object, ..., size, width, height, plot_path = NULL,
	type = NULL,
	envir = parent.frame(), options = list(), simplify = T, unit_out = NULL, createDir = TRUE) {

	# <p> plot size
	# default size
	if (missing(size) && (missing(width) || missing(height))) size = 'a4R';
	if (!missing(size)) {
		width = plotSizes[[size]]$width;
		height = plotSizes[[size]]$height;
	}
	if (class(width) == 'numeric') width = valueU(width, 'cm');
	if (class(height) == 'numeric') height = valueU(height, 'cm');

	if (is.null(plot_path)) file = tempFileName('plot_save', 'pdf', inRtmp = T);
	ret = lapply(plot_path, function(plot_path) {
		if (createDir) Dir.create(plot_path, recursive = T, treatPathAsFile = T);
		if (is.null(type) && !is.null(plot_path)) {
			ext = splitPath(plot_path)$ext;
			type = firstDef(plot_typeMap[[ext]], ext);
		}
		uo = firstDef(options[[type]]$unit_out, unit_out, units_default[[type]]);
		Logs(con("plot_path: %{plot_path}s, device: %{type}s, unit_out: %{uo}s "), logLevel = 5);
		args = c(list(object),
			list(type = type, plot_path = plot_path,
				width = width, height = height, unit_out = uo,
				options = options, envir = envir),
			list(...)
		);
		do.call(plot_save_raw, args);
	});
	if (length(plot_path) == 1 && simplify) ret = ret[[1]];
	r = list(path = plot_path, ret = ret);
	r
}

# USAGE:
# plts = exprR1$Eapply(function(data, probe_name) {
# 	delayedPlot({
# 		boxplot(model, data, main = main);
# 		beeswarm(model, data, add = T)
# 	})
# });
# eval(plts[[1]])

delayedPlot = function(plotExpr, envir = parent.frame()) {
	e = new.env(parent = envir);
	delayedAssign('plot', plotExpr, assign.env = e)
	e
}

#
#	<p> legacy function from other packages
#

# gridExtra
ebimageGrob = function (pic, x = 0.5, y = 0.5, scale = 1, raster = FALSE, angle = NULL, ...) {
    dims <- dim(pic)
    colours = t(channel(pic, "x11"))
    width = unit(scale * dims[1], "points")
    height = unit(scale * dims[2], "points")
    angle <- if (is.null(angle)) 
        0
    else angle
    vp <- viewport(x = x, y = y, width = width, height = height, 
        angle = angle)
    if (raster) {
        child <- rasterGrob(colours, vp = vp, ...)
    }
    else {
        colours <- colours[rev(seq_len(nrow(colours))), ]
        require(RGraphics)
        child <- imageGrob(dims[2], dims[1], cols = colours, 
            gp = gpar(col = colours), byrow = FALSE, vp = vp, 
            ...)
    }
    gTree(width = width[[1]], height = height[[1]], children = gList(child), 
        cl = "ebimage")
}

#
#	<p> transformations
#

# <p> helper functions

# apply transformations on coordinates (given as row-wise points of a nx2 matrix)
applyT = function(coords, transf)t(transf %*% t(cbind(coords, 1)))[, -3]
# rectangle of width/height
rectWH = function(width, height)
	t(c(width, height) * (c(-.5, -.5) + t(matrix(c(0,0, 1,0, 1,1, 0,1), byrow = T, ncol = 2))))
# caculate circumference radius of rectangle
rectRad = function(width, aspectRatio, margin) {
	width = width * (1 + margin);
	height = width/aspectRatio;
	radius = norm(matrix(c(width, height)), '2') / 2;
}

# <p> actual transformations
# transformations are applied right to left (see applyT)

transform2dTranslate = function(delta) {
	matrix(c(1, 0, 0, 0, 1, 0, delta[1], delta[2], 1), ncol = 3)
}

transform2dRotation = function(alpha) {
	matrix(c(cos(alpha), sin(alpha), 0, -sin(alpha), cos(alpha), 0, 0, 0, 1), ncol = 3)
}

# tranlation after rotation
transform2dRotTrans = function(alpha, delta) {
	transform2dTranslate(delta) %*% transform2dRotation(alpha)
}

# move in direction of rotation after rotation
transform2dMoveYRot = function(dist, alpha) {
	transl = transform2dTranslate(c(0, dist));
	rot = transform2dRotation(alpha);
	r = rot %*% transl;
	r
}

# move in direction of rotation after rotation
transform2dRotMove = transform2dRotMoveY = function(alpha, dist) {
	rot = transform2dRotation(alpha);
	v = rot %*% c(dist, 0, 1);
	transform2dTranslate(v[1:2]) %*% rot
}


# create transformation that is created by rotating a points
#	the destination of the point is used to create a translation
transform2dRot2Move = function(alpha, point) {
	p = applyT(matrix(point, ncol = 2), transform2dRotation(alpha));
	transform2dTranslate(p);
}

dfTop = function(d, N)d[1:min(nrow(d), N),, drop = F]
vTop = function(v, N)v[1:min(length(v), N)]

#
#	<p> binning
#

# partition range of data into bins according to bins and choose Nrep representatives for each bin
binPlot = function(data, formula = NULL, bins = c(1e2, 1e2), Nrep = 3, eps = 1e-5, permute = TRUE,
	plotRange = NULL, returnIdcs = FALSE) {
	cols = if (is.null(formula)) 1:2 else match(all.vars(formula), dimnames(data)[[2]]);
	rangeX = if (is.null(plotRange$rangeX)) range(data[, cols[1]]) else plotRange$rangeX;
	rangeY = if (is.null(plotRange$rangeX)) range(data[, cols[2]]) else plotRange$rangeY;
	# bin number defined by numbering in 1st quadrant within range, row-first
	# i.e. (0, 0): origin, bins[1]: (0, 1), ...
	RX = (rangeX[2] - rangeX[1] + eps);
	RY = (rangeY[2] - rangeY[1] + eps);
	bin = as.vector(apply(data[, cols], 1, function(e) {
		binX = as.integer(floor(bins[1] * ((e[1] - rangeX[1]) / RX)));
		binY = as.integer(floor(bins[2] * ((e[2] - rangeY[1]) / RY)));
		bin = binY * bins[1] + binX;
		bin
	}));

	# <p> select data
	idcs = unlist(lapply(unique(bin), function(b) {
		idcs = which(bin == b)
		if (permute) idcs = Sample(idcs);
		vTop(idcs, Nrep)
	}));
	r = if (returnIdcs) idcs else data[idcs, , drop = F];
	r
}

binPlot_1 = function(data, formula = NULL, bins = c(1e2, 1e2), Nrep = 3, eps = 1e-5, permute = TRUE,
	plotRange = NULL, returnIdcs = FALSE) {
	cols = if (is.null(formula)) 1:2 else match(all.vars(formula), dimnames(data)[[2]]);
	rangeX = if (is.null(plotRange$rangeX)) range(data[, cols[1]]) else plotRange$rangeX;
	rangeY = if (is.null(plotRange$rangeX)) range(data[, cols[2]]) else plotRange$rangeY;
	# bin number defined by numbering in 1st quadrant within range, row-first
	# i.e. (0, 0): origin, bins[1]: (0, 1), ...
	RX = (rangeX[2] - rangeX[1] + eps);
	RY = (rangeY[2] - rangeY[1] + eps);
	bin = as.vector(apply(data[, cols], 1, function(e) {
		binX = as.integer(floor(bins[1] * ((e[1] - rangeX[1]) / RX)));
		binY = as.integer(floor(bins[2] * ((e[2] - rangeY[1]) / RY)));
		bin = binY * bins[1] + binX;
		bin
	}));

	# <p> select data
	dataSL = lapply(unique(bin), function(b) {
		d0 = data[which(bin == b), , drop = F];
		d1 = if (permute) d0[sample.int(nrow(d0)), , drop = F] else d0;
		dfTop(d1, Nrep)
	});
	dataS = do.call(rbind, dataSL);
	dataS
}
binPlot_0 = function(data, formula = NULL, bins = c(1e2, 1e2), Nrep = 3, eps = 1e-5, permute = TRUE) {
	cols = if (is.null(formula)) 1:2 else match(all.vars(formula), dimnames(data)[[2]]);
	rangeX = range(data[, cols[1]]);
	rangeY = range(data[, cols[2]]);
	# bin number defined by numbering in 1st quadrant within range, row-first
	# i.e. (0, 0): origin, bins[1]: (0, 1), ...
	bin = as.vector(apply(data[, cols], 1, function(e) {
		binX = as.integer(floor(bins[1] * ((e[1] - rangeX[1]) / (rangeX[2] - rangeX[1] + eps))));
		binY = as.integer(floor(bins[2] * ((e[2] - rangeY[1]) / (rangeY[2] - rangeY[1] + eps))));
		bin = binY * bins[1] + binX;
		bin
	}));
	data1 = cbind(data, bin);
	if (permute) data1 = data1[sample.int(nrow(data1)), ];

	# <p> ordered data
	# <A> assume order to be stable
	dataO = data1[order(data1[, 'bin']), ];

	# <p> selected data
	binsU = unique(dataO[, 'bin']);
	dataDf = as.data.frame(dataO);
	dataSL = lapply(binsU, function(b)subsetTop(dataO, with(dataDf, bin == b), Nrep));
	dataS = do.call(rbind, dataSL);
	dataS
}
#
#	Rreporting.R
#Mon 06 Feb 2006 11:41:43 AM EST

#
#	<p> documentation (by example
#

# Example:
# 	create a Reporter instance to report to LaTeX
# r = new("Rreporter", final.path = "/tmp/output.pdf", patterns = "latex");

#
#	</p> end documentation
#

#
#	<p> generic reporting functions
#

row.standardFormatter = function(e, digits = NA) {
	f = if (is.na(digits) || substring(digits, 1, 1) == 'p') {
		e
	} else {
		e = as.numeric(e);
		if (substring(digits, 1, 1) == "#") {
			sprintf("%.*e", as.numeric(substring(digits, 2)), e)
		} else if (substring(digits, 1, 1) == "%") {
			sprintf('%.*f\\%%', as.numeric(substring(digits, 2)), e * 100)
		} else if (as.numeric(digits) < 0) {
			digits = as.integer(digits);
			ifelse(floor(log10(abs(e))) <= digits,
				sprintf("%.*g", -digits, e),
				sprintf("%.*f", -digits, e))
		} else { sprintf("%.*f", as.integer(digits), e) }
	}
	f
}

latex = list(
	# table patterns
	header = "{LONGTABLESTARTFMT\\begin{longtable}{COLUMN_FORMAT}\nLONGTABLECAPTION",
	columnNames = "%s%s %s\\hline\n",
	separator = " & ",
	hline = "\\hline\n",
	lineEnd = " \\\\\n",
	subHeading = function(h, rowSpan)
		sprintf("\\hline\n & \\multicolumn{%d}{l}{\\bf %s}\\\\\\hline\n", rowSpan, h),
	footer = "\\end{longtable}}\n\n",

	postProcess = function(s, df, row.formatters, digits, caption, na.value, subHeadings,
		ignoreRowNames, patterns, alignment, startFmt, bars) {
		if (is.null(alignment)) alignment = rep(NA, dim(df)[2]);
		alignment[is.na(alignment) & !is.na(digits)] = 'r';
		alignment[is.na(alignment)] = 'l';
		paragraphs = !is.na(digits) & substring(digits, 1, 1) == 'p';
		alignment[paragraphs] = digits[paragraphs];
		bars = c(bars, rep(F, length(alignment) - length(bars)));
		alignment = ifelse(!bars, alignment, paste(alignment, '|', sep = ''));
		colFmt = sprintf("%s%s", ifelse(ignoreRowNames, "", "r|"),
			paste(alignment, collapse = ""));
		captionPt = if (caption == '') list(LONGTABLECAPTION = '') else
			list(LONGTABLECAPTION = '\\caption{CAPTION}\\\\\n', CAPTION = caption)
		s = mergeDictToString(merge.lists(
			list(COLUMN_FORMAT = colFmt, LONGTABLESTARTFMT = startFmt),
			captionPt), s);
		s
	},
	quote = function(s, detectFormula = T) {
		s = gsub('_', '\\\\_', s, perl = T);
		s = gsub('&', '\\\\&', s, perl = T);
		s = gsub('~', '$\\\\sim$', s, perl = T);
		s = gsub('([<>])', '$\\1$', s, perl = T);
		s = gsub('\\^2', '$^2$', s, perl = T);
		#ifelse(length(grep('_', s)) > 0, gsub('_', '\\\\_', s, perl = T), s)
		s
	},

	# general text formatting
	newpage = "\n\n\\newpage\n\n",
	section = "\\section{SECTION_NAME}\n\n",
	subsection = "\\subsection{SECTION_NAME}\n\n",
	paragraph = "PARAGRAPH_TEXT\\par\n\n",

	# finalize
	document = "HEADER\n\\begin{document}\nDOC_HERE\n\\end{document}\n",
	docHeader = "\\documentclass[a4paper,oneside,11pt]{article}\n\\usepackage{setspace,amsmath,amssymb, amsthm, epsfig, epsf, amssymb, amsfonts, latexsym, rotating, longtable, setspace, natbib, a4wide,verbatim, caption}\n\\usepackage[utf8x]{inputenc}",
	docCmd = "cd TMP_DIR ; pdflatex TMP_FILE_BASE 1&>/dev/null ; cp TMP_FILE_BASE.pdf OUTPUT_FILE",

	# figure table
	figureTable = list(
		table = "\\begin{center}\\begin{tabular}{COLS}\nROWS\\end{tabular}\\end{center}",
		figure = '\\includegraphics[width=%.3f\\textwidth]{%s}',
		figureCaption = "\\begin{minipage}[b]{%.3f\\linewidth}\\centering
		\\begin{tabular}{c}
			%s\\\\
			\\includegraphics[width=\\textwidth]{%s}
		\\end{tabular}\\end{minipage}\n",
		formatTable = function(rows, cols = 2, template = latex$figureTable$table) {
			mergeDictToString(list(COLS = join(rep('c', cols), ''), ROWS = rows), template)
		},
		formatRows = function(rows, cols = 2) {
			sep = c(rep(' & ', cols - 1), "\\\\\n");
			seps = rep(sep, (length(rows) + cols - 1) %/% cols);
			seps = seps[1:length(rows)];
			rs = meshVectors(rows, seps);
			r = join(c(pop(rs), "\n"), '');
#browser();
#			texRows = sapply(1:(length(rows) - 1), function(i)sprintf('%s%s', rows[i],
#				ifelse(i %% cols == 1, ' & ', "\\\\\n")));
#			rs = join(c(texRows, rev(rows)[1], "\n"), '');
#			rs
		},
		formatFigure = function(figure, cols = 2, width = 1/cols - 0.05,
			template = latex$figureTable$figure, templateCaption = latex$figureTable$figureCaption,
			caption = '') {
			if (File.exists(figure)) figure = path.absolute(figure);
			caption = if (firstDef(caption, '') != '')
				sprintf(templateCaption, width, caption, figure) else
				sprintf(template, width, figure)
		}
	)
);

# bars: parallel structure to digits: where to insert vertical bars
report.data.frame.toString = function(df = NULL,
	row.formatters = c(row.standardFormatter), digits = NA, caption = "", na.value = "-",
	subHeadings = NULL, ignoreRowNames = F, patterns = latex, names.as = NULL, alignment = NULL,
	quoteHeader = T, quoteRows = T, quoteRowNames = quoteHeader, startFmt = '', bars = NULL) {
	with(patterns, {
	# <p> initialize
	rFmtC = length(row.formatters);
	if (length(digits) == 1) digits = rep(digits, dim(df)[2]);
	t = header;	# the nascent table as string
	if (!is.null(names.as)) names(df) = names.as;

	# <p> complete header
	header = if (quoteHeader) sapply(dimnames(df)[[2]], quote) else dimnames(df)[[2]];
	t = con(t, sprintf("%s%s%s%s", ifelse(!ignoreRowNames, separator, ""),
		paste(header, collapse = separator), lineEnd, hline));

	# <p> append rows
	for (i in Seq(1, nrow(df))) {
		row.fmt = row.formatters[[((i - 1) %% rFmtC) + 1]];

		if (i %in% subHeadings$indeces) {	# insert subheading
			j = which(subHeadings$indeces == i);
			t = con(t, subHeading(subHeadings$headings[j], dim(df)[2] - ignoreRowNames));
		}
		if (!ignoreRowNames) {
			rowName = dimnames(df)[[1]][i];
			t = con(t, sprintf("%s%s", if (quoteRowNames) quote(rowName) else rowName, separator));
		}
		# <p> formatting and quoting
		values = sapply(1:ncol(df), function(j)
			if (is.na(df[i, j])) na.value else row.fmt(as.character(df[i, j]), digits[j])
		);
		if (quoteRows) values = sapply(values, quote);
		t = con(t, sprintf("%s%s", paste(values, collapse = separator), lineEnd));
	}

	t = con(t, footer);
	t = postProcess(t, df, row.formatters, digits, caption, na.value, subHeadings,
		ignoreRowNames, patterns, alignment, startFmt, bars);
	})
}

report.figure.tableSingle = function(figures, cols = 2, width = 1/cols - 0.05, patterns = latex, captions = NULL)
	with(patterns, with(figureTable, {

	figs = sapply(1:length(figures), function(i){
		formatFigure(figures[i], cols = cols, width = width, caption = captions[i])
	});
	rows = formatRows(figs, cols = cols);
	table = formatTable(rows, cols = cols);
	table
}))
report.figure.table = function(figures, cols = 2, width = 1/cols - 0.05, patterns = latex,
	captions = NULL, maxRows = 5) with(patterns, {
	NfiguresPerPage = maxRows * cols;
	Nfigures = ceiling(ceiling(length(figures)/cols) / maxRows);
	if (Nfigures > 1) {
		tables = sapply(1:Nfigures, function(i) {
			Is = ((i - 1)*NfiguresPerPage + 1): min((i*NfiguresPerPage), length(figures));
			report.figure.tableSingle(figures[Is], cols, width, patterns, captions[Is])
		});
		join(tables, "\n")
	} else report.figure.tableSingle(figures, cols, width, patterns, captions)
})


#
#	<p> Rreporter (base on S4 methods)
#

setClass("Rreporter",
	representation(tmp.path = "character", final.path = "character", patterns = "list"),
	prototype(tmp.path = sprintf("%s.rr", tempfile()), final.path = NULL, patterns = latex)
);

setMethod("initialize", "Rreporter", function(.Object, final.path, patterns = latex) {
	.Object@final.path = final.path;
	.Object@patterns = if (is.character(patterns)) get(patterns) else patterns;
	# create temp file
	cat("", file = .Object@tmp.path);
	.Object
});

# <p> generic methods

report.data.frame = function(self, df = NULL, row.formatters = c(row.standardFormatter),
	digits = NA, caption = "", na.value = "-", subHeadings = NULL, ignoreRowNames = F, verbose = T) {
	patterns = self@patterns;
	s = report.data.frame.toString(df, row.formatters , digits, caption, na.value,
		subHeadings, ignoreRowNames, patterns);
	cat(s, file = self@tmp.path, append = T);
	if (verbose) cat(s);
	self
}

report.newpage = function(self) {
	cat(self@patterns$newpage, file = self@tmp.path, append = T);
}
report.newsection = function(self, name) {
	cat(
		mergeDictToString(list(SECTION_NAME = name), self@patterns$section),
		file = self@tmp.path, append = T
	);
}
report.newsubsection = function(self, name) {
	cat(
		mergeDictToString(list(SECTION_NAME = name), self@patterns$subsection),
		file = self@tmp.path, append = T
	);
}
report.paragraph = function(self, text) {
	cat(
		mergeDictToString(list(PARAGRAPH_TEXT = text), self@patterns$paragraph),
		file = self@tmp.path, append = T
	);
}

report.finalize = finalize = function(self) {
	cmd = sprintf("cp \"%s\" \"%s\"", self@tmp.path, absolutePath(self@final.path));
	System(cmd);
}

report.finalizeAsDocument = function(self) {
	# <p> read document to string
	doc = readFile(self@tmp.path);
	# <p> write processed document
	sp = splitPath(self@tmp.path);
	writeFile(sprintf("%s.tex", sp$fullbase),
		mergeDictToString(list(
			HEADER = self@patterns$docHeader, DOC_HERE = readFile(self@tmp.path)
		), self@patterns$document)
	);
	cmd = mergeDictToString(
		list(
			TMP_DIR = sp$dir,
			TMP_FILE = sp$path,
			TMP_FILE_BASE = sp$fullbase,
			OUTPUT_FILE = absolutePath(self@final.path)
		)
	, self@patterns$docCmd)
	System(cmd);
}

#
#	<p> end Rreporter (base on S4 methods)
#

#
#	<p> convenience methods
#

reportDataFrame2pdf = function(df, file = tempfile(), row.formatters = c(row.standardFormatter),
	digits = NA, caption = "", na.value = "-", subHeadings = NULL, ignoreRowNames = F, verbose = T) {
	r = new("Rreporter", final.path = file);
	report.data.frame(r, df,
		row.formatters, digits, caption, na.value, subHeadings, ignoreRowNames, verbose);
	report.finalizeAsDocument(r);
}

#
#	<p> sweave
#

swaeveIt = function(file = NULL, N = 1) {
	System(sprintf("R CMD Sweave '%s.Rnw'", file));
	cmd = sprintf("sh -c 'pdflatex \"./%s\"'", file);
	for (i in 1:N) System(cmd);
}

#
#	<p> Sweave replacement
#

.REP.standardTemplate = '\\input{SETUP}
\\begin{document}
TEMPLATE_MAIN
\\end{document}
';

# REP.plot('Tag', Qplot(rate, geom = 'histogram', xlab = 'heterocygosity', file = 'dest'));
# REP.plot('Tag', Qplot(sample = ps, dist = qunif, file = 'results/qc-markers-hweQQ.jpg'));

Qplot_defaults = list(
	width = 5, height = 5, dpi = 150,
	dimx = c(0, 1), dimy = c(0, 100)
);

Qplot = function(..., file = NULL, pp = Qplot_defaults, theme = theme_bw()) {
	pp = merge.lists(Qplot_defaults, pp);
	args = list(...);
	geom = firstDef(args$geom, 'default');
	# <b> workaround for QQ-plot instead of the expected qplot(...)
	p = if (any(class(args[[1]]) == 'ggplot')) {
		plot = args[[1]];
		
	} else if (
		# histogram
		(all(is.na(args[[1]])) && geom == 'histogram')
		# xy-plot
		|| (all(is.na(args[[1]]) | is.na(args[[2]])))) {
		ggplot(data = data.frame()) + geom_point() +
			xlim(pp$dimx[1], pp$dimx[2]) +
			ylim(pp$dimy[1], pp$dimx[2]);
	} else do.call(qplot, list(...));
	ggsave(p, file = file, width = pp$width, height = pp$height, dpi = pp$dpi);
	file
}
GGplot = function(p, file = NULL, pp = list(width = 5, height = 5, dpi = 150)) {
	ggsave(p, file = file, width = pp$width, height = pp$height, dpi = pp$dpi, encoding = 'AdobeStd');
	file
}
PlotDefaults = list(
	pdf = list(width = 6, height = 6),
	jpeg = list(width = 2048, height = 2048)
);
Plot = function(..., file = NULL, .plotType = 'pdf', o = NULL, f = NULL) {
	if (is.null(file)) file = tempFileName('reporter', .plotType);
	device = get(.plotType);
	plotFunction = firstDef(f, plot);
	o = merge.lists(PlotDefaults[[.plotType]], o);
	do.call(device, c(list(file = file), o));
		do.call(plotFunction, list(...));
	dev.off();
	file
}

.REP.extractFromTemplates = function(templates, re = '(?s)(?<=TEMPLATE_BEGIN).*?(?=TEMPLATE_END)',
	locations = c('.', sprintf('%s/src/Rscripts', Sys.getenv('HOME')))) {
	nst = names(templates);

	# <p> set empty template names
	if (is.null(nst)) nst = rep('', length(templates));
	nst[nst == ''] = paste('TEMPL_', 1:sum(nst == ''), sep = '');

	# <p> parse template definitions
	ts = lapply(1:length(templates), function(i) {
		# raw read templates
		templ = readFile(templates[[i]], prefixes = locations);
		tsRaw = fetchRegexpr(re, templ);
		# inline templates
		r = if (length(tsRaw) != 0) {
			ns = sapplyn(tsRaw, function(e)fetchRegexpr('(?<=^:).*?(?=\\n)', e, globally = F));
			# colon, new-line
			ts = sapply(1:length(ns), function(i)substr(tsRaw[i], nchar(ns[i]) + 3, nchar(tsRaw[i])));
			listKeyValue(ns, ts);
		} else {
			listKeyValue(nst[i], templ);
		}
		r
	});
	#r = unlist.n(ts, 1);
	r = merge.lists(ts, listOfLists = T);
	r
}

.REP.getTemplates = function(templates, locations = c('.', sprintf('%s/src/Rscripts', Sys.getenv('HOME')))) {
	nst = names(templates);

	# <p> set empty template names
	if (is.null(nst)) nst = rep('', length(templates));
	nst[nst == ''] = paste('TEMPL_', 1:sum(nst == ''), sep = '');

	# <p> parse template definitions
	ts = lapply(1:length(templates), function(i) {
		# raw read templates
		templ = readFile(templates[[i]], prefixes = locations);
		tsRaw = fetchRegexpr('(?s)(?<=TEMPLATE_BEGIN).*?(?=TEMPLATE_END)', templ);
		# inline templates
		r = if (length(tsRaw) != 0) {
			ns = sapplyn(tsRaw, function(e)fetchRegexpr('(?<=^:).*?(?=\\n)', e, globally = F));
			# colon, new-line
			ts = sapply(1:length(ns), function(i)substr(tsRaw[i], nchar(ns[i]) + 3, nchar(tsRaw[i])));
			listKeyValue(ns, ts);
		} else {
			listKeyValue(nst[i], templ);
		}
		r
	});
	#r = unlist.n(ts, 1);
	r = merge.lists(ts, listOfLists = T);
	# backward compatibility: determine wether default template should be used
	if (length(r) > 0) {
		if (names(r)[1] != 'TEMPL_1') {	# expect full document template tb specified otherwise
			# interpolate first template into standard template
			r[[1]] = mergeDictToString(list(TEMPLATE_MAIN = r[[1]]), .REP.standardTemplate);
		}
	}
	r
}

.REP.getPatterns = function(templates) {
	.REP.extractFromTemplates(templates, '(?s)(?<=KEY_BEGIN).*?(?=KEY_END)');
}

.REP.defaultParameters = list(
	copy.files = 'setup.tex',
	setup = 'setup.tex',
	latex = 'pdflatex',
	useDefaultTemplate = T
);
# create new, global reporter
REP.new = function(templates = NULL, cache = NULL, parameters = list(), resetCache = F,
	latex = 'pdflatex', setup = 'setup.tex') {
	copy.files = merge.lists(.REP.defaultParameters['copy.files'], list(copy.files = setup), concat = TRUE);
	parameters = merge.lists(.REP.defaultParameters,
		parameters,
		list(latex = latex, setup = setup),
		copy.files,
	concat = FALSE);
	if (!is.null(cache) && file.exists(cache) && !resetCache) {
		REP.tex('SETUP', setup);
		REP.setParameters(parameters);
		load(file = cache, envir = .GlobalEnv);
	} else {
		templatePathes = c(as.list(templates), parameters$subTemplates);
		ts = .REP.getTemplates(templatePathes);
		ps = merge.lists(
			list(SETUP = setup),
			.REP.getPatterns(templatePathes)
		);
		mainPath = splitPath(as.vector(templates)[1]);
		assign('.REPORTER.ITEMS', list(
			# list of named templates
			templates = ts,
			# patterns to be interpolated
			patterns = ps,
			# housekeeping: tags for consecutively reported subtemplates
			templateTags = list(),
			# parameters passed in
			parameters = parameters,
			# path to the cache file
			cache = cache,
			# create default output name
			output = sprintf('%s.pdf', mainPath$fullbase),
			# name of the template to be used for the global, final document
			mainTemplate = names(ts)[1],
			templatePathes = templatePathes,
			# conditionals
			conditionals = list()
			), pos = .GlobalEnv
		);
	}
	NULL
}
REP.refreshTemplates = function(templates) {
	if (!exists('.REPORTER.ITEMS')) return();
	templatePathes = templates;
	ts = .REP.getTemplates(as.list(templates));
	ps = .REP.getPatterns(templatePathes);
	.REPORTER.ITEMS$templates = ts;
	.REPORTER.ITEMS$mainTemplate = names(ts)[1];
	.REPORTER.ITEMS$templatePathes = templatePathes;
	.REPORTER.ITEMS$patterns = merge.lists(.REPORTER.ITEMS$patterns, ps);
	assign('.REPORTER.ITEMS', .REPORTER.ITEMS, pos = .GlobalEnv);
	REP.save();
}
REP.save = function() {
	if (!is.null(.REPORTER.ITEMS$cache)) {
		dir = splitPath(.REPORTER.ITEMS$cache)$dir;
		if (!file.exists(dir)) dir.create(dir, recursive = T);
		save(.REPORTER.ITEMS, file = .REPORTER.ITEMS$cache);
	}
	NULL
}

REP.setParameters = function(parameters = .REP.defaultParameters) {
	.REPORTER.ITEMS$parameters = merge.lists(.REP.defaultParameters, parameters);
	assign('.REPORTER.ITEMS', .REPORTER.ITEMS, pos = .GlobalEnv);
	REP.save();
}
REP.unreport = function(keys) {
	l = get('.REPORTER.ITEMS', pos = .GlobalEnv);
	idcs = which.indeces(keys, names(l$patterns));
	if (!length(idcs)) return(NULL);
	l$patterns = l$patterns[-idcs];
	assign('.REPORTER.ITEMS', l, pos = .GlobalEnv);
	REP.save();
}
setREPentry = function(key, value) {
	if (!exists('.REPORTER.ITEMS')) assign('.REPORTER.ITEMS', list(), pos = .GlobalEnv);
	l = get('.REPORTER.ITEMS', pos = .GlobalEnv);
	l$patterns[[key]] = value;
	assign('.REPORTER.ITEMS', l, pos = .GlobalEnv);
	REP.save();
}
setRI = function(ri)assign('.REPORTER.ITEMS', ri, pos = .GlobalEnv);

REP.setConditional = function(name, v) {
	l = get('.REPORTER.ITEMS', pos = .GlobalEnv);
	if (is.null(l$conditionals)) l$conditionals = list();
	l$conditionals[[name]] = v;
	assign('.REPORTER.ITEMS', l, pos = .GlobalEnv);
	REP.save();
}

REP.get = function(name) {
	get('.REPORTER.ITEMS', envir = .GlobalEnv)$patterns[[name]];
}

outputOf = function(code, print = T, envir = parent.frame()) {
	tempFile = tempFileName('reporter', inRtmp = T);
	sink(tempFile);
		if (print) print(eval(code, envir = envir)) else eval(code, envir = envir);
	sink();
	output = readFile(tempFile);
	output
}
expression2str = function(exp, removeBraces = T) {
	strs = deparse(exp);
	if (removeBraces) strs = strs[2:(length(strs) - 1)];
	sprintf("%s\n", join(strs, "\n"))
}

codeRepresentation = function(code) {
	if (is.character(code)) {
		codeExp = parse(text = code);
		codeText = gsub('^\n?(.*)', '\\1', code);	# remove leading \n
	} else {
		codeExp = code;
		codeText = expression2str(code);
	}
	r = list(code = codeExp, text = codeText);
	r
}

REP.format.sci = function(s, digits = 1) {
	e = floor(log10(as.numeric(s)));
	m = as.numeric(s) * 10^(-e);
	if (round(m, digits) == 1) {
		sprintf("$10^{%d}$", e)
	} else {
		sprintf("$%.*f \\times 10^{%d}$", digits, m, e)
	}
}

REP.formats = list(
	small = function(s)sprintf("{\n\\small %s\n}", s),
	tiny = function(s)sprintf("{\n\\tiny %s\n}", s),
 	percent = function(s)sprintf("%.1f", 100 * as.numeric(s)),
  	`.1` = function(s)sprintf("%.1f", as.numeric(s)), 
	`.2` = function(s)sprintf("%.2f", as.numeric(s)), 
 	`.3` = function(s)sprintf("%.3f", as.numeric(s)), 
 	`.4` = function(s)sprintf("%.4f", as.numeric(s)), 
 	sci0 = function(s) REP.format.sci(s, 0), 
 	sci1 = function(s) REP.format.sci(s, 1), 
 	sci2 = function(s) REP.format.sci(s, 2), 
	file = function(f) {
		ri = .REPORTER.ITEMS;
		# due to caching choose a persistent location <!> uniqueness
		tdir = sprintf('/tmp/%s/Rpreporting/%s', Sys.getenv('USER'), names(ri$templates)[1]);
		if (!file.exists(tdir)) dir.create(tdir, recursive = T);
		tf = sprintf('%s/%s', tdir, splitPath(f)$file);
		unlink(tf);	# overwrite previous version
		# <!> expect relative filename, spaces in file name not eliminated
		file.symlink(sprintf('%s/%s', getwd(), f), tf);
		tf
	}
);

REP.tex = function(name, str, print = T, quote = F, fmt = NULL) {
	if (!is.null(fmt) && !is.na(fmt)) {
		str = if (is.null(REP.formats[[fmt]])) sprintf(fmt, str) else REP.formats[[fmt]](str);
	}
	if (quote) {	#<i> use backend quoting
		#str = gsub('_', '\\\\_', str, perl = T);	# replace _
		str = latex$quote(str);
	}
	setREPentry(sprintf('%s', name), str);
	str
}
REP.texq = function(name, str, print = T, quote = T, fmt = NULL)REP.tex(name, str, print, quote, fmt)
REP.vector = function(name, v, print = T, quote = T, typewriter = T, sep = ', ', max = 50) {
	if (max > 0) v = v[1:min(max, length(v))];
	if (typewriter) {
		v = sapply(v, function(s)sprintf('\\texttt{%s}', s));
	}
	REP.tex(name, sprintf('%s%s', join(v, sep), ifelse(length(v) > max, '...', '')), quote = quote);
}

REP = function(name, code, print = T, execute = T, envir = parent.frame()) {
	c = codeRepresentation(as.list(sys.call())[[3]]);
	setREPentry(sprintf('%s_code', name), c$text);
	if (execute) {
		output = outputOf(c$code, envir = envir);
		setREPentry(sprintf('%s_out', name), output);
		if (print) cat(output);
	}
	NULL
}
REP.plotDefaultOptions = list(width = 5, height = 5, dpi = 150);
REP.plot = function(name, code, ..., file = NULL, type = 'pdf', envir = parent.frame(),
	options = list(), copyToTmp = F) {
	#c = codeRepresentation(as.list(sys.call())[[3]]);
	c = codeRepresentation(sys.call()[[3]]);	# as of version R 3.0.1
	if (is.null(file)) file = tempFileName('reporter', 'pdf', inRtmp = T);
	if (type == 'ggplot') {
		o = merge.lists(REP.plotDefaultOptions, options, list(...));
		with(o, { ggsave(code, file = file, width = width, height = height, dpi = dpi) });
	} else if (is.character(code)) {
		file = code;
	} else {
		device = get(type);
		device(file, ...);
			eval(c$code, envir = envir);
		dev.off();
	}
	pathToFile = path.absolute(file);
	if (copyToTmp) {
		fileTmp = tempFileName('reporter', splitPath(pathToFile)$ext, inRtmp = T);
		file.copy(pathToFile, fileTmp, overwrite = T);
		pathToFile = fileTmp;
	}
	if (file.info(pathToFile)$size == 0) {
		pathToFile = '';
	}
	setREPentry(sprintf('%s_plot', name), pathToFile);
	setREPentry(sprintf('%s_code', name), c$text);
	NULL
}

# tag allows to search for overloading templates (_tag). This can be used in reportSubTemplate to
#	conditionally report templates
.REP.interpolateTemplate = function(templName, conditionals = list(), tag = NULL) {
	ri = .REPORTER.ITEMS;
	if (!is.null(tag) && !is.null(ri$templates[[sprintf('%s_%s', templName, tag)]]))
		templName = sprintf('%s_%s', templName, tag);
	s = ri$templates[[templName]]
	#s = readFile(tpath);
	s = mergeDictToString(.REPORTER.ITEMS$patterns, s, iterative = T);

	lengths = sapply(names(conditionals), nchar);
	for (n in names(conditionals)[rev(order(lengths))]) {
		s = gsub(sprintf('IF_%s(.*?)END_IF', n), if (conditionals[[n]]) '\\1' else '', s);
	}
	s
}

# initialize a series of reportSubTemplate calls followed by a finalizeSubTemplate call
REP.reportSubTemplateInitialize = function(subTemplate) {
	patterns = .REPORTER.ITEMS$patterns;
	subPatterns = sprintf('TEMPLATE:%s:subTemplates', subTemplate);
	REP.unreport(subPatterns);
}

REP.reportSubTemplate = function(subTemplate, tag = NULL, conditionals = list()) {
	ri = .REPORTER.ITEMS;
	# tag
	if (is.null(tag)) {
		tt = ri$templateTags;
		tag = ri$templateTags[[subTemplate]] =
			ifelse (is.null(tt[[subTemplate]]), 0, tt[[subTemplate]]) + 1;
		setRI(ri);
	}
	# finalize subTemplates
	patterns = ri$patterns;
	subPattern = sprintf('TEMPLATE:%s_%s', subTemplate, as.character(tag));
	subPatterns = sprintf('TEMPLATE:%s:subTemplates', subTemplate);

	# set own entry
	setREPentry(subPattern, .REP.interpolateTemplate(subTemplate, tag = tag));

	# collect all subTemplates
#	for (st in names(ri$parameters$subTemplates)) {
#		i = which.indeces(sprintf('TEMPLATE:%s_.*', st), names(.REPORTER.ITEMS$patterns), regex = T);
#		setREPentry(sprintf('TEMPLATE:%s:subTemplates', st), join(unlist(names(patterns[i])), "\n"));
#	}
	#i = which.indeces(sprintf('TEMPLATE:%s_.*', subTemplate), names(.REPORTER.ITEMS$patterns), regex = T);
	# append new element
	setREPentry(subPatterns, join(c(patterns[[subPatterns]], subPattern), "\n"));
	
	REP.save();
}

REP.finalizeSubTemplate = function(subTemplate) {
	# finalize subTemplates
	patterns = .REPORTER.ITEMS$patterns;
	subPatterns = sprintf('TEMPLATE:%s:subTemplates', subTemplate);

	text = mergeDictToString(patterns, patterns[[subPatterns]], iterative = T);
	setREPentry(sprintf('TEMPLATE:%s', subTemplate), text);
	# remove trail
	if (is.null(subPatterns)) return(NULL);
	subPattern = splitString("\n", .REPORTER.ITEMS$patterns[[subPatterns]]);
	#print(c(subPatterns, subPattern));

	REP.unreport(c(subPatterns, subPattern));

	REP.save();
}

REP.finalize = function(conditionals = list(), verbose = FALSE, cycles = 1, output = NULL) {
	# <p> vars
	ri = .REPORTER.ITEMS;
	
	# <p> prepare
	dir = tempFileName('rreporter', inRtmp = T);
	file.remove(dir);
	dir.create(dir);
	# <!> assume relative pathes
	for (cpath in .REPORTER.ITEMS$parameters$copy.files) {
		if (splitPath(cpath)$isAbsolute) {
			dest = sprintf('%s/%s', dir, splitPath(cpath)$file);
			Log(sprintf('Reporting: symlinking %s -> %s', cpath, dest), 4);
			file.symlink(cpath, dest);
		} else {
			for (sdir in c('', getwd(), sapply(ri$templatePathes, function(tp)splitPath(tp)$dir))) {
				source = sprintf('%s/%s/%s', getwd(), sdir, cpath);
				Log(sprintf('Reporting: dir %s', sdir), 4);
				if (file.exists(source)) {
					dest = sprintf('%s/%s', dir, cpath);
					Log(sprintf('Reporting: symlinking %s -> %s', source, dest), 4);
					file.symlink(source, dest);
					break;
				}
			}
		}
	}

	# <p> create final document
	tn = names(ri$templates)[1];
	allConditionals = merge.lists(ri$conditionals, conditionals);
	s = .REP.interpolateTemplate(ri$mainTemplate, allConditionals);

	# <p> run latex to produce temp file
	tmpPath = sprintf('%s/%s.tex', dir, tn);
	writeFile(tmpPath, s);
	Log(readFile(tmpPath), 5)
	latexCmd = firstDef(ri$parameters$latex, 'pdflatex');
	for (i in 1:cycles) {
		r = System(Sprintf('cd %{dir}s ; %{latexCmd}s -interaction=nonstopmode \"%{tn}s\"'),
			4, return.output = T);
		if (r$error > 0) Log(Sprintf("%{latexCmd}s exited with error."), 1);
		if (r$error > 0 || (verbose && i == 1)) Log(r$output, 1);
		#if (r$error > 0) break;
	}

	# <p> output
	postfix = join(names(conditionals[unlist(conditionals)]), '-');
	if (postfix != '') postfix = sprintf('-%s', postfix);
	#fileOut = sprintf('%s%s%s.pdf', splitPath(tpath)$base, if (postfix == '') '' else '-', postfix);
	#fileOut = sprintf('%s%s%s.pdf', tn, if (postfix == '') '' else '-', postfix);
	if (is.null(output))
		output = if (exists('.globalOutput'))
			.fn(sprintf('%s%s', splitPath(ri$output)$base, postfix), 'pdf') else ri$output;
	Log(sprintf('Writing to output %s', output), 4);
	
	file.copy(sprintf('%s.pdf', splitPath(tmpPath)$fullbase), output, overwrite = T);
	file.copy(sprintf('%s.tex', splitPath(tmpPath)$fullbase),
		sprintf('%s.tex', splitPath(output)$fullbase), overwrite = T);
}

#
#	<p> helpers
#

REP.reportFigureTable = function(nameTag, namesPlots, cols = 2, captions = NULL) {
	namesPlots = sapply(namesPlots, function(p) {
		path = if ('ggplot' %in% class(p)) {
			path = tempfile(fileext = '.pdf');
			ggsave(path, plot = p);
			path
		} else p;
		path
	});
	figureTable = report.figure.table(namesPlots, cols = cols, captions = captions);
	REP.tex(nameTag, figureTable);
}

#
#	Example code
#

# #	refresh only valid after a REP.new call
#	REP.refreshTemplates('gwas/reportGwas.tex')
# 	REP.new(
# 		'gwas/reportGwas.tex',
# 		cache = sprintf('%s/reportGWAS_cache', outputDir),
# 		resetCache = resetCache
# 	);
# # reporting
# 	REP.tex('G:DESCRIPTION', firstDef(o$studyDescription, ''));
# 	REP.tex('G:ROUNDNAME', firstDef(o$runName, 'unnamed'));
#	REP.finalize(verbose = T, output = sprintf('%s/reportGwas-%s.pdf', outputDir, o$runName), cycles = 3);

# # reporting patterns
# 	REP.tex('ASS:TABLE', report.data.frame.toString(
# 		psTop,
# 		digits = c(rep(NA, length(varsMap)), '#2', rep(2, length(Evars)), '#2', 2),
# 		names.as = rep.names, quoteHeader = F,
# 		caption = caption
# 	), fmt = 'tiny');
#	REP.tex('ASS:QQ:INFLATION', inflation, fmt = '.2');
# 	REP.plot('ASS:QQ:ASSOCIATION', Qplot(sample = ps$P, dist = qunif,
# 		file = sprintf('%s/ass-QQ-%s.jpg', outputDir, tag2fn(tag))));
#	REP.tex('QC:SAMPLE:MDS:Outlier', fraction(qcMdsOutliers), fmt = 'percent');
#
# # sub-templates
# 	REP.reportSubTemplateInitialize('association');
# 	for (m in expandedModels$models) with(m, {
#		REP.tex('ABC', 2);
#		REP.reportSubTemplate('association', tag);
# 	});
# 	REP.finalizeSubTemplate('association');

#
#	Rfunctions.R
#Tue 14 Aug 2007 01:39:42 PM CEST 

#
#	<§> abstract data functions
#

inverse = function(f, interval = c(-Inf, Inf)) {
	Vectorize(
	function(y, ...) {
		optimize(function(x, ...){ (y - f(x, ...))^2 }, interval = interval, ...)$minimum
	}
	)
}

#
#	<p> meta functions
#

callWithArgs = function(fctName, args) {
	#arguments = paste(sapply(names(args), function(n)sprintf("%s = %s", n, args[[n]])), collapse = ", ");
	fhead = sprintf("%s(%s)", fctName, paste(names(args), collapse = ", "));
	eval(parse(text = fhead))
}

.do.call = function(f, args, restrictArgs = T, usePositional = T, restrictPositional = F) {
	# <p> function arguments
	fargs = names(as.list(args(f)));
	# remove spurious arguments
	fargs = fargs[fargs != ''];

	if (restrictArgs && all(fargs != '...')) {
		idcs = which.indeces(fargs, names(args));
		if (usePositional) {
			positional = which(names(args) == '');
			Npositional = (length(fargs) - length(idcs));
			if (!restrictPositional && length(positional) > Npositional)
				stop(".do.call: unmachted positional arguments");
			idcs = c(idcs, positional[1:Npositional]);
		}
		args = args[sort(idcs)];
	}
	do.call(f, args)
}

callDelegate = function(functionBase, delegation, args, restrictArgs = T) {
	f = get(Sprintf('%{functionBase}s%{delegation}u'));
	.do.call(f, args, restrictArgs = restrictArgs)
}

#
#	<p> generic functions
#

Identity = function(...)list(...)

#
#	<p> benchmarking
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
#
#	Rstatistic.R
#Fri 19 Jan 2007 11:06:44 PM CET 

# contains simple statistics to evaluate consulting questions

sizesDesc = function(s) {
	col.frame(list(
		mean = mean(s),
		median = median(s),
		stddev = sqrt(var(s)),
		quantiles = quantile(s)
	), do.paste = " ", digits = 1)
}

compareSamples = function(l) {
	desc = data.frame(lapply(l, function(e)sizesDesc(e)));
	print(desc);

	tests = col.frame(list(
		test.t = t.test(l[[1]], l[[2]])$p.value,
		test.wilcoxon = wilcox.test(l[[1]], l[[2]])$p.value
	));
	print(tests);
}

df2numeric = function(df) apply(df, 2, function(col)as.numeric(as.vector(col)));
expandCounts = function(tab)  unlist(apply(tab, 1, function(r){rep(r[1], r[2])}));

chisq.test.robust = function(tab, bootstrapCellCount = 5, B = 5e3) {
	# reduce table by removing 0-marginals and check for degeneration
	tab = tab[, !apply(tab, 2, function(c)all(c == 0))];
	if (is.vector(tab)) return(list(p.value = NA));
	tab = tab[!apply(tab, 1, function(r)all(r == 0)), ];
	if (is.vector(tab)) return(list(p.value = NA));
	# determine whether to bootstrap
	r = if (any(tab < bootstrapCellCount))
		chisq.test(tab, simulate.p.value = T, B = B) else
		chisq.test(tab);
	r
}

# depends on coin package <!>, unfinished
armitage.test.robust = function(formula, df, scores) {
	tab = table(df);
	# only eliminate 0-rows of table from score vector
	zRows = sapply(1:dim(tab)[1], function(i){ all(tab[i,] == 0) });
	scores[[1]] = scores[[1]][!zRows];
	r =	independence_test(formula, df, teststat = "quad", scores = scores);
	r
}

# simulations in project 2014-02-Microsatellites
logSumExpRaw = function(v, pivot = median(v))(log(sum(exp(v - pivot))) + pivot)
logSumExpPivot = logSumExpMax = function(v)logSumExpRaw(v, pivot = max(v))
logSumExp = function(x) {
	Imx = which.max(x);
	log1p(sum(exp(x[-Imx] - x[Imx]))) + x[Imx]
}
   
# rejFrac = function(x, alpha = 0.05) {
# 	x = na.omit(x);
# 	f = count(x <= alpha) / length(x);
# 	f
# }
rejFrac = function(x, alpha = 0.05)mean(x <= alpha, na.rm = T);
vector.std = function(v, C = 1)(C * v / sum(v));
vector.std.log = function(v, C = 0)(v - (logSumExp(v) - C));

#
#	<p> ml methods
#

lhWrapperFunctions = c("initialize",
	"parsScale", "parsMap", "parsMapInv", "parsStart", "parsNames", "lh", "null2alt", "alt2null"
);

# <!> transition to S4-objects
lhGetWrapper = function(prefix, self, ...) {
	createNullWrapper = F;
	f = list();
	if (substr(prefix, nchar(prefix) - 3, nchar(prefix)) == "null") {
		createNullWrapper = T;
		prefix = substr(prefix, 1, nchar(prefix) - 5);
	}
	for (n in lhWrapperFunctions) {
		f[[n]] = mget(sprintf("%s.%s", prefix, n), envir = globalenv(), ifnotfound=list(NULL))[[1]];
	}
	f$self = if (is.null(self)) { if (is.null(f$initialize)) list(...) else f$initialize(...) } else self;
	if (createNullWrapper) {
		f1 = f;
		self = f1$self = f$self;
		f1$parsStart = function(self){ f$alt2null(self, f$parsStart(self)) };
		f1$parsScale = function(self){ f$alt2null(self, f$parsScale(self)) };
		f1$parsMap = function(self, p){ f$alt2null(self, f$parsMap(self, f$null2alt(self, p))) };
		f1$parsMapInv = function(self, p){ f$alt2null(self, f$parsMapInv(self, f$null2alt(self, p))) };
		f1$lh = function(self){ lhRaw = f$lh(self); function(p)lhRaw(f$null2alt(self, p)) };
		return(f1);
	}
	f
}
lhCopyWrapper = function(name, template) {
	for (f in lhWrapperFunctions) {
		g = mget(sprintf("%s.%s", template, f), envir = globalenv(), ifnotfound=list(NULL))[[1]];
		if (!is.null(g)) eval.parent(parse(text = sprintf("%s.%s = %s.%s;", name, f, template, f)));
	}
}

lhInit = function(lhWrapper) {
	
}

mapU = function(y){ -log(1/y - 1) }
map2U = function(z){ 1/(1 + exp(-z)) }
# one-dimensional estimation
lhMlEstimatorOD = function(lhWrapper = NULL, start = NULL, c = NULL, ...) {
	if (is.null(c)) c = list(tol = .Machine$double.eps^0.25);
	f = lhGetWrapper(lhWrapper, c$self, ...);

	lhRaw = f$lh(f$self);
	lh = function(p) { lhRaw(mapU(f$parsMap(f$self, p))) }
	o = try(optimize(lh, lower = 0, upper = 1, tol = c$tol, maximum = T));
	r = list(par = mapU(f$parsMap(f$self, o$maximum)), par.os = o$maximum, value = o$objective);
	r
}

# multi-dimensional estimation
lhMlEstimatorMD = function(lhWrapper = NULL, start = NULL, c = NULL, ...) {
	if (is.null(c)) c = list(do.sann = F, sann.cycles = 1000);
	f = lhGetWrapper(lhWrapper, c$self, ...);
	eps = 1e-5;
	#if (!is.null(start)) { starts = matrix(start, nrow = 1); }
	if (is.null(start)) start = f$parsStart(f$self);

	starts = if (!is.matrix(start)) matrix(as.numeric(unlist(start)), nrow = 1) else start;
	parscale = f$parsScale(f$self);
	lhRaw = f$lh(f$self);
	lh = function(p) { lhRaw(f$parsMap(f$self, p)) }
	os = apply(starts, 1, function(s) {
		s = f$parsMapInv(f$self, s);
		o = try(optim(s, lh, method = "Nelder-Mead",
			control = list(fnscale = -1, parscale = parscale, maxit = 1000),
		));
		if (class(o) == "try-error") return(NA);
		if (0) { # if (o$convergence > 0 || c$do.sann) {	# Nelder-Mead failed to converged
		o1 = try(optim(s, lh, method = "SANN",
			control = list(fnscale = -1, parscale = parscale, maxit = c$sann.cycles),
		));
		#if (class(o1) == "try-error") return(NA);
		if (o$convergence > 0 || o1$value > o$value) o = o1;
		}
		o$par.os = o$par;	# parameter values in optimiztation space
		o$par = f$parsMap(f$self, o$par);
		o
	});

	if (all(is.na(os))) return(NA);
	vs = sapply(os, function(o){o$value});
	arg.max = which.max(vs);
	estimate = os[[arg.max[1]]];
	fisher = list();
	#if (!is.null(c$computeFisher) & c$computeFisher)
	if (!is.null(c$computeFisher)) fisher = estimate.fisher(d, estimate, fisher.eps = 1e-1);
	r = c(estimate, fisher);
	r
}

lhMlEstimator = function(lhWrapper = NULL, start = NULL, c = NULL, ...) {
	f = lhGetWrapper(lhWrapper, c$self, ...);
	r = if (length(f$parsStart(f$self)) > 1) {
		lhMlEstimatorMD(lhWrapper, start, c, ...);
	} else if (length(f$parsStart(f$self)) == 1) {
		lhMlEstimatorOD(lhWrapper, start, c, ...);
	} else { # null hypothesis w/o nuisance parameters
		r = f$lh(f$self)();
	}
	r
}


lhLRtest = function(lhWrapper = NULL, start = NULL, c = list(do.sann = F, sann.cycles = 1000), ...) {
	f = lhGetWrapper(lhWrapper, NULL, c$self, ...);	# f$self is likelihood object and absorbs ellipsis parameters
	self = f$self;
	if (is.null(start)) start = f$parsStart(self);

	startNull = if (is.matrix(start))
		t(apply(start, 1, function(r)f$alt2null(self, r))) else
		f$alt2null(self, start);

	e.null = lhMlEstimator(sprintf("%s.%s", lhWrapper, "null"), startNull, c(c, list(self = self)));

	start = rbind(start, f$null2alt(self, e.null$par));
	e.alt = lhMlEstimator(lhWrapper, start, c(c, list(self = self)));

	# <p> calcualte degrees of freedom
	st = f$parsStart(self);
	df = length(st) - length(f$alt2null(self, st));
	stat =  2 * (e.alt$value - e.null$value);
	list(ll.null = e.null$value, ll.alt = e.alt$value,
		test.stat = stat, p = 1 - pchisq(stat, df), df = df, par.null = e.null$par, par.alt = e.alt$par
	)
}

#
#	lh-functions based on likelihood specification
#

# Example: see dataAnalysis.R in hwe project
# Example: binomial distribution
# lhBin = function(p, k, N)dbinom(k, N, p)
# spec_lhBin = list(
# 	ll = "lhBin",
# 	alt = list(
# 		start = c(.5),	# also specifies number of parameters
# 		pars = list(list(name = "rho", type = "freq"))
# 	),
# 	null = list(
# 		start = c(.5),	# assume same likelihood and therefore #pars from alternative
# 		parsFree = 0	# alternative: list free parameters or specify tail from alt
# 	)
# );
# r = lhMl(spec_lhBin)

#define a function
toF = function(expr, args, env = parent.frame()) {
	as.function(c(args, expr), env)
}

logitI = expit = function(x, min = 0, max = 1) { (max - min)/(1 + exp(-x)) + min }
expitD = toF(D(expression((max - min)/(1 + exp(-x)) + min), 'x'), list(x = NULL, min = 0, max = 1));
logit = function(x, min = 0, max = 1) { log((x - min)/(max - x)) }
logitD = toF(D(expression(log((x - min)/(max - x))), 'x'), list(x = NULL, min = 0, max = 1));
# templates assuming X as argument, p as parameter description list
lhArgMappers = list(
	freq =		"expit(X)",
	int =		"expit(X, min, max)",
	real =		"X",
	positive =	"log1p(exp(X))"
);
lhArgMappersD = list(
	freq =		NULL, #D(expression(expit(x), 'x')),
	int =		"expit(X, min, max)",
	real =		"X",
	positive =	"log1p(exp(X))"
);
lhArgMappersI = list(
	freq =	"logit(X)",
	int =	"logit(X, min, max)",
	real =	"X",
	positive =	"log(expm1(X))"
);
lhSpecificationDefaults = list(
	# embed null-parameter into alt-parameter space: variables: npars, parsFree, s (specification),
	#	p: input parameters
	#	<i>: optimization: substitute literals from start
	default = list(mapper = 'c(c(ARGS_FREE), c(ARGS_BOUND))', mapperInv = 'c(ARGS_FREE)')
);
# richest: richest parametrization of the likelihood
# lhInterface: call the likelihood function with a vector (vector) or with separate arguments formula
#	the paramters (inline)
lhSpecificationDefault = list(richest = 'alt', lhInterface = 'vector');
lhSpecificationInterfaces = list(
	vector = 'function(p, ...) { pm = mapper(p); if (any(abs(pm) > 1e10)) return(-Inf); lf(pm, ...) }',
	inline = 'function(p, ...) { pm = mapper(p); if (any(abs(pm) > 1e10)) return(-Inf); lf(ARGS_INLINE, ...) }'
);

#
#	<p> logit derivatives
#simulations in 2014-07-Borstkanker/src/borstKankerExp.R
logExpit1 = function(x)log(expit(x))
logExpit = logExpit2 = function(x)-log1p(exp(-x))
logitExp1 = function(x)logit(exp(x))
logitExp = logitExp2 = function(x)-log(expm1(-x))

logExpit1m1 = function(x)log(1 - expit(x))
logExpit1m = logExpit1m2 = function(x)-log1p(exp(x))
logit1mExp1 = function(x)logit(1 - exp(x))
logit1mExp = logit1mExp2 = function(x)log(expm1(-x))


#
#	<p> helper functions
#

# mappers for individual parameters
# ps: list of parameters
# mappers: mapper templates to used
# target: name of variable on which to apply
# idcs: indeces to iterate
lhMapperPars = function(ps, mappers, target = 'p', idcs = 1:length(ps)) {
	maps = if (length(idcs) == 0) c() else sapply(idcs, function(i) {
		p = ps[[i]];
		a = gsub("X", sprintf("%s[%s]", target,
			deparse(if (length(p$entries)) p$entries else i)), mappers[[p$type]]);
		a = mergeDictToString(ps[[i]]$args, a);
		a
	});
	r = paste(maps, collapse = ", ");
	r
}

# <!> auto inverse mapping has to heed mapperPost time of application
# mappers map individual arguments, mapper sub-sequently maps the whole vector
lhMapperFunction = function(s, mappers, mapper) {
	free = 1:s$parsFree;	# idcs of free variables
	bound = if(s$parsFree < s$npars) (s$parsFree + 1):s$npars else c(); # idcs of bound variables
	mStr = sprintf('function(p){%s}',
		mergeDictToString(list(
			ARGS_FREE = lhMapperPars(s$pars, mappers, 'p', free),
			ARGS_BOUND = lhMapperPars(s$pars, mappers, 'start', bound)
	), mapper));
	mf = with(s, eval(parse(text = mStr)));
	mf
}

lhMapperFunctions = function(s) {
	r = list(
		mapper = lhMapperFunction(s, lhArgMappers, s$mapper),
		mapperInv = lhMapperFunction(s, lhArgMappersI, s$mapperInv)
	);
	r
}

#' Build wrapper function around likelihood
#'
#' @param template parameter specification used as template (usually richest parametrization tb reduced
#'	for other hypotheses)
lhPreparePars = function(pars, defaults = lhSpecificationDefaults$default, spec = lhSpecificationDefault,
	template = pars) {
	# <p> determine free parameters
	t = merge.lists(defaults, pars);
	npars = length(template$pars);
	if (!is.null(t$parsFree)) {
		t$pars = if(t$parsFree == 0) list() else template$pars[(npars - t$parsFree): npars];
	}
	if (is.null(t$start)) t$start = template$start;
	if (is.null(t$parsFree)) t$parsFree = length(t$pars);

	# <p> construct mapped likelihood function
	fs = mergeDictToString(
		list(ARGS_INLINE =
			paste(sapply(1:npars, function(i) { sprintf("pm[%s]",
				deparse(if (length(template$pars[[i]]$entries)) template$pars[[i]]$entries else i)) }
			), collapse = ', ')),
		lhSpecificationInterfaces[[spec$lhInterface]]
	);
	t = merge.lists(t, list(npars = npars));
	t = merge.lists(t, lhMapperFunctions(t), list(lf = get(spec$ll)));
	f = with(t, eval(parse(text = fs)));
	t = merge.lists(t, list(npars = npars, lh = f));
	t
}

# types: names of specifications for which to define wrapped functions
# richest: name of specification for model that includes a superset of parameters of all other types
lhPrepare = function(s, types = c('null', 'alt')) {
	# <p> preparation
	s = merge.lists(lhSpecificationDefault, s);
	ri = s[[s$richest]];
	# number of parameter groups
	npars = length(ri$pars);
	# number of parameters of the likelihood function
	#Npar = sum(list.kp(ri$pars, 'entries', template = 1));
	# <p> build wrappers
	m = nlapply(types, function(type) {
		defaults = merge.lists(lhSpecificationDefaults$default, lhSpecificationDefaults[[type]]);
		lhPreparePars(s[[type]], defaults, s, template = ri)
	});
	m = merge.lists(s, m);
	m
}
# <N> free parameters come first
lhFreePars = function(s, p)with(s, {
	r = if (parsFree > 0) {
		idcs = unlist(list.kp(s$pars[1:parsFree], 'entries'));
		if (length(idcs) == 0) idcs = 1:parsFree;
		p[idcs]
	} else c();
	r
})

# second numeric derivative of x
Dn2f = function(f, x, ..., eps = 1e-5) {
	(f(x + 2*eps, ...) + f(x - 2*eps, ...) - 2*f(x, ...))/(4*eps^2)
}

..OptimizeControl = list(fnscale = -1, tol = .Machine$double.eps^0.25);
# assume unconstraint arguments
Optimize = function(p, f, method = 'BFGS', control = ..OptimizeControl, ...,
	hessian = T, ci = T, alpha = 5e-2) {
	r = if (length(p) > 1) {
		control = .list(control, .min = 'tol');
		o = optim(p, f, method = method, control = control, hessian = hessian, ...);
	} else if (length(p) == 1) {
		f0 = function(p, ...) { f(logit(p), ...) };
		o0 = try(optimize(f0, lower = 0, upper = 1,
			tol = control$tol, maximum = control$fnscale < 0, ...));
		o = if (class(o0) == 'try-error') list(par = NA, value = NA, hessian = NA) else 
			list(par = logit(o0$maximum), value = o0$objective,
				hessian = if(hessian) matrix(Dn2f(f, logit(o0$maximum), ...)/o0$objective) else NA);
	} else {
		o = list(par = c(), value = f(...));
	}
	if (ci && hessian && !is.na(r$hessian)) {
		var = -1/diag(r$hessian);	# assume sharp cramer-rao bound
		sd = sqrt(var);
		r = c(r, list(ci = list(
			ciL = qnorm(alpha/2, r$par, sd, lower.tail = T),
			ciU = qnorm(alpha/2, r$par, sd, lower.tail = F), level = alpha, var = var)));
	}
	r
}

# p: matrix of row-wise start values
OptimizeMultiStart = function(p, f, method = 'BFGS', control = ..OptimizeControl, ...) {
	r = if (is.null(p)) {	# special case of degenerate matrix (does not work in R)
		Optimize(c(), f, method = method, control = control, ...)
	} else if (!is.matrix(p)) {
		Optimize(p, f, method = method, control = control, ...)
	} else {
		os = apply(p, 1, function(s)Optimize(s, f, method = method, control = control, ...));
		# find maximum
		if (all(is.na(os))) return(NA);
		vs = list.key(os, 'value');
		arg.max = which.max(vs);
		r = os[[arg.max[1]]];
	}
	r
}

lhEstMLRaw = function(t, start = NULL, ..., optim_method = 'BFGS') {
	if (is.null(start)) start = t$start;
	for (method in optim_method) {
		o = try(OptimizeMultiStart(t$mapperInv(start), t$lh, method = method, ...));
		if (!('try-error' %in% class(o))) break();
	}
	o$par = t$mapper(o$par);
	o$ci$ciL = t$mapper(o$ci$ciL);
	o$ci$ciU = t$mapper(o$ci$ciU);
	o
}

lhEstML = lhMl = function(s, start = NULL, type = 'alt', ..., optim_method = 'BFGS') {
	# <p> mapping of parameters
	s = lhPrepare(s, types = type);
	lhEstMLRaw(s[[type]], start = start, ..., optim_method = optim_method)
}

lfPrepare = function(s, ...) {
	lhParsOrig = list(...);
	prepare = sprintf('%s%s', s$ll, c('prepare', '_prepare'));
	prepareExists = min(which(sapply(prepare, exists)));
	lhPars = if (prepareExists < Inf) get(prepare[prepareExists])(...) else lhParsOrig;
	lhPars
}

# specification based LR-test
lhTestLR = function(s, startNull = NULL, startAlt = NULL, types = c('null', 'alt'), ...,
	optim_method = 'BFGS', addTypeArg = F) {
	# <p> general preparation
	s = lhPrepare(s, types = types);
	null = s[[types[1]]];
	alt = s[[types[2]]];

	# <p> specific preparation (user defined)
	lhPars = lfPrepare(s, ...);
	
	# <p> null hypothesis
	if (is.null(startNull))
		startNull = if(null$parsFree == 0) NULL else matrix(lhFreePars(null, null$start), nrow = 1);
	lhEstMLRawArgs = c(list(t = null, start = startNull), lhPars, list(optim_method = optim_method));
	if (addTypeArg) lhEstMLRawArgs = c(lhEstMLRawArgs, list(lh_type__ = 'null'));
	o0 = do.call(lhEstMLRaw, lhEstMLRawArgs);

	# <p> alternative hypothesis
	if (is.null(startAlt)) {
		# build from fit under the null
		parNull = lhFreePars(null, o0$par);
		startAlt = matrix(c(parNull, alt$start[(length(parNull) + 1):length(alt$start)]), nrow = 1);
	}
	lhEstMLRawArgs = c(list(t = alt, start = startAlt), lhPars, list(optim_method = optim_method));
	if (addTypeArg) lhEstMLRawArgs = c(lhEstMLRawArgs, list(lh_type__ = 'alt'));
	o1 = do.call(lhEstMLRaw, lhEstMLRawArgs);

	# <p> calcualte degrees of freedom
	df = length(alt$start) - length(lhFreePars(null, o0$par));
	stat =  2 * (o1$value - o0$value);
	r = list(ll.null = o0$value, ll.alt = o1$value,
		test.stat = stat, p = 1 - pchisq(stat, df), df = df, par.null = o0$par, par.alt = o1$par,
		lh.pars = lhPars, lh.pars.orig = lhParsOrig
	);
	r
}

#
#	<p> latest iteration of LH wrapper
#

lhPrepareFormula = function(s, type, formula, data, ...) {
	# <o> compute on subset of data <N> cave: missingness
	X = model.matrix(model.frame(formula, data = data), data = data);

	# <p> expand paramters
	t = s[[type]];
	ps = t$pars;
	fparsI = which(list.key(ps, 'name') == 'formula');
	fpars = ps[[fparsI]];	# formula pars
	ps[[fparsI]] = merge.lists(ps[[fparsI]], list(name = 'beta', count = ncol(X)));
	# <p> determine slots
	counts = cumsum(list.key(ps, 'count'));
	countsStart = pop(c(1, counts + 1));
	ps = lapply(seq_along(ps), function(i)merge.lists(ps[[i]], list(entries = countsStart[i]:counts[i])));
	# <p> determine start
	start = avu(sapply(ps, function(p)rep(p$start, p$count)));
	# <p> map pars
	t$pars = ps;
	t = lhPreparePars(t, spec = merge.lists(lhSpecificationDefault, s));
	t$start = start;
	t
}

lhMlFormula = function(s, formula, data, type = 'formula', ..., optim_method = 'BFGS') {
	# <p> mapping of parameters
	t = lhPrepareFormula(s, type, formula, data, ...);
	# <p> extra args
	lhPars = lfPrepare(s, formula = formula, data = data, ...);
	# <p> call optimizer
	lhEstMLRawArgs = c(list(t = t, start = s$start), lhPars, list(optim_method = optim_method));
	r = try(do.call(lhEstMLRaw, lhEstMLRawArgs), silent = T);
print(r);
	if (class(r) == 'try-error') r = list(par = rep(NA, length(t$start)), value = NA, convergence = 1);
	r
}

#
# <p> model manipulation
#

response.is.binary = function(r) {
	vs = sort(unique(r));
	if (length(vs) != 2) F else all(vs == c(0, 1));
}



#
#	<p> clustered data
#

#
# <p> describe relationships (genetic) given a relational (database) model
#

# given relatedness in a data frame of ids and clusterIds, return a list of clusters containing ids
# clusterRelation2list_old = function(r, idName = "id", idClusterName = "idFam", byIndex = T) {
# 	r = r[, c(idName, idClusterName)];
# 	ns = sort(unique(r[, 2]));
# 	# <p> build clusters
# 	clusters = sapply(ns, function(e)list());		# holds members of clusters
# 	names(clusters) = ns;
# 	# <!> we can iterate the list, given it is ordered lexicographically
# 	for (i in 1:(dim(r)[1])) {
# 		clN = as.character(r[i, 2]);
# 		clusters[[clN]] = unlist(c(clusters[[clN]], ifelse(byIndex, i, as.character(r[i, 1]))));
# 	}
# 	clusters
# }
clusterRelation2list = function(r, idName = "id", idClusterName = "idFam", byIndex = T) {
	r = r[, c(idName, idClusterName)];
	clusters = nlapply(sort(unique(r[[idClusterName]])), function(n) {
		idcs = which(r[[idClusterName]] == n);
		c = if (byIndex) idcs else r[[idName]][idcs];
		c
	});
	clusters
}

# permute clusters of identical size and within clusters
# cluster specification as given by clusterRelation2list assuming byIndex = T
# returned permutation is relative to refIds
permuteClusters = function(cls, refIds = NULL, selectIds = NULL) {
	# allow to filter ids from cluster specification
	if (!is.null(selectIds)) {
		cls = lapply(cls, function(cl)intersect(cl, selectIds));
		cls = clusters[sapply(cls, length) > 0];
	}
	cSizes = sapply(cls, function(e)length(e));
	# which cluster sizes are present in the data set?
	sizes = unique(cSizes);
	# indexable list of ids
	refIds = if (is.null(refIds)) sort(unlist(cls));
	# final permutation of refIds, such that refIds[perm] gives new order
	perm = 1:length(refIds);

	for (s in sort(sizes, decreasing = T)) {	# permute cluster of same size, permute within cluster
		clsS = which(cSizes == s);
		p1 = sample(1:length(clsS));	# permute clusters
		for (i in 1:length(clsS)) {
			p2 = sample(1:s);
			# <p> indeces that are to be replaced
			indT = which.indeces(cls[[clsS[i]]], refIds);
			# <p> indeces where the replacement comes from
			indF = which.indeces(cls[[clsS[p1[i]]]][p2], refIds);
			# <p> save partial permutation
			perm[indT] = indF;
		}
	}
	perm
}

# clusters is a vector with cluster ids
clustersPermute = function(cls) {
	permuteClusters(clusterRelation2list(data.frame(id = 1:length(cls), idFam = cls)))
}

#
#	<p> wrap model fitting for lm/glm/gee fitters
#

#library("geepack");	# <i> move to init method
regressionMethods = list(
	# assume formula to contain random effect
	glmr = list(
		fit = function(formula, data, clusterCol = NULL, ...) {
			glmer(formula, data = data, ...)
		},
		compare = function(m1, m0){
			a = anova(m0$r, m1$r, test = "Chisq");
			list(anova = a, m0 = m0, m1 = m1,
				#p.value = a[["P(>|Chi|)"]][2],
				p.value = a[['Pr(>Chisq)']][2],	# as of R 2.15.1
				effects0 = coefficients(summary(m0$r))[, "Estimate"],
				sdevs0 = coefficients(summary(m0$r))[, "Std. Error"],
				effects1 = coefficients(summary(m1$r))[, "Estimate"],
				sdevs1 = coefficients(summary(m1$r))[, "Std. Error"]
			)
		}
	),
	# use cluster column <!> untested
	glmrcl = list(
		fit = function(formula, data, clusterCol = NULL, ...) {
			f = update(formula, as.formula(Sprintf('~ . + (1|%{clusterCol}s)')));
			glmer(f, data = data, ...)
		},
		compare = function(m1, m0){
			a = anova(m0$r, m1$r, test = "Chisq");
			list(anova = a, m0 = m0, m1 = m1,
				#p.value = a[["P(>|Chi|)"]][2],
				p.value = a[['Pr(>Chisq)']][2],	# as of R 2.15.1
				effects0 = coefficients(summary(m0$r))[, "Estimate"],
				sdevs0 = coefficients(summary(m0$r))[, "Std. Error"],
				effects1 = coefficients(summary(m1$r))[, "Estimate"],
				sdevs1 = coefficients(summary(m1$r))[, "Std. Error"]
			)
		}
	),
	glm = list(
		fit = function(formula, data, clusterCol = NULL, ...)glm(formula, data = data, ...),
		compare = function(m1, m0){
			a = anova(m0$r, m1$r, test = "Chisq");
			list(anova = a, m0 = m0, m1 = m1,
				#p.value = a[["P(>|Chi|)"]][2],
				p.value = a[['Pr(>Chi)']][2],	# as of R 2.15.1
				effects0 = coefficients(summary(m0$r))[, "Estimate"],
				sdevs0 = coefficients(summary(m0$r))[, "Std. Error"],
				effects1 = coefficients(summary(m1$r))[, "Estimate"],
				sdevs1 = coefficients(summary(m1$r))[, "Std. Error"]
			)
		}
	),
	lm = list(
		fit = function(formula, data, clusterCol = NULL, ...)lm(formula, data = data, ...),
		compare = function(m1, m0){
			a = anova(m0$r, m1$r);
			list(anova = a, m0 = m0, m1 = m1, p.value = a[["Pr(>F)"]][2],
				effects0 = coefficients(summary(m0$r))[, "Estimate"],
				sdevs0 = coefficients(summary(m0$r))[, "Std. Error"],
				effects1 = coefficients(summary(m1$r))[, "Estimate"],
				sdevs1 = coefficients(summary(m1$r))[, "Std. Error"]
			)
		}
	),
	gee = list(
		fit = function(formula, data, clusterCol, ...) {
			if (!length(formula.covariates(formula))) return(NULL);
			# geeglm needs ordered clusterIds <!>
			data = data[order(data[[clusterCol]]), ];
			names(data)[which(names(data) == clusterCol)] = "..gee.clusters";	# hack to make geeglm work
			r = geeglm(formula, data = data, id = ..gee.clusters, ...);
			r
		},
		compare = function(m1, m0){
			a = if (is.null(m0)) anova(m1$r) else anova.geeglm(m0$r, m1$r);
			list(anova = a, m0 = m0, m1 = m1, p.value = a[["P(>|Chi|)"]][1],
				effects0 = coefficients(summary(m0$r))[, "Estimate"],
				sdevs0 = coefficients(summary(m0$r))[, "Std.err"],
				effects1 = coefficients(summary(m1$r))[, "Estimate"],
				sdevs1 = coefficients(summary(m1$r))[, "Std.err"]
			)
		}
	)
);

completeRows = function(f1, data) {
	vars = all.vars(as.formula(f1));
	rows = apply(data[, vars, drop = F], 1, function(r)all(!is.na(r)));
	r = which(rows);
	r
}

# <!> clusterIds is needed as argument although just forwarded
regressionFit = function(f, data, type, ...) {
	r = regressionMethods[[type]]$fit(f, data, ...);
	list(type = type, r = r)
}

regressionCompare = function(m1, m0) {
	r = regressionMethods[[m1$type]]$compare(m1, m0);
	r
}

regressionCompareModelsRaw = function(f1, f0, data, type = "lm", clusterCol = NULL, ...) {
	# <p> jointly trim data according to missing data
	#rows = which(apply(data[, c(formula.vars(f1), clusterCol)], 1, function(r)all(!is.na(r))));
	# more robust version
	row.names(data) = NULL;
	#rows = as.integer(row.names(model.frame(f1, data = data)));
	# robust for random effects
	rows = apply(data[, all.vars(as.formula(f1)), drop = F], 1, function(r)!any(is.na(r)));
	d0 = data[rows, ];

	# <p> fit and compare models
	m1 = regressionFit(as.formula(f1), data = d0, type = type, clusterCol = clusterCol, ...);
	m0 = regressionFit(as.formula(f0), data = d0, type = type, clusterCol = clusterCol, ...);
	a = regressionCompare(m1, m0);
	a
}

permuteDefault = list(
	p.value = 0, sdev.rel = .3, Nchunk = 1e3,
	nuisanceCovariates = NULL, .clRunLocal = T
);
# idCol: used for permutation: column specifying identiy of individuals: could be filled automatically <i>
# permute:
#	sdev.rel: sdev relative to p.value to decide how often to permute
regressionCompareModels = function(f1, f0, data, type = "lm", clusterCol = NULL, ...,
	permute = permuteDefault) {
	permute = merge.lists(permuteDefault, permute);
	r = regressionCompareModelsRaw(f1, f0, data, type, clusterCol, ...);

	if (!is.null(r) && !is.null(r$p.value) && !is.na(r$p.value) && r$p.value < permute$p.value)
		r = regressionCompareModelsPermuted(f1, f0, data, type, clusterCol, ..., permute = permute);
	r
}

#
#	<p> permuted cluster regression
#

regressionCompareModelsPermuted = function(f1, f0, data, type = "lm", clusterCol = "cluster", ...,
	idCol = "id", permute = permuteDefault, Nmax = 1e5) {
	# <p> data p-value
	a.data = regressionCompareModelsRaw(f1, f0, data, type, clusterCol = clusterCol, ...);
	p.data = a.data$p.value;
	# <p> logging
	Log(sprintf("Permuting Regression: %s [p = %.2e]", paste(as.character(f1), collapse = " "), p.data), 4);
	# <p> permutation variables indeces
	pvs = setdiff(formula.covariates(f1), permute$nuisanceCovariates);

	# <p> precompute cluster data structure
	cls = clusterRelation2list(data.frame(id = 1:length(data[[clusterCol]]), idFam = data[[clusterCol]]))
	ps = NULL;
	d0 = data;
	# adaptive permutation
	repeat {
		ps0 = clapply(1:permute$Nchunk, function(i, f1, f0, data, type, clusterCol, cls, pvs){
			d0[, pvs] = if (is.null(clusterCol)) d0[sample(1:(dim(data)[1])), pvs] else
				d0[permuteClusters(cls), pvs];
			r = regressionCompareModelsRaw(f1, f0, d0, type, clusterCol, ...);
			r$p.value
		}, f1 = f1, f0 = f0, data = data, type = type, clusterCol = clusterCol, cls = cls, pvs = pvs,
		.clRunLocal = permute$.clRunLocal);
		ps0 = na.exclude(as.numeric(ps0));
		ps = c(ps, ps0);
		#print(ps[1:100]);
		p.emp = fraction(ps <= p.data);
		# <p> stopping criterion
		p.break = if (p.emp == 0) 1 / length(ps) else p.emp;
		sdev.rel = sqrt(p.break * (1 - p.break) / length(ps)) / p.break;
		#print(list(sd = sdev.rel * p.break, sd.rel = sdev.rel, p = p.emp));
		if (sdev.rel <= permute$sdev.rel) break;
		# <p> final stop
		if (length(ps) >= Nmax) break;
	};
	r = list(f1 = f1, f0 = f0, p.value = p.emp, p.data = p.data, anova = a.data$anova, ps = ps);
	r
}

# permute covariates in order to obtain empirical p-values
#	f1: model formula alternative
#	f0: model formula hypothesis
#	M: number of permutations
regressionCompareModelsEmp = function(f1, f0, data, nuisanceCovariates = c(), type = "lm", M = 1e3, ...,
	idName = "id", idClusterName = "cluster", .clRunLocal = T) {
	r = regressionCompareModelsPermuted(f1, f0, type, ..., clusterCol = idClusterName, idCol = idName,
		permute = list(Nchunk = M, nuisanceCovariates = nuisanceCovariates, .clRunLocal = .clRunLocal));
	r
}

# data: data.frame
# stat: function computing test statistic
# vars: formula for permuation
# Nperm: number of permutations
# Pvalue: c('upper', 'lower', 'two.tailed')
permute = function(data, stat, vars, ..., Nperm = 5e3, Pvalue = 'lower', na.rm = T, fracBadStatThres = .01,
	returnT = TRUE) {
	perm.vars = all.vars(as.formula(vars));
	f = function(i, ...) {
	};
	Ts = Sapply(0:Nperm, function(i, data, ...) {
		if (i > 0) data[, perm.vars] = data[sample(nrow(data)), perm.vars];
		stat(data, ...)
	}, data = data, ...);

	fracBadStatistics = mean(is.na(Ts[-1]));
	if (is.na(Ts[1]) || fracBadStatistics >= fracBadStatThres) return(list(p.value = NA));
	Ts = Ts[!is.na(Ts)];
	Tdata = Ts[1];
	Ts = Ts[-1];
	Plower = (1 + sum(Ts <= Tdata)) / Nperm;
	Pupper = (1 + sum(Ts >= Tdata)) / Nperm;
	p.value = switch(Pvalue,
		lower = Plower,
		upper = Pupper,
		two.tailed = 2 * min(Plower, Pupper)
	);
	r = if (returnT)
		list(p.value = p.value, t.data = Tdata, t.perm = Ts) else
		list(p.value = p.value, t.data = Tdata);
	r
}

#
#	<p> error propagation
#

# as derived from the RB project and tested therein

errProd = function(x, sdx, y, sdy, covxy = 0) {
	sdp = (x * y) * sqrt((sdx/x)^2 + (sdy/y)^2 + 2 * sdx * sdy * covxy);
	sdp
}

errFrac = function(x, sdx, y, sdy, covxy = 0) {
	sdp = (x / y) * sqrt((sdx/x)^2 + (sdy/y)^2 - 2 * sdx * sdy * covxy);
	sdp
}

errSum = function(sdx, cx = 1, sdy = 0, cy = 1, covxy = 0) {
	sds = sqrt((cx *sdx)^2 + (cy * sdy)^2 + 2 * cx * cy * covxy);
	sds
}

#
#	<§> some general statistical transformations
#

# convert confidence interval to standard dev based on a normality assumption
ciToSd = function(ci.lo, ci.up, level = .95) {
	# upper centered limit
	ciU = ci.up - mean(c(ci.lo, ci.up));
	span = ci.up - ci.lo;
	# corresponding sd
	sd = Vectorize(inverse(function(s)qnorm(1 - (1 - level)/2, 0, s), interval = c(0, span * 8)))(ciU);
	sd
}
ciToP = function(ci.lo, ci.up, level = .95, one.sided = F, against = 0) {
	sd = ciToSd(ci.lo, ci.up, level)
	P = peSdToP((ci.lo + ci.up)/2 - against, sd, one.sided);
	P
}
# convert point estimate and SD to p-value (assuming normality)
peSdToP = function(beta, sd, one.sided = F) {
	pnorm(-abs(beta), 0, sd, lower.tail = T) * ifelse(one.sided, 1, 2);
}

ciFromBetaSdev = function(beta, sdev, level = .95) {
	r = list(effect = beta,
		lower = qnorm((1 - level)/2, beta, sdev, lower.tail = T),
		upper = qnorm((1 - level)/2, beta, sdev, lower.tail = F)
	);
	r
}

ciFromSummary = function(s, var, level = .95) {
	cs = coefficients(s)[var, ];
	ciFromBetaSdev(cs[["Estimate"]], cs[["Std. Error"]], level = level);
}

pFromBetaSd = function(beta, sd, null = 0)pnorm(null, abs(beta), sd)
sdFromBetaP = function(beta, p)Vectorize(inverse(function(s)peSdToP(beta, s), interval = c(0, 10)))(p);
betaPtoCi = function(beta, p) {
	sd = sdFromBetaP(beta, p);
	ciFromBetaSdev(beta, sd)
}

#
#	meta analysis
#

# meta analysis row-wise
metaPvalue = function(ps) {
	if (!is.matrix(ps)) ps = matrix(ps, nrow = 1);
	if (!all(is.numeric(ps))) ps = apply(ps, 1:2, as.numeric);
	cs = apply(ps, 1, function(r)sum(-2*log(r)));
	psM = pchisq(cs, 2*dim(ps)[2], lower.tail = F);
	psM
}

#
#	data imputation
#

Sample = function(x, ...)if (length(x) == 1)x else sample(x, ...);

mi.simple = function(data, n.imp = 20) {
	r = lapply(1:n.imp, function(i) {
		for (v in names(data)) {
			data[is.na(data[, v]), v] = Sample(na.omit(data[, v]), count(is.na(data[, v])));
		}
		data
	})
	r
}

cross.imputer = function(imputationData, imputationVars = NULL, doExpandFactors = T) {
	if (is.null(imputationVars)) imputationVars = names(imputationData);
	f = function(data) {
		d0 = data;
		for (v in imputationVars) { # cross impute from imputationData
			d0[is.na(d0[, v]), v] = Sample(na.omit(imputationData[[v]]), count(is.na(d0[, v])));
		}
		if (doExpandFactors) d0 = dataExpandFactors(d0)[, vars];
		d0
	};
	f
}

imputeMeanVar = function(col) {
	mn = mean(col, na.rm = T);
	col[is.na(col)] = mn;
	col
}
imputeMean = function(data) {
	d1 = apply(data, 2, imputeMeanVar);
	d1
}

#
#	<p> cross validation
#

# cross validation partitions for classification data
crossValidationPartitionsClassification = function(responses, K = 15, minEls = 3, maxTries = 15) {
	N = length(responses);
	cats = unique(responses);

	for (i in 1:maxTries) {
		# random permutation
		perm = sample(1:N, N);
		# compute partitions
		parts = splitListEls(perm, K, returnElements = T);
		counts = data.frame.types(
			lapply(parts, function(p)table.n(responses[-p], categories = cats)),
			names = cats, do.rbind = T
		);
		doReject = any(apply(counts, 1, function(r)any(r < minEls)));
		if (!doReject) break;
	}
	r = if (i < maxTries) parts else {
		Log("Error: failed to find suitable cross validation partition!");
		NULL
	}
	r
}

# cross validation parititions for clustered data
# return indeces into cluster vector (cluster identities assumed to be given by integers)
# so far do not heed cluster sizes
crossValidationPartitionsClusters = function(clusters, K = 20) {
	N = length(clusters);
	# unique cluster ids
	cls = unique(clusters);
	# random permutation
	perm = Sample(cls, length(cls));
	# compute partitions
	parts = splitListEls(perm, K, returnElements = T);
	r = lapply(parts, function(p)which.indeces(p, clusters, match.multi = T));
	r
}

#
#	<p> optimization
#

nested.search = function(f, ..., key = NULL, parameters = list(p1 = c(0, 10)),
	steps = 3, Ngrid = 4, rscale = 1, return.grid = F, par.as.vector = F, .clRunLocal = rget('.clRunLocal')) {
	ps = ps0 = parameters;

	for (i in 1:steps) {
		# <p> create serach grid
		pars = lapply(ps, function(p)seq(p[1], p[2], length.out = Ngrid));
		grid = merge.multi.list(pars);
		# <p> apply function
		r = clapply(1:dim(grid)[1], function(j, grid, ...) {
			args = if (par.as.vector) list(as.vector(grid[j, ]), ...) else c(as.list(grid[j, ]), list(...));
			do.call(f, args);
		}, grid = grid, ..., .clRunLocal = .clRunLocal);
		# <p> search optimum
		values = if (is.null(key)) r else list.kp(r, key, do.unlist = T);
		opt = which.min(values * rscale);
		pt = grid[opt, ];	# optimal point in the grid search

		ps = lapply(1:length(ps), function(j){
			from = max(pt[j] - (ps[[j]][2] - ps[[j]][1])/Ngrid, ps0[[j]][1]);
			to =   min(pt[j] + (ps[[j]][2] - ps[[j]][1])/Ngrid, ps0[[j]][2]);
			c(from, to)
		});
		names(ps) = names(ps0);
	}
	r = if (return.grid) list(value = values[[opt]], par = pt, grid = r) else
		list(value = values[[opt]], par = pt, r = r[[opt]]);
	r
}
optim.nested.defaults = list(steps = 5, Ngrid = 4, rscale = 1, return.grid = F);
optim.nested = function(par = NULL, f, ..., lower = -Inf, upper = Inf, control = list())
	with(merge.lists(optim.nested.defaults, control), {
	parameters = apply(cbind(lower, upper), 1, function(r)list(r));
	r = nested.search(f, ..., parameters,
		steps = steps, Ngrid = Ngrid, rscale = rscale, return.grid = return.grid, par.as.vector = T);
	r
})

#
#	<p> correlation in data
#

Df.corr = function(df, eps = 1e-2) {
	N = dim(df)[2];
	rc = rcorr(df);
	pairs = t(sapply(which(abs(rc$r) > (1 - eps)), function(e) {
		row = ((e - 1) %/% N) + 1;
		col = ((e - 1) %% N) + 1;
		r = c(row, col);
		r
	}));
	pairs = pairs[pairs[, 1] < pairs[, 2], ];
	clusters = sub.graph(pairs);
	remove = unlist(lapply(clusters, function(e)e[-1]));
	r = list(clusters = clusters, cols.remove = remove);
	r
}

identity = function(e)e
seq.transf = function(from = 0, to = 1, length.out = 1e1, ..., transf = log, transfI = exp, eps = 1e-5) {
	s = transfI(seq(from = transf(from + eps), to = transf(to - eps), length.out = length.out, ...));
	s
}

#
#	<p> bug fixes for packages
#

model_matrix_from_formula = function(f, data, offset = NULL, ignore.case = F, remove.intercept = F) {
	# <p> prepare data matrices
	f1 = formula.re(f, data = data, ignore.case = ignore.case);
	f1vars = all.vars(f1);
	response = formula.response(f1);
	responseValues = if (length(response) > 0) data[[response]] else NULL;
	row.names(data) = NULL;
	complete = !apply(data[, f1vars, drop = F], 1, function(r)any(is.na(r)));
	data = droplevels(data[complete, ]);
	responseValues = responseValues[complete];
	offset = if (!is.null(offset)) offset[complete] else NULL;
	mm = model.matrix(f1, model.frame(f1, data = data));
	if (remove.intercept) mm = mm[, !(dimnames(mm)[[2]] == '(Intercept)')];

	r = list(mm = mm, response = responseValues, offset = offset, indeces = as.integer(row.names(data)));
	r
}
complete_from_formula = function(f, data, offset = NULL, ignore.case = F, remove.intercept = F) {
	model_matrix_from_formula(f, data, offset, ignore.case, remove.intercept)$indeces
}
complete_from_vars = function(vars, data, offset = NULL, ignore.case = F, remove.intercept = F) {
	f = as.formula(Sprintf('~ %{vars}s', vars = join(vars, ' + ')));
	model_matrix_from_formula(f, data, offset, ignore.case, remove.intercept)$indeces
}

glmnet_re = function(f, data, ..., offset = NULL, ignore.case = F, remove.intercept = F,
	lambdas = NULL, cv = T) {
	d = model_matrix_from_formula(f, data, offset, ignore.case, remove.intercept);
	# <p> fit model
	r = if (cv) {
		r0 = cv.glmnet(x = d$mm, y = d$response, lambda = lambdas, ..., offset = d$offset);
		args = c(List(..., min_ = c('foldid', 'nfolds', 'grouped')),
			list(x = d$mm, y = d$response, lambda = r0$lambda.min, offset = d$offset));
# 			list(x = d$mm, y = d$response, lambda = (3*r0$lambda.min + r0$lambda.1se)/4, offset = d$offset));
#			list(x = d$mm, y = d$response, lambda = (r0$lambda.min), offset = d$offset));
		do.call('glmnet', args);
	} else glmnet(x = d$mm, y = d$response, lambda = lambdas, ..., offset = d$offset);
	r = c(r, list(formula = f));
	r
}
glmnet_re_refit = function(model, data, ..., var_cutoff =  1e-6, intercept = '1', impute = NULL) {
	response = formula.response(model$formula);

	if (model$df <= 1) return(list());
	# <p> scrutinize model
	coefs = model$beta;
	varsSel = row.names(coefs)[abs(as.vector(coefs)) > var_cutoff];
	varsSel = setdiff(varsSel, '(Intercept)');
	
	if (!is.null(impute) && impute == 'mean') {
		# <!> use model matrix <i>
		d0 = sapply(varsSel, function(var) {
			data[[var]][is.na(data[[var]])] = mean(data[[var]], na.rm = T);
		});
		data[, varsSel] = d0;
	}
	# <p> refit
	f = as.formula(sprintf('%s ~ %s', response, paste(c(intercept, varsSel), collapse = ' + ')));
	glm1 = glm(f, data = data, ...);
	r0 = list(glm = glm1, score = as.vector(predict(glm1, data, type = 'link')))
	r0
}

#library('glmnet');
grid.glmnet.raw = function(..., glmnet.f = cv.glmnet, max.tries = 3) {
	for (i in 1:max.tries) {
		fit = try(glmnet.f(...), silent = T);
		if (all(class(fit) != 'try-error')) break();
	}
	if (any(class(fit) == 'try-error')) stop(fit[1]);
	fit
}

grid.glmnet.control = list(steps = 4, Ngrid = 50, from = .01,  to = .8, eps = 1e-5,
		transf = identity, transfI = identity);

grid.glmnet = function(..., control = grid.glmnet.control)
	with (merge.lists(grid.glmnet.control, control), {
	# initialize
	fit = NULL;
	fromO = from;
	toO = to;
	options(warn = -1);
	for (i in 1:steps) {
		lambda = seq.transf(from, to, length.out = Ngrid + 1, eps = eps,
			transf = transf, transfI = transfI);
		fit = grid.glmnet.raw(..., lambda = sort(lambda, decreasing = T));
		from = max(fit$lambda.min - (to - from)/Ngrid, 0);
		to = fit$lambda.min + (to - from)/Ngrid;
	}
	options(warn = 0);
	# choose lambdas to contain lambda.min also covering the range between from and to
	lambda = c(
		seq.transf(fromO, toO, length.out = Ngrid + 1, eps = eps,
			transf = transf, transfI = transfI),
		fit$lambda.min
	);
	fit0 = do.call('grid.glmnet.raw', c(list(...), list(lambda = sort(lambda, decreasing = T))));
	args = List(..., min_ = c('nfolds', 'grouped'));
	fit1 = do.call('grid.glmnet.raw', c(args, list(lambda = fit$lambda.min, glmnet.f = glmnet)));
	r = fit0;
	r$glmnet.fit = fit1;
	r
})

#	f: formula, passed through formula.re
#	data: data frame
grid.glmnet.re = function(f, data, ..., offset = NULL, control = grid.glmnet.control,
	ignore.case = F, remove.intercept = T)
	with (merge.lists(grid.glmnet.control, control), {

	# <p> prepare data matrices
	f1 = formula.re(f, data = data, ignore.case = ignore.case);
	f1vars = all.vars(f1);
	response = formula.response(f1);
	complete = !apply(data[, f1vars], 1, function(r)any(is.na(r)));
	d1 = data[complete, ];
	if (!is.null(offset)) offset = offset[complete];
	mm = model.matrix(f1, model.frame(f1, data = d1));
	if (remove.intercept) mm = mm[, !(dimnames(mm)[[2]] == '(Intercept)')];
	# <p> fit model
	r = grid.glmnet(x = mm, y = d1[[response]], ..., offset = offset, control = control);
	r = c(r, list(formula = f1));
	r
})

grid_glmnet_re_refit = function(model, data, ..., var_cutoff =  1e-6, intercept = '1', impute = NULL) {
	# <p> scrutinize model
	coefs = coefficients(model$glmnet.fit);
	varsSel = row.names(coefs)[abs(as.vector(coefs)) > var_cutoff];
	varsSel = setdiff(varsSel, '(Intercept)');
	response = formula.response(model$formula);

	if (!is.null(impute) && impute == 'mean') {
		# <!> use model matrix <i>
		d0 = sapply(varsSel, function(var) {
			data[[var]][is.na(data[[var]])] = mean(data[[var]], na.rm = T);
		});
		data[, varsSel] = d0;
	}
	# <p> refit
	f = as.formula(sprintf('%s ~ %s', response, paste(c(intercept, varsSel), collapse = ' + ')));
	glm1 = glm(f, data = data, ...);
	r0 = list(glm = glm1, score = as.vector(predict(glm1, data, type = 'link')))
	r0
}

refitModel = function(model, f1, f0, data, ..., var_cutoff =  1e-6, ignore.case = F, intercept = '0') {
	# <p> prepare formula and data set
	f1 = formula.re(f1, data = data, ignore.case = ignore.case);
	f0 = formula.re(f0, data = data, ignore.case = ignore.case);
	f0covs = formula.covariates(f0);
	f1vars = all.vars(f1);
	response = formula.response(f1);
	complete = complete.cases(data[, f1vars]);	#!apply(data[, f1vars], 1, function(r)(any(is.na(r))));
	d1 = data[complete, ];
	# <p> extract data set according to model
	coefs = coefficients(model);
	varsSel = row.names(coefs)[abs(as.vector(coefs)) > var_cutoff];
	varsSel = setdiff(varsSel, '(Intercept)');
	varsSel0 = intersect(varsSel, f0covs);
	if (!length(varsSel0)) return(
		list(coefficients = coefs, anova = NA, r2 = NA, r20 = NA, raw = NA, model1 = NA, model0 = NA)
	);
	# <p> re-fit glm
	f1 = as.formula(sprintf('%s ~ %s', response, paste(c(intercept, varsSel), collapse = ' + ')));
	glm1 = glm(f1, data = d1, ...);
	f0 = as.formula(sprintf('%s ~ %s', response, paste(c(intercept, varsSel0), collapse = ' + ')));
	glm0 = glm(f0, data = d1, ...);
	# <p> anova
	a = anova(glm0, glm1, test = 'Chisq');
	# <p> R^2
	mn = mean(d1[[response]]);
	#mm = model.matrix(f1, model.frame(f1, data = d1));
	pr = as.vector(predict(glm1, d1, type = 'response'));
	#r2 = cor((pr - mn), (d1[[response]] - mn));
	r2 = cor(pr, d1[[response]]);
	pr0 = as.vector(predict(glm0, d1, type = 'response'));
	#r20 = cor((pr0 - mn), (d1[[response]] - mn));
	r20 = cor(pr0, d1[[response]]);
	# <p> raw-model fit
	fScore = as.formula(sprintf('y ~ score + %s', paste(c(intercept, varsSel0), collapse = ' + ')));
	d2 = data.frame(
		d1[, varsSel0], y = d1[[response]], score = as.vector(predict(glm1, d1))
	);
	if (length(varsSel0)) names(d2)[1:length(varsSel0)] = varsSel0;
	raw = glm(fScore, data = d2, ...);
	r = list(coefficients = coefs, anova = a, r2 = r2, r20 = r20,
		raw = summary(raw), model1 = glm1, model0 = glm0);
	r
}

#
#	<p> crossvalidation
#

# <!> tb implemented
cv_summary_lm = function(model, pred, data, ...) {
	summary(r0)$fstatistic[1]
	r = mean( (pred - data)^2 );
	r
}

cv_test_glm = function(model, formula, data, ...) {
	response = formula.response(formula);
	responseP = predict(model, data, type = 'response');
	responseD = data[[response]];
	ll = sum(log(responseP));
	ll
}

# cv_prepare = function(data, argsFrom...)
# cv_train = function(data, argsFrom...)
# cv_test = function(model, data, argsFrom...)
# @arg cv_fold number of crossvalidation folds, denotes leave -cv_fold out if negative

crossvalidate = function(cv_train, cv_test, cv_prepare = function(data, ...)list(),
	data, cv_fold = 20, cv_repeats = 1, ..., parallel = F, align_order = TRUE) {
	if (cv_fold == 0) stop('crossvalidate: cv_fold must be an integer != 0');
	if (!parallel) Lapply = lapply;
	N = nrow(data);
	r = with(cv_prepare(data = data, ...), {
		Lapply(1:cv_repeats, function(i, ...) {
			perm = Sample(1:N, N);
			# compute partitions
			fold = if (cv_fold > 0) cv_fold else as.integer(N/-cv_fold);
			parts = splitListEls(perm, fold, returnElements = T);
			o = order(unlist(parts));
			r = Lapply(parts, function(part, cv_train, cv_test, data, cv_repeats, ...) {
				d0 = data[-part, , drop = F];
				d1 = data[part, , drop = F];
				model = cv_train(..., data = d0);
				r = cv_test(model = model, ..., data = d1);
				gc();
				r
			}, cv_train = cv_train, cv_test = cv_test,
				data = data, cv_repeats = cv_repeats, ...);
			# re-establish order
			r = if (align_order
				&& all(sapply(r, class) %in% c('numeric', 'integer'))
				#&& all(sapply(r, length) == 1)) {
				&& sum(sapply(r, length)) == nrow(data)) {
				unlist(r)[o];
			} else if (align_order && all(sapply(r, class) %in% c('data.frame', 'matrix')) &&
				sum(sapply(r, nrow)) == nrow(data)) {
				#<!> untested
				#r = rbindDataFrames(r, colsFromFirstDf = T);
				r = do.call(rbind, r);
				r[o, ]
			} else if (align_order) stop("Crossvalidate: didn't know how to align order.") else r;
			gc();
			r
	}, ...)});
	r
}

#
#	<p> data standardization
#

standardize = function (x, ...)UseMethod("standardize", x);
standardize.numeric = function(v, na.rm = TRUE, orig.mean = FALSE) {
	vM = mean(v, na.rm = na.rm);
	vC = v - vM;
	vS = vC / sd(vC, na.rm = na.rm);
	if (orig.mean) vS + vM else vS
}
standardize.data.frame = function(df, na.rm = TRUE)apply(df, 2, standardize.numeric, na.rm = na.rm);
standardize.matrix = function(m, na.rm = TRUE)apply(m, 2, standardize.numeric, na.rm = na.rm);

df2z = function(data, vars = names(as.data.frame(data))) {
	data = as.data.frame(data);
	df = data.frame.types(sapply(vars, function(v) {
		(data[[v]] - mean(data[[v]], na.rm = T)) / sd(data[[v]], na.rm = T)
	}), do.transpose = F);
	i = which.indeces(vars, names(data));
	d0 = data.frame(data[, -i], df);
	d0
}

lumpFactor = function(factor, minFreq = NULL, minN = 20, levelPrefix = 'l') {
	# <p> preparation
	f0 = as.factor(factor);
	t0 = table(f0);
	ls = levels(f0);
	N = length(f0);
	if (!is.null(minFreq)) minN = as.integer(minFreq * N + 0.5);
	
	# <p> lumping
	map = listKeyValue(ls, ls);
	for (i in 1:length(t0)) {
		t0 = table(factor);
		if (all(t0 >= minN) || length(t0) < 2)  break;
		# combine two smallest groups
		t1 = sort(t0);
		newLevel = sprintf('%s%d', levelPrefix, i);
		factor = as.character(factor);
		factor[factor == names(t1)[1] | factor == names(t1)[2]] = newLevel;
		map[[names(t1)[1]]] = map[[names(t1)[2]]] = newLevel;
		map[[newLevel]] = newLevel;
	}
	# <p> normalize map
	lsNew = as.character(ls);
	repeat {
		lsNew0 = lsNew;
		lsNew = as.character(map[lsNew]);
		if (all(lsNew == lsNew0)) break;
	}
	return(list(map = listKeyValue(ls, lsNew), factor = factor));
}

# lump a variable after checking other variables for non-missingness
lumpVariableOnVariables = function(data, var, vars, postfix = '_lump', minN = 20) {
	# prepare confounder afkomst
	lump = sapply(vars, function(v) {
		dvar = data[[var]][!is.na(data[[v]])];
		lump = lumpFactor(dvar, minN = minN);
		dvarNew = as.character(lump$map[as.factor(data[[var]])]);
		dvarNew[dvarNew == 'NULL'] = NA;
		as.factor(dvarNew)
	});
	d = data.frame(lump);
	names(d) = paste(var, paste(vars, postfix, sep = ''), sep = '_');
	d
}

#
#	<p> descriptive
#

compareVectors = function(l) {
	sets = names(l);
	# marginals
	r0 = nlapply(sets, function(n)c(n, length(l[[n]])));
	r1 = nlapply(sets, function(n)c(sprintf('%s-unique', n), length(unique(l[[n]]))));
	r2 = nlapply(sets, function(n)c(sprintf("%s-NA", n), sum(is.na(l[[n]]))));

	modelList = list(A = sets, B = sets);
	r3 = iterateModels(modelList, .constraint = function(A, B)(A < B), function(i, A, B) {
		r = list(
			c(sprintf("%s inter %s", A, B), length(intersect(l[[A]], l[[B]]))),
			c(sprintf("%s union %s", A, B), length(union(l[[A]], l[[B]]))),
			c(sprintf("%s min %s", A, B), length(setdiff(l[[A]], l[[B]]))),
			c(sprintf("%s min %s", B, A), length(setdiff(l[[B]], l[[A]])))
		);
		r
	}, lapply__ = lapply)$results;

	r = c(r0, r1, r2, unlist.n(r3, 1));
	r = data.frame.types(r, do.rbind = T, names = c('type', 'count'));
	r
}

pairs_std.panel.hist <- function(x, ...) {
	usr <- par("usr"); on.exit(par(usr))
	par(usr = c(usr[1:2], 0, 1.5) )
	h <- hist(x, plot = FALSE)
	breaks <- h$breaks; nB <- length(breaks)
	y <- h$counts; y <- y/max(y)
	rect(breaks[-nB], 0, breaks[-1], y, col = "grey", ...)
}
pairs_std.panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
	usr <- par("usr"); on.exit(par(usr))
	par(usr = c(0, 1, 0, 1))
	c0 = cor.test(x, y);
	txt <- paste0(prefix,
		sprintf("Cor: %.2f (%.2f, %.2f)", c0$estimate, c0$conf.int[1], c0$conf.int[2]), "\n",
		sprintf("P-value: %.2e", c0$p.value)
	)
	if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
	text(0.5, 0.5, txt, cex = cex.cor * 1)	# used tb cex.cor * r
}
pairs_std = function(...) {
	pairs(..., diag.panel = pairs_std.panel.hist, upper.panel = pairs_std.panel.cor)
}

#
#	<p> omics data
#

quantileData = function(d, p) {
	dS = sort(d);
	q = dS[ceiling(p * length(dS))];
	q
}

quantileReference = function(reference, direction = 2, center = TRUE) {
	if (is.matrix(reference) && center) {
		refC =  matrixCenter(reference, direction);
		reference = matrixDeCenter(refC$matrix, mean(refC$center), direction);
	}
	ref = na.omit(as.vector(as.matrix(reference)));
	ref
}

Skewness = function(x, na.rm = T) {
	x0 = if(na.rm) na.omit(x) else x;
    N = length(x0);
    x1 = x0 - mean(x0)
    y = sqrt(N) * sum(x1^3) / (sum(x1^2)^(3/2))
    s = y * ((1 - 1/N))^(3/2);
    s
}
Noutliers = function(x, coef = 1.5)length(boxplot.stats(x, coef = coef)$out)

#' Quantile normalization of frame/matrix with respect to reference distribution
#'
#' Distribution to be normalized are represented as columns or rows of a matrix/data frame.
#' Each value is replaced by the quantile of the reference distribution as given by the value of the
#' empirical distribution function of the given value.
#'
#' @param reference numeric vector with realizations from the target distribution
#' @param data data frame or matrix with data to be normalized
#' @param direction is \code{data} organized per row or column?
#'
#' @examples
#' d = sapply(1:20, rnorm(1e4));
#' dNorm = quantileNormalization(as.vector(d), d)
quantileNormalization = function(reference, data, direction = 2,
	impute = TRUE, ties = 'random', center = TRUE, referenceDirection = direction) {
	ref = quantileReference(reference, referenceDirection, center);
	if (impute) mns = apply(data, 3 - direction, median, na.rm = T);
	dN = apply(data, direction, function(d) {
		d0 = d;
		if (impute) d[is.na(d0)] = mns[is.na(d0)];
		r = quantile(ref, probs = rank(d, na.last = 'keep', ties = ties)/length(na.omit(d)))
		if (impute) r[is.na(d0)] = NA;
		r
	});
	if (direction == 1) dN = t(dN);
	dimnames(dN) = dimnames(data);
	dN
}

# quantile normalization based on samples picked on the basis of their medians (around the medians)
# Nqn: number of reference samples
quantileNormalizationMedians = function(data, direction = 2, Nqn = 5, impute = TRUE) {
	# <p> determine median of medians, corresponding median, IQR
	medians = apply(data, direction, median);
	mediansO = order(medians);
	medianOI = as.integer(length(mediansO)/2 + .5);
	medianI = mediansO[medianOI];
	refMed = summary(data[, medianI]);
	refIQR = refMed[['3rd Qu.']] - refMed[['1st Qu.']];

	# <p> reference samples
	refL = as.integer(medianOI - Nqn/2 + .5);
	refU = refL + Nqn - 1;
	refSamples = mediansO[refL:refU];
	#print(refSamples)

	# <p> standardize reference samples wrt median, IQR
	refSampleValues = sapply(refSamples, function(i) {
		refI = summary(data[, i]);
		refIIQR = refI[['3rd Qu.']] - refI[['1st Qu.']];
		E = (data[, i] - refI[['Median']]) * refIQR/refIIQR + refMed[['Median']];
		#refIE = summary(E);
		#refIEIQR = refIE[['3rd Qu.']] - refIE[['1st Qu.']];
		#print(list(refI = refI, refIIQR = refIIQR, refIE = refIE, refIEIQR = refIEIQR));
		E
	});
	eQn = quantileNormalization(refSampleValues, data,
		direction = direction, impute = impute, center = FALSE);
	eQn
}

dataCentered = function(d, na.rm = T) {
	dC = apply(d, 2, function(col)col - mean(col, na.rm = na.rm));
	dC
}

#
#	<p> distributions
#

qtruncnorm = function(p, mean = 0, sd = 1, lower = -Inf, upper = Inf) {
	plow = pnorm(lower, mean, sd);
	pupp = pnorm(upper, mean, sd);
	qnorm(p * (pupp - plow) + plow, mean, sd = sd)
}
rtruncnorm = function(N, mean = 0, sd = 1, lower = -Inf, upper = Inf) {
	qtruncnorm(runif(N), mean, sd = sd, lower, upper)
}

qqDist = function(Nqts = 1e2, qdist, ...) {
	qtls = (1:Nqtls)/(Nqtls + 1);
	qtlsExp = qdist(qtls, ...);
	qtlsObs = quantile(rtn, qtls);
	qq = qplot(qtlsExp, qtlsObs) + theme_bw();
	qq
}

qqSim = function(Nsim, dist = 'truncnorm', Nqts = Nsim/10, ...) {
	require('ggplot2');
	rdist = get(Sprintf('r%{dist}s'));
	r = rdist(Nsim, ...);
	qdist = get(Sprintf('q%{dist}s'));
	qq = qqDist(Nqts, qdist, ...);
	qq
}

#
#	<p> entropy
#

table.entropy = function(d) {
	p = table.freq(d);
	p = p[p != 0];
	H = - sum(p * log(p));
}

#
#	<p> qvalue
#

Qvalue = function(P.value, ...) {
	require('qvalue');
	P.valuesNotNA = na.omit(P.value);
	qv = qvalue(P.valuesNotNA, ...);
	r = qv;
	r$qvalue = vector.embed(rep(NA, sum(is.na(P.value))), which(!is.na(P.value)), qv$qvalue);
	r
}

#
#	<p> math functions
#

Ceiling = function(x, N = 0)(ceiling(x * 10^N) / 10^N)
Floor = function(x, N = 0)(floor(x * 10^N) / 10^N)

#
#	Rpatches.R
#Fri Nov 20 17:18:37 CET 2009

# geepack patch

anovageePrim2 = function (m1, m2, ...)
{
    mm1 <- model.matrix(m1)
    mm2 <- model.matrix(m2)
    P1 <- mm1 %*% solve(t(mm1) %*% mm1) %*% t(mm1)
    P2 <- mm2 %*% solve(t(mm2) %*% mm2) %*% t(mm2)
    e2 <- mm2 - P1 %*% mm2
    e1 <- mm1 - P2 %*% mm1
    m2inm1 <- all(apply(e2, 2, var) < 1e-10)
    m1inm2 <- all(apply(e1, 2, var) < 1e-10)
    if (!any(c(m2inm1, m1inm2)))
        cat("Models not nested\n")
    else if (all(c(m2inm1, m1inm2)))
        cat("Models are identical\n")
    else {
        if (m1inm2) {
            tmp <- m1
            m1 <- m2
            m2 <- tmp
        }
        mm1 <- model.matrix(m1)
        mm2 <- model.matrix(m2)
        mf1 <- paste(paste(formula(m1))[c(2, 1, 3)], collapse = " ")
        mf2 <- paste(paste(formula(m2))[c(2, 1, 3)], collapse = " ")
        mm <- cbind(mm2, mm1)
        qmm <- qr(mm)
        qmmq <- qr.Q(qmm)
        nymm1 <- as.data.frame(qmmq[, 1:qmm$rank])
        colnames(nymm1) <- paste("parm", 1:ncol(nymm1), sep = ".")
        nymm2 <- nymm1[, 1:ncol(mm2), drop = FALSE]
        formula1 <- formula(paste(formula(m1)[[2]], formula(m1)[[1]],
            paste(c("-1", colnames(nymm1)), collapse = "+"),
            collapse = ""))
        m1call <- m1$call
        nymm1[, paste(formula(m1)[[2]])] <- m1$y
        nymm1[, paste(m1call$id)] <- m1$id
        m1call$offset <- m1$offset
        m1call$weights <- m1$weights
        m1call$formula <- formula1
        m1call$data <- nymm1
        m1ny <- eval(m1call)
        beta <- coef(m1ny)
        vbeta <- summary(m1ny)$cov.unscaled
        df <- dim(mm1)[2] - dim(mm2)[2]
        rbeta <- rep(1, length(beta))
        rbeta[1:df] <- 0
        beta0 <- rev(rbeta)
        zeroidx <- beta0 == 0
        X2 <- t(beta[zeroidx]) %*% solve(vbeta[zeroidx, zeroidx,
            drop = FALSE]) %*% beta[zeroidx]
        topnote <- paste("Model 1", mf1, "\nModel 2", mf2)
        title <- "Analysis of 'Wald statistic' Table\n"
        table <- data.frame(Df = df, X2 = X2, p = 1 - pchisq(X2,
            df))
        dimnames(table) <- list("1", c("Df", "X2", "P(>|Chi|)"))
        val <- structure(table, heading = c(title, topnote),
            class = c("anova", "data.frame"))
        return(val)
    }
}
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


#
#	RpropertyList.R
#Fri Jan  7 17:40:12 2011

# wrap string for property list
ws = function(s) {
	s = if (length(grep('^([_/\\a-zA-Z0-9.]+)$', s)) > 0) { s } else {
		s = gsub('([\\"])', '\\\\\\1', s);
		sprintf('"%s"', s);
	}
	s
}

# can a string be condensed into a single line
condense = function(s, ident, o) {
	if (nchar(s) + ident * o$tabWidth - nchar(grep("\t", s)) < o$screenWidth) {
		s = gsub("\n", ' ', s);
		s = gsub("\t", '', s);
	}
	s
}

stringFromPropertyI = function(obj, ident, o) {
	str = '';
	inS = join(rep("\t", ident), '');
	in1S = join(rep("\t", ident + 1), '');

	if ( class(obj) == 'function' ) {
		str = sprintf('%s%s', str, ws(join(deparse(obj), "\n")))
	} else if ( class(obj) != 'list' & length(obj) == 1 & !(o$kp %in% o$forceVectors)) {
		# <i> data support
		str = sprintf('%s%s', str, ws(obj));
	} else if (class(obj) == 'list' && !is.null(names(obj))) {
		hash = sprintf("{\n%s%s;\n%s}", in1S, paste(sapply(names(obj), function(k) {
			o = merge.lists(o, list(kp = sprintf('%s.%s', o$kp, k)));
			r = sprintf('%s = %s', ws(k), stringFromPropertyI(obj[[k]], ident+1, o))
			r
		}), collapse = sprintf(";\n%s", in1S)), inS);
		if (!o$noFormatting) hash = condense(hash, ident, o);
		str = sprintf('%s%s', str, hash);
	} else { # vector or anonymous list
		obj = as.list(obj);
		array = sprintf("(\n%s%s\n%s)", in1S, if (length(obj) < 1) '' else paste(
			sapply(1:length(obj), function(i) {
			e = obj[[i]];
			o = merge.lists(o, list(kp = sprintf('%s.[%d]', o$kp, i)));
			stringFromPropertyI(e, ident+1, o)
		}), collapse = sprintf(",\n%s", in1S)), inS);
		if (!o$noFormatting) array = condense(array, ident, o);
		str = sprintf('%s%s', str, array);
	}
	str
}

defaults = list(screenWidth = 80, tabWidth = 4, noFormatting = F, kp = '');
stringFromProperty = function(obj, o = list()) {
	o = merge.lists(defaults, o);
	s = stringFromPropertyI(obj, 0, o);
	if (o$noFormatting) {
		s = gsub("[\n\t]", '', s);
	}
	s
}

# tokens: character vector of tokens
# ti: current token cursor (token index)
propertyFromStringRaw = function(tokens, ti = 1) {
	if (length(tokens) < 1) stop("propertyFromString: out of tokens");
	pl = if (tokens[ti] == '(') {	# we have an array here 	# ')' (bracket)
		a = NULL;
		repeat {
			ti = ti + 1;	# advance to next token
			if (ti > length(tokens) || tokens[ti] == ')') break;	# <A> empty list
			r = propertyFromStringRaw(tokens, ti);	# sub propertyList
			if (is.list(r$pl)) r$pl = list(r$pl);	# <A> concatanating of lists
			a = c(a, r$pl);
			ti = r$ti + 1;
			if (ti > length(tokens) || tokens[ti] == ')') break;	# <A> returning to list end
			if (tokens[ti] != ',') stop("propertyFromString: expected ',' or ')'");
		}
		if (ti > length(tokens) || tokens[ti] != ')') stop("propertyFromString: no array termination");
		a
	} else if (tokens[ti] == '{') {
		dict = list();
		repeat {
			ti = ti + 1;	# advance to next token
			if (ti > length(tokens) || tokens[ti] == '}') break;
			key = tokens[ti];
			if (tokens[ti + 1] != '=') stop("propertyFromString: expected '='");
			r = propertyFromStringRaw(tokens, ti + 2);
			dict[[key]] = r$pl;
			ti = r$ti + 1;
			if (tokens[ti] != ';') stop("propertyFromString: expected ';'");
		}
		if (ti > length(tokens) || tokens[ti] != '}') stop("propertyFromString: no dict termination");;
		dict
	#} elsif ($token =~ /^<(.*)>$/so) {		# we encountered data
	# <N> data not supported
	} else {	# string
		s = tokens[ti];
		if (substr(s, 1, 1) == '"') s = substr(s, 2, nchar(s) - 1);
		s
	}
	r = list(pl = pl, ti = ti);
	r
}

plStringRE = '(?:(?:[_\\/\\-a-zA-Z0-9.]+)|(?:\"(?:(?:\\\\.)*(?:[^"\\\\]+(?:\\\\.)*)*)\"))';
plCommentRE = '(?:/\\*(?:.*?)\\*/)';

propertyFromString = function(plistString, o = list()) {
	plistString = gsub(plCommentRE, '', plistString, perl = T);
	tokens = fetchRegexpr(sprintf('%s|[(]|[)]|[{]|[}]|[=]|[,]|[;]|<.*?>', plStringRE), plistString);
	pl = propertyFromStringRaw(tokens);
	pl$pl
}
#
#	Rlinux.R
#Tue May  8 18:05:44 2012

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
