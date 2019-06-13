#
#	Rlibraries.R
#Wed Oct 31 19:00:40 CET 2012

loadLibraries = function() {
	Require('geepack');
	Require('glmnet');
	Require('ggplot2'); if (exists('theme_set')) theme_set(theme_bw());
	Require('grid');
	Require('magrittr');
	Require('set');
}
