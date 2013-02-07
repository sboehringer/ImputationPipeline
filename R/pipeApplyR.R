#
#	pipeApplyR.R
#Wed Feb  6 11:37:54 2013

# Expample:
#cd ~/tmp/pipeline/test_imputation
#RSCRIPTS=~/src/pipeline/R R.pl -- pipeN2one.R --applyFunction --source pipeN2oneTest.R --callFunction myfunction imputation_04/files.spec

source('RgenericAll.R');


applyFunction = function(input, o) {
	# <p> source input script
	script = file.locate(o$source, prefixes = c(
		getwd(), splitString(':', Sys.getenv('RSCRIPTS'))
	));
	#cat(readFile(script));
	Log(sprintf("Sourcing script @ %s\n", script), 1);
	source(script, chdir = T);
	# <p> call function
	args = .List(o, min_ = c('source', 'args', 'callFunction', 'applyFunction', 'meta', 'output'));
	functionArgs = c(list(input = o$args, output = o$output), args);
	do.call(get(o$callFunction), functionArgs);
}
.globalTriggers = list(applyFunction = applyFunction);
