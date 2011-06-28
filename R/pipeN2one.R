#
#	pipeN2one.R
#Mon Jun 27 18:11:13 CEST 2011

# Expample:
#cd ~/tmp/pipeline/test_imputation
#RSCRIPTS=~/src/pipeline/R R.pl -- pipeN2one.R --convertN2one --source pipeN2oneTest.R --callFunction myfunction imputation_04/files.spec

source('RgenericAll.R');


convertN2one = function(input, o) {
	# <p> read input files.spec
	i = propertyFromString(readFile(input));
	# <p> source input script
	script = file.locate(o$source, prefixes = splitString(':', Sys.getenv('RSCRIPTS')));
	Log(sprintf("Sourcing script @ %s\n", script), 5);
	source(script, chdir = T);
	# <p> call function
print(list.kp(i$files, 'name', do.unlist = T));
print(is.list(i));
	do.call(get(o$callFunction), list(i$files, o));
}
.globalTriggers = list(convertN2one = convertN2one);
