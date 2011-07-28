#
#	impute2sqlite.R
#

require('IRanges');
require('RSQLite');
require('snpMatrix');
require('seqinr');
source('RgenericAll.R');
source('allClasses.GVARdb.R');
source('GVARdb-class.GVARdb.R');
source('impGWASdb-class.GVARdb.R');

# <p> debugging code
#RSCRIPTS=~/src/pipeline/R R.pl --logLevel 6  --no-quiet -- pipeN2one.R --convertN2one --source impute2sqlite.R --o imputation_04/imputation_04/LLS_Offspring_Partnerschr21_chr21_imputed-1 --callFunction impute2sqlite imputation_04/files.spec

impute2sqlite = function(input, o) {
	print('welcome to goldrunner');
	#print(o);
	files = sapply(list.kp(input, 'name'), function(s)sprintf('%s.gens', s));
	files0 = files[file.exists(files)];

	options(stringsAsFactors=F);
	create.impGWASdb.from.rens(
		files0,
		splitPath(o$o)$file, splitPath(o$o)$dir,
		parseChrom=F, mapSource="", mapVersion="", quiet=F);
}
