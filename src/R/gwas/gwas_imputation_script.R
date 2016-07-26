#
#	gwas_imputation_script.R
#Wed Jul 17 10:49:54 CEST 2013

source('gwas_imputation.R');

gwas_imputation_script_help = function(o, args) {
	cat('Usage: RSCRIPTS=~/src/Rscripts R.pl gwas/gwas_imputation_script.R --no-quiet -- --summarizeGwas [path]

	path: prefix of files (path, path.pvalues) 
');
}

summarizeGwas = function(args, o) {
	Log(sprintf('Summarizing GWAS results from pipeApplyR in path %s', args[1]), 2);
	gwas_impuatation_summary(o, args[[1]], outputDir = splitPath(o$output)$dir, pipe = 'diag/pipeline_parameters');
}

.globalTriggers = list(
	help = gwas_imputation_script_help,
	summarizeGwas = summarizeGwas
);
