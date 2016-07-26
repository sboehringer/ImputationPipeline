#
#	gwas_imputation_scriptExp.R
#Wed Jul 17 13:39:37 2013

system("cd ~/src/Rprivate ; ./exportR.sh"); source("RgenericAll.R");
source('gwas_imputation.R', chdir = T);

pipe = '/home/pingu/Documents/Projekte/2013-07-colonCancerImp/cc.pipe';
if (1) {
	gwas_impuatation_summary(list(),
		'/home/pingu/tmp/gwas/2012-cc/cc-201307-impexport_chr1_imputed-1', pipe = pipe);
}

if (0) {
	print(pipelineParameter(pipe = pipe, 'G:Model'));
}


