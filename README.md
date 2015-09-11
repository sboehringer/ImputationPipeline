Imputation Pipeline
-------------------------------
This is a workflow manager implemented for OGS to run big parallel computations with dependencies. Its main purpose is to run genotype imputations for Genome Wide Association Studies and ensuing tests for genetic association.
This is an internal project developed at Medical Statistics and Bioinformatics (MedStat).

## Main developer & Maintainer
 * Stefan Böhringer

## Contributors

 * Quinta Helmer
 * Joris Deelen
 * Ramin Monajemi
 * Matthijs Moed

Using the pipeline
--------------------

Log into shark.
```
git clone https://git.lumc.nl/s.boehringer/ImputationPipeline.git pipeline --branch 1.0.shapeit
```

Add the following line to your ~/.bashrc and re-login.

```
. ~/pipeline/bin/pipeline_profile
```

If you clone to a different folder adapt the lines above. After this you can use the pipeline. 
```
pipeline.pl --help
pipeline.pl mypipeline.pipe
```
Inspect the templates subfolder for example or get personal help.


Getting Help
--------------------

For a GWAS project please contact Joris Deelen (j.deelen@lumc.nl).
For technical help please contact Stefan Böhringer (s.boehringer@lumc.nl).