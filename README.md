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

Often it is required to install ```Module::Load`` manually.

```
cpanm Module::Load
```

Getting Help
--------------------

The bug-tracker of the repository can be used for bug-reporting. No other regular support is offered at this moment.
