Imputation Pipeline
-------------------------------
This is a workflow manager implemented for OGS to run big parallel computations with dependencies. Its main purpose is to run genotype imputations for Genome Wide Association Studies and ensuing tests for genetic association.
This is an internal project developed at Medical Statistics and Bioinformatics (MedStat).
Main developer is Stefan Böhringer.

Contributions by

 * Quinta Helmer
 * Joris Deelen
 * Ramin Monajemi
 * Mathijs Moed

Using the pipeline
--------------------

Add the following lines to your ~/.bashrc

{{{
. path_to_pipeline_folder/pipeline_profile

}}}

If you clone into your home folder you can add the following to your .bashrc to use the pipeline
{{{
. ~/pipeline/bin/pipeline_profile
}}}