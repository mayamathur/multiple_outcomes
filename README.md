# Overview

This repository contains all code required to reproduce analyses in: 

>Mathur MB & VanderWeele TJ. New metrics for multiple testing with correlated outcomes. Under review. 

Address any inquiries to mmathur [AT] stanford [DOT] edu.

# How to re-run the simulation study from scratch

Simulation scripts are parallelized and were run on a SLURM cluster. All files required to re-run the simulation study are in R code/For Sherlock:

- [functions.R](https://osf.io/42zne/) contains helper functions that can be run locally, as well as unit tests for each function. 

- [doParallel.R](https://osf.io/4u5rs/) runs a parallelized simulation study. Specifically, it runs sim.reps=10 simulation reps (each with B=2000 bootstrap iterates) on each computing node. Each scenario has 1000 total simulation reps that were spread across multiple sbatch files. For each simulation rep, doParallel.R generates an original dataset, resamples according to a user-specified algorithm, calls functions from functions.R to conduct analyses, and aggregates the joint test and null interval results. Output consists of two types of files: "Short" results files have one row per simulation rep (that aggregates over all B=2000 bootstraps), so should each have sim.reps=10 rows. "Long" results files have a row for every bootstrap rep.

- [gen_sbatch.R](https://osf.io/txnz9/) automatically generates the sbatch files based on user-specified simulation parameters (from which gen_sbatch.R produces [scen_params.csv](https://osf.io/xqkvr/), called later by doParallel.R). This script is specialized for our cluster and would likely need to be rewritten for other computing systems. 

- [stitch_on_sherlock.R](https://osf.io/y8suf/) takes output files written by doParallel.R and stitches them into a single file, stitched.csv, that is used for analysis. 

- [analysis.R](https://osf.io/fp9rb/) uses stitched.csv and the scenario parameters, scen_params.csv, to create the plots and stats reported for the simulation study. 

Auxiliary files that are not necessary to re-run the simulation study: 

- [choose_scen_params.R](https://osf.io/7ak65/) helps choose reasonable scenario parameters using theoretical calculations for the independent case. 



# How to reproduce simulation analyses from our saved results

To reproduce the simulation analyses locally using our saved results ([stitched.csv](https://osf.io/tcgj9/)) without re-running the simulations themselves, run [analysis.R](https://osf.io/fp9rb/). This script produces the results in the main text as well as the extended versions in the Supplement. 

# How to reproduce the applied example

1. Optionally, to start from the raw data, download [the MIDUS 1 data](https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/2760) from ICSPR, which requires a simple, free registration process. There are several waves of MIDUS data, and we used those with the following study citation:

>Brim, Orville G., Baltes, Paul B., Bumpass, Larry L., Cleary, Paul D., Featherman, David L., Hazzard, William R., … Shweder, Richard A. Midlife in the United States (MIDUS 1), 1995-1996. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], 2017-11-16. https://doi.org/10.3886/ICPSR02760.v12


2. We performed the first stage of data prep in SAS. Run the SAS script called [Extract dataset for MAYA.sas](https://osf.io/c72ke/). This SAS script reproduces inclusion/exclusion criteria from Chen et al. (2018) and randomly selects one sibling from each sibship. Per ICSPR terms, we cannot make public the resulting prepped dataset, flourish_new.sas7bdat, but users who have registered with ICSPR can obtain it directly by emailing Maya Mathur (mmathur [AT] stanford [DOT] edu).

3. We performed the second stage of data prep in R. Run [data_prep_applied_example.R](https://osf.io/xnptw/), which does additional data cleaning to turn flourish_new.sas7bdat into the analysis-ready dataset, flourish_prepped.csv. Again, users who have registered with ICSPR can alternatively obtain this dataset directly by emailing Maya Mathur (mmathur [AT] stanford [DOT] edu). Note that this script makes additional subject exclusions (e.g., removing subjects with incomplete data). This script calls the helper functions in [helper_applied_example.R](https://osf.io/2tgh7/). A complete codebook for the analysis dataset is in [Analysis dataset codebook.xlsx](https://osf.io/p4db5/).


4. Run [analyses_applied_example.R](https://osf.io/2tgh7/), which uses the final prepped dataset, flourish_prepped.csv, to produce all plots, stats, and tables reported in the paper. 



# How to reproduce the p-value scatterplot in the Supplement

Run [plot_adjusted_pvalues.R](https://osf.io/gf8uh/).





