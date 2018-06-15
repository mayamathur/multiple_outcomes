# Overview

This repository contains all code required to reproduce analyses in: 

>Mathur MB & VanderWeele TJ. XXX. 

# How to re-run the simulation study from scratch

Simulation scripts are parallelized and were run on a SLURM cluster. All files required to reproduce the simulation study are in R code/For Sherlock:

- **functions.R** contains helper functions that can be run locally.  

- **doParallel.R** runs a parallelized simulation study. Specifically, it runs sim.reps=10 simulation reps (each with B=2000 bootstrap iterates) on each computing node. Each scenario has 1000 total simulation reps that were spread across multiple sbatch files. For each simulation rep, doParallel.R generates an original dataset, resamples according to a user-specified algorithm, calls functions from functions.R to conduct analyses, and aggregates the joint test and null interval results. Output consists of two types of files: "Short" results files have one row per simulation rep (that aggregates over all B=2000 bootstraps), so should each have sim.reps=5 rows. "Long" results files have a row for every bootstrap rep.

- **gen_sbatch.R** automatically generates the sbatch files based on user-specified simulation parameters (from which gen_sbatch.R produces scen_params.csv, called later by doParallel.R). This script is specialized for our cluster and would likely need to be rewritten for other computing systems. 

- **stitch_on_sherlock.R** takes output files written by doParallel.R and stitches them into a single file, stitched.csv, that is used for analysis. 

- **analysis.R** uses stitched.csv and the scenario parameters, scen_params.csv, to create the plots and stats reported for the simulation study. 

Additional files that are not necessary to re-run the simulation study: 

- **choose_scen_params.R** helps choose reasonable scenario parameters using theoretical calculations for the independent case. 

- **push_to_sherlock.txt** contains Unix commands to move local files to and from our cluster computing system. This file is highly specific to our file paths and cluster. 



# How to reproduce simulation results

Simulation scripts are parallelized and were run on a SLURM cluster. All files required to reproduce the simulation study are in R code/For Sherlock:

- functions.R contains helper functions all of which can be run locally.  

- doParallel.R runs a parallelized simulation study. Specifically, it runs sim.reps=10 simulation reps (each with B=2000 bootstrap iterates) on each computing node. Each scenario has 1000 total simulation reps that were spread across multiple sbatch files. It outputs two types of files: "Short" results files have one row per simulation rep (that aggregates over all B=2000 bootstraps), so should each have sim.reps=5 rows. "Long" results files have a row for every bootstrap rep.

- gen_sbatch.R automatically generates the sbatch files 

- analysis.R

 

# How to reproduce the applied example

