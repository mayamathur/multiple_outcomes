# Overview

This repository contains all code required to reproduce analyses in: 

>Mathur MB & VanderWeele TJ. XXX. 

# How to reproduce simulation results

Simulation scripts are parallelized and were run on a SLURM cluster. All files required to reproduce the simulation study are in R code/For Sherlock:

- functions.R contains helper functions all of which can be run locally.  

- doParallel.R runs a parallelized simulation study. Specifically, it runs sim.reps=10 simulation reps (each with B=2000 bootstrap iterates) on each computing node. Each scenario has 1000 total simulation reps that were spread across multiple sbatch files. It outputs two types of files: "Short" results files have one row per simulation rep (that aggregates over all B=2000 bootstraps), so should each have sim.reps=5 rows. "Long" results files have a row for every bootstrap rep.

- gen_sbatch.R automatically generates the sbatch files 

- analysis.R

 

# How to reproduce the applied example

