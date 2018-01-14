# Read-Me

doParallel.R runs sim.reps=10 simulation reps on each node. Each scenario has 1000 total simulation reps spread across multiple sbatch files. 

"Short" results files have one row per simulation rep (that aggregates over all B=2000 bootstraps), so should each have sim.reps=10 rows. "Long" results files have a row for every bootstrap rep. 