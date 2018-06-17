
# run this interactively in ml load R

path = "/home/groups/manishad/multTest"
setwd(path)
source("functions.R")

######## FOR STITCHING LONG FILES ########

s = stitch_files(.results.singles.path = "/home/groups/manishad/multTest/sim_results/short",
                 .results.stitched.write.path = "/home/groups/manishad/multTest/sim_results/overall_stitched",
                 .name.prefix = "short",
                 .stitch.file.name="stitched.csv")

# how close are we to being done?
n.reps.per.scen = 500
nrow(s) / (n.reps.per.scen * nrow(scen.params))
table(s$scen)

# quick check
mean(s$jt.pval.0.05)
mean(s$rej.jt.0.05)
mean(s$jt.rej.Romano)
mean(s$jt.rej.Wstep)

# move it to Desktop
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/multTest/sim_results/overall_stitched/stitched.csv ~/Desktop
Vegemite2017
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/multTest/scen_params.csv ~/Desktop
Vegemite2017

