
# run this interactively in ml load R

path = "/share/PI/manishad/multTest"
setwd(path)
source("functions.R")

######## FOR STITCHING LONG FILES ########

s = stitch_files(.results.singles.path = "/share/PI/manishad/multTest/sim_results/short",
                 .results.stitched.write.path = "/share/PI/manishad/multTest/sim_results",
                 .name.prefix = "short",
                 .stitch.file.name="stitched.csv")

# write results
write.csv( s, "stitched.csv" )

# how close are we to being done?
(nrow(s)/5) / 12000
table(s$scen)

# move it to Desktop
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/multTest/sim_results/overall_stitched/stitched.csv ~/Desktop
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/multTest/scen_params.csv ~/Desktop


