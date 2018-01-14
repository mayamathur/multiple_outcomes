
# run this interactively in ml load R

path = "/share/PI/manishad/multTest"
setwd(path)

source("functions.R")
s = stitch_files(.results.singles.path = "/share/PI/manishad/multTest/sim_results/short",
                 .results.stitched.write.path = "/share/PI/manishad/multTest/sim_results",
             .name.prefix = "short",
             .stitch.file.name="stitched.csv")

# # write results
# write.csv( s, "stitched_prepped.csv" )
# 
# # zip the entire results folder
# setwd("/share/PI/manishad/multTest")
# system( paste("zip -r sim_results.zip sim_results") )
# 


