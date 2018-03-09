
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
(nrow(s)/5) / 1500

# move it to Desktop
# scp mmathur@sherlock:/share/PI/manishad/multTest/sim_results/stitched.csv ~/Desktop




# #### Sneak-Peek Analyes
# mean(s$covers.raw)
# mean(s$covers)
# mean(s$covers.correct)
# 
# summary(s$rate)
# 
# # symmetry of CIs
# mean( abs( s$ci.hi.raw - s$excess ) - abs( s$ci.lo.raw - s$excess ) )
# mean( abs( s$ci.hi.correct - s$excess ) - abs( s$ci.lo.correct - s$excess ) )
# 
# 
# 
# 
# 
# 
# # zip the entire results folder
# setwd("/share/PI/manishad/multTest")
# system( paste("zip -r sim_results.zip sim_results") )
# 
# 
# ######## FOR STITCHING SHORT FILES ########
# source("functions.R")
# s = stitch_files(.results.singles.path = "/share/PI/manishad/multTest/sim_results/short",
#                  .results.stitched.write.path = "/share/PI/manishad/multTest/sim_results",
#              .name.prefix = "short",
#              .stitch.file.name="stitched.csv")
# 
# # zip the entire results folder
# setwd("/share/PI/manishad/multTest")
# system( paste("zip -r sim_results.zip sim_results") )
# 
# 
# 
