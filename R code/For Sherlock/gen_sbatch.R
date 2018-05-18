

########################### SET SIMULATION PARAMETERS MATRIX ###########################

# FOR CLUSTER USE
path = "/home/groups/manishad/multTest"
setwd(path)

# FOR LOCAL USE
# path = "~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Sandbox/2018-1-13"
# setwd(path)

n = 1000
nX = 1
nY = 40
rho.XX = 0
rho.YY = c(0.25, 0.5, 0)
rho.XY = c(0, 0.02, 0.03, 0.04, 0.05, 0.10) # null hypothesis: 0
prop.corr = c(0.5, 1)  # exchangeable vs. half-correlated matrix

# bootstrap iterates and type
boot.reps = 2000
bt.type = c( "ha.resid" )


# matrix of scenario parameters
scen.params = expand.grid( bt.type, n, nX, nY, rho.XX, rho.YY, rho.XY, prop.corr )

# give letters to scenarios
names(scen.params) = c( "bt.type", "n", "nX", "nY", "rho.XX", "rho.YY", "rho.XY", "prop.corr" )


######## Remove Some Scenarios ########
# for rhoXY = 0.10, only want info for confidence intervals
# so cut out the ha.resid ones
# scen.params = scen.params[ scen.params$rho.XY != 0.10 |
#                              scen.params$rho.XY == 0.10 & scen.params$bt.type == "resid", ]

# for rho.XY = 0, don't need to manipulate prop.corr because all 0 anyway
scen.params = scen.params[ scen.params$rho.XY != 0 |
                             scen.params$rho.XY == 0 & scen.params$prop.corr == 1, ]


######## Name the Scenarios ########
# if merging results with other simulations, set this to the last letter already used
#start.at = "a"

# remove letters that are privileged variables in R
# letter.names = c(letters, LETTERS, paste(letters, letters, sep="") )
# letter.names = letter.names[ ! c(letters, LETTERS) %in% c("i","T","F") ]  # exclude privileged characters
# letter.names = letter.names[ grep(start.at, letter.names): length(letter.names) ]
# 
# scen.params$scen.name = letter.names[ 1:dim(scen.params)[1] ]

scen.params$scen.name = 1:nrow(scen.params)
n.scen = length(scen.params[,1])

# write the csv file of params (to Sherlock)
write.csv( scen.params, "scen_params.csv" )


########################### GENERATE SBATCHES ###########################

# load functions for generating sbatch files
source("functions.R")


# number of sbatches to generate (i.e., iterations within each scenario)
n.reps.per.scen = 1000
n.reps.in.doParallel = 5
n.files = ( n.reps.per.scen / n.reps.in.doParallel ) * n.scen


path = "/home/groups/manishad/multTest"

scen.name = rep( scen.params$scen.name, each = ( n.files / n.scen ) )
jobname = paste("job", 1:n.files, sep="_")
outfile = paste("rm_", 1:n.files, ".out", sep="")
errorfile = paste("rm_", 1:n.files, ".err", sep="")
write_path = paste(path, "/sbatch_files/", 1:n.files, ".sbatch", sep="")
runfile_path = paste(path, "/testRunFile.R", sep="")

# was 5 hours with 2K bootstrap and n = 5e4
sbatch_params <- data.frame(jobname,
                            outfile,
                            errorfile,
                            jobtime = "0:30:00",
                            quality = "normal",
                            node_number = 1,
                            mem_per_node = 64000,
                            mailtype =  "NONE",
                            user_email = "mmathur@stanford.edu",
                            tasks_per_node = 16,
                            cpus_per_task = 1,
                            path_to_r_script = paste(path, "/doParallel.R", sep=""),
                            args_to_r_script = paste("--args", jobname, scen.name, boot.reps, sep=" "),
                            write_path,
                            stringsAsFactors = F,
                            server_sbatch_path = NA)

#generateSbatch(sbatch_params, runfile_path)

#5400 files

setwd( paste(path, "/sbatch_files", sep="") )
for (i in 1:1000) {
  system( paste("sbatch -p normal,owners /home/groups/manishad/multTest/sbatch_files/", i, ".sbatch", sep="") )
}


# ######## If Running Only Some Jobs To Fill Gaps ######## 
# 
# # run in Sherlock ml load R
# # sbatch_not_run( "$PI_HOME/multTest/sim_results/short", 
# #                 "$PI_HOME/multTest/sim_results",
# #                 .name.prefix = "short" )
# # scp mmathur@sherlock:$PI_HOME/multTest/sim_results/missed_job_nums.csv ~/Desktop
# 
# setwd("$PI_HOME/multTest/sim_results")
# missed.nums = read.csv("missed_job_nums.csv")$x
# 
# 
# setwd( paste(path, "/sbatch_files", sep="") )
# for (i in missed.nums) {
#   system( paste("sbatch -p normal,owners /home/groups/manishad/multTest/sbatch_files/", i, ".sbatch", sep="") )
# }













