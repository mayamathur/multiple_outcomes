

########################### SET SIMULATION PARAMETERS MATRIX ###########################

# FOR CLUSTER USE
path = "/share/PI/manishad/multTest"

# FOR LOCAL USE
path = "~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Sandbox/2018-1-13"

setwd(path)

# from previous simulation
# n = 1000
# nX = 1
# nY = 100
# rho.XX = 0
# rho.YY = c(0, 0.25, 0.5, 0.75)
# rho.XY = 0.03  # null hypothesis: 0

n = 50
nX = 1
nY = 100
rho.XX = 0
rho.YY = c(0, 0.25, 0.5, 0.75)
rho.XY = 0.03  # null hypothesis: 0

# bootstrap iterates and type
# boot.reps = 2000
boot.reps = 5
bt.type = c( "fcr", "resid" )  # fcr: resample under HA; resid: resample under H0


# matrix of scenario parameters
scen.params = expand.grid( bt.type, n, nX, nY, rho.XX, rho.YY, rho.XY )
names(scen.params) = c( "bt.type", "n", "nX", "nY", "rho.XX", "rho.YY", "rho.XY" )

# name the scenarios
# remove letters that are privileged variables in R
letter.names = c(letters, LETTERS)[ ! c(letters, LETTERS) %in% c("i","T","F") ]
scen.params$scen.name = letter.names[ 1:dim(scen.params)[1] ]
n.scen = length(scen.params[,1])

# write the csv file of params (to Sherlock)
write.csv( scen.params, "scen_params.csv" )


########################### FN: SIMULATE 1 DATASET ###########################

# load functions for generating sbatch files
source("functions.R")


# number of sbatches to generate (i.e., iterations within each scenario)
n.reps.per.scen = 1000
n.reps.in.doParallel = 10
n.files = ( n.reps.per.scen / n.reps.in.doParallel ) * n.scen


path = "/share/PI/manishad/multTest"

scen.name = rep( scen.params$scen.name, each = ( n.files / n.scen ) )
jobname = paste("job", 1:n.files, sep="_")
outfile = paste("rm_", 1:n.files, ".out", sep="")
errorfile = paste("rm_", 1:n.files, ".err", sep="")
write_path = paste(path, "/sbatch_files/", 1:n.files, ".sbatch", sep="")
runfile_path = paste(path, "/testRunFile.R", sep="")


sbatch_params <- data.frame(jobname,
                            outfile,
                            errorfile,
                            jobtime = "5:00:00",
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

# generateSbatch(sbatch_params, runfile_path)


# run them all
#works
setwd( paste(path, "/sbatch_files", sep="") )
for (i in 1:n.files) {
  system( paste("sbatch -p normal,owners /share/PI/manishad/multTest/sbatch_files/", i, ".sbatch", sep="") )
}



########################### AFTER SIMULATION IS DONE ###########################

path = "/share/PI/manishad/multTest"
setwd(path)

source("functions.R")
stitch_files(.results.singles.path = "/share/PI/manishad/multTest/sim_results",
                        .name.prefix = "results",
                        .stitch.file.name="stitched_results.csv")

# move stitched files to desktop
# scp mmathur@sherlock:/share/PI/manishad/multTest/sim_results/stitched_results.csv ~/Desktop

# move all files to desktop for local stitching
# scp mmathur@sherlock:/share/PI/manishad/multTest/sim_results/* ~/Desktop/sim_results

# check overall progress
setwd("~/Desktop")
setwd("/share/PI/manishad/multTest/sim_results") # from within Sherlock
s = read.csv("stitched_results.csv")

# percent finished
dim(s)[1] / 1000

# which scenarios are done?
table(s$scen)


# get the job files that have NOT run
job.files.all = paste("/share/PI/manishad/multTest/sim_results/results_job_", 1:1000, "_.csv", sep="")
jobs.not.run = job.files.all[ ! job.files.all %in% as.character(s$job.file) ]

# just the numbers of the jobs that have NOT run
matches = regmatches( jobs.not.run, gregexpr("[[:digit:]]+", jobs.not.run ) )  # extracts just the numeric part of string
job.nums.not.run = as.numeric(unlist(matches))


# run the ones that haven't run yet
setwd( paste(path, "/sbatch_files", sep="") )
for (i in job.nums.not.run) {
  system( paste("sbatch /share/PI/manishad/multTest/sbatch_files/", i, ".sbatch", sep="") )
}

