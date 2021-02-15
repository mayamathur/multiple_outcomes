

########################### SET SIMULATION PARAMETERS MATRIX ###########################

# FOR CLUSTER USE
path = "/share/PI/manishad/multTest"
setwd(path)

n = 1e5
nX = 1
nY = 30
rho.XX = 0
rho.YY = c(0)
rho.XY = c(0.005)  # null hypothesis: 0
subsamp.ns = "c(400, 600, 900, 1350)"

# bootstrap iterates and type
#boot.reps = 2000
boot.reps = 20000
bt.type = c( "subsample" )  # fcr: resample under HA; resid: resample under H0


# matrix of scenario parameters
scen.params = expand.grid( bt.type, n, nX, nY, rho.XX, rho.YY, rho.XY, subsamp.ns )
names(scen.params) = c( "bt.type", "n", "nX", "nY", "rho.XX", "rho.YY", "rho.XY", "subsamp.ns" )

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
# n.reps.per.scen = 1000
# n.reps.in.doParallel = 10
n.reps.per.scen = 500
n.reps.in.doParallel = 1 # and make sure to actually change it in doParallel!
n.files = ( n.reps.per.scen / n.reps.in.doParallel ) * n.scen


path = "/share/PI/manishad/multTest"

scen.name = rep( scen.params$scen.name, each = ( n.files / n.scen ) )
jobname = paste("job", 1:n.files, sep="_")
outfile = paste("rm_", 1:n.files, ".out", sep="")
errorfile = paste("rm_", 1:n.files, ".err", sep="")
write_path = paste(path, "/sbatch_files/", 1:n.files, ".sbatch", sep="")
runfile_path = paste(path, "/testRunFile.R", sep="")

# used 5:00 limit for bootstrapping with B = 2000 and j=10; this was perfect
# 8:00 for B = 20,000 and j=1
sbatch_params <- data.frame(jobname,
                            outfile,
                            errorfile,
                            jobtime = "2:00:00",
                            quality = "normal",
                            node_number = 1,
                            mem_per_node = 64000,
                            mailtype =  "NONE",
                            user_email = "mmathur@stanford.edu",
                            tasks_per_node = 16,
                            cpus_per_task = 1,
                            path_to_r_script = paste(path, "/doParallel_subsample.R", sep=""),
                            args_to_r_script = paste("--args", jobname, scen.name, boot.reps, sep=" "),
                            write_path,
                            stringsAsFactors = FALSE,
                            server_sbatch_path = NA)

generateSbatch(sbatch_params, runfile_path)


# run them all
# works
setwd( paste(path, "/sbatch_files", sep="") )
for (i in 1:n.files) {
  system( paste("sbatch -p normal,owners /share/PI/manishad/multTest/sbatch_files/", i, ".sbatch", sep="") )
}
