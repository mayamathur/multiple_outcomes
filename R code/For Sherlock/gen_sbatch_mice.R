

########################### SET SIMULATION PARAMETERS MATRIX ###########################

# FOR CLUSTER USE
path = "/share/PI/manishad/multTest"
setwd(path)

# FOR LOCAL USE
# path = "~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Sandbox/2018-1-13"
# setwd(path)

n = 1000
nX = 1
nY = 40
rho.XX = 0
rho.YY = c(0.25, 0.5, 0)
rho.XY = c(0.02, 0.05, 0) # null hypothesis: 0
half = c(0, 1)  # exchangeable vs. half-correlated matrix

# bootstrap iterates and type
boot.reps = 2000
bt.type = c( "MICE.H0" )

# matrix of scenario parameters
scen.params = expand.grid( bt.type, n, nX, nY, rho.XX, rho.YY, rho.XY, half )
names(scen.params) = c( "bt.type", "n", "nX", "nY", "rho.XX", "rho.YY", "rho.XY", "half" )


# name the scenarios
# remove letters that are privileged variables in R
letter.names = c(letters, LETTERS)[ ! c(letters, LETTERS) %in% c("i","T","F") ]
scen.params$scen.name = letter.names[ 1:dim(scen.params)[1] ]
n.scen = length(scen.params[,1])

# we don't need scenarios with rho.XY = 0 and half = 1 because redundant with 
# scenarios where rho.XY = 0 and half = 0
scen.to.toss = scen.params$scen.name[ scen.params$rho.XY == 0 & scen.params$half == 1 ]

scen.params = scen.params[ ! scen.params$scen.name %in% scen.to.toss, ]

# write the csv file of params (to Sherlock)
write.csv( scen.params, "scen_params.csv" )


########################### GENERATE SBATCHES ###########################

# load functions for generating sbatch files
source("functions.R")


# number of sbatches to generate (i.e., iterations within each scenario)
n.reps.per.scen = 500
n.reps.in.doParallel = 5
n.files = ( n.reps.per.scen / n.reps.in.doParallel ) * n.scen


path = "/share/PI/manishad/multTest"

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
                            path_to_r_script = paste(path, "/doParallel_mice.R", sep=""),
                            args_to_r_script = paste("--args", jobname, scen.name, boot.reps, sep=" "),
                            write_path,
                            stringsAsFactors = F,
                            server_sbatch_path = NA)

generateSbatch(sbatch_params, runfile_path)


# run them all
# works
setwd( paste(path, "/sbatch_files", sep="") )
for (i in 1:1) {
  system( paste("sbatch -p normal,owners /share/PI/manishad/multTest/sbatch_files/", i, ".sbatch", sep="") )
}

