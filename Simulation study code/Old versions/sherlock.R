
# goal: each time we have 16 workers, 


######################### RUN IN PARALLEL #########################

# load command line arguments
args = commandArgs(trailingOnly = TRUE)
scen = args[1]

# get simulation parameters from csv file
scen.params = read.csv("scen_params.csv")
p = scen.params[ scen.params$scen.name == scen, ]

# load required libraries
require(plyr)
require(doSNOW)
require(car)

#set up parallelization backend
cl = makeCluster(n_clusters)
registerDoSNOW(cl)

#these should be equal
n_clusters
getDoParWorkers()

#give each worker an ID 
#this is optional, but occasionally is useful 
#in situations where you need workers to write data
#every worker writing to the same file will result in 
#uncaught errors and partially overwritten data
#worker ids enable each worker to write to its own set of 
#files
clusterApply(cl, seq(along=cl), function(id) WORKER.ID <<- paste0("worker_", id))


time = system.time({
  
  l_ply( c( 1:getDoParWorkers() ), .parallel=TRUE, function(.item) {  
    #l_ply iterates through each element of its first argument, here c(1:1000), and passes each element to
    #function() as .item    
    #instead of c(1:1000), you could pass l_ply() a list where each element is a set of subject specific means
    #then each worker/iteration would expand those means into a full set of data for that subject
    #avoid workers writing to the same file
    #if each worker/iteration generates data for one subject, you will also need to write code that stitches
    #all subject data together into one data file.
    
    #everything in here will be performed by workers in parallel    
    #packages and source functions used by workers should be loaded within the this block
    library(mvtnorm)
    
    setwd("/share/PI/manishad/multTest")
    source("functions.R", local=TRUE)
    
    # DELETE THIS - testing only
    #write.csv(.n.Subj, "n.Subj.csv")
    #  write.csv(cat.parameters, "cat.parameters.csv")
    
    # simulate results
    setwd("/share/PI/manishad/genCov/datasets")
    
    
    sim_scenario( .scen=s, .reps=reps, .boot.reps = boot.reps )
    rs$scen.name = s
    
    # merge in scenario parameters
    rs2 = merge(p, rs)
    
    setwd("/share/PI/manishad/multTest/sim_results")
    write.csv( rs2, paste(Sys.Date(), "_scen_", s, ".csv", sep="") )
  } )
  
})



# write a file about the simulation parameters
write(
  x = paste("There were ", getDoParWorkers(), " workers",
            "\nDatasets were generated with n=", n.Subj, ", obs=", obs, ", n.Reps=",
            n.Reps, ", n.Drugs=", n.Drugs,
            "\nThe entire process took ", time[3]/(60*60), " hours",
            sep="" )
  , file= paste( name_prefix, "simulation_info.txt", sep="_" )
)


