
# when outputting all bootstrap iterates, this script aggregates each simulation rep's results

# get scenario parameters
path = "/share/PI/manishad/multTest"
setwd(path)
scen.params = read.csv("scen_params.csv")


# get parameters (ASSUMES ONLY 1 SCENARIO)
alpha = scen.params$alpha
nY = scen.params$nY
N0 = alpha * nY

# get individual simulation results
path = "/share/PI/manishad/multTest/sim_results"
setwd(path)

# get list of all files in folder
all.files = list.files(path, full.names=TRUE)

# we only want the ones whose name includes "results"
keepers = all.files[ grep( "results_job", all.files ) ]
n.files = length(keepers)

print(n.files)
print(head(keepers))

library(doParallel)
library(foreach)

# set the number of cores
registerDoParallel(cores=16)

parallel.time = system.time( {
  s = foreach( i = 1:n.files, .combine=rbind ) %dopar% {
    
    cat("\n\nStarting file number", i, ", called", keepers[i])
    
    d = read.csv( keepers[i] )
  
    # simulation rep
    n.boot = max(d$bt.iterate)
    n.sims.run = dim(d)[1] / n.boot
    d$sim.rep = rep( seq(1, n.sims.run), each = n.boot )
    
    # convert time in seconds to time in hours
    d$boot.hrs = d$boot.time / (60*60)
    
    # job file
    d$job.file = keepers[i]
    
    ##### Statistical Performance ######
    # compute CI coverage
    library(data.table)
    dt = data.table(d)
#      dt[ , lo.bt := quantile( n.rej.bt, alpha / 2 ), by=sim.rep ]
#      dt[ , hi.bt := quantile( n.rej.bt, 1 - (alpha / 2) ), by=sim.rep ]
# 
#     # coverage
#     dt$covers = (dt$lo.bt <= N0 ) & (dt$hi.bt >= N0)
     
    # hypothesis test
    # Hall & Wilson style
    #dt[ , crit.val := quantile( abs( n.rej.bt - n.rej ), 1 - alpha ), by=sim.rep ]
    #dt[ , bt.rej := abs( n.rej - N0 ) > crit.val, by = sim.rep ]
    
    # one-sided hypothesis test
    # basic percentile method
    dt[ , crit := quantile( n.rej.bt, 1 - alpha ), by = sim.rep ]
    dt[ , bt.rej := crit, by = sim.rep ]
    
    # variability of n.rej.bt within this boot iterate
    # should be equal to variability of n.rej across simulations
    dt[ , var.n.rej.bt := var( n.rej.bt ), by = sim.rep ]
    
    ##### Trim Dataset ######
    # keep only 1 row per scenario (for first boot iterate)
    d2 = data.frame(dt)
    d2 = d2[ d2$bt.iterate==1, ]

    print(head(d2))
    
    # this is the equivalent of returning
    #d2
    
    # write results
    # the "i" here does not correspond to a job
    setwd("/share/PI/manishad/multTest/sim_results/within_prepped")
    write.csv( d2, paste( "within_prepped_", i, ".csv", sep="" ) )
    # the returned (not used) r is d2, I guess
  }
} )[3]


print(head(s))

print(head(scen.params))

# merge in scenario parameters
names(s)[ names(s) == "scen" ] = "scen.name"
s = merge( scen.params, s )


path = "/share/PI/manishad/multTest/sim_results/overall_stitched"
setwd(path)
write.csv( s, "stitched_results.csv" )

# how long did it take?
parallel.time



