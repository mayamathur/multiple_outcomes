
########################### LOAD COMMAND-LINE ARGUMENTS  ###########################

# FOR CLUSTER USE
# load command line arguments
args = commandArgs(trailingOnly = TRUE)
jobname = args[1]
scen = args[2]  # this will be a letter
boot.reps = as.numeric( args[3] )

# get scen parameters
setwd("/share/PI/manishad/multTest")
scen.params = read.csv( "scen_params.csv" )
p = scen.params[ scen.params$scen.name == scen, ]

# no longer included in parameters because it's a vector
alpha = c(0.01, 0.05)

# simulation reps to run within this job
sim.reps = 10

# FOR LOCAL USE
# setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/R code/Sandbox/2017-7-21 Test smaller simulation")
# p = read.csv("scen_params.csv")
# p$bt.type = "resid"
# boot.reps = 2000  
# setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/R code/For Sherlock") # for sourcing functions later



########################### THIS SCRIPT COMPLETELY RUNS 1 SIMULATION  ###########################

# draws a new dataset
# runs all 500 bootstrapping iterations
# writes 1-row csv with number rejected and bootstrap CI

library(doParallel)
library(foreach)
library(mvtnorm)

source("functions.R")

# set the number of cores
registerDoParallel(cores=16)



# j is the number of simulation iterations to run sequentially
# so for j=10, we are generating 10 observed datasets, along with 500 bootstrap iterates for each

for ( j in 1:sim.reps ) {
  
#   # ONLY FOR LOCAL USE
#   # monitor progress
#   cat( "\n\nStarting sim rep", j )
#   
#   # occasionally write intermediate results to desktop
#    if ( j %% 50 == 0 ) {
#      setwd("~/Desktop")
#      write.csv(res, "temp_results.csv")
#    }

  rep.time = system.time( {
    
  # make initial dataset from which to bootstrap
  cor = make_corr_mat( .nX = p$nX, .nY = p$nY, .rho.XX = p$rho.XX, .rho.YY = p$rho.YY, .rho.XY = p$rho.XY)
  d = sim_data( .n = p$n, .cor = cor )
  
  # get number rejected for observed dataset
  # vector with same length as .alpha
  
  # for debugging
  print(p)
  print( ifelse( p$bt.type[1] == "resid", TRUE, FALSE ) )
  
  samp.res = dataset_result( .dat = d, .alpha = alpha, .resid = ifelse( p$bt.type == "resid", TRUE, FALSE) )
  n.rej = samp.res$rej
  names(n.rej) = paste( "n.rej.", as.character(alpha), sep="" )
  
  # extract names of outcome variables
  X.names = names(d)[ grep( "X", names(d) ) ]
  Y.names = names(d)[ grep( "Y", names(d) ) ]
  
  # run all bootstrap iterates
    r = foreach( icount( boot.reps ), .combine=rbind ) %dopar% {
      # draw bootstrap sample 
      ids = sample( 1:nrow(d), replace=TRUE )
      
      ##### Bootstrap Under Null #1 #####
      # for bootstrap dataset, replace just the Y columns with the Y columns sampled with replacement
      if( ! p$bt.type == "resid" ) {
        b = d
        b[ , ( length(X.names) + 1 ) : dim(b)[2] ] = b[ ids, ( length(X.names) + 1 ) : dim(b)[2] ]
      }
    
      ##### Bootstrap Under Null #2 (Residuals) #####
      # see Westfall, pg. 133
      if( p$bt.type == "resid" ) {
        # fix the existing covariates
        Xs = as.data.frame( d[ , 1 : length(X.names) ] )
        names(Xs) = X.names
        
        # resample residuals
        resid = samp.res$resid
        names(resid) = Y.names
        b = as.data.frame( cbind( Xs, resid[ids,] ) )
      }
      
      ##### Bootstrap From Original #####
#       b = d[ ids, ]

      # get number rejected for bootstrap sample
      # we don't need to return residuals for this one
      dataset_result( .dat = b, .alpha = alpha, .resid = FALSE )$rej
      # so the returned r is a dataframe of number rejected for each bootstrap iterate 
      # one column for each value of alpha
      # r[["0.01"]] is the vector with length boot.reps of number rejected at alpha = 0.01

    } # end parallelized bootstrap loop

  } )[3]  # end timer
  
  
  ###### Results for This Simulation Rep #####

    names(r) = paste( "n.rej.bt.", as.character(alpha), sep="" )
    
    # performance: rejection of joint null hypothesis
    # alpha for joint test matches alpha for the individual tests
    crit.0.05 = quantile( r$n.rej.bt.0.05, 1 - 0.05 )
    crit.0.01 = quantile( r$n.rej.bt.0.01, 1 - 0.01 )

    # performance: confidence interval for N-hat under joint null
    # this is a 95% CI regardless of which alpha is used for the individual tests
    bt.lo.0.01 = quantile( r$n.rej.bt.0.01, 0.025 )
    bt.hi.0.01 = quantile( r$n.rej.bt.0.01, 0.975 )

    bt.lo.0.05 = quantile( r$n.rej.bt.0.05, 0.025 )
    bt.hi.0.05 = quantile( r$n.rej.bt.0.05, 0.975 )

    # return entire results of this sim rep
    # with a row for each boot iterate
    new.rows = data.frame( scen = rep( scen, boot.reps ),
                      bt.iterate = 1:boot.reps, 
                      rep.minutes = as.vector(rep.time) / 60,
                      crit.0.05, 
                      crit.0.01,
                      bt.lo.0.01, bt.hi.0.01, 
                      bt.lo.0.05, bt.hi.0.05,
                      rej.0.05 = as.numeric( n.rej[["n.rej.0.05"]] > crit.0.05 ),
                      rej.0.01 = as.numeric( n.rej[["n.rej.0.01"]] > crit.0.01 )
                      )

    # merge in the bootstrap results and parameters
    new.rows = cbind( p, new.rows, n.rej, r )

    # remove redundant parameters row
    new.rows = new.rows[ , !names(new.rows) == "scen.name" ]
    
    # add row to output file
    if ( j == 1 ) res = new.rows
    else res = rbind( res, new.rows )
    # res should have boot.reps rows per "j" in the for-loop
  
}  # end loop over j simulation reps


########################### WRITE LONG RESULTS  ###########################
setwd("/share/PI/manishad/multTest/sim_results/long")
write.csv( res, paste( "long_results", jobname, ".csv", sep="_" ) )


########################### WRITE SHORT RESULTS ###########################

# keep only 1 row per simulation rep
keep.row = rep( c( TRUE, rep(FALSE, boot.reps - 1) ), sim.reps )

# keep just first row for this simulation rep
setwd("/share/PI/manishad/multTest/sim_results/short")
write.csv( res[ keep.row, ], paste( "short_results", jobname, ".csv", sep="_" ) )

