
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
crit.bonf = 0.05 / p$nY
alpha = c( crit.bonf, 0.01, 0.05 )

# simulation reps to run within this job
sim.reps = 10

######### FOR LOCAL USE #########
# setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Sandbox/2018-1-13")
# p = read.csv("scen_params.csv")  # should be a single row, I think
n = 1000
nX = 1
nY = 100
rho.XX = 0
rho.YY = c(0)
rho.XY = c(0)  # null hypothesis: 0

# bootstrap iterates and type
boot.reps = 200
bt.type = c( "fcr" )  # fcr: resample under HA; resid: resample under H0


# matrix of scenario parameters
scen.params = expand.grid( bt.type, n, nX, nY, rho.XX, rho.YY, rho.XY )
names(scen.params) = c( "bt.type", "n", "nX", "nY", "rho.XX", "rho.YY", "rho.XY" )

# name the scenarios
# remove letters that are privileged variables in R
letter.names = c(letters, LETTERS)[ ! c(letters, LETTERS) %in% c("i","T","F") ]
scen.params$scen.name = letter.names[ 1:dim(scen.params)[1] ]
p = scen.params

# add alpha corresponding to Bonferroni as first one
# NOTE THAT CODE ASSUMES BONFERRONI IS THE FIRST ONE
crit.bonf = 0.05 / p$nY
alpha = c( crit.bonf, 0.01, 0.05 )

# for sourcing functions later
setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/git_multiple_outcomes/R code/For Sherlock")
######### END OF LOCAL PART #########


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

# I think that on Sherlock, this for-loop is supposed to receive a single ROW of parameters

for ( j in 1:sim.reps ) {
  
  # ######## ONLY FOR LOCAL USE  ########
  # # monitor progress
  # cat( "\n\nStarting sim rep", j )
  # 
  # # occasionally write intermediate results to desktop
  #  if ( j %% 50 == 0 ) {
  #    setwd("~/Desktop")
  #    write.csv(res, "temp_results.csv")
  #  }
  # ######## LOCAL PART ENDS HERE  ######## 

  ##### Bootstrapping Loop ######
  rep.time = system.time( {
    
    # ~~~~~ FOR TESTING ONLY
    # TO ELIMINATE FLOOR EFFECTS
    alpha = 0.5
    
  # make initial dataset from which to bootstrap
  cor = make_corr_mat( .nX = p$nX, .nY = p$nY, .rho.XX = p$rho.XX, .rho.YY = p$rho.YY, .rho.XY = p$rho.XY)
  d = sim_data( .n = p$n, .cor = cor )
  
  # extract names of outcome variables
  X.names = names(d)[ grep( "X", names(d) ) ]
  Y.names = names(d)[ grep( "Y", names(d) ) ]
  
 
  
  # get number rejected for observed dataset
  # vector with same length as .alpha
  samp.res = dataset_result( .dat = d, .alpha = alpha,
                             .resid = ifelse( p$bt.type %in% c("resid", "ha.resid"), TRUE, FALSE) )
  n.rej = samp.res$rej  # first one is Bonferroni
  names(n.rej) = paste( "n.rej.", as.character(alpha), sep="" )
  
  # Bonferroni test of joint null using just original data
  # i.e., do we reject at least one outcome under Bonferroni threshold?
  jt.rej.bonf.naive = n.rej[1] > 0
  
  # run all bootstrap iterates
    r = foreach( icount( boot.reps ), .combine=rbind ) %dopar% {
      # draw bootstrap sample 
      ids = sample( 1:nrow(d), replace=TRUE )
      
      ##### Bootstrap Under Null #1 #####
      # for bootstrap dataset, replace just the Y columns with the Y columns sampled with replacement
      if( p$bt.type == "Y" ) {
        b = d
        b[ , ( length(X.names) + 1 ) : dim(b)[2] ] = b[ ids, ( length(X.names) + 1 ) : dim(b)[2] ]
      }
    
      ##### Bootstrap Under Null #2 (Residuals) #####
      # see Westfall, pg. 133
      if ( p$bt.type == "resid" ) {
        # fix the existing covariates
        Xs = as.data.frame( d[ , 1 : length(X.names) ] )
        names(Xs) = X.names
        
        # resample residuals
        resid = samp.res$resid
        names(resid) = Y.names
        b = as.data.frame( cbind( Xs, resid[ids,] ) )
      }
      
      ##### Bootstrap From Original #####
      # full-case resampling
      if ( p$bt.type == "fcr" ) {
        b = d[ ids, ]
      }
      
      ##### Bootstrap From Original #2 #####
      # re-attach residuals
      if ( p$bt.type == "ha.resid" ) {
        # resample residuals
        resid.random = samp.res$resid[ ids, ]
        # WOULD NEED FITTED VALUES HERE...
        b = as.data.frame( cbind( Xs, resid[ids,] ) )
      }
      
      # get number rejected for bootstrap sample
      # we don't need to return residuals for this one
      dataset_result( .dat = b, .alpha = alpha, .resid = FALSE )$rej
      # so the returned r is a dataframe of number rejected for each bootstrap iterate 
      # one column for each value of alpha
      # r[["0.01"]] is the vector with length boot.reps of number rejected at alpha = 0.01

    } # end r-loop (parallelized bootstrap)

  } )[3]  # end timer
  
  
  ###### Results for This Simulation Rep #####

    names(r) = paste( "n.rej.bt.", as.character(alpha), sep="" )
    
    # performance: rejection of joint null hypothesis
    # alpha for joint test is set to 0.05 regardless of alpha for individual tests
    crit.bonf = quantile( r[,1], 1 - 0.05 )  
    crit.0.05 = quantile( r$n.rej.bt.0.05, 1 - 0.05 )
    crit.0.01 = quantile( r$n.rej.bt.0.01, 1 - 0.05 )

    # performance: confidence interval for N-hat under joint null
    # this is a 95% CI regardless of which alpha is used for the individual tests
    bt.lo.bonf = quantile( r[,1], 0.025 )
    bt.hi.bonf = quantile( r[,1], 0.975 )
    
    bt.lo.0.01 = quantile( r$n.rej.bt.0.01, 0.025 )
    bt.hi.0.01 = quantile( r$n.rej.bt.0.01, 0.975 )
    
    bt.lo.0.05 = quantile( r$n.rej.bt.0.05, 0.025 )
    bt.hi.0.05 = quantile( r$n.rej.bt.0.05, 0.975 )
    
    # p-values for observed rejections
    # if we resampled under H0, want prob of observing at least as many rejections as we did
    if ( p$bt.type == "resid" ) {
      
      jt.pval.bonf = sum( r[,1] >= n.rej[,1] ) / length( r[,1] )
      jt.pval.0.01 = sum( r$n.rej.bt.0.01 >= n.rej[["n.rej.0.01"]] ) /
                        length( r$n.rej.bt.0.01 )
      jt.pval.0.05 = sum( r$n.rej.bt.0.05 >= n.rej[["n.rej.0.05"]] ) /
                        length( r$n.rej.bt.0.05 )

    }
    
    # I BELIEVE THE BELOW P-VALUES AREN'T THEORETICALLY VALID
    # if we resampled under HA, want prob that observed rejections - expected > 0
    else if ( p$bt.type == "fcr" ) {
      
      # expected rejections under H0
      expect = alpha * length(Y.names)
      
      # ~~~~~~ BOOKMARK: SOMETHING IS WRONG HERE...
      # maybe expectations are wrong?
      jt.pval.bonf = sum( r[,1] - expect[1] <= 0 ) / length( r[,1] )
      jt.pval.0.01 = sum( r$n.rej.bt.0.01 - expect[2] <= 0 ) / length(r$n.rej.bt.0.01)
      jt.pval.0.05 = sum( r$n.rej.bt.0.05 - expect[3] <= 0 ) / length( r$n.rej.bt.0.05 )
    }
   
    # did joint tests reject?
    rej.jt.bonf = jt.pval.bonf < 0.05
    rej.jt.0.01 = jt.pval.0.01 < 0.05
    rej.jt.0.05 = jt.pval.0.05 < 0.05

    # return entire results of this sim rep
    # with a row for each boot iterate
    # these variables will be same for each bootstrap iterate
    # the individual bootstrap results are merged in after this
    new.rows = data.frame( scen = rep( scen, boot.reps ),
                      bt.iterate = 1:boot.reps,
                      rep.minutes = as.vector(rep.time) / 60,
                      
                      jt.rej.bonf.naive,
                      
                      crit.bonf,
                      crit.0.05,
                      crit.0.01,
                      
                      bt.lo.bonf, bt.hi.bonf,
                      bt.lo.0.01, bt.hi.0.01,
                      bt.lo.0.05, bt.hi.0.05,
                      
                      jt.pval.bonf,
                      jt.pval.0.01,
                      jt.pval.0.05,
                      
                      rej.jt.bonf,
                      rej.jt.0.01,
                      rej.jt.0.05
                      )
  

    # merge in the individual bootstrap results and parameters
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

setwd("/share/PI/manishad/multTest/sim_results/short")
write.csv( res[ keep.row, ], paste( "short_results", jobname, ".csv", sep="_" ) )

