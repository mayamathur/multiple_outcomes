
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
alpha = 0.05

# simulation reps to run within this job
# each will have its own row in results file
sim.reps = 1

# ######### FOR LOCAL USE #########
# setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Sandbox/2018-1-13")
# p = read.csv("scen_params.csv")  # should be a single row, I think
n = 1e5  # INCREASED FROM BEFORE
nX = 1
nY = 30
rho.XX = 0
rho.YY = c(0)
rho.XY = c(0.005)  # null hypothesis: 0
subsamp.ns = "c(400, 600, 900, 1350)"


# bootstrap iterates and type
boot.reps = 100
bt.type = c( "subsample" )  # fcr: resample under HA; resid: resample under H0


# matrix of scenario parameters
scen.params = expand.grid( bt.type, n, nX, nY, rho.XX, rho.YY, rho.XY, subsamp.ns )
names(scen.params) = c( "bt.type", "n", "nX", "nY", "rho.XX", "rho.YY", "rho.XY", "subsamp.ns" )

# name the scenarios
# remove letters that are privileged variables in R
letter.names = c(letters, LETTERS)[ ! c(letters, LETTERS) %in% c("i","T","F") ]
scen.params$scen.name = letter.names[ 1:dim(scen.params)[1] ]
p = scen.params

# add alpha corresponding to Bonferroni as first one
# NOTE THAT CODE ASSUMES BONFERRONI IS THE FIRST ONE
# crit.bonf = 0.05 / p$nY
# alpha = c( crit.bonf, 0.01, 0.05 )

alpha = 0.05

sim.reps = 10

# for sourcing functions later
setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/git_multiple_outcomes/R code/For Sherlock")
######### END OF LOCAL PART #########




########################### THIS SCRIPT COMPLETELY RUNS 1 SIMULATION  ###########################

# draws a new dataset
# runs all bootstrapping iterations
# writes 1-row csv with number rejected and bootstrap CI

library(doParallel)
library(foreach)
library(mvtnorm)

source("functions.R")

# set the number of cores
registerDoParallel(cores=16)


# j is the number of simulation iterations to run sequentially
# so for j=10, we are generating 10 observed datasets, along with 500 bootstrap iterates for each
# on Sherlock, this for-loop is supposed to receive a single ROW of parameters

for ( j in 1:sim.reps ) {
  
  # make initial dataset from which to bootstrap
  # generate under null
  cor = make_corr_mat( .nX = p$nX, .nY = p$nY, .rho.XX = p$rho.XX,
                       .rho.YY = p$rho.YY, .rho.XY = p$rho.XY)
  d = sim_data( .n = p$n, .cor = cor )
  
  # excess hits in observed data
  # ASSUMES SCALAR ALPHA
  expected = alpha * p$nY
  n.rej = as.numeric( dataset_result( .dat = d, .alpha = alpha,
                                     .resid = FALSE )$rej )
  excess = n.rej - expected
  
  # vector of subsample sizes
  ns = eval( parse( text = as.character(p$subsamp.ns) ) )
  
  # run all bootstrap iterates
  parallel.time = system.time( {
    excesses = foreach( icount( boot.reps ), .combine=rbind ) %dopar% {
      
      ######## Fit Model to Each Subsample ########
      b = d
      
      # row for this iterate
      # will have one element for each subsample size containing its number of excesses
      excess.row = c()
      
      for ( k in length(ns):1 ) {
        
        # subsample with replacement from the previous subsample
        ids = sample( 1:nrow(b), size = ns[k], replace = FALSE )
        b = b[ ids, ]
        excess.row[k] = dataset_result( .dat = b, .alpha = alpha, .resid = FALSE )$rej - expected  # i here is the bootstrap iterate
      } ##### end subsampling loop
      
      # return it for rbinding
      as.numeric( excess.row )
    } ##### end bootstrapping loop
  } )[3]
  
  # should be 0 if generating under null
  bt.mean.excess = mean( excesses )

  # subtract scalar from each element of matrix
  zlist = as.data.frame( excesses - excess )
  names(zlist) = ns
  #boxplot(zlist, xlab = "subsample size", ylab = expression(hat(theta)[b] - hat(theta)[n]))
  
  # vector of indices (of bootstrap samples) for each quantile
  k = (boot.reps + 1) * seq( 0.05, 0.45, 0.05 )  # index number for lower quantiles that range from 5% to 45%
  l = (boot.reps + 1) * seq( 0.55, 0.95, 0.05 )  # index number for lower quantiles that range from 55% to 95%
  
  # initialize list of distance between quantiles
  # list containing 4 vectors (1 for each subsample size)
  qlist = list()
  for ( i in 1:length(ns) ) {
    z.star <- zlist[[i]]  # take a column from zlist (all iterates for a given subsample size)
    sz.star <- sort(z.star, partial = c(k, l))
    qlist[[i]] <- sz.star[l] - sz.star[k]
  }
  names(qlist) = ns
  
  # I ADDED THIS
  # prevent zeroes in qlist (where quantiles are exactly the same) by replacing them with 0.0001
  #  to avoid problems with logs later on
  qlist = lapply( qlist, FUN = function(x) as.numeric( gsub(0, 0.0001, x) ) )
  
  # logs of quantile differences
  lqlist <- lapply(qlist, log)

  # each point is for a different pair of quantiles? I think?
  # stripchart(lqlist, xlab = "subsample size",
  #            ylab = "log(high quantile - low quantile)",
  #            vertical = TRUE)
  
  # mean distance (log) between quantiles
  y = sapply( lqlist, mean )
  
  # rate estimate from OLS
  rate = cov( -y, log(ns) ) / var( log(ns) )
  # inter <- mean(y) + beta * mean(log(ns))
  # plot(log(rep(ns, each = length(k))), unlist(lqlist),
  #      xlab = "log(subsample size)", ylab = "log(high quantile - low quantile)")
  # abline(inter, - beta)
  

  ####### Make CI ######
  # get bhats across all iterates for just the fourth, largest sample size (n=1350)
  m=4
  
  ### Method 1: as in lab handout (centered on estimate)
  z.star = ns[m]^rate * (excesses[,m] - excess)
  # apply equation 11 from lab handout
  # see lab, pg 10 for 2-sided CI
  crit.vals = c( quantile( z.star, 0.025 ),
                 quantile( z.star, 0.975 ) ) 
  ci.bds = c( excess - crit.vals[2] / p$n^rate, excess - crit.vals[1] / p$n^rate )
  
  ### Method 2: same as above, but using correct rate - WILL ONLY BE CORRECT FOR NULL
  rate.correct = 0
  z.star.correct = ns[m]^rate.correct * (excesses[,m] - excess)
  crit.vals.correct = c( quantile( z.star.correct, 0.025 ),
                         quantile( z.star.correct, 0.975 ) ) 
  ci.bds.correct = c( excess - crit.vals.correct[2] / p$n^rate.correct,
                      excess - crit.vals.correct[1] / p$n^rate.correct )
  
  ### Method 3: just quantiles; not centered on estimate
  ci.bds.raw = ( ns[m] / p$n )^rate * c( quantile( excesses[,m], 0.025 ),
                           quantile( excesses[,m], 0.975 ) )
  # IS THIS RIGHT?
  
  # # TEST ONLY
  # # TRY GIVING IT THE EXACTLY CORRECT RATE FOR THE NULL CASE
  # # AND DON'T USE RATE BECAUSE IT'S 0
  # # JUST TAKE EMPIRICAL QUANTILES OF THE THIRD SAMPLE SIZE
  # # THIS HAS COVERAGE 100%
  # SO SOMETHING IS WRONG WITH THE Z.STAR?
  # ci.bds = c( quantile( excesses[,m], 0.025 ),
  #             quantile( excesses[,m], 0.975 ) )
  
 
  # does CI cover the truth?
  # HARD-CODES that we are under the null
  # for other scenarios, will have to do this after the sims are over using mean excess across reps
  expected.excess = 0
  covers = ( ci.bds[1] <= expected.excess ) & ( ci.bds[2] >= expected.excess )
  covers.correct = ( ci.bds.correct[1] <= expected.excess ) & ( ci.bds.correct[2] >= expected.excess )
  covers.raw = ( ci.bds.raw[1] <= expected.excess ) & ( ci.bds.raw[2] >= expected.excess )
  
  ###### Results for This Simulation Rep #####
  
  # return results of each bootstrap iterate
  new.rows = data.frame( scen, 
                          n.rej,
                         excess,
                         rate,
                         bt.mean.excess,
                         crit.lo = crit.vals[1],
                         crit.hi = crit.vals[2],
                        ci.lo = ci.bds[1],
                         ci.hi = ci.bds[2],
                        crit.lo.correct = crit.vals.correct[1],
                        crit.hi.correct = crit.vals.correct[2],
                        ci.lo.correct = ci.bds.correct[1],
                        ci.hi.correct = ci.bds.correct[2],
                        ci.lo.raw = ci.bds.raw[1],
                        ci.hi.raw = ci.bds.raw[2],
                        covers,
                        covers.correct,
                        covers.raw,
                         rep.time = parallel.time )
 
  # add row to output file
  if ( j == 1 ) res = new.rows
  else res = rbind( res, new.rows )
  # res should have boot.reps rows per "j" in the for-loop
  
}  # end loop over j simulation reps


# res has 1 row per sim.rep

########################### WRITE LONG RESULTS ###########################
setwd("/share/PI/manishad/multTest/sim_results/long")
write.csv( res, paste( "long_results", jobname, ".csv", sep="_" ) )


# ########################### WRITE SHORT RESULTS ###########################
# 
# # keep only 1 row per simulation rep
# keep.row = rep( c( TRUE, rep(FALSE, boot.reps - 1) ), sim.reps )
# 
# setwd("/share/PI/manishad/multTest/sim_results/short")
# write.csv( res[ keep.row, ], paste( "short_results", jobname, ".csv", sep="_" ) )


