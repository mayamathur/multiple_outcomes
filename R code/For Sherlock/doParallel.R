
######### FOR CLUSTER USE #########
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
# this need to match n.reps.in.doParallel in the genSbatch script
sim.reps = 1
######### END OF CLUSTER PART #########


# ######### FOR LOCAL USE #########
# # setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Sandbox/2018-1-13")
# # p = read.csv("scen_params.csv")  # should be a single row, I think
# 
# n = 500
# nX = 1
# nY = 100
# rho.XX = 0
# rho.YY = c(0)
# rho.XY = c(0.03)  # null hypothesis: 0
# 
# # bootstrap iterates and type
# boot.reps = 200
# sim.reps = 1
# scen = "a"
# bt.type = c( "h0.parametric" )
# 
# 
# # matrix of scenario parameters
# scen.params = expand.grid( bt.type, n, nX, nY, rho.XX, rho.YY, rho.XY )
# names(scen.params) = c( "bt.type", "n", "nX", "nY", "rho.XX", "rho.YY", "rho.XY" )
# 
# # name the scenarios
# # remove letters that are privileged variables in R
# letter.names = c(letters, LETTERS)[ ! c(letters, LETTERS) %in% c("i","T","F") ]
# scen.params$scen.name = letter.names[ 1:dim(scen.params)[1] ]
# p = scen.params
# 
# # add alpha corresponding to Bonferroni as first one
# # NOTE THAT CODE ASSUMES BONFERRONI IS THE FIRST ONE
# crit.bonf = 0.05 / p$nY
# alpha = c( crit.bonf, 0.01, 0.05 )
# 
# # for sourcing functions later
# setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/git_multiple_outcomes/R code/For Sherlock")
# ######### END OF LOCAL PART #########


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

  # make initial dataset from which to bootstrap
  cor = make_corr_mat( .nX = p$nX, .nY = p$nY, .rho.XX = p$rho.XX,
                       .rho.YY = p$rho.YY, .rho.XY = p$rho.XY)
  d = sim_data( .n = p$n, .cor = cor )
  
  # extract names of outcome variables
  X.names = names(d)[ grep( "X", names(d) ) ]
  Y.names = names(d)[ grep( "Y", names(d) ) ]
  
  # get number rejected for observed dataset
  # vector with same length as .alpha
  # only return residuals and/or model estimates if we need them later for resampling
  samp.res = dataset_result( .dat = d, .alpha = alpha,
                             .resid = ifelse( p$bt.type %in% c("resid", "ha.resid", "ha.resid.2"),
                                              TRUE, FALSE), 
                             .sigma = ifelse( p$bt.type %in% c( "h0.parametric", "ha.resid.2"),
                                              TRUE, FALSE),
                             .intercept = (p$bt.type == "h0.parametric") )
  n.rej = samp.res$rej  # first one is Bonferroni
  pvals = samp.res$pvals
  names(n.rej) = paste( "n.rej.", as.character(alpha), sep="" )
  
  # Bonferroni test of joint null using just original data
  # i.e., do we reject at least one outcome using Bonferroni threshold?
  jt.rej.bonf.naive = n.rej[1] > 0
  
  # run all bootstrap iterates
    r = foreach( i = 1:boot.reps, .combine=rbind ) %dopar% {
      # draw bootstrap sample 
      ids = sample( 1:nrow(d), replace=TRUE )
      
      ##### Bootstrap Under Null #1 (Resample Y; Fix X) #####
      # for bootstrap dataset, replace just the Y columns with the Y columns sampled with replacement
      if( p$bt.type == "Y" ) {
        b = d
        b[ , ( length(X.names) + 1 ) : dim(b)[2] ] = b[ ids, ( length(X.names) + 1 ) : dim(b)[2] ]
      }
    
      ##### Bootstrap Under Null #2 (Resample Residuals) #####
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
      
      ##### Bootstrap Under Null #3 - Regenerate Y Parametrically #####
      # re-attach residuals
      if ( p$bt.type == "h0.parametric" ) {
        # extract fitted model parameters
        sigma = samp.res$sigma
        intercept = samp.res$intercept

        # fix the existing covariates
        Xs = as.data.frame( d[ , 1 : length(X.names) ] )
        names(Xs) = X.names
        
        # parametrically generate Ys, setting beta of interest to 0
        new.Ys = matrix( NA, nrow = nrow(d), ncol = length(Y.names) )
        n.cells = length(new.Ys)
        
        # go through each outcome (column)
        #  and generate using appropriate parameter estimates
        new.Ys = sapply( 1:ncol(new.Ys),
                             function(x) rnorm( n = n.cells, mean = intercept[x], sd = sigma[x] ) )
        b = as.data.frame( cbind( Xs, new.Ys ) )
      }
      
      ##### Bootstrap Under HA #1 - FCR #####
      # doesn't work! 
      # full-case resampling
      if ( p$bt.type == "fcr" ) {
        b = d[ ids, ]
      }
      
      ##### Bootstrap Under HA #2 - Reattach Residuals #####
      # re-attach residuals
      if ( p$bt.type == "ha.resid" ) {
        # extract residuals from original data
        # matrix with same dimensions as the outcomes matrix
        resid = samp.res$resid
      
        # compute Y-hat using residuals
        Ys = d[ , (p$nX + 1) : ncol(d) ]  # remove covariates
        Yhat = Ys - resid
        
        # fix the existing covariates
        Xs = as.data.frame( d[ , 1 : length(X.names) ] )
        names(Xs) = X.names
        
        # resample residuals and add them to fitted values
        b = as.data.frame( cbind( Xs, Yhat + resid[ids,] ) )
      }
      
      ##### Bootstrap Under HA #3 - Regenerate Residuals #####
      # re-attach residuals
      if ( p$bt.type == "ha.resid.2" ) {
        # extract residuals from original data
        # matrix with same dimensions as the outcomes matrix
        resid = samp.res$resid
        sigma = samp.res$sigma
        
        # compute Y-hat using residuals
        Ys = d[ , (p$nX + 1) : ncol(d) ]  # remove covariates
        Yhat = Ys - resid
        
        # fix the existing covariates
        Xs = as.data.frame( d[ , 1 : length(X.names) ] )
        names(Xs) = X.names

        # regenerate residuals using fitted values
        n.cells = dim(resid)[1] * dim(resid)[2]
        # generate columns of new residuals using the appropriate sigma
        new.resids = sapply( 1:ncol(resid), function(x) rnorm( n=n.cells, sd = sigma[x] ) )
        
        # regenerate residuals using fitted values
        b = as.data.frame( cbind( Xs, Yhat + new.resids ) )
      }
      
      # important for dataset_result to be able to find the right names
      names(b) = c(X.names, Y.names)
      
      bt.res = dataset_result( .dat = b, .alpha = alpha, .resid = FALSE, .tval=TRUE )
      
      # return all the things
      list( rej = bt.res$rej,
            pvals = bt.res$pvals )

      # get number rejected for bootstrap sample
      # we don't need to return residuals for this one
      # dataset_result( .dat = b, .alpha = alpha, .resid = FALSE )$rej
      # so the returned r is a dataframe of number rejected for each bootstrap iterate 
      # one column for each value of alpha
      # r[["0.01"]] is the vector with length boot.reps of number rejected at alpha = 0.01

    } ###### end r-loop (parallelized bootstrap)
    
  } )[3]  # end timer
  
  
  ###### Data Wrangling for This Simulation Rep #####
  
  # organize rejections by alpha level
  u = unlist( r[,"rej"] )
  n.rej.bt.0.05 = u[ grepl( "0.05", names(u) ) ]
  n.rej.bt.0.01 = u[ grepl( "0.01", names(u) ) ]
  n.rej.bt.0.005 = u[ grepl( "0.005", names(u) ) ]
  
  # resampled p-value matrix for Westfall
  # rows = Ys
  # cols = resamples
  p.bt = do.call( cbind, r[ , "pvals" ] )
  
  
  ###### Joint Test Results for This Simulation Rep #####

    # names(r) = paste( "n.rej.bt.", as.character(alpha), sep="" )
    # 
    # performance: rejection of joint null hypothesis
    # alpha for joint test is set to 0.05 regardless of alpha for individual tests
    # crit.bonf = quantile( n.rej.bt.0.005, 1 - 0.05 )  
    crit.0.05 = quantile( n.rej.bt.0.05, 1 - 0.05 )
    crit.0.01 = quantile( n.rej.bt.0.01, 1 - 0.05 )

    # performance: confidence interval for N-hat under joint null
    # this is a 95% CI regardless of which alpha is used for the individual tests
    # bt.lo.bonf = quantile( n.rej.bt.0.005, 0.025 )
    # bt.hi.bonf = quantile( n.rej.bt.0.005, 0.975 )
    
    bt.lo.0.01 = quantile( n.rej.bt.0.01, 0.025 )
    bt.hi.0.01 = quantile( n.rej.bt.0.01, 0.975 )
    
    bt.lo.0.05 = quantile( n.rej.bt.0.05, 0.025 )
    bt.hi.0.05 = quantile( n.rej.bt.0.05, 0.975 )
    
    # p-values for observed rejections
    # if we resampled under H0, want prob of observing at least as many rejections as we did
    if ( p$bt.type == "h0.parametric" ) {
      
      # jt.pval.bonf = sum( n.rej.bt.0.005 >= n.rej[,1] ) / length( n.rej.bt.0.005 )
      jt.pval.0.01 = sum( n.rej.bt.0.01 >= n.rej[["n.rej.0.01"]] ) /
                        length( n.rej.bt.0.01 )
      jt.pval.0.05 = sum( n.rej.bt.0.05 >= n.rej[["n.rej.0.05"]] ) /
                        length( n.rej.bt.0.05 )
    }
   
    # did joint tests reject?
    # rej.jt.bonf = jt.pval.bonf < 0.05
    rej.jt.0.01 = jt.pval.0.01 < 0.05
    rej.jt.0.05 = jt.pval.0.05 < 0.05
    

    ######## Westfall's single-step ######## 
    
    # do adjusted p-vals make sense?
    p.adj.minP = adjust_minP( pvals, p.bt )
    cbind( pvals, p.adj.minP )
    # plot( pvals, p.adj.minP )
    
    ( jt.rej.minP = any( p.adj.minP < 0.05 ) )
    
    # sanity check: how many does Westfall reject?
    table( p.adj.minP < 0.05 )
    
    
    ######## Westfall's step-down ######## 
    
    ( p.adj.stepdown = adj_Wstep( pvals, p.bt ) )
    plot( pvals, p.adj.stepdown )
    
    ( jt.rej.Wstep = any( p.adj.minP < 0.05 ) )
    
    # should reject more than either procedure above
    table( p.adj.stepdown < 0.05 )
    
    
    ###### Write Results #####

    # return entire results of this sim rep
    # with a row for each boot iterate
    # these variables will be same for each bootstrap iterate
    # the individual bootstrap results are merged in after this
    # suppressWarnings because it whines about row names from a short variable
    new.rows = suppressWarnings( data.frame( scen = rep( scen, boot.reps ),
                      bt.iterate = 1:boot.reps,
                      rep.minutes = as.vector(rep.time) / 60,
                      
                      jt.rej.bonf.naive = ifelse( as.numeric(jt.rej.bonf.naive) == 1, TRUE, FALSE ),
                      jt.rej.minP,
                      jt.rej.Wstep,
                      
                      # crit.bonf,
                      crit.0.05,
                      crit.0.01,
                      
                      # bt.lo.bonf, bt.hi.bonf,
                      bt.lo.0.01, bt.hi.0.01,
                      bt.lo.0.05, bt.hi.0.05,
                      
                      # jt.pval.bonf,
                      jt.pval.0.01,
                      jt.pval.0.05,
                      
                      # rej.jt.bonf,
                      rej.jt.0.01,
                      rej.jt.0.05
                      ) )
  
    # ALL ROWS ARE STATIC VARIABLES BECAUSE WE'RE NOT MERGING IN RESAMPLE-LEVEL
    #  RESULTS. 
    
    # merge in the individual bootstrap results and parameters
    # new.rows = cbind( p, new.rows, n.rej, r )

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

