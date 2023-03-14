
run.local = FALSE

######### FOR CLUSTER USE #########

if ( run.local == FALSE ) {
  # load command line arguments
  args = commandArgs(trailingOnly = TRUE)
  jobname = args[1]
  scen = args[2]  # this will be a letter
  boot.reps = as.numeric( args[3] )
  
  # get scen parameters
  setwd("/home/groups/manishad/multTest")
  scen.params = read.csv( "scen_params.csv" )
  p = scen.params[ scen.params$scen.name == scen, ]
  
  print(p)
  
  # no longer included in parameters because it's a vector
  crit.bonf = 0.05 / p$nY
  alpha = c( crit.bonf, 0.01, 0.05 )
  
  # simulation reps to run within this job
  # this need to match n.reps.in.doParallel in the genSbatch script
  sim.reps = 10
  
  ### load packages
  toLoad = c("foreach",
             "dplyr",
             "doParallel",
             "mvtnorm",
             "StepwiseTest", # Romano
             "matrixcalc")
  
  # load packages with informative messages if one can't be installed
  # **Common reason to get the "could not library" error: You did ml load R/XXX using an old version
  any.failed = FALSE
  for (pkg in toLoad) {
    
    cat( paste("\nAbout to try loading package", pkg) )
    
    tryCatch({
      # eval below needed because library() will otherwise be confused
      # https://www.mitchelloharawild.com/blog/loading-r-packages-in-a-loop/
      eval( bquote( library( .(pkg) ) ) )
    }, error = function(err) {
      cat( paste("\n*** COULD NOT LOAD PACKAGE:", pkg) )
      any.failed <<- TRUE
    })
    
  }
  if ( any.failed == TRUE ) stop("Some packages couldn't be loaded. See outfile for details of which ones.")
  ### end loading packages
  
  # for use in ml load R
  # install.packages( c("doParallel", "foreach", "mvtnorm", "StepwiseTest", "matrixcalc") )
  
  source("functions.R")
  
  # set the number of cores
  registerDoParallel(cores=16)
}


######### END OF CLUSTER PART #########


# ######### FOR LOCAL USE #########
# # setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Sandbox/2018-1-13")
# # p = read.csv("scen_params.csv")  # should be a single row, I think
# 

if ( run.local == TRUE ) {
  # THIS SCENARIO WORKED FINE LOCALLY
  n = 1000
  nX = 1
  nY = 40
  rho.XX = 0
  rho.YY = c(0)
  rho.XY = c(0)  # null hypothesis: 0
  prop.corr = 0.2
  
  # # SCEN IN 1.SBATCH
  # n = 1000
  # nX = 1
  # nY = 200
  # rho.XX = 0
  # rho.YY = c(0)
  # rho.XY = c(0) # null hypothesis: 0
  # prop.corr = c(0.20)  # exchangeable vs. half-correlated matrix
  
  # bootstrap iterates and type
  boot.reps = 50
  sim.reps = 1
  scen = 1
  bt.type = c( "ha.resid" )
  
  
  # matrix of scenario parameters
  scen.params = expand.grid( bt.type, n, nX, nY, rho.XX,
                             rho.YY, rho.XY, prop.corr )
  names(scen.params) = c( "bt.type", "n", "nX", "nY", "rho.XX",
                          "rho.YY", "rho.XY", "prop.corr" )
  
  # name the scenarios
  # remove letters that are privileged variables in R
  letter.names = c(letters, LETTERS)[ ! c(letters, LETTERS) %in% c("i","T","F") ]
  scen.params$scen.name = letter.names[ 1:dim(scen.params)[1] ]
  p = scen.params
  
  # add alpha corresponding to Bonferroni as first one
  # NOTE THAT CODE ASSUMES BONFERRONI IS THE FIRST ONE
  crit.bonf = 0.05 / p$nY
  alpha = c( crit.bonf, 0.01, 0.05 )
  
  library(doParallel)
  library(foreach)
  library(mvtnorm)
  library(StepwiseTest)  # Romano
  library(matrixcalc)
  
  setwd("~/Dropbox/Personal computer/Harvard/THESIS/Thesis paper #2 (MO)/Linked to OSF (MO)/Code git (MO)/Simulation study code/For Sherlock")
  source("functions.R")
  
  # set the number of cores
  registerDoParallel(cores=16)
}
# ######### END OF LOCAL PART #########


########################### THIS SCRIPT COMPLETELY RUNS 1 SIMULATION  ###########################

# draws a new dataset
# runs all bootstrapping iterations
# writes 1-row csv with number rejected and bootstrap CI

# names of methods that resample under H0 vs. HA
h0.methods = c("Y", "resid", "h0.parametric", "h0.freedman")
# **winning algorithm used in paper: ha.resid
ha.methods = c("fcr", "ha.resid", "ha.resid.2")

# j is the number of simulation iterations to run sequentially
# so for j=10, we are generating 10 observed datasets, along with 500 bootstrap iterates for each

# on Sherlock, this for-loop is supposed to receive a single ROW of parameters

# system.time is in seconds
doParallel.seconds = system.time({
  rs = foreach( i = 1:sim.reps, .combine = bind_rows ) %dopar% {
    #for debugging (out file will contain all printed things):
    #for ( i in 1:1 ) {
    
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
    # make initial dataset from which to bootstrap
    cor = make_corr_mat( .nX = p$nX,
                         .nY = p$nY,
                         .rho.XX = p$rho.XX,
                         .rho.YY = p$rho.YY,
                         .rho.XY = p$rho.XY,
                         .prop.corr = as.numeric(p$prop.corr) )
    d = sim_data( .n = p$n, .cor = cor )
    
    # extract names of outcome variables
    X.names = names(d)[ grep( "X", names(d) ) ]
    Y.names = names(d)[ grep( "Y", names(d) ) ]
    
    # get number rejected for observed dataset
    # vector with same length as .alpha
    # only return residuals and/or model estimates if we need them later for resampling
    samp.res = dataset_result( .dat = d,
                               .alpha = alpha,
                               .center.stats = FALSE,  # never center stats for the original sample
                               .bhat.orig = NA )
    n.rej = samp.res$rej  # first one is Bonferroni
    pvals = samp.res$pvals
    tvals = samp.res$tvals
    bhats = samp.res$bhats
    names(n.rej) = paste( "n.rej.", as.character(alpha), sep="" )
    
    # run all bootstrap iterates
    # no longer parallelized via doparallel
    r = foreach( i = 1:boot.reps, .combine=rbind )  %do% {
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
      # doesn't work! (generates uncorrelated Ys)
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
      #  doesn't work! (loses correlation between Ys)
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
      
      bt.res = dataset_result( .dat = b,
                               .alpha = alpha,
                               .center.stats = ifelse( p$bt.type %in% ha.methods, TRUE, FALSE ),
                               .bhat.orig = bhats )
      
      # return all the things
      list( rej = bt.res$rej,
            pvals = bt.res$pvals,
            tvals = bt.res$tvals )
      
      # get number rejected for bootstrap sample
      # we don't need to return residuals for this one
      # dataset_result( .dat = b, .alpha = alpha, .resid = FALSE )$rej
      # so the returned r is a dataframe of number rejected for each bootstrap iterate
      # one column for each value of alpha
      # r[["0.01"]] is the vector with length boot.reps of number rejected at alpha = 0.01
      
    } ###### end bootstrap
    
    
    
    
    ###### Data Wrangling for This Simulation Rep #####
    
    # organize rejections by alpha level
    u = unlist( r[,"rej"] )
    n.rej.bt.0.05 = u[ grepl( "0.05", names(u) ) ]
    n.rej.bt.0.01 = u[ grepl( "0.01", names(u) ) ]
    # n.rej.bt.0.005 = u[ grepl( "0.005", names(u) ) ]
    
    # resampled p-value matrix for Westfall
    # rows = Ys
    # cols = resamples
    p.bt = do.call( cbind, r[ , "pvals" ] )
    
    # resampled test statistic matrix (centered) for Romano
    # rows = Ys
    # cols = resamples
    t.bt = do.call( cbind, r[ , "tvals" ] )
    
    
    ###### Joint Test Results for This Simulation Rep #####
    
    # performance: one-sided rejection of joint null hypothesis
    # alpha for joint test is set to 0.05 regardless of alpha for individual tests
    # crit.bonf = quantile( n.rej.bt.0.005, 1 - 0.05 )
    crit.0.05 = quantile( n.rej.bt.0.05, 1 - 0.05 )
    crit.0.01 = quantile( n.rej.bt.0.01, 1 - 0.05 )
    
    # null interval limits
    bt.lo.0.01 = quantile( n.rej.bt.0.01, 0.025 )
    bt.hi.0.01 = quantile( n.rej.bt.0.01, 0.975 )
    
    bt.lo.0.05 = quantile( n.rej.bt.0.05, 0.025 )
    bt.hi.0.05 = quantile( n.rej.bt.0.05, 0.975 )
    
    # excess hits
    n.true.ours.0.05 = n.rej[["n.rej.0.05"]] - bt.hi.0.05
    n.true.ours.0.01 = n.rej[["n.rej.0.01"]] - bt.hi.0.01
    
    # p-values for observed rejections
    # jt.pval.bonf = sum( n.rej.bt.0.005 >= n.rej[,1] ) / length( n.rej.bt.0.005 )
    jt.pval.0.01 = sum( n.rej.bt.0.01 >= n.rej[["n.rej.0.01"]] ) /
      length( n.rej.bt.0.01 )
    jt.pval.0.05 = sum( n.rej.bt.0.05 >= n.rej[["n.rej.0.05"]] ) /
      length( n.rej.bt.0.05 )
    
    # did joint tests reject?
    # rej.jt.bonf = jt.pval.bonf < 0.05
    rej.jt.0.01 = jt.pval.0.01 < 0.05
    rej.jt.0.05 = jt.pval.0.05 < 0.05
    
    ######## Bonferroni joint test ########
    # Bonferroni test of joint null using just original data
    # i.e., do we reject at least one outcome using Bonferroni threshold?
    jt.rej.bonf.naive = n.rej[1] > 0
    # number of true effects with 95% confidence
    n.true.bonf = n.rej[1]
    
    ######## Holm joint test ########
    p.adj.holm = p.adjust( p = pvals, method = "holm" )
    jt.rej.holm = any( p.adj.holm < 0.05 )
    n.true.holm = sum( p.adj.holm < 0.05 )
    
    ######## Westfall's single-step and step-down ########
    p.adj.minP = adjust_minP( pvals, p.bt )
    jt.rej.minP = any( p.adj.minP < 0.05 )
    n.true.minP = sum( p.adj.minP < 0.05 )
    
    p.adj.stepdown = adj_Wstep( pvals, p.bt )
    jt.rej.Wstep = any( p.adj.stepdown < 0.05 )
    n.true.Wstep = sum( p.adj.stepdown < 0.05 )
    
    ######## Romano ########
    # regular FWER control (not k-FWER)
    
    # if we bootstrapped under Ha
    if ( p$bt.type %in% ha.methods ) {
      # test stats are already centered
      res = FWERkControl(tvals, as.matrix(t.bt), k = 1, alpha = 0.05)
      jt.rej.Romano = sum(res$Reject) > 0
      n.true.Romano = sum(res$Reject)
    }
    
    # if we bootstrapped under H0
    if ( p$bt.type %in% h0.methods ) {
      jt.rej.Romano = NA
      n.true.Romano = NA
    }
    
    
    ######## Mean Log-P-Value Joint Test ########
    # reject if mean log p-value is small compared to those in bootstraps
    # mean.log.p.bt = colMeans( log(p.bt) )
    # jt.rej.mean.p = mean( log(pvals) ) < quantile( mean.log.p.bt, 0.025 )
    
    ###### Write Results #####
    
    # return entire results of this sim rep
    # with a row for each boot iterate
    # these variables will be same for each bootstrap iterate
    # the individual bootstrap results are merged in after this
    # suppressWarnings because it whines about row names from a short variable
    rep.res = suppressWarnings( data.frame( scen = rep( scen, boot.reps ),
                                            bt.iterate = 1:boot.reps,
                                            #rep.minutes = as.vector(rep.time) / 60,
                                            
                                            # rejections in original dataset
                                            n.rej.bonf = n.rej[1],
                                            n.rej.0.01 = n.rej[["n.rej.0.01"]],
                                            n.rej.0.05 = n.rej[["n.rej.0.05"]],
                                            
                                            # mean rejections in bootstraps at 2 different alpha levels
                                            n.rej.bt.0.01.mean = mean(n.rej.bt.0.01),
                                            n.rej.bt.0.05.mean = mean(n.rej.bt.0.05),
                                            
                                            # joint test results for entire study
                                            jt.rej.bonf.naive = ifelse( as.numeric(jt.rej.bonf.naive) == 1, TRUE, FALSE ),
                                            jt.rej.holm,
                                            jt.rej.minP,
                                            jt.rej.Wstep,
                                            jt.rej.Romano,
                                            
                                            # number of true effects
                                            n.true.bonf,
                                            n.true.holm,
                                            n.true.minP,
                                            n.true.Wstep,
                                            n.true.Romano,
                                            n.true.ours.0.01,
                                            n.true.ours.0.05,
                                            
                                            # crit.bonf
                                            crit.0.05,
                                            crit.0.01,
                                            
                                            # bt.lo.bonf, bt.hi.bonf
                                            bt.lo.0.01, bt.hi.0.01,
                                            bt.lo.0.05, bt.hi.0.05,
                                            
                                            # jt.pval.bonf
                                            jt.pval.0.01,
                                            jt.pval.0.05,
                                            
                                            # rej.jt.bonf
                                            rej.jt.0.01,
                                            rej.jt.0.05
    ) )
    
    # all rows are static variables because we're not merging in resample-level
    #  results
    
    
    # keep only 1 row for this simulation rep (don't keep every boot iterate)
    rep.res = rep.res[1,]
    
    # add in scenario parameters
    # do NOT use rbind here; bind_cols accommodates possibility that some methods' rep.res
    #  have more columns than others
    rep.res = p %>% bind_cols( rep.res )
    
    # # add more info
    # rep.res = rep.res %>% add_column( rep.name = i, .before = 1 )
    # rep.res = rep.res %>% add_column( scen.name = scen, .before = 1 )
    # rep.res = rep.res %>% add_column( job.name = jobname, .before = 1 )
    
    return(rep.res)
    
    # # merge in the individual bootstrap results and parameters
    # # new.rows = cbind( p, new.rows, n.rej, r )
    # 
    # # remove redundant parameters column
    # new.rows = new.rows[ , !names(new.rows) == "scen.name" ]
    # 
    # # add row to output file
    # if ( j == 1 ) results = new.rows
    # if ( j > 1 ) results = rbind( results, new.rows )
    # # results should have boot.reps rows per "j" in the for-loop
    
  }  # end loop over j simulation reps
}) # end timer
  
  # results dataframe should now have boot.reps * sim.reps rows
  
  # sanity check of results
  r = results
  print( mean(r$jt.pval.0.05) )
  print( mean(r$rej.jt.0.05) )
  print( mean(r$jt.rej.Romano) )
  print( mean(r$jt.rej.Wstep) )
  
  
  ########################### WRITE LONG RESULTS  ###########################
  # setwd("/home/groups/manishad/multTest/sim_results/long")
  # write.csv( results, paste( "long_results", jobname, ".csv", sep="_" ) )
  
  
  ########################### WRITE SHORT RESULTS ###########################
  # keep only 1 row per simulation rep
  #keep.row = rep( c( TRUE, rep(FALSE, boot.reps - 1) ), sim.reps )
  
  setwd("/home/groups/manishad/multTest/sim_results/short")
  write.csv( rs, paste( "short_results", jobname, ".csv", sep="_" ) )
  
  