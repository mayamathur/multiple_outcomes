
# library(devtools)
# setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/git_multiple_outcomes/R package/NRejections")
# document()
# library(NRejections)
# test()

########################### FN: FIT ONE OUTCOME MODEL (OLS) ###########################

fit_model = function( X,
                      C = NA,
                      Y,
                      Ys,
                      d,
                      center.stats = FALSE,
                      bhat.orig = NA,  # bhat.orig is a single value now for just the correct Y
                      alpha = 0.05 ) {

  # all covariates, including the one of interest
  if ( is.na(C) ) covars = X
  else covars = c( X, C )
  
  if ( is.na(C) ) m = lm( d[[Y]] ~ d[[X]] )
  # https://stackoverflow.com/questions/6065826/how-to-do-a-regression-of-a-series-of-variables-without-typing-each-variable-nam
  else m = lm( d[[Y]] ~ ., data = d[ , covars] )
  
  #browser()
  
  # stats for covariate of interest
  if ( is.na(C) ) m.stats = summary(m)$coefficients[ 2, ]
  else m.stats = summary(m)$coefficients[ X, ]
    
  # should we center stats by the original-sample estimates?
  if( !center.stats ) {
    b = m.stats[["Estimate"]]
    tval = m.stats[["t value"]]
    SE = m.stats[["Std. Error"]]
  }
  if( center.stats ) {
    # ~~~ CHECK THIS WAY OF ACCESSING BHAT.ORIG
    b = m.stats[["Estimate"]] - bhat.orig[ which(Ys==Y) ]
    SE = m.stats[["Std. Error"]]
    tval = b / SE
  }
  
  df = nrow(d) - length(covars) - 1
  pval = 2 * ( 1 - pt( abs( b / SE ), df ) )
  
  stats = data.frame( outcome = Y,
                      b = b,
                      SE = SE,  
                      df = df,
                      tval = tval,
                      pval = pval,
                      reject = pval < alpha )
  
  return( list( stats = stats,
                resids = residuals(m) ) )
}

# # test for 1 outcome
# raw.res = fit_model(  Y.name = outcomes[1],
#             X.name = "A1SEPA_z",
#             .d = d,
#             .covars = covars,
#             .center.stats = FALSE )

# data(attitude)
# fit_model( X = "complaints",
#            C = c("privileges", "learning"),
#            Y = "rating",
#            Ys = c("rating", "raises"),
#            d = attitude,
#            center.stats = FALSE,
#            bhat.orig = NA,  # bhat.orig is a single value now for just the correct Y
#            alpha = 0.05 )



########################### FN: GIVEN DATASET, RETURN STATS ###########################

dataset_result = function( X,
                           C = NA,
                           Ys,  # all outcome names
                           d,
                           alpha = 0.05,
                           center.stats = TRUE,
                           bhat.orig = NA ) {  
  
  # for each outcome, fit regression model
  # see if each has p < alpha for covariate of interest
  if ( is.na(C) ) covars = X
  else covars = c( X, C )
  
  # get the correct bhat for the outcome we're using
  
  # this is a list of lists:
  #  length is equal to number of outcomes
  #  each entry is another list
  # with elements "pval" (scalar) and "resid" (length matches number of subjects)
  
  lists = lapply( X = Ys,
                  FUN = function(y) fit_model( X = X,
                                               C = C,
                                               Y = y,
                                               Ys = Ys,
                                               d = d,
                                               center.stats = center.stats,
                                               bhat.orig = bhat.orig,
                                               alpha = alpha ) )
  
  # "flatten" the list of lists
  u = unlist(lists)
  pvals = as.vector( u[ names(u) == "stats.pval" ] )
  
  tvals = as.vector( u[ names(u) == "stats.tval" ] )
  bhats = as.vector( u[ names(u) == "stats.b" ] )
  pvals = as.vector( u[ names(u) == "stats.pval" ] )

  # save residuals
  # names of object u are resid.1, resid.2, ..., hence use of grepl 
  mat = matrix( u[ grepl( "resid", names(u) ) ], byrow=FALSE, ncol=length(Ys) ) 
  resid = as.data.frame(mat)
  names(resid) = Ys
  
  # returns vector for number of rejections at each alpha level
  # length should match length of .alpha
  n.reject = vapply( X = alpha, FUN = function(a) sum( pvals < a ), FUN.VALUE=-99 )
  
  return( list( rej = n.reject,
                tvals = tvals,
                bhats = bhats,
                pvals = pvals,
                resid = resid ) )
}



# samp.res = dataset_result( X = "complaints",
#            C = c("privileges", "learning"),
#            Ys = c("rating", "raises"),
#            d = attitude,
#            center.stats = FALSE,
#            bhat.orig = NA,  # bhat.orig is a single value now for just the correct Y
#            alpha = 0.05 )



########################### FN: GENERATE RESAMPLES ###########################


#' Generate OLS resamples under global null
#' 
#' XXX 
#' @param d Dataframe 
#' @param X Single quoted name of covariate of interest
#' @param C Vector of quoted covariate names
#' @param Y Vector of quoted outcome names
#' @param B Number of resamples to generate
#' @export
#' @examples
#' # compute E-value if Cohen's d = 0.5 with SE = 0.25
#' evalues.MD( .5, .25 )

resample_resid = function( 
                           X,
                           C = NA,
                           Ys,
                           d,
                           alpha,
                           resid,
                           bhat.orig,
                           B=20,
                           cores = NULL ) {
  
  if ( is.na(C) ) covars = X
  else covars = c( X, C )
  
  # compute Y-hat using residuals
  Yhat = d[, Ys] - resid
  
  # fix the existing covariates
  Xs = as.data.frame( d[ , covars ] )
  if( is.na(C) ) names(Xs) = X
  
  # run all bootstrap iterates
  library(doParallel)
  registerDoParallel(cores=cores) 
  
  # run all resamples: takes ~10 min
  r = foreach( i = 1:B, .combine=rbind ) %dopar% {
    
    # resample residuals and add them to fitted values
    ids = sample( 1:nrow(d), replace=TRUE )
    b = as.data.frame( cbind( Xs, Yhat + resid[ids,] ) )
    
    bhats = rep( NA, length(Ys) )
    
    bt.res = dataset_result( X = X,
                             C = C,
                             Ys = Ys, 
                             d = b,
                             alpha = alpha,
                             center.stats = TRUE,
                             bhat.orig = bhat.orig )

    # return all the things
    list( rej = bt.res$rej,
          pvals = bt.res$pvals,
          tvals = bt.res$tvals )
    
  } ###### end r-loop (parallelized bootstrap)
  
  # resampled p-value matrix for Westfall
  # rows = Ys
  # cols = resamples
  p.bt = do.call( cbind, r[ , "pvals" ] )
  
  # resampled test statistic matrix (uncentered) for Romano
  # rows = Ys
  # cols = resamples
  t.bt = do.call( cbind, r[ , "tvals" ] )
  
  # number of rejections
  rej.bt = do.call( cbind, r[ , "rej" ] )
  #rej.bt.0.05 = rej.bt[1,]
  #rej.bt.0.01 = rej.bt[2,]
  
  return( list( p.bt = as.matrix(p.bt),
                t.bt = as.matrix(t.bt),
                rej.bt = as.matrix(rej.bt) ) )
  
}

# samp.res = dataset_result( X = "complaints",
#                 C = c("privileges", "learning"),
#                 Ys = c("rating", "raises"),
#                 d = attitude,
#                 center.stats = FALSE,
#                 bhat.orig = NA,  # bhat.orig is a single value now for just the correct Y
#                 alpha = 0.05 )
# 
# resamps = resample_resid(  X = "complaints",
#                   C = c("privileges", "learning"),
#                   Ys = c("rating", "raises"),
#                   d = attitude,
#                   alpha = 0.05,
#                   resid = samp.res$resid,
#                   bhat.orig = samp.res$b,
#                   B=20,
#                   cores = 8)


########################### WRAPPER FN: ESTIMATE OUR METRICS ###########################

#' Our metrics
#' 
#' XXX 
#' @param d Dataframe 
#' @param X Single quoted name of covariate of interest
#' @param C Vector of quoted covariate names
#' @param Y Vector of quoted outcome names
#' @param B Number of resamples to generate
#' @param alpha Vector of alpha levels
#' @param method Which methods to report (ours, Westfall's two methods, Bonferroni, Holm, Romano)
#' @export


corr_tests = function( d,
                       X,
                       C,
                       Ys,
                       B=20,
                       cores,
                       alpha = 0.05,
                       alpha.fam = 0.05,
                       method = "nreject" ) {
  
  # ~~~ TO DO:
  # discard incomplete cases and warn user
  # check to exclude any weird lm() specifications that we can't handle
  
  # fit models to original data
  samp.res = dataset_result( X = X,
                             C = C,
                             Ys = Ys,
                             d = d,
                             center.stats = FALSE,
                             bhat.orig = NA, 
                             alpha = alpha )
  
  resamps = resample_resid(  X = X,
                             C = C,
                             Ys = Ys,
                             d = attitude,
                             alpha = alpha,
                             resid = samp.res$resid,
                             bhat.orig = samp.res$b,
                             B=B,
                             cores = cores)

  ###### Joint Test Results for This Simulation Rep #####
  
  # initialize results
  global.test = data.frame( method = method, 
                            reject = rep( NA, length(method) ),
                            pval = rep( NA, length(method) ), 
                            crit = rep( NA, length(method) ) )
  
  # null interval limits
  bt.lo = quantile( resamps$rej.bt, alpha.fam/2 )
  bt.hi = quantile( resamps$rej.bt, 1 - alpha.fam/2 )
  
  if ( "nreject" %in% method ) {
    # performance: one-sided rejection of joint null hypothesis
    # alpha for joint test is set to 0.05 regardless of alpha for individual tests
    # crit.bonf = quantile( n.rej.bt.0.005, 1 - 0.05 )
    crit = quantile( resamps$rej.bt, 1 - alpha.fam )
    
    # p-values for observed rejections
    jt.pval = sum( resamps$rej.bt >= samp.res$n.rej ) /
      length( resamps$rej.bt )
    
    # did joint tests reject?
    rej.jt = jt.pval < alpha.fam
    
    # store results
    global.test$reject[ global.test$method == "nreject" ] = rej.jt
    global.test$pval[ global.test$method == "nreject" ] = jt.pval
    global.test$crit[ global.test$method == "nreject" ] = crit
  }

  
  ######## Bonferroni joint test ########
  if ( "bonferroni" %in% method ) {
    # Bonferroni test of joint null using just original data
    # i.e., do we reject at least one outcome using Bonferroni threshold?
    p.adj.bonf = p.adjust( p = samp.res$pvals, method = "bonferroni" )
    jt.rej.bonf.naive = any( p.adj.bonf < alpha.fam )
    
    # store results
    global.test$reject[ global.test$method == "bonferroni" ] = jt.rej.bonf.naive
    global.test$pval[ global.test$method == "bonferroni" ] = min(p.adj.bonf)
  }
  
  ######## Holm joint test ########
  if ( "holm" %in% method ) {
    p.adj.holm = p.adjust( p = samp.res$pvals, method = "holm" )
    jt.rej.holm = any( p.adj.holm < alpha.fam )
    
    # store results
    global.test$reject[ global.test$method == "holm" ] = jt.rej.holm
    global.test$pval[ global.test$method == "holm" ] = min(p.adj.holm)
  }
  
  ######## Westfall's single-step and step-down ########
  
  library(StepwiseTest)
  
  if ( "minP" %in% method ) {
    p.adj.minP = adjust_minP( samp.res$pvals, resamps$p.bt )
    jt.rej.minP = any( p.adj.minP < alpha.fam )
    
    # store results
    global.test$reject[ global.test$method == "minP" ] = jt.rej.minP
    global.test$pval[ global.test$method == "minP" ] = min(p.adj.minP)
  }
  
  if ( "Wstep" %in% method ) {
    p.adj.stepdown = adj_Wstep( samp.res$pvals, resamps$p.bt )
    jt.rej.Wstep = any( p.adj.stepdown < alpha.fam )
    
    # store results
    global.test$reject[ global.test$method == "Wstep" ] = jt.rej.Wstep
    global.test$pval[ global.test$method == "Wstep" ] = min(p.adj.stepdown)
  }
  
  ######## Romano ########

  if ("romano" %in% method ) {
  
    # test stats are already centered
    rom = FWERkControl( samp.res$tvals, as.matrix( resamps$t.bt ), k = 1, alpha = alpha.fam )
    jt.rej.Romano = sum(rom$Reject) > 0
    
    # store results
    global.test$reject[ global.test$method == "romano" ] = jt.rej.Romano
  }
  
  ######## Return Stuff ########
  
  # for global tests, concantante them and their names to a new vector
  
  
  return( list( samp.res = samp.res,
                nrej.bt = resamps$rej.bt,
                null.int = c(bt.lo, bt.hi), # ours
                excess.hits = as.numeric( samp.res$rej - bt.hi ), # ours
                global.test = global.test # dataframe of all user-specified methods with their names
                ) )
}

# # BOOKMARK
# corr_tests( d = attitude,
#           X = "complaints",
#           C = c("privileges", "learning"),
#           Ys = c("rating", "raises"),
#           B=50,
#           cores = 8,
#           alpha = 0.05,
#           alpha.fam = 0.05,
#           method = c( "nreject", "bonferroni", "holm", "minP", "Wstep", "romano" ) )


######################## FNS FOR WESTFALL's SINGLE-STEP ########################

# AUDITED :) 

# Returns minP-adjusted p-values (single-step)
# See Westfall text, pg. 48.

# Arguments: 
# p: Original p-values (vector)
# p.bt: Bootstrapped p-values (an W X B matrix)

adjust_minP = function( p, p.bt ) {
  
  n.boot = ncol(p.bt)
  
  # keep only minimum p-value in each resample
  minP.bt = apply( p.bt, MARGIN = 2, FUN = min )
  
  # for each element of p, get the proportion of resamples
  #  whose minP was less than the present p-value
  p.adj = unlist( lapply( p, FUN = function(x) sum( minP.bt <= x ) / n.boot ) )
  
  return(p.adj)
}

# # sanity check
# B = 200
# n.tests = 10
# 
# # generate fake p-values under strong null
# p.bt = matrix( runif(B*n.tests, 0, 1), nrow = n.tests)
# 
# # generate fake p-values from real dataset
# p = runif( n.tests, 0, .1)  
# 
# p.adj = adjust_minP( p, p.bt )
# plot(p, p.adj)
# 
# # manually adjust second p-value
# mins = apply( p.bt, MARGIN = 2, FUN = min )
# prop.table( table( mins <= p[2] ) )[["TRUE"]]
# p.adj[2]


######################## FNS FOR WESTFALL's SINGLE-STEP ########################

#' Return ordered critical values for Wstep
#' 
#' Returns minP-adjusted p-values (single-step). See Westfall text, pg. 48. 
#' @param p Original dataset p-values (W-vector) 
#' @param p.bt Bootstrapped p-values (an W X B matrix)
#' @export

adjust_minP = function( p, p.bt ) {
  
  n.boot = ncol(p.bt)
  
  # keep only minimum p-value in each resample
  minP.bt = apply( p.bt, MARGIN = 2, FUN = min )
  
  # for each element of p, get the proportion of resamples
  #  whose minP was less than the present p-value
  p.adj = unlist( lapply( p, FUN = function(x) sum( minP.bt <= x ) / n.boot ) )
  
  return(p.adj)
}




######################## FNS FOR WESTFALL's STEP-DOWN ########################


#' Return Wstep-adjusted p-values
#' 
#' Returns Wstep-adjusted p-values. See Westfall text, pg. 66-67. 
#' @param p Original dataset p-values (W-vector) 
#' @param p.bt Bootstrapped p-values (an W X B matrix)
#' @export

adj_Wstep = function( p, p.bt ) {
  
  # attach indices to original p-values
  # to keep track of their original order
  p.dat = data.frame( ind = 1:length(p), p )
  
  # sort original p-values
  p.dat = p.dat[ order( p.dat$p, decreasing = FALSE ), ]
  
  # in this order, ind is now the same as Westfall's r_i on pg 67:
  #  e.g., r_1 is the location in the original list of the 
  #  smallest p-value
  r.ind = p.dat$ind
  
  # critical values
  # they get smaller and smaller, as do the sorted p-vals
  # pass one resample's bootstrapped p-values to get_crit
  crit.mat = apply( p.bt, MARGIN = 2,
                    FUN = function(x) get_crit( p.dat, x) )
  
  # for each column of crit values (i.e., each resample), 
  # see if the sorted p-value is greater than its crit value
  less = apply( crit.mat, MARGIN = 2, 
                FUN = function(x) x <= p.dat$p )
  
  # adjusted p-values are means of the above
  p.adj.1 = apply( less, MARGIN = 1, mean )
  
  # enforce monotonicity
  p.adj.2 = rep( NA, length(p.adj.1) )
  
  for ( i in 1:length(p.adj.1) ) {
    if (i == 1) p.adj.2[i] = p.adj.1[1]
    else p.adj.2[i] = max( p.adj.2[i-1], p.adj.1[i] )
  }
  p.dat$p.adj = p.adj.2
  
  # put back in order of original p-values
  p.dat = p.dat[ order( p.dat$ind, decreasing = FALSE ), ]
  
  return( p.dat$p.adj )
  #plot( p.dat$p, p.dat$p.adj )
}




########################### FN: CALCULATE CRITICAL VALUES FOR WSTEP ###########################

#' Return ordered critical values for Wstep
#' 
#' XXX 
#' @param p.dat p-values from dataset (W-vector?) 
#' @param col.p Column of resampled p-values (for the single p-value for which we're
#   getting the critical value)?
#' @export

get_crit = function( p.dat, col.p ) {
  
  # sort bootstrapped p-values according to original ones
  col.p.sort = col.p[ p.dat$ind ]
  
  qstar = rep( NA, length(col.p.sort) )
  
  k = length(col.p.sort)
  for ( i in k:1 ) {  # count backwards
    if (i == k) qstar[i] = col.p.sort[k]
    else qstar[i] = min( qstar[i+1], col.p.sort[i] )
  }
  
  return(qstar)
}

########################### FN: CREATE CORRELATION MATRIX ###########################

#' Makes correlation matrix to simulate data
#' 
#' If correlation matrix isn't positive definite, try reducing some of the correlations
#' @param nX Number of covariates, including the one of interest 
#' @param nY Number of outcomes
#' @param rho.XX Correlation between all pairs of Xs
#' @param rho.YY Correlation between all pairs of Ys
#' @param rho.XY Correlation between pairs of X-Y that are not null (see below)
#' @param prop.corr Proportion of X-Y pairs that are non-null (non-nulls will be first .prop.corr * .nY pairs)
#' @export

make_corr_mat = function( nX,
                          nY,
                          rho.XX,
                          rho.YY,
                          rho.XY,
                          prop.corr = 1) {
  
  nVar = nX + nY
  
  # name the variables
  X.names = paste0( "X", 1 : nX )
  Y.names = paste0( "Y", 1 : nY )
  vnames = c( X.names, Y.names )
  
  # initialize correlation matrix
  cor = as.data.frame( matrix( NA, nrow = nVar, ncol = nVar ) )
  names(cor) = vnames
  row.names(cor) = vnames
  
  # populate each cell 
  for ( r in 1:dim(cor)[1] ) {
    for ( c in 1:dim(cor)[2] ) {
      cor[ r, c ] = cell_corr( vname.1 = vnames[r],
                               vname.2 = vnames[c],
                               rho.XX = rho.XX,
                               rho.YY = rho.YY,
                               rho.XY = rho.XY,
                               nY = nY,
                               prop.corr = prop.corr )
    }
  }
  
  # check if positive definite
  if( ! is.positive.definite( as.matrix(cor) ) ) stop( "Correlation matrix not positive definite")
  
  return(cor)  # this is still a data.frame in order to keep names
}



########################### FN: CREATE CORRELATION MATRIX ###########################

#' Cell correlation for simulating data
#' 
#' Assumes X1 is the covariate of interest and that none of the covariates is associated with any outcomes. 
#' @param vname.1 Quoted name of first variable 
#' @param vname.2 Quoted name of second variable
#' @param rho.XX Correlation between pairs of Xs
#' @param rho.YY Correlation between all pairs of Ys
#' @param rho.XY rho.XY Correlation between pairs of X-Y (of non-null ones)
#' @param nY Number of outcomes
#' @param prop.corr Proportion of X-Y pairs that are non-null (non-nulls will be first .prop.corr * .nY pairs)
#' @export

cell_corr = function( vname.1,
                      vname.2,
                      rho.XX,
                      rho.YY,
                      rho.XY,
                      nY,
                      prop.corr = 1) {
  
  # use grep to figure out if variables are covariates or outcomes
  if ( length( grep("X", vname.1 ) ) == 1 ) {
    vtype.1 = "covariate"
  } else if ( length( grep("Y", vname.1 ) ) == 1 ) {
    vtype.1 = "outcome"
  }
  
  if ( length( grep("X", vname.2 ) ) == 1 ) { 
    vtype.2 = "covariate"
  } else if ( length( grep("Y", vname.2 ) ) == 1 ) {
    vtype.2 = "outcome"
  }
  
  # case 1: diagonal entry
  if ( vname.1 == vname.2 ) return(1)
  
  # case 2: both are covariates
  # fixed correlation between covariates
  if ( vtype.1 == "covariate" & vtype.2 == "covariate" ) return( rho.XX )
  
  # case 3: both are outcomes
  if ( vtype.1 == "outcome" & vtype.2 == "outcome" ) return( rho.YY )
  
  # case 4: one is a covariate and one is an outcome
  if ( vtype.1 != vtype.2 ) {
    # check if this is the covariate of interest
    # equal correlation between X1 and all outcomes
    # all other covariates have correlation 0 with outcome
    if ( "X1" %in% c( vname.1, vname.2 ) ) {
      
      if (prop.corr == 1) return( rho.XY ) 
      
      # if only some X-Y pairs are non-null
      if ( prop.corr != 1 ) {
        # find the one that is the outcome
        outcome.name = ifelse( vtype.1 == "outcome", vname.1, vname.2 )
        
        # extract its number ("4" for "Y4")
        num = as.numeric( substring( outcome.name, first = 2 ) )
        
        # the first last.correlated outcomes are correlated with X (rho.XY)
        #  and the rest aren't
        last.correlated = round( nY * prop.corr )
        
        # see if number for chosen outcome exceeds the last correlated one
        return( ifelse( num > last.correlated, 0, rho.XY ) )
      }
    }
    
    # if we have multiple covariates and this isn't the one of interest,
    #  its effect size should be 0
    else return(0)
  }
}




########################### FN: SIMULATE 1 DATASET ###########################

#' Simulate MVN data
#' 
#' Simulates 1 dataset with MVN(0,1) correlated covariates and outcomes
#' @param n Number of covariates, including the one of interest 
#' @param cor Correlation matrix (e.g., from make_corr_mat)
#' @export

sim_data = function( n, cor ) {
  
  # variable names
  vnames = names( cor )
  
  # simulate the dataset
  # everything is a standard Normal
  d = as.data.frame( rmvnorm( n = n,
                              mean = rep( 0, dim(cor)[1] ),
                              sigma = as.matrix(cor) ) )
  names(d) = vnames
  
  # return the dataset
  return(d)
}

