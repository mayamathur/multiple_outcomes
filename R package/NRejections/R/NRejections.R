

########################### FN: FIT ONE OUTCOME MODEL (OLS) ###########################

fit_model = function( X,
                      C,
                      Y,
                      Ys,
                      d,
                      center.stats = FALSE,
                      bhat.orig = NA,  # bhat.orig is a single value now for just the correct Y
                      alpha = 0.05 ) {

  # all covariates, including the one of interest
  covars = c(X,C)
  
  # https://stackoverflow.com/questions/6065826/how-to-do-a-regression-of-a-series-of-variables-without-typing-each-variable-nam
  m = lm( d[[Y]] ~ ., data = d[ , covars] )
  
  # stats for covariate of interest
  m.stats = summary(m)$coefficients[ X, ]
  
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

data(attitude)
fit_model( X = "complaints",
           C = c("privileges", "learning"),
           Y = "rating",
           Ys = c("rating", "raises"),
           d = attitude,
           center.stats = FALSE,
           bhat.orig = NA,  # bhat.orig is a single value now for just the correct Y
           alpha = 0.05 )



########################### FN: GIVEN DATASET, RETURN STATS ###########################

dataset_result = function( X,
                           C,
                           Ys,  # all outcome names
                           d,
                           alpha = 0.05,
                           center.stats = TRUE,
                           bhat.orig = NA ) {  
  
  # for each outcome, fit regression model
  # see if each has p < alpha for covariate of interest
  covars = c( X, C )
  
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



samp.res = dataset_result( X = "complaints",
           C = c("privileges", "learning"),
           Ys = c("rating", "raises"),
           d = attitude,
           center.stats = FALSE,
           bhat.orig = NA,  # bhat.orig is a single value now for just the correct Y
           alpha = 0.05 )



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
                           C,
                           Ys,
                           d,
                           alpha,
                           resid,
                           bhat.orig,
                           B=20,
                           cores = NULL ) {
  

  covars = c( X, C )
  
  # compute Y-hat using residuals
  Yhat = d[, Ys] - resid
  
  # fix the existing covariates
  Xs = as.data.frame( d[ , covars ] )
  
  # run all bootstrap iterates
  library(doParallel)
  registerDoParallel(cores=cores) 
  
  # run all resamples: takes ~10 min
  r = foreach( i = 1:B, .combine=rbind ) %dopar% {
    
    # resample residuals and add them to fitted values
    ids = sample( 1:nrow(d), replace=TRUE )
    b = as.data.frame( cbind( Xs, Yhat + resid[ids,] ) )
    
    bhats = rep( NA, length(Y) )
    
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

samp.res = dataset_result( X = "complaints",
                C = c("privileges", "learning"),
                Ys = c("rating", "raises"),
                d = attitude,
                center.stats = FALSE,
                bhat.orig = NA,  # bhat.orig is a single value now for just the correct Y
                alpha = 0.05 )

resamps = resample_resid(  X = "complaints",
                  C = c("privileges", "learning"),
                  Ys = c("rating", "raises"),
                  d = attitude,
                  alpha = 0.05,
                  resid = samp.res$resid,
                  bhat.orig = samp.res$b,
                  B=20,
                  cores = 8)


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
  # fit original OLS model to get bhat.orig
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

# BOOKMARK
corr_tests( d = attitude,
          X = "complaints",
          C = c("privileges", "learning"),
          Ys = c("rating", "raises"),
          B=50,
          cores = 8,
          alpha = 0.05,
          alpha.fam = 0.05,
          method = c( "nreject", "bonferroni", "holm", "minP", "Wstep", "romano" ) )


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



######################## FNS FOR WESTFALL's STEP-DOWN ########################

# AUDITED :) 
# Westfall textbook, pages 66-67

# Arguments: 
#  p.dat: p-values from dataset (W-vector?)
#  p.bt: Bootstrapped p-values (a W X B matrix)

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


# # Sanity Check
# nX = 1
# nY = 3
# B = 5
# cor = make_corr_mat( .nX = nX,
#                      .nY = nY,
#                      .rho.XX = 0,
#                      .rho.YY = 0.25,
#                      .rho.XY = 0.05,
#                      .prop.corr = 1 )
# 
# d = sim_data( .n = 1000, .cor = cor )
# 
# samp.res = dataset_result( .dat = d,
#                 .alpha = 0.05,
#                 .center.stats = FALSE )
# 
# 
# p.bt = matrix( NA, nrow = B, ncol = nY)
# 
# # do 5 bootstraps
# for (i in 1:B) {
#   # extract residuals from original data
#   # matrix with same dimensions as the outcomes matrix
#   resid = samp.res$resid
#   
#   # compute Y-hat using residuals
#   Ys = d[ , (nX + 1) : ncol(d) ]  # remove covariates
#   Yhat = Ys - resid
#   
#   # fix the existing covariates
#   Xs = as.data.frame( d[ , 1 : nX ] )
#   names(Xs) = X.names
#   
#   # resample residuals and add them to fitted values
#   ids = sample( 1:nrow(d) )
#   b = as.data.frame( cbind( Xs, Yhat + resid[ids,] ) )
#   
#   bt.res = dataset_result( .dat = b,
#                            .alpha = 0.05,
#                            .center.stats = TRUE,
#                            .bhat.orig = samp.res$bhats )
#   
#   p.bt[i,] = bt.res$pvals
# }
# 
# pvals = c(0.00233103655078803, 0.470366742594242, 0.00290278216035089
# )
# 
# p.bt = structure(c(0.308528665936264, 0.517319402377912, 0.686518314693482, 
#                    0.637306248855186, 0.106805510862352, 0.116705315041494, 0.0732076817175753, 
#                    0.770308936364482, 0.384405349738909, 0.0434358213611965, 0.41497067850141, 
#                    0.513471489744384, 0.571213377144122, 0.628054979652722, 0.490196884985226
# ), .Dim = c(5L, 3L))
# 
# 
# 
# p.adj.Wstep = adj_Wstep(pvals, t(p.bt))
# plot( samp.res$pvals, p.adj.Wstep )
# 
# # BOOKMARK - CHECK MANUALLY AS IN PREVIOUS TOY EXAMPLE
# 
# # indicators of which hypothesis the sorted p-vals go with
# sort(pvals)
# r = c(1,3,2)
# 
# qstar = matrix( NA, nrow = B, ncol = 3)
# 
# for (i in 1:nrow(p.bt)) {
#   qstar[i,3] = p.bt[ i, r[3] ]
#   qstar[i,2] = min( qstar[i,3], p.bt[ i, r[2] ] )
#   qstar[i,1] = min( qstar[i,2], p.bt[ i, r[1] ] )
# }
# 
# less = t( apply( qstar, MARGIN = 1,
#               function(row) row <= sort(pvals) ) )
# 
# p.tilde = colMeans(less)
# 
# # enforce monotonicity
# p.tilde.sort = sort(p.tilde)
# p.tilde.sort[2] = max( p.tilde.sort[1], p.tilde.sort[2] )
# p.tilde.sort[3] = max( p.tilde.sort[2], p.tilde.sort[3] )



########################### FN: CALCULATE CRITICAL VALUES FOR WSTEP ###########################

# AUDITED :) 

# Arguments: 
#  p.dat: p-values from dataset (W-vector?)
#  col.p: Column of resampled p-values (for the single p-value for which we're
#   getting the critical value)?

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

# sanity check: see above for Wstep