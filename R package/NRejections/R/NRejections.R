
# library(devtools)
# setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/git_multiple_outcomes/R package/NRejections")
# document()
# library(NRejections)
# test()


########################### CHECK FOR BAD USER INPUT ###########################

#' Fix bad user input
#' 
#' The user does not need to call this function. Warns about and fixes bad user input: missing data on analysis variables,
#' datasets containing extraneous variables, or datasets containing covariates that are not
#' mean-centered.
#' @param X Single quoted name of covariate of interest
#' @param C Vector of quoted covariate names
#' @param Ys Vector of quoted outcome names
#' @param d Dataframe 
#' @export

fix_input = function( X, 
                      C, 
                      Ys,
                      d ) {

  # all covariates, including the one of interest
  if ( all( is.na(C) ) ) covars = X
  else covars = c( X, C )
  
  ##### Missing Data #####
  # remove subjects missing any outcome or covariate (to have same sample size for all regressions)
  # also remove any non-analysis variables from dataset
  has.analysis.vars = complete.cases( d[ , c(covars, Ys) ] ) 
  d = d[ has.analysis.vars, ]
  dropped = sum( has.analysis.vars == FALSE )
  if (dropped > 0) warning( paste( dropped,
                                   " observations were dropped from analysis because they were
                                   missing at least one analysis variable", 
                                   sep = "") )
  
  ##### Extraneous Variables #####
  # remove non-analysis variables from dataset
  analysis.vars = c(covars, Ys)
  extras = names(d)[ ! names(d) %in% analysis.vars ]
  if ( length(extras) > 0 ){ 
    warning( paste( "The following variables were removed from the dataset because they are not in X, C, or Ys: ",
                    paste( extras, collapse = ", " ), 
                    sep = "" ) )
    d = d[ , analysis.vars ]
  }
  return(d)
}


########################### WRAPPER FN: ESTIMATE OUR METRICS ###########################

#' Global evidence strength across correlated tests
#' 
#' This is the main wrapper function for the user to call. For an arbitrary number of outcome variables, regresses the outcome
#' on an exposure of interest (\code{X}) and adjusted covariates (\code{C}). Returns the results of the original sample
#' (statistics and inference corresponding to X for each model, along with the observed number of rejections),
#' a 100*(1 - \code{alpha.fam}) percent null interval for the
#' number of rejections in samples generated under the global null, the excess hits
#' (the difference between the observed number of rejections and the upper null interval limit), 
#' and results of a test of the global null hypothesis at \code{alpha.fam} of the global null. The global test 
#' can be conducted based on the number of rejections or based on various FWER-control methods (see References).
#' @param d Dataframe 
#' @param X Single quoted name of covariate of interest
#' @param C Vector of quoted covariate names
#' @param Ys Vector of quoted outcome names
#' @param B Number of resamples to generate
#' @param cores Number of cores to use for parallelization. Defaults to number available.
#' @param alpha Alpha level for individual hypothesis tests
#' @param alpha.fam Alpha level for global test and null interval
#' @param method Which methods to report (ours, Westfall's two methods, Bonferroni, Holm, Romano)
#' @return \code{samp.res} is a list containing the number of observed rejections (\code{rej}), 
#' the coefficient estimates of interest for each outcome model (\code{bhats}), their t-values
#' (\code{tvals}), and their uncorrected p-values at level \code{alpha} (\code{pvals}).
#' 
#' \code{nrej.bt} contains the number of rejections in each bootstrap resample. 
#' 
#' \code{null.int} contains the lower and upper limits of a 100*(1 - \code{alpha.fam}) percent null interval.
#' 
#' \code{excess.hits} is the difference between the observed rejections and the upper limit of the null interval.
#' 
#' \code{global.test} is a dataframe containing global test results for each user-specified method, including
#' an indicator for whether the test rejects the global null at \code{alpha.fam} (\code{reject}), the p-value
#' of the global test where possible (\code{reject}), and the critical value of the global test based on the number
#' of rejections (\code{crit}).
#' @import
#' StepwiseTest
#' stats
#' @references 
#' Mathur, M.B., & VanderWeele, T.J. (in preparation). New metrics for multiple testing with correlated
#' outcomes.
#' 
#' Romano, J. P., & Wolf, M. (2007). Control of generalized error rates in multiple testing. The
#' Annals of Statistics, 1378-1408.
#' 
#' Westfall, P. H., & Young, S. S. (1993). Resampling-based multiple testing: Examples and
#' methods for p-value adjustment. Taylor & Francis Group.
#' @export
#' @examples
#' # uses a too-small number of resamples as a toy example
#' corr_tests( d = attitude,
#' X = "complaints",
#' C = c("privileges", "learning"),
#' Ys = c("rating", "raises"),
#' B=50,
#' cores=1,
#' alpha = 0.05,
#' alpha.fam = 0.05,
#' method = c( "nreject", "bonferroni", "holm", "minP", "Wstep", "romano" ) )
#' 
#' \dontrun{
#' data(rock)
#'
#'res = corr_tests( d = rock,
#'                  X = c("area"),
#'                  C = NA,
#'                  Ys = c("perm", "peri", "shape"),
#'                  method = "nreject" )
#'
#'# mean rejections in resamples
#'# should be close to 0.05 * 3 = 0.15
#'mean( as.numeric(res$nrej.bt) ) }
#'
#'\dontrun{
#' cor = make_corr_mat( nX = 10,
#'nY = 20,
#'rho.XX = 0.10,
#'rho.YY = 0.5,
#'rho.XY = 0.1,
#'prop.corr = .4 )
#'
#'d = sim_data( n = 300, cor = cor )
#'
#'# X1 is the covariate of interest, and all other X variables are adjusted
#'all.covars = names(d)[ grep( "X", names(d) ) ]
#'C = all.covars[ !all.covars == "X1" ]
#'
#'# may take 10 min to run
#'res = corr_tests( d,
#'                  X = "X1",
#'                  C = C,
#'                  Ys = names(d)[ grep( "Y", names(d) ) ],
#'                  method = "nreject" )
#'
#'# look at the main results
#'res$null.int
#'res$excess.hits
#'res$global.test
#' }

corr_tests = function( d,
                       X,
                       C = NA,
                       Ys,
                       B=2000,
                       cores,
                       alpha = 0.05,
                       alpha.fam = 0.05,
                       method = "nreject" ) {
  
  # check for and fix bad user input
  d = fix_input( X = X,
                 C = C,
                 Ys = Ys,
                 d = d )
  if ( length(X) > 1 ) stop("X must have length 1")
  
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
                             d = d,
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

  if ( "minP" %in% method ) {
    p.adj.minP = adj_minP( samp.res$pvals, resamps$p.bt )
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
    return( list( samp.res = samp.res,
                nrej.bt = resamps$rej.bt,
                null.int = c(bt.lo, bt.hi), # ours
                excess.hits = as.numeric( samp.res$rej - bt.hi ), # ours
                global.test = global.test # dataframe of all user-specified methods with their names
  ) )
}




########################### FN: FIT ONE OUTCOME MODEL (OLS) ###########################

#' Fit OLS model for a single outcome
#' 
#' The user does not need to call this function. Fits OLS model for a single outcome with or without centering the test statistics
#' to enforce the global null. 
#' @param d Dataframe 
#' @param X Single quoted name of covariate of interest
#' @param C Vector of quoted covariate names
#' @param Y Quoted name of single outcome for which model should be fit
#' @param Ys Vector of all quoted outcome names
#' @param alpha Alpha level for individual tests
#' @param center.stats Should test statistics be centered by original-sample estimates to enforce
#' global null?
#' @param bhat.orig Estimated coefficients for covariate of interest in original sample (W-vector).
#' Can be left NA for non-centered stats. 
#' @export
#' @examples
#' data(attitude)
#' fit_model( X = "complaints",
#'            C = c("privileges", "learning"),
#'            Y = "rating",
#'            Ys = c("rating", "raises"),
#'            d = attitude,
#'            center.stats = FALSE,
#'            bhat.orig = NA,  
#'            alpha = 0.05 )


fit_model = function( X,
                      C = NA,
                      Y,
                      Ys,
                      d,
                      center.stats = FALSE,
                      bhat.orig = NA,  # bhat.orig is a single value now for just the correct Y
                      alpha = 0.05 ) {
  
  if ( length(X) > 1 ) stop("X must have length 1")

  # all covariates, including the one of interest
  if ( all( is.na(C) ) ) covars = X
  else covars = c( X, C )
  
  if ( all( is.na(C) ) ) m = lm( d[[Y]] ~ d[[X]] )
  # https://stackoverflow.com/questions/6065826/how-to-do-a-regression-of-a-series-of-variables-without-typing-each-variable-nam
  else m = lm( d[[Y]] ~ ., data = d[ , covars] )
  
  # stats for covariate of interest
  if ( all( is.na(C) ) ) m.stats = summary(m)$coefficients[ 2, ]
  else m.stats = summary(m)$coefficients[ X, ]
    
  # should we center stats by the original-sample estimates?
  if( !center.stats ) {
    b = m.stats[["Estimate"]]
    tval = m.stats[["t value"]]
    SE = m.stats[["Std. Error"]]
  }
  if( center.stats ) {
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


########################### FN: GIVEN DATASET, RETURN STATS ###########################

#' Fit all models for a single dataset
#' 
#' The user does not need to call this function. For a single dataset, fits separate OLS models for W outcomes with or without centering the test statistics
#' to enforce the global null. 
#' @param d Dataframe 
#' @param X Single quoted name of covariate of interest
#' @param C Vector of quoted covariate names
#' @param Ys W-vector of quoted outcome names
#' @param alpha Alpha level for individual tests
#' @param center.stats Should test statistics be centered by original-sample estimates to enforce
#' global null?
#' @param bhat.orig Estimated coefficients for covariate of interest in original sample (W-vector).
#' Can be left NA for non-centered stats. 
#' @return Returns a list containing the number of observed rejections (\code{rej}), 
#' the coefficient estimates of interest for each outcome model (\code{bhats}), their t-values
#' (\code{tvals}), their uncorrected p-values at level \code{alpha} (\code{pvals}), and a matrix of
#' residuals from each model (\code{resid}). The latter is used for residual resampling under the 
#' global null. 
#' @export
#' @examples
#' samp.res = dataset_result( X = "complaints",
#'            C = c("privileges", "learning"),
#'            Ys = c("rating", "raises"),
#'            d = attitude,
#'            center.stats = FALSE,
#'            bhat.orig = NA,  # bhat.orig is a single value now for just the correct Y
#'            alpha = 0.05 )

dataset_result = function( d,
                           X,
                           C = NA,
                           Ys,  # all outcome names
                           alpha = 0.05,
                           center.stats = TRUE,
                           bhat.orig = NA ) { 
  
  if ( length(X) > 1 ) stop("X must have length 1")
  
  # for each outcome, fit regression model
  # see if each has p < alpha for covariate of interest
  if ( any( all( is.na(C) ) ) ) covars = X
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



########################### FN: GENERATE RESAMPLES ###########################

#' Resample residuals for OLS
#' 
#' Implements the residual resampling OLS algorithm described in Mathur & VanderWeele (in preparation). 
#' Specifically, the design matrix is fixed while the resampled outcomes are set equal to the original fitted values
#' plus a vector of residuals sampled with replacement. 
#' @param d Dataframe 
#' @param X Single quoted name of covariate of interest
#' @param C Vector of quoted covariate names
#' @param Ys Vector of quoted outcome names
#' @param alpha Alpha level for individual tests
#' @param resid Residuals from original sample (XXX matrix)
#' @param bhat.orig Estimated coefficients for covariate of interest in original sample (W-vector)
#' @param B Number of resamples to generate
#' @param cores Number of cores available for parallelization
#' @return Returns a list containing the number of rejections in each resample, a matrix of p-values
#' in the resamples, and a matrix of t-statistics in the resamples.
#' @import
#' doParallel
#' foreach
#' @references 
#' Mathur, M.B., & VanderWeele, T.J. (in preparation). New metrics for multiple testing with correlated
#' outcomes.
#' @export
#' @examples
#' samp.res = dataset_result( X = "complaints",
#'                 C = c("privileges", "learning"),
#'                 Ys = c("rating", "raises"),
#'                 d = attitude,
#'                 center.stats = FALSE,
#'                 bhat.orig = NA,  # bhat.orig is a single value now for just the correct Y
#'                 alpha = 0.05 )
#' 
#' resamps = resample_resid(  X = "complaints",
#'                   C = c("privileges", "learning"),
#'                   Ys = c("rating", "raises"),
#'                   d = attitude,
#'                   alpha = 0.05,
#'                   resid = samp.res$resid,
#'                   bhat.orig = samp.res$b,
#'                   B=20,
#'                   cores = 2)

resample_resid = function( d,
                          X,
                           C = NA,
                           Ys,
                           alpha,
                           resid,
                           bhat.orig,
                           B=2000,
                           cores = NULL ) {
  if ( length(X) > 1 ) stop("X must have length 1")
  
  if ( all( is.na(C) ) ) covars = X
  else covars = c( X, C )
  
  ##### Check for Bad Input
  # warn about too-small N or B
  if ( nrow(d) < 100 ) warning("Sample size is too small to ensure good asymptotic behavior of resampling.")
  if ( B < 1000 ) warning("Number of resamples is too small to ensure good asymptotic behavior of resampling.")
  
  # compute Y-hat using residuals
  Yhat = d[, Ys] - resid
  
  # fix the existing covariates
  Xs = as.data.frame( d[ , covars ] )
  if( all( is.na(C) ) ) names(Xs) = X
  
  # run all bootstrap iterates
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



######################## FNS FOR WESTFALL's SINGLE-STEP ########################

#' Adjust p-values using minP
#' 
#' Returns minP-adjusted p-values (single-step). See Westfall & Young (1993), pg. 48. 
#' @param p Original dataset p-values (W-vector) 
#' @param p.bt Bootstrapped p-values (a W X B matrix)
#' @references 
#' Westfall, P. H., & Young, S. S. (1993). Resampling-based multiple testing: Examples and
#' methods for p-value adjustment. Taylor & Francis Group.
#' @export
#' @examples
#' # observed p-values for 3 tests
#'  pvals = c(0.00233103655078803, 0.470366742594242, 0.00290278216035089
#')
#'
#' # bootstrapped p-values for 5 resamples
#'p.bt = t( structure(c(0.308528665936264, 0.517319402377912, 0.686518314693482,
#'                   0.637306248855186, 0.106805510862352, 0.116705315041494, 0.0732076817175753,
#'                   0.770308936364482, 0.384405349738909, 0.0434358213611965, 0.41497067850141,
#'                   0.513471489744384, 0.571213377144122, 0.628054979652722, 0.490196884985226
#'), .Dim = c(5L, 3L)) )
#'
#'# adjust the p-values
#'adj_minP( p = pvals, p.bt = p.bt )

adj_minP = function( p, p.bt ) {
  
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
#' Returns p-values adjusted based on Westfall & Young (1993)'s step-down algorithm (see pg. 66-67). 
#' @param p Original dataset p-values (W-vector) 
#' @param p.bt Bootstrapped p-values (an W X B matrix)
#' @references 
#' Westfall, P. H., & Young, S. S. (1993). Resampling-based multiple testing: Examples and
#' methods for p-value adjustment. Taylor & Francis Group.
#' @export
#' @examples 
#' # observed p-values for 3 tests
#'  pvals = c(0.00233103655078803, 0.470366742594242, 0.00290278216035089
#')
#'
#' # bootstrapped p-values for 5 resamples
#'p.bt = t( structure(c(0.308528665936264, 0.517319402377912, 0.686518314693482,
#'                   0.637306248855186, 0.106805510862352, 0.116705315041494, 0.0732076817175753,
#'                   0.770308936364482, 0.384405349738909, 0.0434358213611965, 0.41497067850141,
#'                   0.513471489744384, 0.571213377144122, 0.628054979652722, 0.490196884985226
#'), .Dim = c(5L, 3L)) )
#'
#'# adjust the p-values
#'adj_Wstep( p = pvals, p.bt = p.bt )

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
}




########################### FN: CALCULATE CRITICAL VALUES FOR WSTEP ###########################

#' Return ordered critical values for Wstep
#' 
#' The user does not need to call this function. This is an internal function for use by 
#' \code{adj_minP} and \code{adj_Wstep}.  
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
#' Simulates a dataset with a specified number of standard MVN covariates and outcomes
#' with a specified correlation structure. If correlation matrix isn't positive definite,
#' try reducing the correlation magnitudes.
#' @param nX Number of covariates, including the one of interest 
#' @param nY Number of outcomes
#' @param rho.XX Correlation between all pairs of Xs
#' @param rho.YY Correlation between all pairs of Ys
#' @param rho.XY Correlation between pairs of X-Y that are not null (see below)
#' @param prop.corr Proportion of X-Y pairs that are non-null (non-nulls will be first \code{prop.corr} * \code{nY} pairs)
#' @import
#' matrixcalc
#' @export
#' @examples
#' make_corr_mat( nX = 1,
#' nY = 4,
#' rho.XX = 0,
#' rho.YY = 0.25,
#' rho.XY = 0,
#' prop.corr = 0.8 )

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
#' The user does not need to call this function. This internal function is called by \code{make_corr_mat}
#' and populates a single cell. Assumes X1 is the covariate of interest and that none of the covariates is associated with any outcomes. 
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
#' Simulates one dataset with standard MVN correlated covariates and outcomes.
#' @param n Number of covariates, including the one of interest 
#' @param cor Correlation matrix (e.g., from make_corr_mat)
#' @import
#' mvtnorm
#' @export
#' @examples 
#' cor = make_corr_mat( nX = 5,
#'nY = 2,
#'rho.XX = -0.06,
#'rho.YY = 0.1,
#'rho.XY = -0.1,
#'prop.corr = 8/40 )
#'
#' d = sim_data( n = 50, cor = cor )

sim_data = function( n, cor ) {
  
  # variable names
  vnames = names(cor)
  
  # simulate the dataset
  # everything is a standard Normal
  d = as.data.frame( rmvnorm( n = n,
                              mean = rep( 0, dim(cor)[1] ),
                              sigma = as.matrix(cor) ) )
  names(d) = vnames
  
  # return the dataset
  return(d)
}

