library(testthat)
library(devtools)

###################### TEST FNS FOR APPLYING OUR METRICS ###################### 

test_that("fix_input #1", {
cor = make_corr_mat( nX = 5,
                     nY = 10,
                     rho.XX = -0.06,
                     rho.YY = 0.1,
                     rho.XY = -0.1,
                     prop.corr = 8/40 )

d = sim_data( n = 20, cor = cor )
all.covars = names(d)[ grep( "X", names(d) ) ]
C = all.covars[ !all.covars == "X1" ]

##### Add Bad Input ######
# insert missing data
d[1,4] = NA

# insert a decoy variable that should be removed in analysis
d$X20 = rnorm( n = nrow(d) )
d$X21 = rnorm( n = nrow(d) )

# make one of the covariates not mean-centered
d$X5 = d$X5 + 2

d = fix_input( X="X1",
           C=C,
           Ys=names(d)[ grep( "Y", names(d) ) ],
           d = d )

# check that it caught bad input
expect_equal( as.numeric( colMeans(d) ),
              rep(0, ncol(d) ) )

expect_equal( c( "X20", "X21" ) %in% names(d), 
              c(FALSE, FALSE) )

expect_equal( any( is.na(d) ), 
              FALSE )
} )





res = corr_tests( d,
                  X = "X1",
                  C = C,
                  Ys = names(d)[ grep( "Y", names(d) ) ],
                  B=5,
                  cores,
                  alpha = 0.05,
                  alpha.fam = 0.05,
                  method = "nreject" )



# fit_model doesn't need a test because we test it through the dataset_result tests

# without centering test stats
test_that("dataset_result #1", {

  cor = make_corr_mat( nX = 5,
                       nY = 2,
                       rho.XX = -0.06,
                       rho.YY = 0.1,
                       rho.XY = -0.1,
                       prop.corr = 8/40 )
  
  d = sim_data( n = 50, cor = cor )
  
  # try to confuse fn by choosing a different X as covariate of interest
  Ys = names(d)[ grep( "Y", names(d) ) ]
  X = "X2"
  all.covars = names(d)[ grep( "X", names(d) ) ]
  C = all.covars[ !all.covars == X ]
  

  # do analysis manually
  alpha = 0.05
  
  rej.man = 0
  tvals.man = c()
  bhats.man = c()
  pvals.man = c()
  resid.man = matrix( NA, nrow = 50, ncol = 2 )
  
  for ( i in 1:length(Ys) ) {
    m = lm( d[[ Ys[i] ]] ~ X1 + X2 + X3 + X4 + X5, data = d )
    bhats.man[i] = coef(m)[[X]]
    tvals.man[i] = summary(m)$coefficients[X,"t value"]
    pvals.man[i] = summary(m)$coefficients[X, "Pr(>|t|)"]
    resid.man[,i] = residuals(m)
    
    # did we reject it?
    if ( summary(m)$coefficients[X, "Pr(>|t|)"] < alpha ) rej.man = rej.man + 1
  
  }  
  
  # with function
  samp.res = dataset_result( d = d,
                             X = X,
                             C = C,
                             Ys = Ys,  # all outcome names
                             alpha = alpha,
                             center.stats = FALSE,
                             bhat.orig = NA )
  
  resid.man = as.data.frame(resid.man)
  names(resid.man) = Ys
  
  expect_equal( rej.man, samp.res$rej )
  expect_equal( bhats.man, samp.res$bhat )
  expect_equal( tvals.man, samp.res$tvals )
  expect_equal( pvals.man, samp.res$pvals )
  
  expect_equal( as.matrix(resid.man), as.matrix(samp.res$resid) )
} )



# with centered test stats
test_that("dataset_result #2", {
  
  cor = make_corr_mat( nX = 5,
                       nY = 20,
                       rho.XX = 0.16,
                       rho.YY = 0.1,
                       rho.XY = 0.1,
                       prop.corr = 1 )
  
  d = sim_data( n = 50, cor = cor )
  
  # try to confuse fn by choosing a different X as covariate of interest
  Ys = names(d)[ grep( "Y", names(d) ) ]
  X = "X2"
  all.covars = names(d)[ grep( "X", names(d) ) ]
  C = all.covars[ !all.covars == X ]
  
  
  # do analysis manually
  # choose an unusual alpha level to make sure it's working
  alpha = 0.4
  
  rej.man = 0
  tvals.man = c()
  bhats.man = c()
  pvals.man = c()
  resid.man = matrix( NA, nrow = 50, ncol = length(Ys) )
  
  # fake original coefficients
  bhat.orig = rnorm( n=length(Ys), mean = 0.8, sd = 2 )

  for ( i in 1:length(Ys) ) {
    m = lm( d[[ Ys[i] ]] ~ X1 + X2 + X3 + X4 + X5, data = d )
    bhats.man[i] = coef(m)[[X]] - bhat.orig[i]
    
    df = 50 - 5 - 1
    se = summary(m)$coefficients[X, "Std. Error"]
    tvals.man[i] = bhats.man[i] / se
    
    pvals.man[i] = 2 * ( 1 - pt( abs( tvals.man[i] ), df = df ) )
    
    resid.man[,i] = residuals(m)
    
    # did we reject it?
    if ( pvals.man[i] < alpha ) rej.man = rej.man + 1
  }  
  
  # with function
  samp.res = dataset_result( d = d,
                             X = X,
                             C = C,
                             Ys = Ys,  # all outcome names
                             alpha = alpha,
                             center.stats = TRUE,
                             bhat.orig = bhat.orig )
  
  resid.man = as.data.frame(resid.man)
  names(resid.man) = Ys
  
  expect_equal( rej.man, samp.res$rej )
  expect_equal( bhats.man, samp.res$bhat )
  expect_equal( tvals.man, samp.res$tvals )
  expect_equal( pvals.man, samp.res$pvals )
  expect_equal( as.matrix(resid.man), as.matrix(samp.res$resid) )
} )




###################### TEST FNS FOR SIMULATING DATA ###################### 

test_that("cell_corr #1", {

  expect_equal( -0.1,
    cell_corr( vname.1 = "X1",
               vname.2 = "Y3",
               rho.XX = 0,
               rho.YY = 0.25,
               rho.XY = -0.1,
               nY = 6,
               prop.corr = 1 ) )
  
  expect_equal( 0.25,
    cell_corr( vname.1 = "Y1",
               vname.2 = "Y3",
               rho.XX = 0,
               rho.YY = 0.25,
               rho.XY = -0.1,
               nY = 6,
               prop.corr = 1 ) )

  expect_equal( 0,
    cell_corr( vname.1 = "X2",
               vname.2 = "Y3",
               rho.XX = 0,
               rho.YY = 0.25,
               rho.XY = -0.1,
               nY = 6,
               prop.corr = 1 ) )
  
  expect_equal( -0.1,
    cell_corr( vname.1 = "X1",
               vname.2 = "Y2",
               rho.XX = 0,
               rho.YY = 0.25,
               rho.XY = -0.1,
               nY = 10,
              prop.corr = .2 ) )
    
  expect_equal( 0,
    cell_corr( vname.1 = "X1",
               vname.2 = "Y3",
               rho.XX = 0,
               rho.YY = 0.25,
               rho.XY = -0.1,
               nY = 10,
               prop.corr = .2 ) )
} )


test_that("make_corr_mat #1", {
  # sanity checks
  cor = make_corr_mat( nX = 1,
                  nY = 40,
                  rho.XX = 0,
                  rho.YY = 0.25,
                  rho.XY = 0.1,
                  prop.corr = 8/40 )

  # do we have the right number of each type of correlation?
  # only look at first row (correlations of X1 with everything else)
  expect_equal( c( 1, rep(0.10, 8), rep(0, 40-8) ),
                   as.numeric( cor[1,] ) )
} )


test_that("make_corr_mat #1", {
  cor = make_corr_mat( nX = 2,
                       nY = 40,
                       rho.XX = 0.35,
                       rho.YY = 0.25,
                       rho.XY = 0.1,
                       prop.corr = 8/40 )
  
  d = sim_data( n = 10000, cor = cor )
  
  # rho.XX correlations
  expect_equal( cor(d$X1, d$X2), 0.35, tolerance = 0.05 )
  
  # rho.XY correlations for non-null ones
  names = paste( "Y", seq(1,8,1), sep="" )
  expect_equal( as.numeric( cor( d$X1, d[, names] ) ),
                rep( 0.1, 8 ),
                tolerance = 0.05 )
  
  # rho.XY correlations for null ones
  names = paste( "Y", seq(9,40,1), sep="" )
  expect_equal( as.numeric( cor( d$X1, d[, names] ) ),
                rep( 0, 40-8 ),
                tolerance = 0.05 )

  # plot empirical vs. real correlations
  #plot( as.numeric(cor(d)), as.numeric(as.matrix(cor)) ); abline( a = 0, b = 1, col="red")
} )


###################### TEST WESTFALL FNS ###################### 

test_that("adjust_minp #1", {
  # sanity check
  B = 200
  n.tests = 10

  # generate fake p-values under strong null
  p.bt = matrix( runif(B*n.tests, 0, 1), nrow = n.tests)

  # generate fake p-values from real dataset
  p = runif( n.tests, 0, .1)

  p.adj = adjust_minP( p, p.bt )
  #plot(p, p.adj)

  # manually adjust second p-value
  mins = apply( p.bt, MARGIN = 2, FUN = min )

  expect_equal( prop.table( table( mins <= p[2] ) )[["TRUE"]],
                p.adj[2] )
})



test_that("adjust_Wstep #1", {
  # # Sanity Check
  # nX = 1
  # nY = 3
  # B = 5
  # 
  # library(matrixcalc)
  # library(mvtnorm)
  # 
  # cor = make_corr_mat( nX = nX,
  #                      nY = nY,
  #                      rho.XX = 0,
  #                      rho.YY = 0.25,
  #                      rho.XY = 0.05,
  #                      prop.corr = 1 )
  # 
  # d = sim_data( n = 1000, cor = cor )
  # 
  # samp.res = dataset_result( X = "X1",
  #                 C = NA,
  #                 Ys = c("Y1", "Y2", "Y3"), 
  #                 d = d,
  #                 alpha = 0.05,
  #                 center.stats = FALSE )
  #               
  # 
  # # do 5 bootstraps
  # resamps = resample_resid( X = "X1",
  #                           C = NA,
  #                           Ys = c("Y1", "Y2", "Y3"),
  #                           d = d,
  #                           alpha = 0.05,
  #                           resid = samp.res$resid,
  #                           bhat.orig = samp.res$bhats,
  #                           B=5,
  #                           cores = 8 )
  # p.bt = t( resamps$p.bt )
  # pvals = samp.res$pvals
  
  pvals = c(0.00233103655078803, 0.470366742594242, 0.00290278216035089
  )

  p.bt = structure(c(0.308528665936264, 0.517319402377912, 0.686518314693482,
                     0.637306248855186, 0.106805510862352, 0.116705315041494, 0.0732076817175753,
                     0.770308936364482, 0.384405349738909, 0.0434358213611965, 0.41497067850141,
                     0.513471489744384, 0.571213377144122, 0.628054979652722, 0.490196884985226
  ), .Dim = c(5L, 3L))
  

  # indicators of which hypothesis the sorted p-vals go with
  sort(pvals)
  r = c(1,3,2)
  
  qstar = matrix( NA, nrow = B, ncol = 3)
  
  for (i in 1:nrow(p.bt)) {
    qstar[i,3] = p.bt[ i, r[3] ]
    qstar[i,2] = min( qstar[i,3], p.bt[ i, r[2] ] )
    qstar[i,1] = min( qstar[i,2], p.bt[ i, r[1] ] )
  }
  
  less = t( apply( qstar, MARGIN = 1,
                function(row) row <= sort(pvals) ) )
  
  p.tilde = colMeans(less)
  
  # enforce monotonicity
  p.tilde.sort = sort(p.tilde)
  p.tilde.sort[2] = max( p.tilde.sort[1], p.tilde.sort[2] )
  p.tilde.sort[3] = max( p.tilde.sort[2], p.tilde.sort[3] )
  
  # put back in original order
  p.adj = p.tilde.sort[r]
  
  expect_equal( p.adj, adj_Wstep( p = pvals, p.bt = resamps$p.bt) )
})




###################### TEST RESAMPLE_RESID ###################### 

# check that mean p-value is .5 in resamples
# and that we have the expected number of rejections

test_that("resample_resid #1", {
  # Sanity Check
  nX = 1
  nY = 3
  B = 5
  
  library(matrixcalc)
  library(mvtnorm)
  
  cor = make_corr_mat( nX = nX,
                       nY = nY,
                       rho.XX = 0,
                       rho.YY = 0.25,
                       rho.XY = 0.05,
                       prop.corr = 1 )
  
  d = sim_data( n = 1000, cor = cor )
  
  samp.res = dataset_result( X = "X1",
                             C = NA,
                             Ys = c("Y1", "Y2", "Y3"), 
                             d = d,
                             alpha = 0.05,
                             center.stats = FALSE )
  
  # do 5 bootstraps
  resamps = resample_resid( X = "X1",
                            C = NA,
                            Ys = c("Y1", "Y2", "Y3"),
                            d = d,
                            alpha = 0.05,
                            resid = samp.res$resid,
                            bhat.orig = samp.res$bhats,
                            B=500,
                            cores = 8 )
  
  expect_equal( mean(resamps$p.bt), .5, tolerance = 0.03 )
  expect_equal( mean(resamps$rej.bt), .05*nY, tolerance = 0.03 )
  
} )







