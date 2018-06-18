library(testthat)
library(devtools)



test_that("cell_corr #1", {
  # sanity checks
  # should be -0.1
  cell_corr( vname.1 = "X1",
             vname.2 = "Y3",
             rho.XX = 0,
             rho.YY = 0.25,
             rho.XY = -0.1,
             nY = 6,
             prop.corr = 1 )
  
  # should be 0.25
  cell_corr( vname.1 = "Y1",
             vname.2 = "Y3",
             rho.XX = 0,
             rho.YY = 0.25,
             rho.XY = -0.1,
             nY = 6,
             prop.corr = 1 )
  
  # should be 0
  cell_corr( vname.1 = "X2",
             vname.2 = "Y3",
             rho.XX = 0,
             rho.YY = 0.25,
             rho.XY = -0.1,
             nY = 6,
             prop.corr = 1 )
  
  # should be -0.1
  cell_corr( vname.1 = "X1",
             vname.2 = "Y2",
             rho.XX = 0,
             rho.YY = 0.25,
             rho.XY = -0.1,
             nY = 10,
            prop.corr = .2 )
  
  # should be 0
  cell_corr( vname.1 = "X1",
             vname.2 = "Y3",
             rho.XX = 0,
             rho.YY = 0.25,
             rho.XY = -0.1,
             nY = 10,
             prop.corr = .2 )
} )


###################### EVALUE: ANNALS PAPER EXAMPLES ###################### 

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
  # Sanity Check
  nX = 1
  nY = 3
  B = 5
  
  library(matrixcalc)
  library(mvtnorm)
  
  cor = make_corr_mat( .nX = nX,
                       .nY = nY,
                       .rho.XX = 0,
                       .rho.YY = 0.25,
                       .rho.XY = 0.05,
                       .prop.corr = 1 )
  
  d = sim_data( .n = 1000, .cor = cor )
  
  samp.res = dataset_result( .dat = d,
                  .alpha = 0.05,
                  .center.stats = FALSE )
  
  
  p.bt = matrix( NA, nrow = B, ncol = nY)
  
  # do 5 bootstraps
  for (i in 1:B) {
    # extract residuals from original data
    # matrix with same dimensions as the outcomes matrix
    resid = samp.res$resid
  
    # compute Y-hat using residuals
    Ys = d[ , (nX + 1) : ncol(d) ]  # remove covariates
    Yhat = Ys - resid
  
    # fix the existing covariates
    Xs = as.data.frame( d[ , 1 : nX ] )
    names(Xs) = X.names
  
    # resample residuals and add them to fitted values
    ids = sample( 1:nrow(d) )
    b = as.data.frame( cbind( Xs, Yhat + resid[ids,] ) )
  
    bt.res = dataset_result( .dat = b,
                             .alpha = 0.05,
                             .center.stats = TRUE,
                             .bhat.orig = samp.res$bhats )
  
    p.bt[i,] = bt.res$pvals
  }
  
  pvals = c(0.00233103655078803, 0.470366742594242, 0.00290278216035089
  )
  
  p.bt = structure(c(0.308528665936264, 0.517319402377912, 0.686518314693482,
                     0.637306248855186, 0.106805510862352, 0.116705315041494, 0.0732076817175753,
                     0.770308936364482, 0.384405349738909, 0.0434358213611965, 0.41497067850141,
                     0.513471489744384, 0.571213377144122, 0.628054979652722, 0.490196884985226
  ), .Dim = c(5L, 3L))
  
  
  
  p.adj.Wstep = adj_Wstep(pvals, t(p.bt))
  plot( samp.res$pvals, p.adj.Wstep )
  
  # BOOKMARK - CHECK MANUALLY AS IN PREVIOUS TOY EXAMPLE
  
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
  
})