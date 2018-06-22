
# Goal: 

# load Westfall fns
setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/git_multiple_outcomes/R code/For Sherlock")
source("functions.R")


######################## GENERATE DATA ########################

# parameters chosen to have a lot of small p-values so that we can
#  see how they're affected

# try with my own example
# source functions
source("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/git_multiple_outcomes/R code/For Sherlock/functions.R")

# generate data
set.seed(452)
n = 1000
nY = 100
nX = 1
rho.XX = 0
rho.YY = .25
rho.XY = 0.10
alpha = 0.05

# make initial dataset from which to bootstrap
cor = make_corr_mat( .nX = nX, .nY = nY, .rho.XX = rho.XX,
                     .rho.YY = rho.YY, .rho.XY = rho.XY)
d = sim_data( .n = n, .cor = cor )
X.names = "X1"
Y.names = names(d)[2:length(names(d))]

samp.res = dataset_result( .dat = d, .alpha = 0.05, .resid = TRUE, .sigma = TRUE, .tval = TRUE )
tvals = as.matrix( samp.res$tvals )
pvals = as.matrix( samp.res$pvals )

n.rej = samp.res$rej
names(n.rej) = paste( "n.rej.", as.character(alpha), sep="" )


######################## RUN BOOTSTRAPPING ########################

source("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/git_multiple_outcomes/R code/For Sherlock/functions.R")

boot.reps = 500

library(doParallel)
library(foreach)
registerDoParallel(16)

# run all bootstrap iterates
# CHANGED TO CBIND
sec = system.time( {
  r = foreach( i = 1:boot.reps, .combine=cbind ) %dopar% {
    # draw bootstrap sample 
    ids = sample( 1:nrow(d), replace=TRUE )
    
    ##### Bootstrap Under Null #2 (Resample Residuals) #####
    # see Westfall, pg. 133
    if ( TRUE ) {
      # fix the existing covariates
      Xs = as.data.frame( d[ , 1 : length(X.names) ] )
      names(Xs) = X.names
      
      # resample residuals
      resid = samp.res$resid
      names(resid) = Y.names
      b = as.data.frame( cbind( Xs, resid[ids,] ) )
    }
    
    # important for dataset_result to be able to find the right names
    names(b) = c(X.names, Y.names)
    
    # for Westfall
    # save info to dataframes
    bt.res = dataset_result( .dat = b, .alpha = alpha, .resid = FALSE, .tval=TRUE )
    
    # return all the things
    list( rej = bt.res$rej,
          pvals = bt.res$pvals )
    
    # get number rejected for bootstrap sample
    # we don't need to return residuals for this one
    #bt.res$rej
    # so the returned r is a dataframe of number rejected for each bootstrap iterate 
    # one column for each value of alpha
    # r[["0.01"]] is the vector with length boot.reps of number rejected at alpha = 0.01
    
  } ###### end r-loop (parallelized bootstrap)
})[["elapsed"]] # end timer

# r is now the number of rejections in each iterate




######################## RESULTS ########################

# list of rejections
n.rej.bt = as.numeric( unlist( r[ "rej",] ) )

# list of pvals in bootstraps
p.bt = do.call( cbind, r[ "pvals",] )

# save results
setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/git_multiple_outcomes/R code/Auxiliary code/Compare joint tests/Compare Westfall minP vs step-down")
write.csv(n.rej.bt, "nrejbt.csv")
write.csv(p.bt, "pbt.csv")

# read back in
setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/git_multiple_outcomes/R code/Auxiliary code/Compare joint tests/Compare Westfall minP vs step-down")
n.rej.bt = read.csv("nrejbt.csv")$x
p.bt = read.csv("pbt.csv", header=TRUE)[,-1]



######## Westfall's single-step ######## 

# do adjusted p-vals make sense?
p.adj.minP = adjust_minP( pvals, p.bt )
cbind( pvals, p.adj.minP )
plot( pvals, p.adj.minP )
( rej.jt.minP = any( p.adj.minP < 0.05 ) )

######## Westfall's step-down ######## 

( p.adj.stepdown = adj_Wstep( pvals, p.bt ) )
plot( pvals, p.adj.stepdown )


######## Compare the Westfalls ######## 

# number rejected
# step-down rejects an additional 5 compared to minP
table( p.adj.minP < 0.05 )
table( p.adj.stepdown < 0.05 )

# THIS IS IMPORTANT - SHOWS WHY MINP AND STEP-DOWN JOINT TESTS ARE ALMOST ALWAYS THE SAME
# compare single-step and step-down p-values
# makes sense: the minP ones are always larger

library(ggplot2)

d = data.frame( minP = p.adj.minP, step = p.adj.stepdown )

ggplot( data = d, aes( x = minP, y = step ) ) +
  geom_point( ) +
  geom_vline( xintercept = 0.05, lty = 2, color = "red" ) +
  geom_hline( yintercept = 0.05, lty = 2, color = "red" ) +
  geom_abline( slope = 1, intercept = 0 ) +
  scale_y_continuous( limits = c(0,1), breaks = seq(0, 1, .1) ) +
  scale_x_continuous( limits = c(0,1), breaks = seq(0, 1, .1) ) +
  xlab("Westfall minP adjusted p-values") +
  ylab("Westfall step-down adjusted p-values") +
  theme_bw()
  

