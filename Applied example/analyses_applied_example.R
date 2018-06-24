

# Note: To avoid rerunning analyses (e.g., bootstraps) and only have access to resulting objects, run this.
# This also allows exact reproduction of bootstrap analyses. 

root.path = "~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/git_multiple_outcomes/Applied example"
setwd(root.path)
setwd("Analysis objects for manuscript")
load("analysis_objects.rds")


############################## READ IN DATA ############################## 

root.path = "~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/git_multiple_outcomes/Applied example"
setwd(root.path)

# read in data
setwd("Prepped data")
d = read.csv("flourish_prepped.csv", header=TRUE); nrow(d)
# should be 2697

# source helper functions
setwd(root.path)
source("helper_applied_example.R")

# names of adjusted covariates
# see SAS file 9
covars = c( "A1SEPA_z",
            "A1PAGE_M2",
            "A1PRSEX",
            "raceA",
            "A1SE2",
            "A1SE3",
            "A1SE4",
            "A1PC1",
            "sibling",
            "CEDUC4cat",
            "A1PC14",
            "A1SE9",
            "A1SE7",
            "A1SE8c",
            "mom_smk",
            "dad_smk",
            "B1PA58",
            "A1SE6" )

# names of outcomes
outcomes = c("flourish_z",
             "emotion_z",
             "social_z",
             "psych_z",
             "B1SPOSAFz",
             "B1SQ1z",
             "B1SSWBMSz",
             "B1SSWBSIz",
             "B1SSWBAOz",
             "B1SSWBSCz",
             "B1SSWBSAz",
             "B1SPWBA1z",
             "B1SPWBE1z",
             "B1SPWBG1z",
             "B1SPWBR1z",
             "B1SPWBU1z",
             "B1SPWBS1z")


############################## APPENDIX TABLE E.3: DEMOGRAPHICS ##############################

# merge table-friendly variable names into dataset
setwd(root.path); setwd("MIDUS codebooks")
library(xlsx)
cb = read.xlsx(file="Analysis dataset codebook.xlsx",
               sheetName="codebook",
               header=TRUE)

# for now, only keep variables needed for table
temp = d[ , covars ]
names(temp) = cb$Long.name[ cb$Variable. %in% names(temp) ]

# which variables should be treated as factors in the table?
factor.vars = c( "Race", "Born in US", "Mother born in US", "Father born in US",
                "Lived with biological parents", "Childhood welfare",
                "Residential stability", "Mother smoked", "Father smoked",
                "Lived with alcoholics")

# make the table
library(tableone)
t = CreateTableOne( data=temp, factorVars = factor.vars )

library(xtable)
table_matrix = print(t, noSpaces = TRUE, printToggle = FALSE )
xtable(table_matrix)

setwd(root.path)
setwd("Plots for manuscript")
write.csv(table_matrix, "table_one.csv")


############################## TABLE 2: ANALYZE ORIGINAL DATA WITH OLS MODELS ##############################


# initialize stats data frame
stats = data.frame( matrix( NA,
                            nrow = length(outcomes),
                            ncol = 10  # one for each returned stat above
) )

# initialize place to store residuals
resids = data.frame( matrix( NA,
                             nrow = nrow(d),
                             ncol = length(outcomes)  # one for each outcome
) )

for( i in 1:length(outcomes) ) {
  raw.res = fit_model( Y.name = outcomes[i],
                       X.name = "A1SEPA_z",
                       .d = d,
                       .covars = covars,
                       .center.stats = FALSE )
  stats[i,] = raw.res$stats
  resids[,i] = raw.res$resids
}

# housekeeping
names(stats) = names(raw.res$stats)
stats$outcome = outcomes  # gets turned into "1" for some reason
 

# # sanity check: fit one model manually
# m = lm( flourish_z ~ A1SEPA_z + A1PAGE_M2 + A1PRSEX + raceA + A1SE2 + A1SE3 + A1SE4 + A1PC1 + sibling + CEDUC4cat + A1PC14 + A1SE9 + A1SE7 + A1SE8c + mom_smk + dad_smk + B1PA58 + A1SE6,
#     data = d)
# summary(m)
# confint(m)
# # compare: 
# stats[1,]


# observed rejections
( th.0.05 = sum(stats$reject.0.05) )
( th.0.01 = sum(stats$reject.0.01) )

# sanity check: use stats to reproduce lm's p-values
# cbind( format.pval( 2 * ( 1 - pt( abs( stats$b / stats$SE ), stats$df ) ), 2 ), format.pval( stats$pval, 2 ) )


##### Make Table 2 #####
# note that since Y is standard normal, it's already interpretable as a Cohen's d
digits = 2
stats.pretty = data.frame( outcome = stats$outcome,
                           std.b = paste( round( stats$b, digits ), " [", round( stats$lb, 2 ), ", ", round( stats$ub, digits ), "]", sep="" ),
                           pval = format.pval( stats$pval, 1 ) )

# rename variables for ease of reading
stats.pretty$outcome = cb$Long.name[ cb$Variable. %in% stats.pretty$outcome ]

print( xtable(stats.pretty), include.rownames = FALSE )


# # sanity check - check the ones with exactly duplicated results
# # positive affect
# m1 = lm( B1SPOSAFz ~ A1SEPA_z + A1PAGE_M2 + A1PRSEX + raceA + A1SE2 + A1SE3 + A1SE4 + A1PC1 + sibling + CEDUC4cat + A1PC14 + A1SE9 + A1SE7 + A1SE8c + mom_smk + dad_smk + B1PA58 + A1SE6,
#     data = d)
# summary(m1)
# confint(m1)
# 
# # life satisfaction
# m2 = lm( B1SQ1z ~ A1SEPA_z + A1PAGE_M2 + A1PRSEX + raceA + A1SE2 + A1SE3 + A1SE4 + A1PC1 + sibling + CEDUC4cat + A1PC14 + A1SE9 + A1SE7 + A1SE8c + mom_smk + dad_smk + B1PA58 + A1SE6,
#         data = d)
# summary(m2)
# confint(m2)
  

# ############################## BOOTSTRAPPING ##############################
# 
# # commented out because takes a long time
# 
# B = 5000
# 
# # compute Y-hat using residuals
# Ys = d[ , outcomes ]  # remove covariates
# Yhat = Ys - resids
# 
# # fix the existing covariates
# Xs = as.data.frame( d[ , covars ] )
# 
# # run all bootstrap iterates
# library(doParallel)
# registerDoParallel(cores=8)
# 
# # run all resamples: takes ~10 min
# r = foreach( i = 1:B, .combine=rbind ) %dopar% {
#   
#   # resample residuals and add them to fitted values
#   ids = sample( 1:nrow(d), replace=TRUE )
#   b = as.data.frame( cbind( Xs, Yhat + resids[ids,] ) )
#   
#   bhats = rep( NA, length(outcomes) )
#   
#   bt.res = dataset_result( .d = b,
#                            .alpha = c(0.05, 0.01 ),
#                            .center.stats = TRUE,
#                            .bhat.orig = stats$b )
#   
#   # return all the things
#   list( rej = bt.res$rej,
#         pvals = bt.res$pvals,
#         tvals = bt.res$tvals )
#   
# } ###### end r-loop (parallelized bootstrap)
# 
# 
# # resampled p-value matrix for Westfall
# # rows = Ys
# # cols = resamples
# p.bt = do.call( cbind, r[ , "pvals" ] )
# 
# # resampled test statistic matrix (uncentered) for Romano
# # rows = Ys
# # cols = resamples
# t.bt = do.call( cbind, r[ , "tvals" ] )
# 
# # number of rejections
# rej.bt = do.call( cbind, r[ , "rej" ] )
# rej.bt.0.05 = rej.bt[1,]
# rej.bt.0.01 = rej.bt[2,]
# 
# # sanity check
# 0.05*length(outcomes); mean(rej.bt.0.05) 
# 0.01*length(outcomes); mean(rej.bt.0.01) 


############################## OUR METRICS ##############################

( null.int.0.05 = quantile( rej.bt.0.05, probs = c(0.025, .975) ) )
( null.int.0.01 = quantile( rej.bt.0.01, probs = c(0.025, .975) ) )

( excess.hits.0.05 = th.0.05 - null.int.0.05[2] )
( excess.hits.0.01 = th.0.01 - null.int.0.01[2] )

( joint.pval = sum( rej.bt.0.05 >= th.0.05 ) / length(rej.bt.0.05) )


############################## FIGURE 1: NUMBER OF REJECTIONS HISTOGRAM ##############################

library(ggplot2)

# dataframe for plotting joy
pd = data.frame( rej.bt = c( as.numeric(rej.bt.0.05), as.numeric(rej.bt.0.01) ),
                 alpha = c( rep( 0.05, B ), rep( 0.01, B) ) )


colors = c("orange", "black")

x.max = max( pd$rej.bt )
expected.0.05 = 0.05 * length(outcomes)
expected.0.01 = 0.01 * length(outcomes)

ggplot( data = pd ) +
  theme_classic() +
  xlab( expression( hat(theta)^(j) ) ) +
  ylab("Frequency") +
  geom_vline( xintercept = expected.0.01, color=colors[1]) +
  geom_vline( xintercept = expected.0.05, color=colors[2]) +
  geom_vline( xintercept = null.int.0.01[2], lty = 2, color=colors[1]) +
  geom_vline( xintercept = null.int.0.05[2], lty = 2, color=colors[2]) +

  geom_histogram( aes( x = rej.bt,
                       fill = as.factor(alpha) ), bins=16, binwidth=0.5, position="dodge" ) +
  scale_fill_manual( name = expression(alpha), values = colors ) +

  scale_x_continuous( limits=c(-0.5, x.max), breaks=seq(0, x.max, 1) ) +
  scale_y_continuous( limits=c(0,5000), breaks=seq(0, 5000, 500) ) +
  
  annotate(geom = "text", x = expected.0.05 + .25, y = 4000, label = "Expected", color = colors[2],
           angle = 90) +
  annotate(geom = "text", x = expected.0.01 + .25, y = 4000, label = "Expected", color = colors[1],
           angle = 90) +
  
  annotate(geom = "text", x = null.int.0.05[2] + .25, y = 4000, label = "Upper interval limit", color = colors[2],
           angle = 90) +
  annotate(geom = "text", x = null.int.0.01[2] + .25, y = 4000, label = "Upper interval limit", color = colors[1],
           angle = 90)

setwd(root.path)
setwd("Plots for manuscript")
ggsave("resamples_hist.png", width = 6, height = 5)


############################## ROMANO ##############################

library(StepwiseTest)

romano = FWERkControl(stats$tval, as.matrix(t.bt), k = 1, alpha = 0.05)
jt.rej.Romano = sum(romano$Reject) > 0
# rejects all the outcomes


# Bonferroni
alpha.bonf = 0.05/length(outcomes)
table( stats$pval <= alpha.bonf )
# rejects all but 2 outcomes


############################## OTHER INFO FOR PAPER ##############################

# in order of reporting in manuscript

# sample size
( N = nrow(d) )

# average correlation magnitude
d.outs = d[, outcomes]
cor.mat = cor(d.outs)

# keep only lower-triangular correlations (not diagonal)
cor.vec = cor.mat[ lower.tri(cor.mat) ]
( med.abs.cor = median( abs( cor.vec ) ) )
( min.abs.cor = min( abs( cor.vec ) ) )
( max.abs.cor = max( abs( cor.vec ) ) )
( q1.abs.cor = quantile( abs( cor.vec ), 0.25 ) )
( q3.abs.cor = quantile( abs( cor.vec ), 0.75 ) )

# composite analysis 
stats.pretty[ stats.pretty$outcome == "Overall flourishing (continuous)", ]

# mean effect size
mean(stats$b)

# null intervals
null.int.0.05
null.int.0.01

# exact binomial distribution inference (alpha = 0.05)
qbinom( .025, size=17, prob=0.05)
qbinom( .975, size=17, prob=0.05)
# prob of rejecting all 18
( pval.binom = .05^17 )

qbinom( .025, size=1, prob=0.01)
qbinom( .975, size=17, prob=0.01)
# prob of rejecting all 18
( pval.binom = .05^17 )



############################## SANITY CHECK: USE OUR R PACKAGE INSTEAD ##############################

setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/git_multiple_outcomes/R package/NRejections/R")
source("NRejections.R")

library(testthat)
library(matrixcalc)
library(doParallel)
library(foreach)


X = "A1SEPA_z"
C = covars[ !covars == "A1SEPA_z" ]

# # commented out because it takes a long time
# res.0.05 = corr_tests( d = d, 
#             X = X,
#             C = C,
#             Ys = outcomes,
#             B = 5000,
#             cores = 8,
#             alpha = 0.05, 
#             alpha.fam = 0.05,
#             method = c( "nreject", "bonferroni", "holm", "minP", "Wstep", "romano" )
#             )
# 
# res.0.01 = corr_tests( d = d, 
#                        X = X,
#                        C = C,
#                        Ys = outcomes,
#                        B = 5000,
#                        cores = 8,
#                        alpha = 0.01, 
#                        alpha.fam = 0.05,
#                        method = c( "nreject", "bonferroni", "holm", "minP", "Wstep", "romano" )
# )

# check package results against manual results above
# bootstrap results allowed to be off by 1 rejection at max because of randomness
library(testthat)
expect_equal( res.0.05$samp.res$pvals, stats$pval, tolerance = 1 )
expect_equal( res.0.05$samp.res$bhats, stats$b )
expect_equal( res.0.05$samp.res$rej, sum(stats$reject.0.05) )

expect_equal( res.0.05$null.int, null.int.0.05, tolerance = 1 )
expect_equal( res.0.05$global.test$pval[ res.0.05$global.test$method == "nreject" ], joint.pval )
expect_equal( res.0.05$excess.hits, as.numeric(excess.hits.0.05), tolerance = 1 )

expect_equal( res.0.01$samp.res$rej, sum(stats$reject.0.01) )
expect_equal( res.0.01$null.int, null.int.0.01 )
expect_equal( res.0.01$excess.hits, as.numeric(excess.hits.0.01) )


############################## SAVE ANALYSIS OBJECTS ##############################

# setwd(root.path)
# setwd("Analysis objects for manuscript")
# save.image(file = "analysis_objects.rds")
