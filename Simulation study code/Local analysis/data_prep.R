
########################### PRELIMINARIES ###########################

library(here)
library(data.table)
setwd(here())
source("helper_analysis.R")

# previous sims with nY=40
data.dir.old = "~/Dropbox/Personal computer/Harvard/THESIS/Thesis paper #2 (MO)/Linked to OSF (MO)/Simulation results in paper/2023-03-13 Merge previous sims and higher-W sims/Data from Sherlock/nY=40 from 2018-8-18"

# new sims with nY=200
data.dir.new = "~/Dropbox/Personal computer/Harvard/THESIS/Thesis paper #2 (MO)/Linked to OSF (MO)/Simulation results in paper/2023-03-13 Merge previous sims and higher-W sims/Data from Sherlock/Results from R"

results.dir = "~/Dropbox/Personal computer/Harvard/THESIS/Thesis paper #2 (MO)/Linked to OSF (MO)/Simulation results in paper/2023-03-13 Merge previous sims and higher-W sims/Results from R"




#### BELOW IS FOR NEW SIMS ONLY

########################### READ IN DATA ###########################

setwd(data.dir.new)

# read in scenario parameters
scen.params = read.csv("scen_params.csv")
names(scen.params)[ names(scen.params) == "scen.name" ] = "scen"
#
# read in stitched data
s = fread("stitched.csv")
# s = merge(s, scen.params, by = "scen" )
#
# # what percent done are we?
# n.reps.per.scen = 500
# nrow(s) / (500 * nrow(scen.params))

# how many reps per scenario do we have?
table( s$scen )

# because of weird row with scen=FALSE
library(gdata)
s = drop.levels(s)


########################### DATA PREP: N-TRUE-EFFECTS PLOT ###########################

# need 1 row per scenario-method combination
# methods are ours, Wstep, bonf.naive, and minP

library(reshape2)
library(tidyverse)
library(dplyr)
library(magrittr)
library(tidyr)

# names of joint rejection variables for all methods
( n.true.names = c( names(s)[ grep( "n.true.", names(s) ) ] ) )
n.true.names = c("n.rej.0.00125", n.true.names)

# MAKE SURE TO LIST THEM IN SAME ORDER AS names above
( method.names = c( "bonf.naive",
                    "holm",
                    "minP",
                    "Wstep",
                    "Romano",
                    "ours.0.01",
                    "ours.0.05") )

# reshape wide to long
# https://stackoverflow.com/questions/12466493/reshaping-multiple-sets-of-measurement-columns-wide-format-into-single-columns
s.small = s %>% select( all_of( c(n.true.names, "scen") ) )

lt = reshape( s.small,
              varying = n.true.names,
              v.names = "n.true",
              times = method.names,
              timevar = "method",  # name to use for the above variable
              direction="long" )


# since each scenario still has multiple rows, take means 
n.trues = lt %>% group_by(scen, method) %>%
  summarise( n.true = mean( n.true ) )

# merge in scenario parameters
n.trues = merge(n.trues, scen.params, by = "scen" )

# for plotting joy
n.trues$group = as.factor( paste( "X-Y correlation: ", n.trues$rho.XY,
                                  " for ",
                                  n.trues$prop.corr * 100,
                                  "% of pairs", 
                                  sep = "" ) )

# relevel to get facets in correct order
n.trues = n.trues[ order( n.trues$rho.XY, n.trues$prop.corr, n.trues$rho.YY ), ]
ordered.levels = unique(n.trues$group)
# put the strong null scenario last for prettiness
ordered.levels = c( as.character(ordered.levels), as.character(ordered.levels[1]) )
ordered.levels = ordered.levels[-1]
n.trues$group = factor( n.trues$group, levels = ordered.levels )
levels(n.trues$group)

# for more plotting joy
labels = c("Bonferroni",
           "Holm",
           "minP",
           "meanP",
           "Global (alpha=0.01)",
           "Global (alpha=0.05)",
           "Wstep",
           "Romano")
n.trues$method.label = NA
n.trues$method.label[ n.trues$method == "bonf.naive" ] = labels[1]
n.trues$method.label[ n.trues$method == "holm" ] = labels[2]
n.trues$method.label[ n.trues$method == "minP" ] = labels[3]
n.trues$method.label[ n.trues$method == "meanP" ] = labels[4]  # for "log-P"
n.trues$method.label[ n.trues$method == "ours.0.01" ] = labels[5]
n.trues$method.label[ n.trues$method == "ours.0.05" ] = labels[6]
n.trues$method.label[ n.trues$method == "Wstep" ] = labels[7]
n.trues$method.label[ n.trues$method == "Romano" ] = labels[8]

# remove experimental method
n.trues = n.trues[ n.trues$method != "meanP", ]

# set method ordering for plot
correct.order = rev( c( "Bonferroni",
                        "Holm",
                        "minP",
                        "Romano",
                        "Wstep",
                        "Global (alpha=0.01)",
                        "Global (alpha=0.05)" ) )


n.trues$method.label = factor(n.trues$method.label, levels = rev(correct.order))


# sanity check
table(n.trues$method, n.trues$method.label)

# set number of true effects identified equal to 0 when excess hits < 0
n.trues$n.true[ n.trues$n.true < 0 ] = 0

# benchmark value: the actual number of true effects in that scenario
n.trues$benchmark = n.trues$nY * n.trues$prop.corr
n.trues$benchmark[ n.trues$prop.corr == 1 & n.trues$rho.XY == 0 ] = 0  # fix the control scenario because it reads as -

##### Save Results #####
setwd(results.dir)
write.csv( n.trues, "results_ntrues.csv")

write.csv( lt, "results_ntrues_long.csv")



########################### DATA PREP: FOR POWER PLOT ###########################

# need 1 row per scenario-method combination
# methods are ours, Wstep, bonf.naive, and minP

library(reshape2)
library(tidyverse)
library(dplyr)
library(magrittr)
library(tidyr)

# names of joint rejection variables for all methods
( jt.rej.names = c( names(s)[ grep( "jt.rej.", names(s) ) ],
                    names(s)[ grep( "rej.jt", names(s) ) ] ) )

# MAKE SURE TO LIST THEM IN SAME ORDER AS JT.REJ.NAMES
( method.names = c("bonf.naive",
                   "holm",
                   "minP",
                   "Wstep",
                   "Romano",
                   "meanP",
                   "ours.0.01",
                   "ours.0.05") )

# reshape wide to long
# https://stackoverflow.com/questions/12466493/reshaping-multiple-sets-of-measurement-columns-wide-format-into-single-columns
lp = reshape( s[ , names(s) %in% c(jt.rej.names, "scen") ],
              varying = jt.rej.names,
              v.names = "jt.rej",
              times = method.names,
              timevar = "method",  # name to use for the above variable
              direction="long" )


# # sanity check
# # long data should have 1 row per method-scenario combination
# nrow( lp[ lp$scen == 1, ] ); nrow( s[ s$scen == 1, ]) * length(method.names)


# fix factor levels because of scenario that was TRUE/FALSE as strings
lp$jt.rej = as.logical(lp$jt.rej)


# # sanity check
# # reconstruct for a given scenario
# # stack results for each method
# scen = s$scen[1]
# 
# s.chunk = s[ s$scen == scen, jt.rej.names ]
# 
# jt.rej = c()
# for ( i in 1:ncol(s.chunk) ) {
#   jt.rej = c( jt.rej, as.logical(s.chunk[,i]) )
# }
# 
# lp.chunk = lp[ lp$scen == scen, ]
# all( jt.rej == lp.chunk$jt.rej )  # should be TRUE
# # make sure it's a scenario with not 100% rejections
# #  to avoid chance matches
# table(jt.rej)


# since each scenario still has multiple rows, take means 
pwr = lp %>% group_by(scen, method) %>%
  summarise( power = mean( as.logical(jt.rej) ) )

# merge in scenario parameters
pwr = merge(pwr, scen.params, by = "scen" )

# for plotting joy
pwr$group = as.factor( paste( "X-Y correlation: ", pwr$rho.XY,
                              " for ",
                              pwr$prop.corr * 100,
                              "% of pairs", 
                              sep = "" ) )

# relevel to get facets in correct order
pwr = pwr[ order( pwr$rho.XY, pwr$prop.corr, pwr$rho.YY ), ]
ordered.levels = unique(pwr$group)
# put the strong null scenario last for prettiness
ordered.levels = c( as.character(ordered.levels), as.character(ordered.levels[1]) )
ordered.levels = ordered.levels[-1]
pwr$group = factor( pwr$group, levels = ordered.levels )
levels(pwr$group)


# for more plotting joy
labels = c("Bonferroni",
           "Holm",
           "minP",
           "meanP",
           "Global (alpha=0.01)",
           "Global (alpha=0.05)",
           "Wstep",
           "Romano")
pwr$method.label = NA
pwr$method.label[ pwr$method == "bonf.naive" ] = labels[1]
pwr$method.label[ pwr$method == "holm" ] = labels[2]
pwr$method.label[ pwr$method == "minP" ] = labels[3]
pwr$method.label[ pwr$method == "meanP" ] = labels[4]  # for "log-P"
pwr$method.label[ pwr$method == "ours.0.01" ] = labels[5]
pwr$method.label[ pwr$method == "ours.0.05" ] = labels[6]
pwr$method.label[ pwr$method == "Wstep" ] = labels[7]
pwr$method.label[ pwr$method == "Romano" ] = labels[8]

# sanity check
# table(pwr$method, pwr$method.label)

# remove experimental method
pwr = pwr[ pwr$method != "meanP", ]

# set method ordering for plot
correct.order = rev( c( "Bonferroni",
                        "Holm",
                        "minP",
                        "Romano",
                        "Wstep",
                        "Global (alpha=0.01)",
                        "Global (alpha=0.05)" ) )


pwr$method.label = factor(pwr$method.label, levels = rev(correct.order))


##### Save Results ##### 
pwr2 = pwr
setwd(results.dir)
write.csv( pwr2, "results_pwr.csv")

lp2 = lp
write.csv( lp2, "results_pwr_long.csv")


########################### DATA PREP: FOR NULL INTERVAL PLOT ###########################

# only need our method

# variables to keep:
#  means of n.rej.0.01, n.rej.0.05 for looking at their distance above CI limit
#  means of n.rej.bt.0.05.mean n.rej.bt.0.01.mean for the points inside the CIs
#  means of bt.lo.0.01, bt.hi.0.01, bt.lo.0.05, bt.hi.0.05 for CI limits

# each of these will become a single column in long data
( n.rej.names = c( "n.rej.0.01", "n.rej.0.05" ) )  # avoid Bonferroni
( n.rej.bt.names = names(s)[ grep( "n.rej.bt.", names(s) ) ] )
( bt.lo.names = names(s)[ grep( "bt.lo.", names(s) ) ] )
( bt.hi.names = names(s)[ grep( "bt.hi.", names(s) ) ] )

# MAKE SURE TO LIST THEM IN SAME ORDER AS IN JT.REJ.NAMES
method.names = c("ours.0.01", "ours.0.05")

# reshape wide to long
# https://stackoverflow.com/questions/12466493/reshaping-multiple-sets-of-measurement-columns-wide-format-into-single-columns
lc = reshape( s,
              varying = list( A = n.rej.names, B = n.rej.bt.names,
                              C = bt.lo.names, D = bt.hi.names ),
              v.names= c( "n.rej", "n.rej.bt", "bt.lo", "bt.hi" ),
              times = method.names,
              timevar = "method",  # name to be used for the above variable
              direction="long" )

# average CI limits and rejections in bootstraps
ci = lc %>% group_by(scen, method) %>%
  summarise( n.rej.mn = mean(n.rej),
             n.rej.bt.mn = mean(n.rej.bt),
             bt.lo.mn = mean(bt.lo),
             bt.hi.mn = mean(bt.hi) )

ci = merge(ci, scen.params, by = "scen" )

ci$group = paste( "X-Y correlation: ", ci$rho.XY,
                  " for ",
                  ci$prop.corr * 100,
                  "% of pairs", 
                  sep = "" )

# relevel to get facets in correct order
ci = ci[ order( ci$rho.XY, ci$prop.corr, ci$rho.YY ), ]
ordered.levels = unique(ci$group)
# put the strong null scenario last for prettiness
ordered.levels = c( as.character(ordered.levels), as.character(ordered.levels[1]) )
ordered.levels = ordered.levels[-1]
ci$group = factor( ci$group, levels = ordered.levels )
levels(ci$group)


# add horizontal stagger for visible error bars in plot
ci2 = ci
buffer = 0.02
ci2$rho.YY[ ci2$method == "ours.0.05" ] = ci2$rho.YY[ ci2$method == "ours.0.05" ] + buffer

# save results (unstaggered dataset)
setwd(results.dir)
write.csv(ci, "results_null_interval.csv")

