

########################### HELPER FNS ###########################

# Note that these access global variables for, e.g., method.label


ntrues_plot = function(dat, benchmark.line = FALSE) {
  library(ggplot2)
  p = ggplot( data = dat, aes( x = rho.YY, y = n.true,
                           color = method,
                           label = method ) ) +
    geom_text( aes( label = method.label) ) +
    theme_bw() +
    facet_wrap(~ group, ncol = 3 ) +  # for changing rows/columns
    ylab("Number of nulls rejected") +
    scale_color_manual( values = colors) +
    scale_x_continuous( limits = c( min(x.breaks), max(x.breaks) ), breaks = x.breaks ) +
    scale_y_continuous( limits = c( min(y.breaks), max(y.breaks) ), breaks = y.breaks ) +
    xlab( "Correlation between each pair of outcomes" ) +
    theme(legend.position="none") # remove legend
  
  if ( benchmark.line == FALSE) return(p)
  else return( p + geom_hline( aes(yintercept = benchmark ), color="red", lty=2 ) )
}



pwr_plot = function(dat) {
  library(ggplot2)
  ggplot( data = dat, aes( x = rho.YY, y = power,
                           color = method,
                           label = method ) ) +
    geom_text( aes( label = method.label) ) +
    theme_bw() +
    #facet_wrap( ~group) +
    facet_wrap(~ group, ncol = 3 ) +  # for changing rows/columns
    ylab("Power") +
    scale_color_manual( values = colors) +
    scale_x_continuous( limits = c( min(x.breaks), max(x.breaks) ), breaks = x.breaks ) +
    scale_y_continuous( limits = c( min(y.breaks), max(y.breaks) ), breaks = y.breaks ) +
    xlab( "Correlation between each pair of outcomes" ) +
    #ggtitle("Power of bootstrapped hypothesis test of joint null") +
    theme(legend.position="none") # remove legend
}

ci_plot = function(dat) {
  library(ggplot2)
  ggplot( data = dat ) +
    # bootstrap results
    geom_point( aes( x = rho.YY, y = n.rej.bt.mn, color = method ), size=2.5 ) +
    geom_errorbar( aes( x=rho.YY, ymin = bt.lo.mn,
                        ymax = bt.hi.mn, color = method ), width=0.02, size=1.05 ) +
    
    # original dataset results
    geom_point( aes( x = rho.YY, y = n.rej.mn, shape="the shape" ), color = "black", size=3.5 ) +
    scale_shape_manual( values = c('the shape' = 4),
                        name = "Original dataset", guide = 'legend', labels = c("Mean rejections")) +
    
    theme_bw() +
    facet_wrap(~ group, ncol = 3 ) +
    ylab("Average rejections") +
    scale_color_manual( name="Joint test", values = colors, labels = legend.labs ) +
    
    xlab( "Correlation between each pair of outcomes" ) +
    #ggtitle("Average CI limits and rejections in resamples and originals") +
    theme(legend.position="none") # remove legend
}




########################### READ IN DATA ###########################

# as in Frontiers_0 (only W=40)
setwd("~/Dropbox/Personal computer/Harvard/THESIS/Thesis paper #2 (MO)/Linked to OSF (MO)/Simulation results in paper/2018-6-17 rerun Freedman after fixing bug")


# read in scenario parameters
scen.params = read.csv("scen_params.csv")
names(scen.params)[ names(scen.params) == "scen.name" ] = "scen"

# read in stitched data
s = read.csv("stitched.csv", header=TRUE)
s = merge(s, scen.params, by = "scen" )

# what percent done are we?
n.reps.per.scen = 500
nrow(s) / (500 * nrow(scen.params))

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
( n.true.names = c( names(s)[ grep( "n.true.", names(s) ) ] ) )  #@BREAKS
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
lt = reshape( s[ , names(s) %in% c(n.true.names, "scen") ],
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
labels = c("B", "H", "MP", "G1", "G5", "WS", "R")
n.trues$method.label = NA
n.trues$method.label[ n.trues$method == "bonf.naive" ] = labels[1]
n.trues$method.label[ n.trues$method == "holm" ] = labels[2]
n.trues$method.label[ n.trues$method == "minP" ] = labels[3]
n.trues$method.label[ n.trues$method == "ours.0.01" ] = labels[4]
n.trues$method.label[ n.trues$method == "ours.0.05" ] = labels[5]
n.trues$method.label[ n.trues$method == "Wstep" ] = labels[6]
n.trues$method.label[ n.trues$method == "Romano" ] = labels[7]


# sanity check
table(n.trues$method, n.trues$method.label)

# set number of true effects identified equal to 0 when excess hits < 0
n.trues$n.true[ n.trues$n.true < 0 ] = 0

# benchmark value: the actual number of true effects in that scenario
n.trues$benchmark = n.trues$nY * n.trues$prop.corr
n.trues$benchmark[ n.trues$prop.corr == 1 & n.trues$rho.XY == 0 ] = 0  # fix the control scenario because it reads as -

##### Save Results #####
setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Simulation results/2018-8-18 with ntrues plot")
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
labels = c("B", "H", "MP", "G1", "G5", "WS", "R")
pwr$method.label = NA
pwr$method.label[ pwr$method == "bonf.naive" ] = labels[1]
pwr$method.label[ pwr$method == "holm" ] = labels[2]
pwr$method.label[ pwr$method == "minP" ] = labels[3]
pwr$method.label[ pwr$method == "ours.0.01" ] = labels[4]
pwr$method.label[ pwr$method == "ours.0.05" ] = labels[5]
pwr$method.label[ pwr$method == "Wstep" ] = labels[6]
pwr$method.label[ pwr$method == "Romano" ] = labels[7]

# sanity check
table(pwr$method, pwr$method.label)


##### Save Results #####
pwr2 = pwr
setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Simulation results/2018-8-18 with ntrues plot")
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
  summarise( n.rej.mn = mean( as.numeric( as.character(n.rej) ) ),
             n.rej.bt.mn = mean( as.numeric( as.character(n.rej.bt) ) ),
             bt.lo.mn = mean( as.numeric( as.character(bt.lo) ) ),
             bt.hi.mn = mean( as.numeric( as.character(bt.hi) ) ) )

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
setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Simulation results/2018-8-18 with ntrues plot")
write.csv(ci, "results_null_interval.csv")
write.csv(lc, "results_null_interval_long.csv")




########################### N TRUE EFFECTS PLOTS ###########################

# for main text: simplify the plot 
# by removing intermediate effect sizes
n.trues.short = n.trues[ !n.trues$rho.XY %in% c(0.1, 0.15), ]

# set global variables needed for plotting fn
x.breaks = c(0, 0.1, 0.3, 0.6)
colors = c( "#999999", "orange", "#009E73", "black", "#E69F00", "#D55E00", "black", "darkgreen" )

dat = n.trues

y.breaks = seq(0, 40, 10)
p5 = ntrues_plot(n.trues, benchmark.line = TRUE); p5

y.breaks = seq(0, 10, 2)
p6 = ntrues_plot(n.trues.short, benchmark.line = FALSE); p6

##### Save Results #####
setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Simulation results/2018-8-18 with ntrues plot")
width = 8
square.size = 8/3 # divide by number of cols
height = square.size*5  # multiply by number of rows
ggsave( filename = paste("ntrues_full.png"),
        plot=p5, path=NULL, width=width, height=height, units="in")

width = 8
square.size = 8/3  # divide by number of cols
height = square.size*3  # multiply by number of rows
ggsave( filename = paste("ntrues_short.png"),
        plot=p6, path=NULL, width=width, height=height, units="in")



########################### JOINT TEST POWER PLOTS ###########################

# X-axis: Strength of YY correlation
# Y-axis: Power to reject joint null
# Panels: Strength of XY correlation
# Lines: Different methods

# for main text: simplify the plot 
# by removing intermediate effect sizes
pwr.short = pwr[ !pwr$rho.XY %in% c(0.1, 0.15), ]

# set global variables needed for plotting fn
x.breaks = c(0, 0.1, 0.3, 0.6)
y.breaks = seq(0, 1, 0.1)
colors = c( "#999999", "orange", "#009E73", "black", "#E69F00", "#D55E00", "black", "darkgreen" )


# for Appendix version, use pwr instead of pwr.short
p1 = pwr_plot(pwr); p1
p2 = pwr_plot(pwr.short); p2

width = 8
square.size = 8/3 # divide by number of cols
height = square.size*5  # multiply by number of rows
name = "joint_test_full.png"
ggsave( filename = paste(name),
       plot=p1, path=NULL, width=width, height=height, units="in")

width = 8
square.size = 8/3 # divide by number of cols
height = square.size*3  # multiply by number of rows
name = "joint_test_short.png"
ggsave( filename = paste(name),
        plot=p2, path=NULL, width=width, height=height, units="in")



########################### NULL INTERVAL PLOTS ###########################

# shorter version for main text
# run this, then re-run the above
ci2.short = ci2[ ci2$rho.XY %in% c(0, 0.05, 0.1, 0.15), ]


##### Make Plot #####

# X-axis: Strength of YY correlation
# Y-axis: Power to reject joint null
# Panels: Strength of XY correlation
# Lines: Different methods

library(ggplot2)
y.breaks = seq(0, 1, 0.1)

colors = c( "#E69F00", "#D55E00" )
legend.labs = c( "G1 (alpha = 0.01)", "G5 (alpha = 0.05)" )

p3 = ci_plot(ci2); p3
p4 = ci_plot(ci2.short); p4

width = 8
square.size = 8/3 # divide by number of cols
height = square.size*5  # multiply by number of rows
ggsave( filename = paste("null_ci_full.png"),
         plot=p3, path=NULL, width=width, height=height, units="in")

width = 8
square.size = 8/3  # divide by number of cols
height = square.size*4  # multiply by number of rows
ggsave( filename = paste("null_ci_short.png"),
         plot=p4, path=NULL, width=width, height=height, units="in")



########################### OTHER STATS FOR PAPER ###########################


# "mean upper limit of the null interval was more than twice as high"

# back to ci dataframe because has unstaggered X-axis
# upper CI limits under independence vs. moderate correlation
( bt.hi = ci %>% filter(method=="ours.0.05") %>%
  group_by(rho.YY) %>%
  summarise(bt.hi.mn = mean(bt.hi.mn) ) )

print( paste( "...more than twice as high for rhoYY = 0.60 versus rhoYY = 0 (i.e., ",
              round( bt.hi$bt.hi.mn[ bt.hi$rho.YY == 0.60 ], 1 ),
              " versus ",
              round( bt.hi$bt.hi.mn[ bt.hi$rho.YY == 0 ], 1 ),
              " rejections; ",
              sep=""
              ) )


##### Look at a Particular Scenario #####

# "Thus, with a true effect size of..."

true.ES = 0.05
# mean rejections
( n.rej.mean = ci %>%
  filter(method=="ours.0.05") %>%
  filter(rho.XY == true.ES) %>%
  filter(prop.corr == 1) %>%
  summarise(n.rej.mn = mean(n.rej.mn) ) )

# excess hits at different rho.YY
print( paste( "...the mean number of observed rejections at alpha = 0.05 (",
              round( n.rej.mean, 1 ),
       ")",
       sep=""
       ) )

# "would be only just outside the null interval if..."
print( paste( round( n.rej.mean, 1 ),
              " - ",
              round( bt.hi$bt.hi.mn[ bt.hi$rho.YY == 0.6 ], 1 ),
              " = ",
              round( n.rej.mean - bt.hi$bt.hi.mn[ bt.hi$rho.YY == 0.6 ], 2 ),
              sep="" ) )

# "...but would be well outside the null interval if"...
print( paste( round( n.rej.mean, 1 ),
              " - ",
              round( bt.hi$bt.hi.mn[ bt.hi$rho.YY == 0 ], 1 ),
              " = ",
              round( n.rej.mean - bt.hi$bt.hi.mn[ bt.hi$rho.YY == 0 ], 1 ),
              sep="" ) )


# Bonferroni rejections
( bonf.rej = s %>%
    filter(rho.YY==0.6) %>%
    filter(rho.XY == true.ES) %>%
    filter(prop.corr == 1) %>%
    summarise( n.rej.bonf = mean(n.rej.0.00125) )
  )


##### Number of True Effects Detected #####

n.trues.Romano = n.trues %>% group_by(scen) %>%
  filter( method == "Romano" ) 

n.trues.ours = n.trues %>% group_by(scen) %>%
  filter( method == "ours.0.05" )

# "In such scenarios, our proposed methods sometimes rejected as many as 6 more null hypotheses than the next-best method"
ind = which.max( n.trues.ours$n.true - n.trues.Romano$n.true )

print( paste( "as many as ",
              signif( n.trues.ours$n.true[ind] - n.trues.Romano$n.true[ind], 3 ),
              " more null hypotheses",
              sep="" ) )

# in Discussion
print( paste( 
              signif( n.trues.ours$n.true[ind], 3 ),
              " vs. ",
              signif( n.trues.Romano$n.true[ind], 3 ),
              sep="" ) )

