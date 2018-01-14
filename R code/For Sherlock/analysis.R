
########################### READ IN DATA ###########################

# LOCAL TEST
setwd("~/Desktop")
scen.params = read.csv("scen_params.csv")
names(scen.params)[ names(scen.params) == "scen.name" ] = "scen"

# just use a single file instead of stitched one
s = read.csv("short_results_1_.csv", header=TRUE)

# remove columns from individual bootstrap results
kill = c( names(s)[ grepl( "n.rej.bt", names(s) ) ], "n.rej.5e.04" )
s = s[ , !names(s) %in% kill ]

# did naive Bonferroni reject?
s$naive_bonf_rej = s$n.rej.5e.04.1 > 0


# TEMP: see if reshape problem fixed by random noise
#s[,11:30] = s[,11:30] + rnorm(n=100, mean=0, sd=0.01)
#temp = s[,c(10,14,15,16)]


########################### MAKE DATASETS FOR FIRST PLOT (RESIDUAL RESAMPLING) ###########################

# http://www.milanor.net/blog/reshape-data-r-tidyr-vs-reshape2/
library(tidyr)
library(dplyr)
# replace underscores (for use with separate function)
#names(temp) = sub( pattern = "t[.]", replacement = "t_", x=names(temp) )
names(s)[ grepl( "crit", names(s) ) ] = sub( pattern = "t[.]",
                                             replacement = "t_", x=names(s)[ grepl( "crit", names(s) ) ] )
names(s)[ grepl( "rej.jt", names(s) ) ] = sub( pattern = "t[.]",
                                             replacement = "t_", x=names(s)[ grepl( "rej.jt", names(s) ) ] )


######## Aggregated Dataset 1: Our Method's Power ######## 
# only keep residual resamples and useful columns
crit.names = names(s)[ grepl( "crit", names(s) ) ]
rej.jt.names = names(s)[ grepl( "rej.jt", names(s) ) ]
temp = s[ s$bt.type == "resid", names(s) %in% c( "scen", "rho.YY", "rho.XY", crit.names, rej.jt.names ) ] 

# ~~~~~ HARD-CODES BONFERRONI!!!!
options(scipen=999)
bonf = 0.05/100
names(temp) = sub( pattern = "bonf", replacement = bonf, x=names(temp) )

# get in tidy format
gathered.messy = gather(temp, key, value, -scen, -rho.YY, -rho.XY )
head(gathered.messy)

tidy = separate( gathered.messy, key, into = c("stat", "alpha"), sep = "_", convert=FALSE )

# make summary datasets
power = tidy %>% group_by( scen, alpha ) %>% filter( stat == "rej.jt" ) %>% summarise( power = mean(value) )
power = merge( scen.params, power )
power$group = paste( "X-Y correlation: ", power$rho.XY )  # for plotting joy

######## Aggregated Dataset 2: Naive Bonferroni Power ######## 

temp = s[ s$bt.type == "resid", names(s) %in% c( "scen", "naive_bonf_rej" ) ] 

# get in tidy format
gathered.messy = gather(temp, key, value, -scen )

# make summary datasets
naive.power = gathered.messy %>% group_by( scen ) %>% summarise( power = mean(value) )
naive.power = merge( scen.params, naive.power )
naive.power$group = paste( "X-Y correlation: ", naive.power$rho.XY )  # for plotting joy


########################### MAKE DATASETS FOR SECOND PLOT (FCR RESAMPLING) ###########################



# TAKE MEANS BY SCENARIO AND BY ALPHA
# ALSO EMPIRICAL QUANTILES
# SO, FOR TEST CASE, WILL BE JUST 3 ROWS


# # add expected number of hits
# agg$expect = agg$alpha * agg$nY
# 
# # for plotting
# agg$group = paste( "nX: ", agg$nX, "; nY: ", agg$nY, sep="" )
# 



########################### PLOT 1: JOINT NULL HYPOTHESIS TEST ###########################

# X-axis: Strength of YY correlation
# Y-axis: Power to reject joint null
# Panels: Strength of XY correlation
# Lines: Different alpha levels for our method and a single line for naive Bonferroni


library(ggplot2)
y.breaks = seq(0, 0.05, 0.01)

colors = c( "black", "orange", "red", "blue" )

p1 = ggplot( ) +
  geom_point( data=power, aes(x=rho.YY, y=power, shape=alpha ) ) +
  # geom_line( data=power, aes(x=rho.YY, y=power, shape=alpha ) ) + 
  geom_point( data = naive.power, aes( x = rho.YY, y = power, shape="Naive") ) +
  theme_bw() + facet_wrap(~ group ) +
  ylab("Power") +
  # HAVEN'T DEALT WITH LOWER LINES YET:
  #scale_color_manual(values=colors) +
  #scale_x_continuous( breaks = x.breaks ) + 
  scale_y_continuous( limits = c(0,0.05), breaks = y.breaks ) +
  guides(color=guide_legend(title="Alpha")) +
  xlab( "Correlation between each pair of Ys" ) +
  ggtitle("Performance of bootstrap CIs as hypothesis test of joint H0")

ggsave( filename = paste("plot1.png"),
        plot=p1, path=NULL, width=10, height=10, units="in")










##### ~~~~~~~~~~~ FROM BEFORE


########################### READ IN DATA ###########################

# check overall progress
#setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2/Simulation results/2017-7-10")
setwd("~/Desktop")
scen.params = read.csv("scen_params.csv")

setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2/Simulation results/2017-7-10/sim_results/overall_stitched")
s = read.csv("stitched_results.csv")

# # which scenarios are done?
# table(s$scen)


# does it achieve nominal coverage?
# should be 2.5% true
#prop.table( table( s$n.rej > s$hi.bt ) )
# should be 0.05
mean(s$bt.rej)

# THESE SHOULD BE EQUAL (since generated under H0)
# real quantiles of n.rej
quantile( s$n.rej, c(0.025, 0.975) )
# vs. quantiles estimated by bootstraps
c( mean(s$lo.bt), mean(s$hi.bt) )

# these should also be equal
quantile( s$n.rej, c(0.025, 0.975) )

# compare variances
var(s$n.rej); mean(s$var.n.rej.bt)

# did bootstrap hypo test work?
# invert-CI method
prop.table( table( ( s$n.rej > mean(s$hi.bt) ) ) )

# sanity check: should be 5%
prop.table( table( ( s$n.rej > quantile(s$n.rej, 0.95) ) ) )









# later get this from parameters
alpha = scen.params$alpha
nY = scen.params$nY
N0 = alpha * nY


# simulation rep
n.boot = max(s$bt.iterate)
n.sims.run = dim(s)[1] / n.boot
s$sim.rep = rep( seq(1, n.sims.run), each = n.boot )

# compute CI coverage
library(data.table)
st = data.table(s)
st[ , lo.bt := quantile( n.rej.bt, alpha / 2 ), by=sim.rep ]
st[ , hi.bt := quantile( n.rej.bt, 1 - (alpha / 2) ), by=sim.rep ]

# coverage
st$covers = (st$lo.bt <= N0 ) & (st$hi.bt >= N0)

# hypothesis test
st[ , crit.val := quantile( abs( n.rej.bt - n.rej ), 1 - alpha ), by=sim.rep ]
st[ , bt.rej := abs( n.rej - N0 ) > crit.val, by = sim.rep ]

s = data.frame(st)



#####################################################################################
#                              FOR SINGLE SCENARIO
#####################################################################################

# mean coverage
mean(s$covers)

# does test have nominal alpha?
mean(s$bt.rej)

# make simulation rep variable
s$sim.rep = 1:dim(s)[1]


########################### PLOT: BOOTSTRAP CIS ACROSS SCENARIOS ###########################

# take a random sample since it's too many to plot
samp = s[ sample( 1:dim(s)[1], size = 50, replace = FALSE), ]
samp$Y = 1:dim(samp)[1]  # number the rows

# color by coverage: red lines are ones that do not cover
samp$color = "darkgray"
samp$color[ !samp$covers ] = "red"

library(ggplot2)

ggplot( samp, aes( x=n.rej, y=Y ) ) +
  geom_segment( aes( x=lo.bt, xend=hi.bt, y=Y, yend=Y ), color=samp$color ) +
  geom_vline( xintercept = .05*100, lty=2, color="red" ) +  # expected number rejections under H0
  geom_point() +  
  theme_bw()



#####################################################################################
#                              FOR MULTIPLE SCENARIOS 
#####################################################################################

# aggregate the results columns across reps
library(data.table)
st = data.table(s)

st[, n.rej.mean := mean( n.rej ), by=scen ]
st[, lo.bt.mean := mean( lo.bt ), by=scen ]
st[, hi.bt.mean := mean( hi.bt ), by=scen ]
st[, rep.time.mean := mean( rep.time ), by=scen ]
st[, bt.rej.mean := mean( bt.rej ), by=scen ]

# keep only 1 row per scenario
s2 = data.frame( st[ !duplicated(st$scen), ] )
keepers = c("scen", "boot.reps", "rep.time.mean", "n.rej.mean", "lo.bt.mean", "hi.bt.mean",
            "bt.rej.mean")
s2 = s2[ , names(s2) %in% keepers ]   

# merge scenario parameters
names(s2)[ names(s2) == "scen" ] = "scen.name"
agg = merge( scen.params, s2 )

# add expected number of hits
agg$expect = agg$alpha * agg$nY

# for plotting
agg$group = paste( "nX: ", agg$nX, "; nY: ", agg$nY, sep="" )


########################### PLOT: BOOTSTRAP CI WIDTHS ACROSS SCENARIOS ###########################

library(ggplot2)

x.breaks = c( 0, 0.1, 0.5 )
y.breaks = seq(0, 30, 2)

colors=c("black")

p1 = ggplot( data=agg, aes(x=rho.YY, y=n.rej.mean, color=as.factor(alpha) ) ) +
  geom_hline( aes( yintercept=expect, group=group, color=as.factor(alpha) ), linetype=2 ) +
  geom_point(size=3, position = position_dodge(width=0.05) ) +
  theme_bw() + facet_wrap(~ group ) +
  ylab("Number of rejections under H0") + scale_color_manual(values=colors) +
  geom_errorbar( aes(ymin=lo.bt.mean, ymax=hi.bt.mean), width=.00, lwd=1, position = position_dodge(width=0.05) ) +
  scale_x_continuous( breaks = x.breaks ) + 
  scale_y_continuous( breaks = y.breaks ) +
  guides(color=guide_legend(title="Alpha")) +
  xlab( "Correlation between each pair of Ys" ) +
  ggtitle("Bootstrap CIs for number of rejections under joint H0")

ggsave( filename = paste("plot1.png"),
        plot=p1, path=NULL, width=10, height=10, units="in")


########################### EXAMINE PERFORMANCE OF BOOTSTRAP AS A HYPOTHESIS TEST ###########################

y.breaks = seq(0, 0.05, 0.01)

p2 = ggplot( data=agg, aes(x=rho.YY, y=bt.rej.mean, color=as.factor(alpha) ) ) +
  geom_hline( aes( yintercept=alpha, group=group, color=as.factor(alpha) ), linetype=2 ) +
  geom_point(size=3, position = position_dodge(width=0.05) ) +
  geom_line( position = position_dodge(width=0.05) ) +
  theme_bw() + facet_wrap(~ group ) +
  ylab("Proportion of joint H0 rejections by inverting bootstrap CI") + scale_color_manual(values=colors) +
  scale_x_continuous( breaks = x.breaks ) + 
  scale_y_continuous( limits = c(0,0.05), breaks = y.breaks ) +
  guides(color=guide_legend(title="Alpha")) +
  xlab( "Correlation between each pair of Ys" ) +
  ggtitle("Performance of bootstrap CIs as hypothesis test of joint H0")

ggsave( filename = paste("plot2.png"),
        plot=p2, path=NULL, width=10, height=10, units="in")





