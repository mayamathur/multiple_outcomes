
########################### READ IN DATA ###########################

# LOCAL TEST
setwd("~/Desktop")
scen.params = read.csv("scen_params.csv")
names(scen.params)[ names(scen.params) == "scen.name" ] = "scen"

# just use a single file instead of stitched one
s = read.csv("stitched.csv", header=TRUE)
s = merge(s, scen.params, by = "scen" )

# what percent done are we?
nrow(s) / (500 * nrow(scen.params))

# how many reps per scenario do we have?
table( s$scen )



########################### COMPARATIVE POWER PLOT ###########################

# need 1 row per scenario-method combination
# methods are ours, Wstep, bonf.naive, and minP

# variables to keep:
# scenario parameters
#  mean of jt.rej.bonf.naive, jt.rej.minP, jt.rej.Wstep
#  mean of rej.jt.0.01, rej.jt.0.05

library(reshape2)


# 
( jt.rej.names = c( names(s)[ grep( "jt.rej.", names(s) ) ],
                    names(s)[ grep( "rej.jt", names(s) ) ] ) )
  
# MAKE SURE TO LIST THEM IN SAME ORDER AS JT.REJ.NAMES
method.names = c("bonf.naive", "minP", "Wstep", "ours.0.01", "ours.0.05")

# reshape wide to long
# https://stackoverflow.com/questions/12466493/reshaping-multiple-sets-of-measurement-columns-wide-format-into-single-columns
lp = reshape( s[ , names(s) %in% c(jt.rej.names, "scen") ],
               varying = jt.rej.names,
               v.names="jt.rej",
               times = method.names,
              timevar = "method",  # name of the above variable
               direction="long" )

# sanity check
# long data should have 1 row per method-scenario combination
# nrow( lp[ lp$scen == "a", ] ); nrow( s[ s$scen == "a", ]) * length(method.names)

# since each scenario still has multiple rows, take means 
library(tidyr)

pwr = lp %>% group_by(scen, method) %>%
  summarise( power = mean(jt.rej) )

pwr = merge(pwr, scen.params, by = "scen" )

pwr$group = paste( "X-Y correlation: ", pwr$rho.XY )  # for plotting joy

##### Make Plot ##### 

# X-axis: Strength of YY correlation
# Y-axis: Power to reject joint null
# Panels: Strength of XY correlation
# Lines: Different alpha levels for our method and a single line for naive Bonferroni

library(ggplot2)
y.breaks = seq(0, 1, 0.1)

colors = c( "black", "green", "blue", "lightblue", "darkgreen" )
legend.labs = c("Naive Bonferroni", "Westfall minP",
                "Ours (alpha = 0.01)", "Ours (alpha = 0.05)",
                "Westfall step-down")

ggplot( data = pwr, aes( x = rho.YY, y = power, color=method ) ) +
  geom_point( size=3.2 ) +
  geom_line( ) +
  theme_bw() + facet_wrap(~ group ) +
  ylab("Power") +
  scale_color_manual( name="Joint test", values = colors, labels = legend.labs ) +
  scale_shape_manual( name = "Joint test", values = c(20, 17, 18, 1), labels = legend.labs ) +
  scale_y_continuous( limits = c(0,1), breaks = y.breaks ) +
  xlab( "Correlation between each pair of Ys" ) +
  ggtitle("Power of bootstrapped hypothesis test of joint null")



ggsave( filename = paste("plot1.png"),
        plot=p1, path=NULL, width=10, height=10, units="in")




########################### NULL CI PLOTS ###########################

# only need our method

# variables to keep: 
#  means of n.rej.0.01, n.rej.0.05 for looking at their distance above CI limit
#  means of n.rej.bt.0.05.mean n.rej.bt.0.01.mean for the points inside the CIs
#  means of bt.lo.0.01, bt.hi.0.01, bt.lo.0.05, bt.hi.0.05 for CI limits


n.rej.names = names(s)[ grep( "n.rej.0.", names(s) ) ]










# ########################### OLD CODE ###########################
# 
# 
# # # is coverage different from 95%?
# # mean(s$covers)
# # mean(s$covers.correct)
# # prop.test( sum(s$covers), length(s$covers), p=0.95 )
# 
#  
# # PLAN
# # 1.) Joint test power plot is basically the means of
# #  jt.rej.bonf.naive, jt.rej.minP, jt.rej.Wstep,
# #  rej.jt.0.05 (ours), and rej.jt.0.05 (also ours) by scenario
# 
# 
# # 2.) CI plot
# #  limits are the average bt.lo.0.01 and bt.hi.0.05 for each scenario
# #  mean is the mean of n.rej.bt.0.05.mean for each scenario
# 
# # MAKE IT SAVE THE MEAN REJECTIONS FOR USE IN SECOND PLOT! 
# 
# 
# ########################### MAKE DATASETS FOR FIRST PLOT (RESIDUAL RESAMPLING) ###########################
# 
# # http://www.milanor.net/blog/reshape-data-r-tidyr-vs-reshape2/
# library(tidyr)
# library(dplyr)
# # replace periods with underscores (for use with separate function)
# #names(temp) = sub( pattern = "t[.]", replacement = "t_", x=names(temp) )
# names(s)[ grepl( "crit", names(s) ) ] = sub( pattern = "t[.]",
#                                              replacement = "t_", x=names(s)[ grepl( "crit", names(s) ) ] )
# names(s)[ grepl( "jt.rej", names(s) ) ] = sub( pattern = "t[.]",
#                                              replacement = "t_", x=names(s)[ grepl( "jt.rej", names(s) ) ] )
# 
# 
# ######## Aggregated Dataset 1: Our Method's Power ######## 
# 
# # only keep useful columns
# crit.names = names(s)[ grepl( "crit", names(s) ) ]
# rej.jt.names = names(s)[ grepl( "jt.rej", names(s) ) ]
# temp = s[ , names(s) %in% c( "scen", "rho.YY", "rho.XY", crit.names, rej.jt.names ) ] 
# 
# # # ~~~~~ HARD-CODES NUMBER OF OUTCOMES!!!!!!!!
# # options(scipen=999)
# # bonf = 0.05/40
# # names(temp) = sub( pattern = "bonf", replacement = bonf, x=names(temp) )
# # names(temp) = sub( pattern = "5e.04.1", replacement = bonf, x=names(temp) )
# 
# # get in tidy format
# gathered.messy = gather(s, key, value, -scen, -rho.YY, -rho.XY )
# head(gathered.messy)
# 
# tidy = separate( gathered.messy, key, into = c("stat", "alpha"), sep = "_", convert=FALSE )
# tidy = merge( scen.params, tidy )
# 
# # make summary dataset
# power = tidy %>% group_by( scen, alpha, rho.XY, rho.YY ) %>% filter( stat == "jt.rej" & bt.type == "h0.parametric" ) %>% summarise( power = mean(value) )
# power$group = paste( "X-Y correlation: ", power$rho.XY )  # for plotting joy
# 
# # # summary dataset for confidence interval through inversion
# # crit = tidy %>% group_by( scen, alpha, rho.XY, rho.YY ) %>% filter( stat == "crit" & bt.type == "resid" ) %>% summarise( crit = mean(value) )
# # crit$group = paste( "X-Y correlation: ", power$rho.XY )  # for plotting joy
# # crit$expect = as.numeric( crit$alpha ) * 100
# # #crit$crit.excess = crit$crit - 
# 
# 
# ######## Aggregated Dataset 2: Naive Bonferroni Power ######## 
# 
# temp = s[ s$bt.type == "h0.parametric", names(s) %in% c( "scen", "naive_bonf_rej" ) ] 
# 
# # get in tidy format
# gathered.messy = gather(temp, key, value, -scen )
# 
# # make summary datasets
# naive.power = gathered.messy %>% group_by( scen ) %>% summarise( power = mean(value) )
# naive.power = merge( scen.params, naive.power )
# naive.power$group = paste( "X-Y correlation: ", naive.power$rho.XY )  # for plotting joy
# 
# 
# 
# ########################### PLOT 1: JOINT NULL HYPOTHESIS TEST ###########################
# 
# # X-axis: Strength of YY correlation
# # Y-axis: Power to reject joint null
# # Panels: Strength of XY correlation
# # Lines: Different alpha levels for our method and a single line for naive Bonferroni
# 
# 
# library(ggplot2)
# y.breaks = seq(0, 1, 0.1)
# 
# # library(RColorBrewer)
# # colors = c( colorRampPalette( c("lightblue", "black") )(3), "red" )
# 
# colors = c( "lightblue", "blue", "black", "orange" )
# legend.labs = c("Bootstrap (alpha = 0.0005)", "Bootstrap (alpha = 0.01)",
#                 "Bootstrap (alpha = 0.05)", "Naive Bonferroni (alpha = 0.0005)")
# 
# ggplot( ) +
#   geom_point( data = naive.power, aes( x = rho.YY, y = power, shape="arbitrary", color="arbitrary"), size=3.2 ) +
#   geom_line( data = naive.power, aes( x = rho.YY, y = power, shape="arbitrary", color="arbitrary"), size=1.1 ) +
#   
#   geom_point( data=power, aes(x=rho.YY, y=power, shape=alpha, color=alpha ), size=3.2 ) +
#   geom_line( data=power, aes(x=rho.YY, y=power, shape=alpha, color=alpha ), size=1.1 ) + 
#   
#   theme_bw() + facet_wrap(~ group ) +
#   
#   geom_hline( yintercept=0.05, linetype=2, color="red" ) +
#   
#   ylab("Power") +
#   scale_color_manual( name="Joint test", values = colors, labels = legend.labs ) +
#   scale_shape_manual( name = "Joint test", values = c(20, 17, 18, 1), labels = legend.labs ) +
#   scale_y_continuous( limits = c(0,1), breaks = y.breaks ) +
#   xlab( "Correlation between each pair of Ys" ) +
#   ggtitle("Power of bootstrapped hypothesis test of joint null")
# 
# 
# 
# ggsave( filename = paste("plot1.png"),
#         plot=p1, path=NULL, width=10, height=10, units="in")
# 
# 
# 
# 
# ########################### PLOT 2: GOOD CONFIDENCE INTERVAL (INVERT H0 TEST) ###########################
# 
# # X-axis: Strength of YY correlation
# # Y-axis: Power to reject joint null
# # Panels: Strength of XY correlation
# # Lines: Different alpha levels for our method and a single line for naive Bonferroni
# 
# library(ggplot2)
# y.max = 100
# y.breaks = seq(0, y.max, 10)
# 
# colors = colors[1:3]
# legend.labs = c("0.0005", "0.01",
#                 "0.05" )
# 
# ggplot( ) +
#   geom_point( data=excess, aes(x=rho.YY, y=excess.hits, shape=alpha, color=alpha ), size=3 ) +
#   
#   geom_errorbar( data = ci, aes( x=rho.YY, ymin = bt.lo, ymax = bt.hi, color=alpha ), width=0.02 ) +
#   
#   geom_line( data=excess, aes(x=rho.YY, y=excess.hits, shape=alpha, color=alpha ) ) + 
#   theme_bw() + facet_wrap(~ group ) +
#   ylab("Excess hits") +
#   scale_color_manual( name="Alpha", values = colors, labels = legend.labs ) +
#   scale_shape_manual( name = "Alpha", values = c(20,17,18), labels = legend.labs ) +
#   scale_y_continuous( limits = c(0,y.max), breaks = y.breaks ) +
#   xlab( "Correlation between each pair of Ys" ) +
#   ggtitle("Power of bootstrapped hypothesis test of joint null")
# 
# 
# # BOOKMARK: FIX CIS BECAUSE THEY DON'T INCLUDE THE VALUE, BUT MAYBE BECAUSE OF FEW SIM REPS
# 
# # scenarios where boot ci doesn't include truth: d, f, h (light blue line in middle plot with rhoYY=0)
# 
# 
# ggsave( filename = paste("plot2.png"),
#         plot=p2, path=NULL, width=10, height=10, units="in")
# 
# 
# 
# 
# 
# 
# 
# ########################### MAKE DATASETS FOR CI PLOT (FCR RESAMPLING) ###########################
# 
# 
# # replace underscores (for use with separate function)
# #names(temp) = sub( pattern = "t[.]", replacement = "t_", x=names(temp) )
# names(s)[ grepl( "n.rej", names(s) ) ] = sub( pattern = "j[.]",
#                                              replacement = "j_", x=names(s)[ grepl( "n.rej", names(s) ) ] )
# names(s)[ grepl( "bt.lo", names(s) ) ] = sub( pattern = "o[.]",
#                                                replacement = "o_", x=names(s)[ grepl( "bt.lo", names(s) ) ] )
# names(s)[ grepl( "bt.hi", names(s) ) ] = sub( pattern = "i[.]",
#                                               replacement = "i_", x=names(s)[ grepl( "bt.hi", names(s) ) ] )
# 
# ######## Aggregated Dataset 1: Our Method's Power ######## 
# 
# # only keep FCR resamples and useful columns
# names(s) = sub( pattern = "5e.04.1", replacement = "0.0005", x=names(s) )
# n.rej.names = names(s)[ grepl( "n.rej_0", names(s) ) ]
# lo.names = names(s)[ grepl( "bt.lo", names(s) ) ]
# hi.names = names(s)[ grepl( "bt.hi", names(s) ) ]
# temp = s[ names(s) %in% c( "scen", "rho.YY", "rho.XY", "bt.type",
#                                                n.rej.names, lo.names, hi.names ) ] 
# 
# # ~~~~~ HARD-CODES BONFERRONI!!!!
# options(scipen=999)
# bonf = 0.05/100
# names(temp) = sub( pattern = "bonf", replacement = bonf, x=names(temp) )
# 
# 
# # get in tidy format
# gathered.messy = gather(temp, key, value, -scen, -rho.YY, -rho.XY, -bt.type )
# head(gathered.messy)
# tidy = separate( gathered.messy, key, into = c("stat", "alpha"), sep = "_", convert=FALSE )
# 
# # add variable for expected rejections
# # HARD-CODES THE NUMBER OF OUTCOMES
# tidy$expected = as.numeric( tidy$alpha ) * 100
# 
# # make summary dataset of excess hits
# # includes both FCR and residual resamples
# excess = tidy %>% group_by( scen, alpha ) %>% filter( stat == "n.rej" ) %>% summarise( excess.hits = mean(value) - mean(expected),
#                                                                                        n.rej = mean(value))
# excess = merge( scen.params, excess )
# excess$group = paste( "X-Y correlation: ", excess$rho.XY )  # for plotting joy
# 
# # add horizontal stagger for visible error bars in plot
# buffer = 0.02
# excess$rho.YY[ excess$alpha == 0.01 ] = excess$rho.YY[ excess$alpha == 0.01 ] + buffer
# excess$rho.YY[ excess$alpha == 0.05 ] = excess$rho.YY[ excess$alpha == 0.05 ] + 2 * buffer
# 
# 
# 
# ######## CI limits for FCR (crappy) method ######## 
# # summary dataset of bootstrap CIs
# ci.fcr = tidy %>% group_by( scen, alpha ) %>% filter( stat == "bt.lo" & bt.type == "fcr" ) %>% summarise( bt.lo = mean(value) )
# ci.fcr = merge( scen.params, ci.fcr )
# ci.fcr$group = paste( "X-Y correlation: ", ci.fcr$rho.XY )  # for plotting joy
# 
# hi = tidy %>% group_by( scen, alpha ) %>% filter( stat == "bt.hi" & bt.type == "fcr" ) %>% summarise( bt.hi = mean(value) )
# ci.fcr$bt.hi = hi$bt.hi
# 
# # add horizontal stagger for visible error bars in plot
# buffer = 0.02
# ci.fcr$rho.YY[ ci.fcr$alpha == 0.01 ] = ci.fcr$rho.YY[ ci.fcr$alpha == 0.01 ] + buffer
# ci.fcr$rho.YY[ ci.fcr$alpha == 0.05 ] = ci.fcr$rho.YY[ ci.fcr$alpha == 0.05 ] + 2 * buffer
# 
# 
# 
# ######## CI limits under the null for residual (good) method ######## 
# # summary dataset of bootstrap CIs
# ci.r = tidy %>% group_by( scen, alpha ) %>% filter( stat == "bt.lo" & bt.type == "resid" ) %>% summarise( bt.lo = mean(value) )
# ci.r = merge( scen.params, ci.r )
# ci.r$group = paste( "X-Y correlation: ", ci.r$rho.XY )  # for plotting joy
# 
# hi = tidy %>% group_by( scen, alpha ) %>% filter( stat == "bt.hi" & bt.type == "resid" ) %>% summarise( bt.hi = mean(value) )
# ci.r$bt.hi = hi$bt.hi
# 
# # add horizontal stagger for visible error bars in plot
# buffer = 0.02
# ci.r$rho.YY[ ci.r$alpha == 0.01 ] = ci.r$rho.YY[ ci.r$alpha == 0.01 ] + buffer
# ci.r$rho.YY[ ci.r$alpha == 0.05 ] = ci.r$rho.YY[ ci.r$alpha == 0.05 ] + 2 * buffer
# 
# 
# 
# 
# 
# ########################### PLOT 2: CRAPPY CONFIDENCE INTERVAL (FCR) ###########################
# 
# # X-axis: Strength of YY correlation
# # Y-axis: Power to reject joint null
# # Panels: Strength of XY correlation
# # Lines: Different alpha levels for our method and a single line for naive Bonferroni
# 
# 
# 
# library(ggplot2)
# y.max = 100
# y.breaks = seq(0, y.max, 10)
# 
# colors = colors[1:3]
# legend.labs = c("0.0005", "0.01",
#                 "0.05" )
# 
# ggplot( ) +
#   geom_point( data=excess[ excess$bt.type == "fcr", ], aes(x=rho.YY, y=excess.hits, shape=alpha, color=alpha ), size=3 ) +
#   
#   geom_errorbar( data = ci.fcr, aes( x=rho.YY, ymin = bt.lo, ymax = bt.hi, color=alpha ), width=0.02 ) +
# 
#   geom_line( data=excess[ excess$bt.type == "fcr", ], aes(x=rho.YY, y=excess.hits, shape=alpha, color=alpha ) ) + 
#   theme_bw() + facet_wrap(~ group ) +
#   ylab("Excess hits") +
#   scale_color_manual( name="Alpha", values = colors, labels = legend.labs ) +
#   scale_shape_manual( name = "Alpha", values = c(20,17,18), labels = legend.labs ) +
#   scale_y_continuous( limits = c(0,y.max), breaks = y.breaks ) +
#   xlab( "Correlation between each pair of Ys" ) +
#   ggtitle("Power of bootstrapped hypothesis test of joint null")
# 
# 
# # BOOKMARK: FIX CIS BECAUSE THEY DON'T INCLUDE THE VALUE, BUT MAYBE BECAUSE OF FEW SIM REPS
# # ACTUALLY, THIS IS THE VERY ISSUE. THE FCR RESAMPLES REJECT MORE OFTEN THAN THE TRUTH. 
# 
# # scenarios where boot ci doesn't include truth: d, f, h (light blue line in middle plot with rhoYY=0)
# 
# 
# ggsave( filename = paste("plot2.png"),
#         plot=p2, path=NULL, width=10, height=10, units="in")
# 
# 
# 
# 
# ########################### PLOT 2: GOOD CONFIDENCE INTERVAL (FCR) ###########################
# 
# # X-axis: Strength of YY correlation
# # Y-axis: Power to reject joint null
# # Panels: Strength of XY correlation
# # Lines: Different alpha levels for our method and a single line for naive Bonferroni
# 
# 
# library(ggplot2)
# y.max = 100
# y.breaks = seq(0, y.max, 10)
# 
# colors = colors[1:3]
# legend.labs = c("0.0005", "0.01",
#                 "0.05" )
# 
# # JUST CIS UNDER THE NULL: MAKES SENSE THAT PANELS ARE IRRELEVANT
# ggplot( ) +
#   #geom_point( data=excess[ excess$bt.type == "resid", ], aes(x=rho.YY, y=excess.hits, shape=alpha, color=alpha ), size=3 ) +
#   
#   geom_errorbar( data = ci.r, aes( x=rho.YY, ymin = bt.lo, ymax = bt.hi, color=alpha ), width=0.02 ) +
#   
#   #geom_line( data=excess[ excess$bt.type == "resid", ], aes(x=rho.YY, y=excess.hits, shape=alpha, color=alpha ) ) + 
#   theme_bw() + facet_wrap(~ group ) +
#   
#   
#   ylab("Excess hits") +
#   scale_color_manual( name="Alpha", values = colors, labels = legend.labs ) +
#   scale_shape_manual( name = "Alpha", values = c(20,17,18), labels = legend.labs ) +
#   scale_y_continuous( limits = c(0,y.max), breaks = y.breaks ) +
#   xlab( "Correlation between each pair of Ys" ) +
#   ggtitle("Power of bootstrapped hypothesis test of joint null")
# 
# 
# # SUBTRACTING OBSERVED HITS - CI LIMITS
# 
# ci.r$lo.excess = excess[ excess$bt.type == "resid", ]$n.rej - ci.r$bt.hi
# ci.r$hi.excess = excess[ excess$bt.type == "resid", ]$n.rej - ci.r$bt.lo
# 
# ymin = -40
# ymax = 80
# y.breaks = seq(ymin,ymax,10)
# 
# ggplot( ) +
#   geom_point( data=excess[ excess$bt.type == "resid", ], aes(x=rho.YY, y=excess.hits, shape=alpha, color=alpha ), size=3 ) +
#   
#   geom_errorbar( data = ci.r, aes( x=rho.YY, ymin = lo.excess, ymax = hi.excess, color=alpha ), width=0.02 ) +
#   
#   geom_line( data=excess[ excess$bt.type == "resid", ], aes(x=rho.YY, y=excess.hits, shape=alpha, color=alpha ) ) + 
#   theme_bw() + facet_wrap(~ group ) +
#   ylab("Excess hits") +
#   scale_color_manual( name="Alpha", values = colors, labels = legend.labs ) +
#   scale_shape_manual( name = "Alpha", values = c(20,17,18), labels = legend.labs ) +
#   scale_y_continuous( limits = c(ymin,ymax), breaks = y.breaks ) +
#   xlab( "Correlation between each pair of Ys" ) +
#   ggtitle("Power of bootstrapped hypothesis test of joint null")
# 
# 
# 
# ########################### COMPARE HYPOTHESIS TEST WITH CI ###########################
# 
# 
# # pair of scenarios generated under HA
# mean( s$rej.jt_0.05[ s$scen=="j" ] )
# mean( s$rej.jt_0.05[ s$scen=="k" ] )
# 
# # scenarios generated under H0, so should have power = 0.05
# mean( s$rej.jt_0.05[ s$rho.XY == 0 & s$bt.type == "resid" ])  # MAKES SENSE
# mean( s$rej.jt_0.05[ s$rho.XY == 0 & s$bt.type == "fcr" ])  # WRONG
# 
# # sanity check
# mean( s$rej.jt_0.05[ s$scen == "s"] )
# 
# # THIS MAKES SENSE: AROUND 5
# mean( s$n.rej_0.05[ s$rho.XY == 0 & s$bt.type == "fcr" ] )
# 
# # THIS IS WRONG
# mean( s$jt.pval.0.05[ s$rho.XY == 0 & s$bt.type == "fcr" ] )




