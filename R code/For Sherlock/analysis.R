
########################### READ IN DATA ###########################

setwd("~/Desktop")
scen.params = read.csv("scen_params.csv")
names(scen.params)[ names(scen.params) == "scen.name" ] = "scen"

# just use a single file instead of stitched one
s = read.csv("stitched.csv", header=TRUE)
s = merge(s, scen.params, by = "scen" )

# what percent done are we?
nrow(s) / (1000 * nrow(scen.params))

# how many reps per scenario do we have?
table( s$scen )



########################### DATA PREP ###########################

# need 1 row per scenario-method combination
# methods are ours, Wstep, bonf.naive, and minP

library(reshape2)
library(tidyverse)

# names of joint rejection variables for all methods
( jt.rej.names = c( names(s)[ grep( "jt.rej.", names(s) ) ],
                    names(s)[ grep( "rej.jt", names(s) ) ] ) )

# MAKE SURE TO LIST THEM IN SAME ORDER  AS JT.REJ.NAMES
method.names = c("bonf.naive",
                 "holm",
                 "minP",
                 "Wstep",
                 "Romano",
                 "meanP",
                 "ours.0.01",
                 "ours.0.05")

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
pwr = lp %>% group_by(scen, method) %>%
  summarise( power = mean(jt.rej) )

pwr = merge(pwr, scen.params, by = "scen" )

# for plotting joy
pwr$group = paste( "X-Y correlation: ", pwr$rho.XY,
                   " for ",
                   pwr$prop.corr * 100,
                   "% of pairs", 
                   sep = "" )

# for more plotting joy
labels = c("NB", "H", "MP", "LP", "J1", "J5", "WS", "R")
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

# # sanity check
# # should have NAs for method-bt.type combinations that are impossible
# pwr %>% filter( method %in% c("Romano") ) %>%
#   filter( bt.type == "h0.resid" )
# 
# # should have NA power
# pwr %>% filter( method %in% c("minP", 
#                               "meanP",
#                               "ours.0.01", 
#                               "ours.0.05", 
#                               "Wstep") ) %>%
#   filter( bt.type == "ha.resid")


##### Save Results ##### 
pwr2 = pwr
setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Simulation results/2019-5-18 all scenarios Freedman resampling")
write.csv( pwr2, "results_pwr.csv")

lp2 = lp
write.csv( lp2, "results_pwr_long.csv")

lp2 = lp


########################### COMPARATIVE POWER PLOT ###########################

# X-axis: Strength of YY correlation
# Y-axis: Power to reject joint null
# Panels: Strength of XY correlation
# Lines: Different methods

# for slide presentations: simplify the plot 
# by removing intermediate effect sizes
# pwr = pwr[ !pwr$rho.XY %in% c(0.02, .1), ]

library(ggplot2)
x.breaks = seq(0, 0.5, 0.25)
y.breaks = seq(0, 1, 0.1)

colors = c( "#999999", "orange", "#009E73", "black", "#E69F00", "#D55E00", "black", "darkgreen" )


ggplot( data = pwr, aes( x = rho.YY, y = power,
                         color = method,
                         label = method ) ) +
  geom_text( aes( label = method.label) ) +
  theme_bw() +
  facet_wrap( ~group) +
  #facet_wrap(~ group, nrow = 2 ) +  # for changing rows/columns
  ylab("Power") +
  scale_color_manual( values = colors) +
  scale_x_continuous( limits = c( min(x.breaks), max(x.breaks) ), breaks = x.breaks ) +
  scale_y_continuous( limits = c( min(y.breaks), max(y.breaks) ), breaks = y.breaks ) +
  xlab( "Correlation between each pair of Ys" ) +
  ggtitle("Power of bootstrapped hypothesis test of joint null")


# PREVIOUS ATTEMPT TO HAVE LABELS IN GRAPH

# legend.labs = c("Naive Bonferroni",
#                 "Holm",
#                 "Westfall minP",
#                 "Mean log-P",
#                 "Ours (alpha = 0.01)",
#                 "Ours (alpha = 0.05)",
#                 "Romano",
#                 "Westfall step-down")


# # hackily use acronyms in the legend
# # https://stackoverflow.com/questions/49965758/change-geom-texts-default-a-legend-to-label-string-itself/49966057?noredirect=1#comment86951861_49966057
# oldK = GeomText$draw_key # to save for later
# 
# # define new key
# # if you manually add colours then add vector of colours 
# # instead of `scales::hue_pal()(length(var))`
# GeomText$draw_key <- function (data, params, size, 
#                                var = unique(pwr$method.label), 
#                                longvar = unique(pwr$method), 
#                                cols=scales::hue_pal()(length(var))) {
#   
#   #browser()
#   
#   # sort as ggplot sorts these alphanumerically / or levels of factor
#   txt <- if(is.factor(var)) levels(var) else sort(var)
#   txt <- txt[match(data$colour, cols)]
#   
#   textGrob(txt, 0.5, 0.5,  
#            just="center", 
#            gp = gpar(col = alpha(data$colour, data$alpha), 
#                      fontfamily = data$family, 
#                      fontface = data$fontface, 
#                      fontsize = data$size * .pt))
# }
# 
# 
# ggplot( data = pwr, aes( x = rho.YY, y = power,
#                          color = method,
#                          label = method ) ) +
#   geom_text( aes( label = method.label) ) +
#   theme_bw() +
#   facet_wrap( ~group) +
#   #facet_wrap(~ group, nrow = 2 ) +  # for changing rows/columns
#   ylab("Power") +
#   scale_x_continuous( limits = c( min(x.breaks), max(x.breaks) ), breaks = x.breaks ) +
#   scale_y_continuous( limits = c( min(y.breaks), max(y.breaks) ), breaks = y.breaks ) +
#   xlab( "Correlation between each pair of Ys" ) +
#   ggtitle("Power of bootstrapped hypothesis test of joint null")
# 
# # reset key
# GeomText$draw_key = oldK



########################### NULL CI PLOTS ###########################

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

# MAKE SURE TO LIST THEM IN SAME ORDER AS JT.REJ.NAMES
# I DON'T GET WHY COLS GET ARRANGED IN OPPOSITE ORDER!!!!!
method.names = c("ours.0.01", "ours.0.05")

# reshape wide to long
# https://stackoverflow.com/questions/12466493/reshaping-multiple-sets-of-measurement-columns-wide-format-into-single-columns
lc = reshape( s[ s$bt.type == "resid", ],
              varying = list( A = n.rej.names, B = n.rej.bt.names,
                              C = bt.lo.names, D = bt.hi.names ),
              v.names= c( "n.rej", "n.rej.bt", "bt.lo", "bt.hi" ),
              times = method.names,
              timevar = "method",  # name of the above variable
              direction="long" )

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


# add horizontal stagger for visible error bars in plot
ci2 = ci
buffer = 0.02
ci2$rho.YY[ ci2$method == "ours.0.05" ] = ci2$rho.YY[ ci2$method == "ours.0.05" ] + buffer

##### Make Plot #####

# X-axis: Strength of YY correlation
# Y-axis: Power to reject joint null
# Panels: Strength of XY correlation
# Lines: Different methods

library(ggplot2)
y.breaks = seq(0, 1, 0.1)


colors = c( "#E69F00", "#D55E00" )
legend.labs = c( "J1 (alpha = 0.01)", "J5 (alpha = 0.05)" )

ggplot( data = ci2 ) +
  # bootstrap results
  geom_point( aes( x = rho.YY, y = n.rej.bt.mn, color = method ), size=2.5 ) +
  #geom_line( aes( x = rho.YY, y = n.rej.bt.mn, color = method ), size=1.1 ) +
  geom_errorbar( aes( x=rho.YY, ymin = bt.lo.mn,
                      ymax = bt.hi.mn, color = method ), width=0.02, size=1.05 ) +
  
  # original dataset results
  geom_point( aes( x = rho.YY, y = n.rej.mn, shape="the shape" ), color = "black", size=3.5 ) +
  scale_shape_manual( values = c('the shape' = 4),
                      name = "Original dataset", guide = 'legend', labels = c("Mean rejections")) +
  
  
  theme_bw() +
  #  facet_wrap(~ group ) +
  facet_wrap(~ group, nrow = 2 ) +
  ylab("Average null CIs") +
  scale_color_manual( name="Joint test", values = colors, labels = legend.labs ) +
  
  #scale_y_continuous( limits = c(0,1), breaks = y.breaks ) +
  xlab( "Correlation between each pair of Ys" ) +
  ggtitle("Average CI limits and rejections in resamples and originals")


# ggsave( filename = paste("null_ci_plot.png"),
#         plot=p2, path=NULL, width=10, height=8, units="in")


# shorter version for slide talk
# run this, then re-run the above
ci2 = ci2[ ci2$rho.XY %in% c(0, 0.05, 0.1), ]
