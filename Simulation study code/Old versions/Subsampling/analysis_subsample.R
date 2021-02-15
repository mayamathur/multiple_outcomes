
########################### READ IN DATA ###########################

# # LOCAL TEST
# setwd("~/Desktop")
# scen.params = read.csv("scen_params.csv")
# names(scen.params)[ names(scen.params) == "scen.name" ] = "scen"
# 
# # just use a single file instead of stitched one
# s = read.csv("stitched.csv", header=TRUE); dim(s)



# fix stitching because I was stupid and didn't put scenario name in results files...





setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/git_multiple_outcomes/R code/For Sherlock")
source("functions.R")

# stitch Scenario A
sa = stitch_files(.results.singles.path = "~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Simulation results/2018-1-22/long/Scenario a",
                 .results.stitched.write.path = "~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Simulation results/2018-1-22/long/Scenario a",
                 .name.prefix = "long",
                 .stitch.file.name="stitched_scen_a.csv")
sa$scen = "a"


# stitch Scenario B
sb = stitch_files(.results.singles.path = "~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Simulation results/2018-1-22/long/Scenario b",
                 .results.stitched.write.path = "~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Simulation results/2018-1-22/long/Scenario b",
                 .name.prefix = "long",
                 .stitch.file.name="stitched_scen_b.csv")
sb$scen = "b"

s = rbind(sa, sb)

setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Simulation results/2018-1-22")
write.csv( s, "stitched_all.csv" )


########################### BASIC ANALYSES ###########################

# TEMPORARY!!!
setwd("~/Desktop")
s = read.csv("stitched.csv")

# runtimes for next round
rep.time = quantile(s$rep.time, 0.98)
rep.time / (60^2)

# how good are rate estimates?
summary(s$rate)
# almost perfect

# coverage with estimated rate vs. real rate
mean(s$covers.raw); mean(s$covers); mean(s$covers.correct)


# recompute CIs exactly as in lab handout (page 10)
crit.vals2 = cbind( s$crit.hi, s$crit.lo )
ci = s$excess - crit.vals2
s$ci2.lo = ci[,1]
s$ci2.hi = ci[,2]
s$covers2 = ci[,1] <= mean(s$n.rej) & ci[,2] >= s$n.rej

table( s$excess < ci[,1] )


# makes sense: the ones with good coverage are those with more extreme observed excesses
library(ggplot2)
ggplot( data = s, aes(x=covers, y=abs(excess) ) ) +
  geom_boxplot() +
  theme_bw()

# try removing the ones where we observed FEWER hits than expected
temp = s[ s$excess >= 0, ]; dim(temp)

# what if we had guaranteed coverage for those trials?
covers.imaginary = s$covers
covers.imaginary[ s$excess <= 0 ] = 1
mean(covers.imaginary)
# that would bring us to 87.5%

# symmetry of confidence intervals
plot( abs(s$ci.hi.raw - s$excess), abs(s$ci.lo.raw - s$excess) )
plot( abs(s$ci.hi.correct - s$excess), abs(s$ci.lo.correct - s$excess) )

# plot random sample of the confidence intervals
n.small = 50
set.seed(451)
temp = s[ sample( 1:nrow(s), size = n.small), ]
temp$yval = 1:nrow(temp)
colors = c("red", "black")



# CHECK: HOW COME CENTERED CIS SOMETIMES DON'T INCLUDE OBSERVED VALUE?
# AND OFTEN THE CIS ARE ON THE VERY EDGE OF THE OBSERVED VALUE
temp[ temp$excess < temp$ci.lo, ]


# compare CIs
library(ggplot2)
ggplot(temp) + 
  theme_bw() + 
  aes( 
    
    # x = as.numeric( excess ),
    # xmin = as.numeric( ci2.lo ), xmax = as.numeric( ci2.hi ),
    # color = covers2,
    
    x = as.numeric( excess ),
    xmin = as.numeric( ci.lo.raw ), xmax = as.numeric( ci.hi.raw ),
    #color = covers.raw,
    
    #  x = as.numeric( excess ),
    # xmin = as.numeric( ci.lo ), xmax = as.numeric( ci.hi ),
    # color = covers,
    
    y = as.numeric(yval)
  ) + 
  #scale_color_manual(values=colors) +
  geom_point( ) + 
  geom_errorbarh(height = 0.3, size=0.8 ) + 
  geom_vline(xintercept = mean(excess) ) +
  scale_x_continuous( limits = c( min( c( temp$ci.lo.raw, temp$excess ) ),
                                  max( c( temp$ci.hi.raw, temp$excess ) ) ) )



# compare CIs on rejections scale, not excess hits
library(ggplot2)
ggplot(temp) + 
  theme_bw() + 
  aes( 
    
    # x = as.numeric( excess + expected ),
    # xmin = as.numeric( ci2.lo + expected ), xmax = as.numeric( ci2.hi + expected ),
    # color = covers2,
     
    x = as.numeric( excess + expected ),
    xmin = as.numeric( ci.lo.raw + expected ), xmax = as.numeric( ci.hi.raw + expected ),
    color = covers.raw,
    
    #  x = as.numeric( excess ),
    # xmin = as.numeric( ci.lo ), xmax = as.numeric( ci.hi ),
    # color = covers,
    
    y = as.numeric(yval)
  ) + 
  scale_color_manual(values=colors) +
  geom_point( ) + 
  geom_errorbarh(height = 0.3, size=0.8 ) + 
  geom_vline(xintercept = 0.05 * 30) +
  scale_x_continuous( limits = c( min(temp$ci.lo), max(temp$ci.hi) ) )





