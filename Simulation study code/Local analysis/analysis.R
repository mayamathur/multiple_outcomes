

########################### PRELIMINARIES ###########################

library(here)
library(data.table)
setwd(here())
source("helper_analysis.R")

sim.set = "old"

# previous sims with nY=40
# data.dir.old = "~/Dropbox/Personal computer/Harvard/THESIS/Thesis paper #2 (MO)/Linked to OSF (MO)/Simulation results in paper/2023-03-13 Merge previous sims and higher-W sims/Data from Sherlock/nY=40"
data.dir.old = "~/Dropbox/Personal computer/Harvard/THESIS/Thesis paper #2 (MO)/Linked to OSF (MO)/Simulation results in paper/2023-03-13 Merge previous sims and higher-W sims/Data from Sherlock/nY=40 from 2018-8-18"

# new sims with nY=200
data.dir.new = "~/Dropbox/Personal computer/Harvard/THESIS/Thesis paper #2 (MO)/Linked to OSF (MO)/Simulation results in paper/2023-03-13 Merge previous sims and higher-W sims/Data from Sherlock/Results from R"

results.dir = "~/Dropbox/Personal computer/Harvard/THESIS/Thesis paper #2 (MO)/Linked to OSF (MO)/Simulation results in paper/2023-03-13 Merge previous sims and higher-W sims/Results from R"


########################### READ IN PREPPED DATA ###########################


# these were already prepped back in 2018 and I no longer have the stitched results
# just need to tweak the methods labels for new plot style

if ( sim.set == "old" ) {

  setwd(data.dir.old)

  n.trues = fread("results_ntrues.csv")
  #lt = fread("results_ntrues_long.csv")
  pwr = fread("results_pwr.csv")
  #lp2 = fread("results_pwr_long.csv")
  ci = fread("results_null_interval.csv")

  
  n.trues = make_methods_labels(n.trues); table(n.trues$method.label, useNA = "ifany")
  pwr = make_methods_labels(pwr); table(pwr$method.label, useNA = "ifany")
  
  n.trues = order_panel_labels(n.trues); levels(n.trues$group)
  pwr = order_panel_labels(pwr); levels(pwr$group)

}

# # read in scenario parameters
# scen.params = read.csv("scen_params.csv")
# names(scen.params)[ names(scen.params) == "scen.name" ] = "scen"



########################### SHARED FOR MULTIPLE PLOTS ###########################

# order needs to match correct.order in helper_analysis.R/order_methods_labels
#  or else plot legend order will look silly
.colors = c(Bonferroni = "#016301",
            Holm = "#00cc00",
            minP = "#999999",
            Romano = "black",
            Wstep = "#a834eb",
            
            `Global (alpha=0.01)` = "#ff9900",
            `Global (alpha=0.05)` = "red")




myFillScale = scale_fill_manual(name = "",
                                values = .colors)


########################### N TRUE EFFECTS PLOTS ###########################

# for main text: simplify the plot 
# by removing intermediate effect sizes
n.trues.short = n.trues[ !n.trues$rho.XY %in% c(0.1, 0.15), ]

# set global variables needed for plotting fn
x.breaks = c(0, 0.1, 0.3, 0.6)

y.breaks = seq(0, 40, 10)
p5 = ntrues_plot(n.trues, benchmark.line = TRUE); p5

y.breaks = seq(0, 10, 2)
p6 = ntrues_plot(n.trues.short, benchmark.line = FALSE); p6

##### Save Results #####
setwd(results.dir)
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


# save different versions of plot (some with only a few scenarios)
p1 = pwr_plot(pwr); p1  # for Appendix 
p2 = pwr_plot(pwr.short); p2  # for main text

width = 8
square.size = 8/3 # divide by number of cols
height = square.size*5  # multiply by number of rows
name = "joint_test_full.png"
ggsave( filename = paste(name),
        plot=p1, path=results.dir, width=width, height=height, units="in")

width = 8
square.size = 8/3 # divide by number of cols
height = square.size*3  # multiply by number of rows
name = "joint_test_short.png"
ggsave( filename = paste(name),
        plot=p2, path=results.dir, width=width, height=height, units="in")



########################### NULL INTERVAL PLOTS ###########################

# shorter version for main text
# run this, then re-run the above
ci.short = ci[ ci$rho.XY %in% c(0, 0.05, 0.1, 0.15), ]

# even shorter for slide talk
ci.super.short = ci[ ci$rho.XY %in% c(0, 0.1, 0.15), ]

##### Make Plot #####

# X-axis: Strength of YY correlation
# Y-axis: Power to reject joint null
# Panels: Strength of XY correlation
# Lines: Different methods

library(ggplot2)
y.breaks = seq(0, 1, 0.1)

colors = c( "#E69F00", "#D55E00" )
legend.labs = c( "G1 (alpha = 0.01)", "G5 (alpha = 0.05)" )

p3 = ci_plot(ci); p3
p4 = ci_plot(ci.short); p4
p5 = ci_plot(ci.super.short); p5

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

width = 8
square.size = 8/3  # divide by number of cols
height = square.size*3  # multiply by number of rows
ggsave( filename = paste("null_ci_super_short.png"),
        plot=p5, path=NULL, width=width, height=height, units="in")



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
              round( n.rej.mean - bt.hi$bt.hi.mn[ bt.hi$rho.YY == 0.6 ], 1 ),
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


