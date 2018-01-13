
path = "/share/PI/manishad/multTest"
setwd(path)

source("functions.R")
s = stitch_files(.results.singles.path = "/share/PI/manishad/multTest/sim_results",
             .name.prefix = "results",
             .stitch.file.name="stitched_raw.csv")

# merge in scenario parameters
scen.params = read.csv("scen_params.csv")
names(s)[ names(s) == "scen" ] = "scen.name"
s = merge( scen.params, s )

# get parameters (ASSUMES ONLY 1 SCENARIO)
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

# keep only 1 row per scenario (for first boot iterate)
s2 = data.frame(st)
s2 = s2[ s2$bt.iterate==1, ]

# write results
write.csv( s2, "stitched_prepped.csv" )

# zip the entire results folder
setwd("/share/PI/manishad/multTest")
system( paste("zip -r sim_results.zip sim_results") )



