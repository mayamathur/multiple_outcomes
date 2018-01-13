
# remove all variables
#rm(list=ls(all=TRUE))

#################### SET SCENARIO PARAMETERS #################### 
n = 50
alpha = c( 0.05 )
nX = 1
nY = 5000
rho.XX = 0.1
rho.YY = c( 0.5 )
rho.XY = 0  # null

reps = 1
boot.reps = 1

# matrix of scenario parameters
scen.params = expand.grid( n, alpha, nX, nY, rho.XX, rho.YY, rho.XY )
names(scen.params) = c( "n", "alpha", "nX", "nY", "rho.XX", "rho.YY", "rho.XY" )

# name the scenarios
 scen.params$scen.name = c(letters, LETTERS)[ 1:dim(scen.params)[1] ]
#scen.params$scen.name = c(letters, LETTERS)[ 7: ( dim(scen.params)[1] + 6) ]
n.scen = length(scen.params[,1])



#################### RUN SIMULATION #################### 
setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2/R code/For Sherlock")
source("functions.R")

path = "~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2/Simulation results"
setwd(path)

# loop through the scenarios and run each one
system.time(
for (s in scen.params$scen.name[1]) {  # REMOVE THE 1
    rs = sim_scenario( .scen=s, .reps=reps, .boot.reps = boot.reps)
    rs$scen.name = s
    rs2 = merge(scen.params, rs)
    write.csv( rs2, paste(Sys.Date(), "_scen_", s, ".csv", sep="") )
}
)

# stitch results files
s = stitch_files(.results.singles.path = path,
                      .name.prefix="_scen", .stitch.file.name="stitched_results.csv")

# remove annoying rows

# organize it and remove annoying rows
s = s[ order(s$alpha, s$rho.YY), c( names(scen.params), "n.rejected", "lo.bt", "hi.bt" ) ]

# higher correlation between Ys => wider CI
# lower alpha => narrower CI

#################### SUMMARIZE RESULTS ####################


