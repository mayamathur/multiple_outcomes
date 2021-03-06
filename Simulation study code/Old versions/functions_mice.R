


######################## FNS FOR WESTFALL's SINGLE-STEP ########################

# returns minP-adjusted p-values (single-step)

# p = original p-values
# p.bt = bootstrapped p-values (vector) - an m X B matrix
adjust_minP = function( p, p.bt ) {
  
  n.boot = ncol(p.bt)
  
  # keep only minimum p-value in each resample
  minP.bt = apply( p.bt, MARGIN = 2, FUN = min )
  
  # for each element of p, get the proportion of resamples
  #  whose minP was less than the present p-value
  p.adj = unlist( lapply( p, FUN = function(x) sum( minP.bt <= x ) / n.boot ) )
  
  return(p.adj)
}

# # test
# B = 200
# n.tests = 10
# p.bt = matrix( runif(B*n.tests, 0, 1), nrow = n.tests)
# p = runif( n.tests, 0, .1)
# 
# p.adj = adjust_minP( p, p.bt )
# plot(p,p.adj)


######################## FNS FOR WESTFALL's STEP-DOWN ########################

# Westfall textbook, pages 66-67
adj_Wstep = function( p, p.bt ) {
  
  # # TEST ONLY
  # p = structure(c(0.636112475277457, 0.116408752390887, 0.260603159221029,
  #                 0.575086408582202, 0.625789849782125), .Dim = c(5L, 1L))
  # p.bt = structure(c(0.728140113074014, 0.828557135138776, 0.728464927503583,
  #                    0.376518353153738, 0.241363541131996, 0.415330227654616, 0.75686321503498,
  #                    0.865494617532842, 0.839819273240726, 0.540707408457488, 0.149482739547785,
  #                    0.506274953008223, 0.122605643925883, 0.747018941160177, 0.168076477020114,
  #                    0.0582566955778811, 0.284459435324328, 0.561050485854955, 0.13839444320949,
  #                    0.641335055023296, 0.195313599009888, 0.174692123827015, 0.982035588216878,
  #                    0.713525352129964, 0.548770486028363), .Dim = c(5L, 5L), .Dimnames = list(
  #                      NULL, c("result.1", "result.2", "result.3", "result.4", "result.5"
  #                      )))
  
  # if using read-in or simulated dat
  # p = pvals
  
  
  # attach indices to original p-values
  # to keep track of their original order
  p.dat = data.frame( ind = 1:length(p), p )
  
  # sort original p-values
  p.dat = p.dat[ order( p.dat$p, decreasing = FALSE ), ]
  
  # in this order, ind is now the same as Westfall's r_i on pg 67:
  #  e.g., r_1 is the location in the original list of the 
  #  smallest p-value
  r.ind = p.dat$ind
  
  # critical values
  # they get smaller and smaller, as do the sorted p-vals
  # pass one resample's bootstrapped p-values to get_crit
  crit.mat = apply( p.bt, MARGIN = 2,
                    FUN = function(x) get_crit( p.dat, x) )
  
  # for each column of crit values (i.e., each resample), 
  # see if the sorted p-value is greater than its crit value
  less = apply( crit.mat, MARGIN = 2, 
                FUN = function(x) x <= p.dat$p )
  
  # adjusted p-values are means of the above
  p.adj.1 = apply( less, MARGIN = 1, mean )
  
  # enforce monotonicity
  p.adj.2 = rep( NA, length(p.adj.1) )
  
  for ( i in 1:length(p.adj.1) ) {
    if (i == 1) p.adj.2[i] = p.adj.1[1]
    else p.adj.2[i] = max( p.adj.2[i-1], p.adj.1[i] )
  }
  p.dat$p.adj = p.adj.2
  
  # put back in order of original p-values
  p.dat = p.dat[ order( p.dat$ind, decreasing = FALSE ), ]
  
  return( p.dat$p.adj )
  #plot( p.dat$p, p.dat$p.adj )
}

# p.adj.Wstep = adj_Wstep(pvals, p.bt)
# plot( p, p.adj.Wstep )

get_crit = function( p.dat, col.p ) {
  
  # sort bootstrapped p-values according to original ones
  col.p.sort = col.p[ p.dat$ind ]
  
  # # OLD VERSION (wrong)
  # qstar = rep( NA, length(col.p.sort) )
  # for ( i in 1:length(col.p.sort) ) {
  #   if (i == 1) qstar[i] = col.p.sort[1]
  #   else qstar[i] = min( qstar[i-1], col.p.sort[i] )
  # }
  
  qstar = rep( NA, length(col.p.sort) )

  k = length(col.p.sort)
  for ( i in k:1 ) {  # count backwards
    if (i == k) qstar[i] = col.p.sort[k]
    else qstar[i] = min( qstar[i+1], col.p.sort[i] )
  }
  
  return(qstar)
}




########################### FN: RETURN CORRELATION BETWEEN TWO ARBITRARY CELLS ###########################

# eventually could make this an argument in the scen_params matrix somehow so that it can be varied
# assume that X1 is the covariate of interest
# assume that none of the other covariates are associated with any outcomes

# .half: Should only the last half the outcomes have the specified correlation?
#  (if FALSE, correlation matrix is exchangeable; otherwise first half are 0 and 
#  second half are rho.XY)

cell_corr = function( vname.1, vname.2, .rho.XX, .rho.YY, .rho.XY,
                      .nY, .half = FALSE ) {
  
  # use grep to figure out if variables are covariates or outcomes
  if ( length( grep("X", vname.1 ) ) == 1 ) {
    vtype.1 = "covariate"
  } else if ( length( grep("Y", vname.1 ) ) == 1 ) {
    vtype.1 = "outcome"
  }
  
  if ( length( grep("X", vname.2 ) ) == 1 ) { 
    vtype.2 = "covariate"
  } else if ( length( grep("Y", vname.2 ) ) == 1 ) {
    vtype.2 = "outcome"
  }
  
  # case 1: diagonal entry
  if ( vname.1 == vname.2 ) return(1)
  
  # case 2: both are covariates
  # fixed correlation between covariates
  if ( vtype.1 == "covariate" & vtype.2 == "covariate" ) return( .rho.XX )
  
  # case 3: both are outcomes
  if ( vtype.1 == "outcome" & vtype.2 == "outcome" ) return( .rho.YY )
  
  # browser()

  # case 4: one is a covariate and one is an outcome
  if ( vtype.1 != vtype.2 ) {
    # check if this is the covariate of interest
    # equal correlation between X1 and all outcomes
    # all other covariates have correlation 0 with outcome
    if( "X1" %in% c( vname.1, vname.2 ) ) {
      # exchangeable case
      if ( !.half ) return( .rho.XY )
      
      # "half are correlated" case
      if (.half) {
        # find the one that is the outcome
        outcome.name = ifelse( vtype.1 == "outcome", vname.1, vname.2 )
        
        # extract its number (4 for "Y4")
        num = substring( outcome.name, first = 2 )
        
        # see if its number is greater than halfway or not
        return( ifelse( num > .nY / 2, .rho.XY, 0 ) )
        # bookmark
      }
    }
    
    # if we have multiple covariates and this isn't the one of interest
    else return(0)
  }
}

# test drive
# cell_corr( vname.1 = "X1", vname.2 = "Y3", .rho.XX = 0,
#            .rho.YY = 0.04, .rho.X1.Y = 0.8, .rho.Xj.Y = .1 )


########################### FN: CREATE CORRELATION MATRIX ###########################

# .nX: number of covariates including the one of interest
# .nY: number of outcomes

# if correlation matrix isn't positive definite, try reducing some of the correlations

make_corr_mat = function( .nX, .nY, .rho.XX, .rho.YY, .rho.XY, .half = FALSE ) {
  
  nVar = .nX + .nY
  
  # name the variables
  X.names = paste0( "X", 1 : .nX )
  Y.names = paste0( "Y", 1 : .nY )
  vnames = c( X.names, Y.names )
  
  cor = as.data.frame( matrix( NA, nrow = nVar, ncol = nVar ) )
  names(cor) = vnames
  row.names(cor) = vnames
   
  # populate each cell 
  for ( r in 1:dim(cor)[1] ) {
    for ( c in 1:dim(cor)[2] ) {
      cor[ r, c ] = cell_corr( vnames[r], vnames[c], .rho.XX, .rho.YY, .rho.XY, .nY, .half )
    }
  }
  
  # check if positive definite
  library(matrixcalc)
  if( ! is.positive.definite( as.matrix(cor) ) ) stop( "Correlation matrix not positive definite")
  
  return( cor )  # this is still a data.frame in order to keep names
}

# # test: exchangeable
# make_corr_mat( .nX = 1, .nY = 6, .rho.XX = 0, .rho.YY = .1, .rho.XY = -.1, .half = FALSE )
# 
# # test: half are correlated
# make_corr_mat( .nX = 1, .nY = 6, .rho.XX = 0, .rho.YY = .1, .rho.XY = -.1, .half = TRUE )

########################### FN: SIMULATE 1 DATASET ###########################

# simulate 1 dataset with correlated covariates
# then simulate 

sim_data = function( .n, .cor,
                     .Ytype = "normal",
                     .nX = NULL, .nY = NULL ) # only needed for binary Ys
  {

  vnames = names( .cor )
  
  # names for variables
  X.names = paste0( "X", 1 : .nX )
  Y.names = paste0( "Y", 1 : .nY )
  
  # simulate the dataset
  if ( .Ytype == "normal" ) {
    # everything is a standard Normal
    library(mvtnorm)
    d = as.data.frame( rmvnorm( n = .n, mean = rep( 0, dim( .cor )[1] ), sigma = as.matrix( .cor ) ) )
    vnames = c( X.names, Y.names )
  }

  # simulate the dataset
  else if ( .Ytype == "binary" ) {
    library(BinNor)
    sigma.star = compute.sigma.star( no.bin = .nY, no.nor = .nX, prop.vec.bin = rep(0.5, .nY),
                                   corr.mat = as.matrix(.cor) )
    
    d = as.data.frame( 
      jointly.generate.binary.normal( no.rows = .n, no.bin = .nY, no.nor = .nX,
                                      prop.vec.bin = rep(0.5, .nY),
                                      mean.vec.nor = 0,
                                      var.nor = 1, 
                                      sigma_star = sigma.star$sigma_star,
                                      continue.with.warning = TRUE )
    )
    
    # respect ordering from BinNor
    # opposite of mvrnorm case
    vnames = c( Y.names, X.names )
  }
  
  names(d) = vnames
  
  # return the dataset
  return(d)
}

# test drive
# cor = make_corr_mat(3, 40)
# d = sim_data( .n = 5000, .cor = cor )
# head( cor(d) ); head( cor )



########################### FN: FIT REGRESSION MODEL FOR A PARTICULAR OUTCOME ###########################

# fit regression model for a single outcome Y regressed on all Xs
# return p-value for the exposure of interest (X1)

fit_model = function( Y.name, .dat, .resid, .sigma, .tval, .intercept ) {

  # extract names
  X.names = names( .dat )[ grep( "X", names( .dat ) ) ]
  
  # for LHS of regression model
  if ( length(X.names) > 1 ) X.string = paste0( X.names, sep=" + " )
  else X.string = X.names 
  
  # strings for regression model
  RHS = paste(X.names, collapse=" + ")
  formula = paste( c( Y.name, RHS ), collapse = " ~ ")
  
  # ~~~ NEW STUFF
  # is Y binary? Should work for either 0/1 coding or factor coding
  n.levels = length( unique( .dat[[Y.name]] ) )
  
  # fit appropriate model
  if ( n.levels > 2 ) m = lm( eval( parse(text=formula) ), data=.dat )
  else if ( n.levels == 2 ) m = glm( eval( parse(text=formula) ), data=.dat, family=binomial )
  
  # extract p-value for exposure of interest (X1)
  pval = summary(m)$coefficients[ "X1", 4 ]
  
  if ( ! .resid ) resid.return = NA
  else resid.return = residuals(m)
    
  if ( ! .sigma ) sigma.return = NA
  else sigma.return = summary(m)$sigma
  
  if ( ! .intercept ) intercept.return = NA
  else intercept.return = coef(m)[["(Intercept)"]]
  
  # extract test stat for this test
  # for use with Romano's code
  if ( ! .tval ) tval.return = NA
  else tval.return = summary(m)$coefficients[ "X1", 3 ]
  
  return( list( pval = pval,
                resid = resid.return,
                sigma = sigma.return, 
                intercept = intercept.return,
                tval = tval.return ) )
  
}

# test drive
# cor = make_corr_mat(3, 40)
# d = sim_data( .n = 5000, .cor = cor )
# fit_model( "Y2", .dat = d )




########################### FN: GIVEN DATASET, RETURN STATS ###########################
# .resid: should it return dataset with residuals (for regenerating residuals)?
# . sigma: should it return fitted error SD (for regenerating residuals)?
# .intercept: should it return fitted intercept (for regenerating residuals)?

dataset_result = function( .dat, .alpha,
                           .imputed = FALSE,
                           .resid = FALSE,
                           .sigma = FALSE,
                           .intercept = FALSE,
                           .tval = FALSE ) {
  

  # extract names of outcome variables
  X.names = names( .dat )[ grep( "X", names(.dat) ) ]
  Y.names = names( .dat )[ grep( "Y", names(.dat) ) ]
  
  # for each outcome, fit regression model
  # see if each has p < alpha for covariate of interest

  # this is a list of lists:
  #  length is equal to number of outcomes
  #  each entry is another list
  # with elements "pval" (scalar) and "resid" (length matches number of subjects)
  lists = lapply( X = Y.names,
                  FUN = function(y) fit_model( Y.name = y,
                                               .dat = .dat,
                                               .resid = .resid,
                                               .sigma = .sigma,
                                               .tval = .tval,
                                               .intercept = .intercept ) )
  
  # "flatten" the list of lists
  u = unlist(lists)
  pvals = as.vector( u[ names(u) == "pval" ] )

  # if we will need residuals for later bootstrapping
  if ( .resid ) {
    # names of object u are resid.1, resid.2, ..., hence use of grepl 
    mat = matrix( u[ grepl( "resid", names(u) ) ], byrow=FALSE, ncol=length(Y.names) ) 
    resid = as.data.frame(mat)
    names(resid) = Y.names
  }
  
  # if we are returning the estimated error SD
  if ( .sigma ) sigmas = as.vector( u[ names(u) == "sigma" ] )
  
  # if we are returning the estimated intercept
  if ( .intercept ) intercepts = as.vector( u[ names(u) == "intercept" ] )
  
  # if we are returning the test stats for use with Romano
  if ( .tval ) tvals = as.vector( u[ names(u) == "tval" ] )
  
  ##### For looking at distribution of p-values within a single sample #####
  # first call browser to stop within a given simulation
#   browser()
#   setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2/Simulation results/Average sizes")
#   write.csv(pvals, "pvals.csv")
  ######################
  
  # returns vector for number of rejections at each alpha level
  # length should match length of .alpha
  n.reject = vapply( X = .alpha, FUN = function(a) sum( pvals < a ), FUN.VALUE=-99 )

  # dataframe "rej" has 1 row and a column for each value of .alpha
  # col names are "0.01" and "0.05" if those are the alpha levels
  # values are the number of rejections at that .alpha level
  rej = as.data.frame( matrix( n.reject, nrow=1 ) )
  names(rej) = as.character(.alpha)

  
  if ( ! .resid ) resid.return = NA
  else resid.return = resid
  
  if ( ! .sigma ) sigmas.return = NA
  else sigmas.return = sigmas
  
  if ( ! .intercept ) intercept.return = NA
  else intercept.return = intercepts
  
  if ( ! .tval ) tvals.return = NA
  else tvals.return = tvals
  
  return( list( rej = rej,
                resid = resid.return,
                sigmas = sigmas.return,
                intercepts = intercept.return,
                tvals = tvals.return,
                pvals = pvals ) )
}



########################### FN: STITCH RESULTS FILES ###########################

# given a folder path for results and a common beginning of file name of results files
#   written by separate workers in parallel, stitch results files together into a
#   single csv.

stitch_files = function(.results.singles.path, .results.stitched.write.path=.results.singles.path,
                        .name.prefix, .stitch.file.name="stitched_model_fit_results.csv") {

  # get list of all files in folder
  all.files = list.files(.results.singles.path, full.names=TRUE)
  
  # we only want the ones whose name includes .name.prefix
  keepers = all.files[ grep( .name.prefix, all.files ) ]
  
  # grab variable names from first file
  names = names( read.csv(keepers[1] )[-1] )
  
  # initialize stitched dataframe
  s = as.data.frame( matrix(nrow=1, ncol=length(names)) )
  names(s) = names
  
  # stitch the files
  for ( i in 1:length(keepers) ) {
    new.chunk = read.csv(keepers[i])[,-1]
    s = rbind(s, new.chunk)
  }
  
  s = s[-1,]  # delete annoying NA row
  write.csv(s, paste(.results.stitched.write.path, .stitch.file.name, sep="/") )
  return(s)
}


########################### SLURM FUNCTIONS ###########################


# DO NOT CHANGE THE INDENTATION IN THE BELOW OR ELSE SLURM 
#  WILL SILENTLY IGNORE THE BATCH COMMANDS DUE TO EXTRA WHITESPACE!!
sbatch_skeleton <- function() {
  return(
"#!/bin/bash
#################
#set a job name  
#SBATCH --job-name=JOBNAME
#################  
#a file for job output, you can check job progress
#SBATCH --output=OUTFILE
#################
# a file for errors from the job
#SBATCH --error=ERRORFILE
#################
#time you think you need; default is one hour
#SBATCH --time=JOBTIME
#################
#quality of service; think of it as job priority
#SBATCH --qos=QUALITY
#################
#submit to both owners and normal partition
#SBATCH -p normal,owners
#################
#number of nodes you are requesting
#SBATCH --nodes=NODENUMBER
#################
#memory per node; default is 4000 MB
#SBATCH --mem=MEMPERNODE
#you could use --mem-per-cpu; they mean what we are calling cores
#################
#get emailed about job BEGIN, END, and FAIL
#SBATCH --mail-type=MAILTYPE
#################
#who to send email to; please change to your email
#SBATCH  --mail-user=USER_EMAIL
#################
#task to run per node; each node has 16 cores
#SBATCH --ntasks=TASKS_PER_NODE
#################
#SBATCH --cpus-per-task=CPUS_PER_TASK
#now run normal batch commands
    
ml load R
srun R -f PATH_TO_R_SCRIPT ARGS_TO_R_SCRIPT")
}



generateSbatch <- function(sbatch_params, runfile_path = NA, run_now = F) {
  
  #sbatch_params is a data frame with the following columns
  #jobname: string, specifies name associated with job in SLURM queue
  #outfile: string, specifies the name of the output file generated by job
  #errorfile: string, specifies the name of the error file generated by job
  #jobtime: string in hh:mm:ss format, max (maybe soft) is 48:00:00 
  #specifies the amoung of time job resources should be allocated
  #jobs still running after this amount of time will be aborted
  #quality: kind of like priority, normal works
  #node_number, integer: the number of nodes (computers w/16 cpus each) to allocate 
  #mem_per_node, integer: RAM, in MB, to allocate to each node
  #mailtype, string: ALL, BEGIN, END, FAIL: what types of events should you be notified about via email
  #user_email string: email address: email address to send notifications
  #tasks_per_node: integer, number of tasks, you should probably use 1
  #cpus_per_task: integer, 1-16, number of cpus to use, corresponds to number of available cores per task
  #path_to_r_script: path to r script on sherlock
  #args_to_r_script: arguments to pass to r script on command line
  #write_path: where to write the sbatch file
  #server_sbatch_path: where sbatch files will be stored on sherlock
  #runfile_path is a string containing a path at which to write an R script that can be used to run
  #the batch files generated by this function. 
  #if NA, no runfile will be written
  #run_now is a boolean specifying whether batch files should be run as they are generated
  
  sbatches <- list()
  if (!is.na(runfile_path)) {
    outfile_lines <- c(paste0("# Generated on ",  Sys.time()))
  }
  for (sbatch in 1:nrow(sbatch_params) ) {
    gen_batch <- sbatch_skeleton()
    #set job name
    if (is.null(sbatch_params$jobname[sbatch])) { 
      gen_batch <- gsub("JOBNAME", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("JOBNAME", sbatch_params$jobname[sbatch], gen_batch) 
    }
    #set outfile name
    if (is.null(sbatch_params$outfile[sbatch])) { 
      gen_batch <- gsub("OUTFILE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("OUTFILE", sbatch_params$outfile[sbatch], gen_batch) 
    }
    #set errorfile name
    if (is.null(sbatch_params$errorfile[sbatch])) { 
      gen_batch <- gsub("ERRORFILE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("ERRORFILE", sbatch_params$errorfile[sbatch], gen_batch) 
    }
    #set jobtime
    if (is.null(sbatch_params$jobtime[sbatch])) { 
      gen_batch <- gsub("JOBTIME", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("JOBTIME", sbatch_params$jobtime[sbatch], gen_batch) 
    }
    #set quality
    if (is.null(sbatch_params$quality[sbatch])) { 
      gen_batch <- gsub("QUALITY", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("QUALITY", sbatch_params$quality[sbatch], gen_batch) 
    }
    #set number of nodes
    if (is.null(sbatch_params$node_number[sbatch])) { 
      gen_batch <- gsub("NODENUMBER", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("NODENUMBER", sbatch_params$node_number[sbatch], gen_batch) 
    }
    #set memory per node
    if (is.null(sbatch_params$mem_per_node[sbatch])) { 
      gen_batch <- gsub("MEMPERNODE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("MEMPERNODE", sbatch_params$mem_per_node[sbatch], gen_batch) 
    }
    #set requested mail message types
    if (is.null(sbatch_params$mailtype[sbatch])) { 
      gen_batch <- gsub("MAILTYPE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("MAILTYPE", sbatch_params$mailtype[sbatch], gen_batch) 
    }
    #set email at which to receive messages
    if (is.null(sbatch_params$user_email[sbatch])) { 
      gen_batch <- gsub("USER_EMAIL", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("USER_EMAIL", sbatch_params$user_email[sbatch], gen_batch) 
    }
    #set tasks per node
    if (is.null(sbatch_params$tasks_per_node[sbatch])) { 
      gen_batch <- gsub("TASKS_PER_NODE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("TASKS_PER_NODE", sbatch_params$tasks_per_node[sbatch], gen_batch) 
    }
    #set cpus per task
    if (is.null(sbatch_params$cpus_per_task[sbatch])) { 
      gen_batch <- gsub("CPUS_PER_TASK", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("CPUS_PER_TASK", sbatch_params$cpus_per_task[sbatch], gen_batch) 
    }
    #set path to r script
    if (is.null(sbatch_params$path_to_r_script[sbatch])) { 
      gen_batch <- gsub("PATH_TO_R_SCRIPT", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("PATH_TO_R_SCRIPT", sbatch_params$path_to_r_script[sbatch], gen_batch) 
    }
    #set args to r script
    if (is.null(sbatch_params$args_to_r_script[sbatch])) { 
      gen_batch <- gsub("ARGS_TO_R_SCRIPT", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("ARGS_TO_R_SCRIPT", sbatch_params$args_to_r_script[sbatch], gen_batch) 
    }
    
    #write batch file
    if (is.null(sbatch_params$write_path[sbatch])) { 
      cat(gen_batch, file = paste0("~/sbatch_generated_at_", gsub(" |:|-", "_", Sys.time()) ), append = F)
    } else { 
      cat(gen_batch, file = sbatch_params$write_path[sbatch], append = F)
    }
    
    if (!is.na(sbatch_params$server_sbatch_path[sbatch])) {
      outfile_lines <- c(outfile_lines, paste0("system(\"sbatch ", sbatch_params$server_sbatch_path[sbatch], "\")"))
    } 
    sbatches[[sbatch]] <- gen_batch
  }
  if (!is.na(runfile_path)) {
    cat(paste0(outfile_lines, collapse = "\n"), file = runfile_path)
  }
  if(run_now) { system(paste0("R -f ", runfile_path)) } 
  
  return(sbatches)
}

