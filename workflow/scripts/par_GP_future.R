###### USAGE: $ Rscript par_CV1.R Output.file.name Nreps Ncores

library(dplyr)
library(EnvRtype)
library(foreach)
library(doParallel)
source('https://raw.githubusercontent.com/gcostaneto/SelectivePhenotyping/master/cvrandom.R')

args <- commandArgs(trailingOnly = TRUE) # need command line args for outputfile name and N reps

#cores=Sys.getenv("SLURM_CPUS_PER_TASK")
cores=(as.numeric(args[3]))
message(paste0(cores," cores detected"))
registerDoParallel(cores=cores)

load("/netfiles02/kellrlab/Baxter/GenomicPrediction/GPsnake/GPfuture/future.RData")

main <- function() {
  
  
  message(paste0("Running for ",as.numeric(args[2]), " reps..."))
  
  
  rep  = as.numeric(args[2])
  seed = 414
  f    = 0.80
  iter = 6500
  burn = 5000
  thin = 10
  
  
  all_sites <- site_fb_NA$env 

  out <-foreach(site=iter(all_sites), .combine=rbind)%dopar% {  
    
    return(site)
    
  }
    