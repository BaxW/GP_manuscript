
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

load("/netfiles02/kellrlab/Baxter/GenomicPrediction/GPsnake/maxi.RData")

main <- function() {
  


  message(paste0("Running for ",as.numeric(args[2]), " reps..."))
  

  rep  = as.numeric(args[2])
  seed = 414
  f    = 0.80
  iter = 10000
  burn = 1000
  thin = 2
  
  
    pmat <- pmat_snp
    
  
    Y <- pmat %>%
      rename(value=y)
 
  

  notNA_pmat <- which(!is.na(Y$value))
  
  TS3 <- Sampling.CV1(gids = pmat_snp$gid[notNA_pmat], f = f,seed = seed,rep = rep, gidlevel = T)
  
  #model <- c("BaselineMM", "BaselineMDs", "EMDs", "RxnNormMM", "RxnNormMDs")
  #Models <- list(MM_snp, MDs_snp, EMDs_snp, RNMM_snp, RNMDs_snp)
  
  model <- c("BaselineMM","EMDs", "RxnNormMDs")
  

  Models <- list(MM_snp, EMDs_snp, RNMDs_snp)
 
  
  results <-foreach(REP = 1:rep, .combine = "rbind")%:%
    foreach(MODEL = 1:length(model), .combine = "rbind")%dopar% {
      
      ### basically masking ~20% of genotypes across all environments rather than random like above 
      tr <- TS3[,REP]
      tr_ids <- unique(pmat_snp$gid)[tr]
      yNA <- pmat_snp %>%
        mutate(y=ifelse(gid %in% tr_ids,y,NA))
      
      
      Z_E = model.matrix(~0+grdn+yr,data=yNA)
      
      
      fit <- kernel_model(data = yNA,y = y,env = env,gid = gid,
                          random = Models[[MODEL]],fixed = Z_E,
                          iterations = iter,burnin = burn,thining = thin)
      
  
      
      out <- pmat_snp %>%
        mutate(yhat=fit$yHat) %>%
        filter( ! gid %in% tr_ids) %>%
        group_by(gid) %>%
        summarise(cor=cor(y,yhat,use = 'complete.obs')) %>%
        mutate(model=model[REP])
      
      
      return(out)

      
      
    }

  # check results!!!
    
  write.table(x = results,file = args[1], sep=',',,row.names=F, quote = F)
  return(out)
  
  stopImplicitCluster()
}

main()