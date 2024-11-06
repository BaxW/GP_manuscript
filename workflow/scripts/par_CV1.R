
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
  thin = 10
  

  
  
    pmat <- pmat_snp
    
  
    Y <- pmat %>%
      rename(value=y)
 
  
  #TS = Sampling.CV1(gids = pmat$gid,f = f,seed = seed,rep = rep,gidlevel = F)
  
  notNA_pmat <- which(!is.na(Y$value))
  
  TS2 <- Sampling.CV1(gids = pmat$gid[notNA_pmat], f = f,seed = seed,rep = rep,gidlevel = F)
  
  #model <- c("BaselineMM", "BaselineMDs", "EMDs", "RxnNormMM", "RxnNormMDs")
  #Models <- list(MM_snp, MDs_snp, EMDs_snp, RNMM_snp, RNMDs_snp)
  
  model <- c("BaselineMM","EMDs", "RxnNormMDs")
  

  Models <- list(MM_snp, EMDs_snp, RNMDs_snp)
 
  
  results <-foreach(REP = 1:rep, .combine = "rbind")%:%
    foreach(MODEL = 1:length(model), .combine = "rbind")%dopar% {
      
      yNA      <- pmat
      tr       <- TS2[[REP]]
      yNA$y[-tr] <- NA
      
      Z_E = model.matrix(~0+grdn+yr,data=yNA) # fixed environmental effects
      
      fit <- kernel_model(data = yNA,y = y,env = env,gid = gid,
                          random = Models[[MODEL]],fixed = Z_E,
                          iterations = iter,burnin = burn,thining = thin)
      
      
      df<-data.frame(Model = model[MODEL],rep=REP,
                     rTr=cor(Y$value[tr], fit$yHat[tr],use = 'complete.obs'),
                     rTs=cor(Y$value[-tr], fit$yHat[-tr],use = 'complete.obs'))
      
      
      
      write.table(x = df,file = args[1], sep=',',append = T,row.names=F, quote = F)
      return(df)
    }
  stopImplicitCluster()
}

main()