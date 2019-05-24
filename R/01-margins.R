source("R/common.R")

library(rio)
library(simIReff)
library(doParallel)
stopImplicitCluster()
registerDoParallel(cores = .CORES)

path_out <- "scratch/01-margins"

for(collection in .COLLECTIONS) {
  for(measure in .MEASURES) {
    collmeas <- paste0(collection, "_", measure)
    path_scores <- file.path("data", paste0(collmeas, ".csv"))
    
    if(file.exists(path_scores)) { # Do we have this collection-measure combination?
      path_out_collmeas <- file.path(path_out, collmeas)
      dir.create(path_out_collmeas, recursive = TRUE)
      cat(path_out_collmeas, "\n"); flush.console()
      
      # Read data and remove bottom 10% and duplicates
      dat <- import(path_scores)
      dat <- removeDuplicates(dat)
      dat <- removeBottom(dat, p = .1)
      
      set.seed(ncol(dat))
      
      #for(i in 1:ncol(dat)) {
      foreach(i = 1:ncol(dat), .packages = c("rio", "simIReff")) %dopar% {
        # Fit margin, continuous or discrete
        if(measure %in% c("ap", "ndcg20", "err20")) {
          eff <- effContFitAndSelect(dat[,i], method = "AIC", silent = TRUE)
        }else{
          eff <- effDiscFitAndSelect(dat[,i], support(measure), method = "AIC", silent = TRUE)
        }
        
        export(eff, file.path(path_out_collmeas, paste0(colnames(dat)[i], ".rds")))
      }
    }
  }
}

stopImplicitCluster()
