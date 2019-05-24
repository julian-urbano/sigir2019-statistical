source("R/common.R")

library(rio)
library(VineCopula)
library(doParallel)
stopImplicitCluster()
registerDoParallel(cores = .CORES)

path_margins <- "scratch/01-margins/"
path_out <- "scratch/03-bicops"

for(collection in .COLLECTIONS) {
  for(measure in .MEASURES) {
    collmeas <- paste0(collection, "_", measure)
    path_scores <- file.path("data", paste0(collmeas, ".csv"))
    
    if(file.exists(path_scores)) { # Do we have this collection-measure combination?
      path_margins_collmeas <- file.path(path_margins, collmeas)
      path_out_collmeas <- file.path(path_out, collmeas)
      dir.create(path_out_collmeas, recursive = TRUE)
      cat(path_out_collmeas, "\n"); flush.console()
      
      # Read data and remove bottom 10% and duplicates
      dat <- import(path_scores)
      dat <- removeDuplicates(dat)
      dat <- removeBottom(dat, p = .1)
      
      set.seed(ncol(dat))
      
      # Precompute the list of copulas to fit: only those where i < j.
      # They're the same as j > i, rotated, so let's keep it simple.
      g <- expand.grid(i = 1:ncol(dat), j = 1:ncol(dat))
      g <- g[g$i < g$j,]
      
      #for(r in 1:nrow(g)) {
      foreach(r = 1:nrow(g), .packages = c("rio", "VineCopula")) %dopar% { # for every (i,j) pair
        i <- g$i[r]
        j <- g$j[r]
        x <- dat[,c(i,j)]
        u <- pobs(x, ties.method = "random") # compute speudo-observations
        cop <- BiCopSelect(u[,1], u[,2], selectioncrit = "AIC") # fit copula
        
        export(cop, file.path(path_out_collmeas,
                              paste0(colnames(dat)[i], "_", colnames(dat)[j], ".rds")))
      }
    }
  }
}

stopImplicitCluster()
