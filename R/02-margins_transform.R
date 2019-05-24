source("R/common.R")
source("R/import.R")

library(rio)
library(simIReff)
library(doParallel)
stopImplicitCluster()
registerDoParallel(cores = .CORES)

path_margins <- "scratch/01-margins/"
path_out <- "scratch/02-margins_transform"

for(collection in .COLLECTIONS) {
  for(measure in .MEASURES) {
    collmeas <- paste0(collection, "_", measure)
    path_scores <- file.path("data", paste0(collmeas, ".csv"))

    if(file.exists(path_scores)) { # Do we have this collection-measure combination?
      path_margins_collmeas <- file.path(path_margins, collmeas)
      path_out_collmeas <- file.path(path_out, collmeas)
      dir.create(path_out_collmeas, recursive = TRUE)
      cat(path_out_collmeas, "\n"); flush.console()

      # Import all marginal distributions
      effs <- import_margins(measure, collection)[[1]]

      set.seed(length(effs))

      # Expected values and indices of baseline systems (bottom 75% of runs)
      mu <- sapply(effs, function(eff) eff$mean)
      baselines <- order(mu, decreasing = TRUE)[-1:-round(length(mu)*.25)]
      
      for(delta in .DELTAS) {
        cat(path_out_collmeas, delta, "\n"); flush.console()
        # for(i in baselines) {
        foreach(i = baselines, .packages = c("rio", "simIReff")) %dopar% { # for each baseline
          effi <- effs[[i]] # baseline margin
          
          js <- order(abs(mu - effi$mean - delta))
          js <- js[js!=i]
          js <- js[1:10] # 10 other margins closest in mean (+delta)
          for(j in js) {
            effj <- effs[[j]]
            path_effj <- file.path("scratch/02-margins_transform", collmeas,
                                   paste0(names(effs)[i], "_", delta, "_", names(effs)[j], ".rds"))
            
            effj <- try(effTransform(effj, effi$mean + delta), silent = TRUE)
            if(!inherits(effj, "try-error")) { # if successful transformation, save
              export(effj, path_effj)
              
            }
          }
        }
      }
    }
  }
}

stopImplicitCluster()
