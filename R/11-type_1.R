source("R/common.R")
source("R/import.R")
source("R/ir_tests.R")

library(rio)
library(simIReff)
library(VineCopula)
library(doParallel)
stopImplicitCluster()
registerDoParallel(cores = .CORES)

# Compute ------------------------------------------------------------------------------------------

path_out <- "scratch/11-type_1"

for(measure in .MEASURES) {
  # Import margins of runs evaluated with this measure
  effs <- import_margins(measure)
  effs_c <- sapply(effs, length) # count of margins per collection, for sampling
  # Import bivariate copulas
  cops <- import_bicops(measure)
  
  for(n_topics in .N_TOPICS) {
    path_out_measure_n <- file.path(path_out, paste0(measure, "_", n_topics))
    dir.create(path_out_measure_n, recursive = TRUE)
    cat(path_out_measure_n, "\n"); flush.console()
    
    #for(batch in 1:.BATCHES) {
    foreach(batch = 1:.BATCHES, .packages = c("rio", "simIReff", "VineCopula")) %dopar% {
      # initialize table to store all data from this batch
      res <- data.frame(collection = rep("", .N_PER_BATCH), b = "", e = "",
                        margin = "", cop = "", d = 0,
                        t1 = 0, w1 = 0, s1 = 0, b1 = 0, p1 = 0,
                        t2 = 0, w2 = 0, s2 = 0, b2 = 0, p2 = 0,
                        stringsAsFactors = FALSE)
      
      for(r in 1:nrow(res)) { # for every trial in the batch
        # sample collection, proportional to number of runs in it
        collection <- sample(names(effs), 1, prob = effs_c / sum(effs_c))
        # sample two runs within the collection, uniformly
        runs <- sample(names(effs[[collection]]), 2)
        
        # margin of the 1st run and copula
        eff <- effs[[collection]][[runs[1]]]
        cop <- cops[[collection]][[runs[1]]][[runs[2]]]
        if(is.null(cop)) # we only compute copulas for (i,j) where i<j, so we may need to swap
          cop <- cops[[collection]][[runs[2]]][[runs[1]]]
        
        # simulate pseudo-observations
        u <- BiCopSim(n_topics, obj = cop)
        # pass through the same margin to get final scores
        b <- qeff(u[,1], eff)
        e <- qeff(u[,2], eff)
        
        # tests
        d <- round(mean(e-b), .SIGNIF) # difference d=e-b
        p_t <- round(test_t(b, e), .SIGNIF)
        p_w <- round(test_wilcoxon(b,e ), .SIGNIF)
        p_s <- round(test_sign(b, e), .SIGNIF)
        p_b <- round(test_bootstrap(b, e, .B), .SIGNIF)
        p_p <- round(test_permutation(b, e, .B), .SIGNIF)
        
        res[r,] <- list(collection, runs[1], runs[2],
                        eff$model$type, BiCopName(cop$family, short = TRUE), d,
                        p_t[1], p_w[1], p_s[1], p_b[1], p_p[1],
                        p_t[2], p_w[2], p_s[2], p_b[2], p_p[2])
      }
      # remove the "run" part and save
      res$b <- as.numeric(gsub("run", "", res$b))
      res$e <- as.numeric(gsub("run", "", res$e))
      
      path_out_batch <- file.path(path_out_measure_n, paste0(batch, ".csv"))
      export(res, path_out_batch)
    }
  }
}

# Reduce -------------------------------------------------------------------------------------------

path_in <- path_out
path_out <- "output/type_1"
dir.create(path_out, recursive = TRUE)

# for(measure in .MEASURES) {
foreach(measure = .MEASURES, .packages = "rio") %dopar% {
  for(n_topics in .N_TOPICS){
    meatop <- paste0(measure, "_", n_topics)
    
    # Read data form all batches and compute Type I errors at all alpha levels
    res <- import_list(list.files(file.path(path_in, meatop), full.names = TRUE), rbind = TRUE)
    type1 <- sapply(c("t1", "w1", "s1","b1", "p1", "t2", "w2", "s2", "b2", "p2"), function(tn) {
      ecdf(res[,tn])(.ALPHAS)
    })
    type1 <- cbind(alpha = .ALPHAS, type1)
    
    export(type1, file.path(path_out, paste0("type_1_", meatop, ".csv")))
  }
}

stopImplicitCluster()
