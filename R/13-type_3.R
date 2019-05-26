source("R/common.R")

library(stringr)
library(rio)
library(doParallel)
stopImplicitCluster()
registerDoParallel(cores = .CORES)

path_in <- "scratch/12-type_2"

#for(measure in .MEASURES) {
foreach(measure = .MEASURES, .packages = c("rio", "stringr")) %dopar% {
  for(n_topics in .N_TOPICS){
    meatop <- paste0(measure, "_", n_topics)

    # Read data form all batches to compute error rates later on
    pvals <- sapply(.DELTAS, function(delta) {
      meatopdelta <- paste0(meatop, "_", delta)
      res <- import_list(list.files(file.path(path_in, meatopdelta), full.names = TRUE), rbind = TRUE)
    }, simplify = FALSE)
    names(pvals) <- sdelta(.DELTAS)

    # For each alpha, compute error rates
    for(alpha in .ALPHAS) {
      path_out <- file.path("output/type_3_by_delta", paste0("alpha", salpha(alpha)))
      dir.create(path_out, recursive = TRUE)

      res <- t(sapply(pvals, function(pval) {
        sapply(c("t2", "w2", "s2", "b2", "p2"), function(tn) {
          sum(pval$d < 0 & pval[,tn] <= alpha) / nrow(pval)
        })
      }))
      res <- cbind(delta = .DELTAS, res)

      export(res, file.path(path_out, paste0("type_3_by_delta_", meatop, "_alpha", salpha(alpha), ".csv")))
    }

    # For each delta, compute error rates
    for(delta in names(pvals)) {
      path_out <- file.path("output/type_3_by_alpha", paste0("delta", delta))
      dir.create(path_out, recursive = TRUE)

      pval <- pvals[[delta]]
      res <- sapply(c("t2", "w2", "s2", "b2", "p2"), function(tn) {
        sapply(.ALPHAS, function(alpha) {
          sum(pval$d < 0 & pval[,tn] <= alpha) / nrow(pval)
        })
      })
      res <- cbind(alpha = .ALPHAS, res)

      export(res, file.path(path_out, paste0("type_3_by_alpha_", meatop, "_delta", delta, ".csv")))
    }
  }
}

stopImplicitCluster()
