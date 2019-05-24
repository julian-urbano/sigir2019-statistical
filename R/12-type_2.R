source("R/common.R")
source("R/import.R")
source("R/ir_tests.R")

library(stringr)
library(rio)
library(simIReff)
library(VineCopula)
library(doParallel)
stopImplicitCluster()
registerDoParallel(cores = .CORES)

# Compute ------------------------------------------------------------------------------------------

path_out <- "scratch/12-type_2"

for(measure in .MEASURES) {
  # Import margins of runs evaluated with this measure (will be baselines)
  effs_b <- import_margins(measure)
  # Import bivariate copulas
  cops <- import_bicops(measure)

  for(n_topics in .N_TOPICS) {
    for(delta in .DELTAS) {
      path_out_measure_n_delta <- file.path(path_out, paste0(measure, "_", n_topics, "_", delta))
      dir.create(path_out_measure_n_delta, recursive = TRUE)
      cat(path_out_measure_n_delta, "\n"); flush.console()

      # Import transformed margins with this measure (will be experimentals)
      effs_e <- import_margins_transform(measure, delta)
      effs_c <- sapply(effs_e, length)

      #for(batch in 1:ceiling(.BATCHES / length(.DELTAS))) {
      foreach(batch = 1:ceiling(.BATCHES / length(.DELTAS)),
              .packages = c("rio", "simIReff", "VineCopula")) %dopar% {
        # initialize table to store all data from this batch
        res <- data.frame(collection = rep("", .N_PER_BATCH), b = "", e = "",
                          marginb = "", margine = "", cop = "", d = 0,
                          t1 = 0, w1 = 0, s1 = 0, b1 = 0, p1 = 0,
                          t2 = 0, w2 = 0, s2 = 0, b2 = 0, p2 = 0,
                          stringsAsFactors = FALSE)

        for(r in 1:nrow(res)) { # for every trial in the batch
          # sample collection, proportional to number of runs in it
          collection <- sample(names(effs_e), 1, prob = effs_c / sum(effs_c))
          # sample baseline run within THE collection, uniformly
          run_b <- sample(names(effs_e[[collection]]), 1)
          eff_b <- effs_b[[collection]][[run_b]]
          # sample experimental run
          run_e <- sample(names(effs_e[[collection]][[run_b]]), 1)
          eff_e <- effs_e[[collection]][[run_b]][[run_e]]
          # and copula between them
          swap <- FALSE
          cop <- cops[[collection]][[run_b]][[run_e]]
          if(is.null(cop)) { # we only compute copulas for (i,j) where i<j, so we may need to swap
            swap <- TRUE
            cop <- cops[[collection]][[run_e]][[run_b]]
          }

          # simulate pseudo-observations
          u <- BiCopSim(n_topics, obj = cop)
          # pass through margins to get final scores
          if(swap){
            b <- qeff(u[,2], eff_b)
            e <- qeff(u[,1], eff_e)
          }else{
            b <- qeff(u[,1], eff_b)
            e <- qeff(u[,2], eff_e)
          }

          # tests
          d <- round(mean(e-b), .SIGNIF) # difference d=e-b
          p_t <- round(test_t(b, e), .SIGNIF)
          p_w <- round(test_wilcoxon(b,e ), .SIGNIF)
          p_s <- round(test_sign(b, e), .SIGNIF)
          p_b <- round(test_bootstrap(b, e, .B), .SIGNIF)
          p_p <- round(test_permutation(b, e, .B), .SIGNIF)

          res[r,] <- list(collection, run_b, run_e,
                          eff_b$model$type, eff_e$model$original$model$type,
                          BiCopName(cop$family, short = TRUE), d,
                          p_t[1], p_w[1], p_s[1], p_b[1], p_p[1],
                          p_t[2], p_w[2], p_s[2], p_b[2], p_p[2])
        }
        # remove the "run" part and save
        res$b <- as.numeric(gsub("run", "", res$b))
        res$e <- as.numeric(gsub("run", "", res$e))

        path_out_batch <- file.path(path_out_measure_n_delta, paste0(batch, ".csv"))
        export(res, path_out_batch)
      }
    }
  }
}

# Reduce -------------------------------------------------------------------------------------------

path_in <- path_out

#for(measure in .MEASURES) {
foreach(measure = .MEASURES, .packages = c("rio", "stringr")) %dopar% {
  for(n_topics in .N_TOPICS){
    meatop <- paste0(measure, "_", n_topics)

    # Read data form all batches and compute the ecdfs to compute power rates later on
    pvals <- sapply(.DELTAS, function(delta) {
      res <- import_list(list.files(file.path(path_in, paste0(meatop, "_", delta)),
                                    full.names = TRUE), rbind = TRUE)
      sapply(c("t1", "w1", "s1","b1", "p1", "t2", "w2", "s2", "b2", "p2"), function(tn) {
        ecdf(res[,tn])
      })
    }, simplify = FALSE)
    names(pvals) <- sdelta(.DELTAS)

    # For each significance level, compute power
    for(alpha in .ALPHAS) {
      path_out <- file.path("output/type_2_by_delta", paste0("alpha", salpha(alpha)))
      dir.create(path_out, recursive = TRUE)

      res <- t(sapply(pvals, function(pval) {
        sapply(c("t1", "w1", "s1","b1", "p1", "t2", "w2", "s2", "b2", "p2"), function(tn) {
          pval[[tn]](alpha)
        })
      }))
      res <- cbind(delta = .DELTAS, res)

      export(res, file.path(path_out,
                            paste0("type_2_by_delta_", meatop, "_alpha", salpha(alpha), ".csv")))
    }

    # For each delta, compute power
    for(delta in names(pvals)) {
      path_out <- file.path("output/type_2_by_alpha", paste0("delta", delta))
      dir.create(path_out, recursive = TRUE)

      res <- sapply(pvals[[delta]], function(pval) {
        pval(.ALPHAS)
      })
      res <- cbind(alpha = .ALPHAS, res)

      export(res, file.path(path_out,
                            paste0("type_2_by_alpha_", meatop, "_delta", delta, ".csv")))
    }
  }
}

stopImplicitCluster()
