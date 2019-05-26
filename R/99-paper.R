source("R/common.R")
source("R/ir_tests.R")

library(rio)

.TEST_COLORS <- c(t = '#e41a1c', w = '#377eb8', s = '#4daf4a', p = '#ff7f00', b = '#984ea3')
.TEST_NAMES <- c(t = "t-test", w = "Wilcoxon", s = "Sign", p = "Permutation", b = "Bootstrap")
.MEASURE_NAMES <- c(ap = "AP", p10 = "P@10", rr = "RR", ndcg20 = "nDCG@20", err20 = "ERR@20")
.ALPHAS_PLOTS <- c(.001, .005, .01, .05, .1)

path_out <- "output"
dir.create(path_out, recursive = TRUE)

# function to drop the leading zero in a decimal numer
drop0 <- function(x) { gsub("0.", ".", as.character(x), fixed = TRUE)}

# TYPE I ERRORS ####################################################################################

for(measure in .MEASURES) {
  for(n_topics in .N_TOPICS) {
    dat <- import(file.path("output/type_1/", paste0("type_1_", measure, "_", n_topics, ".csv")))

    for(tails in 1:2) {
      my.dev.new(file = file.path(path_out, "type_1",
                                  paste0("type_1_", measure, "_", n_topics, "_tail", tails, ".pdf")),
                 num = 5, ratio = .9)

      plot(dat$alpha, dat$alpha, col = "darkgrey", type = "l",
           log = "xy", xaxs = "i", yaxs = "i", axes = FALSE,
           xlab = expression("Significance level "*alpha), ylab = "Type I error rate",
           main = paste0(.MEASURE_NAMES[measure], " n=", n_topics, " tails=", tails))
      abline(h = c(.005,.01,.05), v = c(.005,.01,.05), lty = 3, col = "darkgrey")
      axis(1, at = dat$alpha, rep("", length(dat$alpha)), tck = -.025)
      axis(2, at = dat$alpha, rep("", length(dat$alpha)), tck = -.025)
      for(x in drop0(c(.001, .005, .01, .05, .1))) {
        axis(1, at = as.numeric(x), labels = x)
        axis(2, at = as.numeric(x), labels = x)
      }

      lines(dat$alpha, dat[,paste0("w", tails)], lwd = 1, col = .TEST_COLORS["w"])
      lines(dat$alpha, dat[,paste0("s", tails)], lwd = 1, col = .TEST_COLORS["s"])
      lines(dat$alpha, dat[,paste0("b", tails)], lwd = 1, col = .TEST_COLORS["b"])
      lines(dat$alpha, dat[,paste0("p", tails)], lwd = 1, col = .TEST_COLORS["p"])
      lines(dat$alpha, dat[,paste0("t", tails)], lwd = 1, col = .TEST_COLORS["t"])
      # re-plot permutation with dashes, because it tends to overlap with t-test
      lines(dat$alpha, dat[,paste0("p", tails)], lwd = 1, col = .TEST_COLORS["p"], lty = 2)


      if(measure == "ap" && n_topics == 25) {
        legend("bottomright", col = .TEST_COLORS, .TEST_NAMES, lwd = 1, bg = "white",
               cex = .7, box.lty = 0)
      }
      box()
      my.dev.off()
    }
  }
}

# TYPE II ERRORS ###################################################################################

for(alpha in salpha(.ALPHAS)) {
  path_out_alpha <- file.path(path_out, "type_2_by_delta", paste0("alpha", alpha))
  for(measure in .MEASURES) {
    for(n_topics in .N_TOPICS) {
      dat <- import(file.path("output/type_2_by_delta/", paste0("alpha", alpha),
                              paste0("type_2_by_delta_", measure, "_", n_topics,
                                     "_alpha", alpha, ".csv")))

      for(tails in 1:2) {
        my.dev.new(file = file.path(path_out_alpha, paste0("type_2_by_delta_", measure,
                                                           "_", n_topics, "_alpha", alpha,
                                                           "_tail", tails, ".pdf")),
                   num = 5, ratio = .9)

        plot(NA, xlim = range(.DELTAS), ylim = 0:1, xaxs = "i", yaxs = "i", axes = FALSE,
             xlab = expression("Effect size "*delta), ylab = "Power",
             main = bquote(bold(.(.MEASURE_NAMES[measure])*" n="*.(n_topics)*
                                  " tails="*.(tails)~alpha*"="*.(alpha))))
        abline(h = seq(.2,.8,.2), v = seq(.02,.09,.01), lty = 3, col = "darkgrey")
        axis(2)
        for(x in drop0(.DELTAS)) {
          axis(1, at = as.numeric(x), labels = x)
        }

        lines(dat$delta, dat[,paste0("w", tails)], lwd = 1, col = .TEST_COLORS["w"])
        lines(dat$delta, dat[,paste0("s", tails)], lwd = 1, col = .TEST_COLORS["s"])
        lines(dat$delta, dat[,paste0("b", tails)], lwd = 1, col = .TEST_COLORS["b"])
        lines(dat$delta, dat[,paste0("p", tails)], lwd = 1, col = .TEST_COLORS["p"])
        lines(dat$delta, dat[,paste0("t", tails)], lwd = 1, col = .TEST_COLORS["t"])
        # re-plot permutation with dashes, because it tends to overlap with t-test
        lines(dat$delta, dat[,paste0("p", tails)], lwd = 1, col = .TEST_COLORS["p"], lty = 2)


        if(measure == "ap" && n_topics == 25) {
          legend("bottomright", col = .TEST_COLORS, .TEST_NAMES, lwd = 1, bg = "white",
                 cex = .7, box.lty = 0)
        }
        box()
        my.dev.off()
      }
    }
  }
}

for(delta in sdelta(.DELTAS)) {
  path_out_delta <- file.path(path_out, "type_2_by_alpha", paste0("delta", delta))
  for(measure in .MEASURES) {
    for(n_topics in .N_TOPICS) {
      dat <- import(file.path("output/type_2_by_alpha/", paste0("delta", delta),
                              paste0("type_2_by_alpha_", measure, "_", n_topics,
                                     "_delta", delta, ".csv")))

      for(tails in 1:2) {
        my.dev.new(file = file.path(path_out_delta, paste0("type_2_by_alpha_", measure,
                                                           "_", n_topics, "_delta", delta,
                                                           "_tail", tails, ".pdf")),
                   num = 5, ratio = .9)

        plot(NA, xlim = range(.ALPHAS), ylim = 0:1, xaxs = "i", yaxs = "i", axes = FALSE, log = "x",
             xlab = expression("Significance level "*alpha), ylab = "Power",
             main = bquote(bold(.(.MEASURE_NAMES[measure])*" n="*.(n_topics)*
                                  " tails="*.(tails)~delta*"="*.(delta))))
        abline(h = seq(.2,.8,.2), v = c(.005,.01,.05), lty = 3, col = "darkgrey")
        axis(1, at = dat$alpha, rep("", length(dat$alpha)), tck = -.025)
        axis(2)
        for(x in drop0(c(.001, .005, .01, .05, .1))) {
          axis(1, at = as.numeric(x), labels = x)
        }

        lines(dat$alpha, dat[,paste0("w", tails)], lwd = 1, col = .TEST_COLORS["w"])
        lines(dat$alpha, dat[,paste0("s", tails)], lwd = 1, col = .TEST_COLORS["s"])
        lines(dat$alpha, dat[,paste0("b", tails)], lwd = 1, col = .TEST_COLORS["b"])
        lines(dat$alpha, dat[,paste0("p", tails)], lwd = 1, col = .TEST_COLORS["p"])
        lines(dat$alpha, dat[,paste0("t", tails)], lwd = 1, col = .TEST_COLORS["t"])
        # re-plot permutation with dashes, because it tends to overlap with t-test
        lines(dat$alpha, dat[,paste0("p", tails)], lwd = 1, col = .TEST_COLORS["p"], lty = 2)


        if(measure == "ap" && n_topics == 25) {
          legend("topleft", col = .TEST_COLORS, .TEST_NAMES, lwd = 1, bg = "white",
                 cex = .7, box.lty = 0)
        }
        box()
        my.dev.off()
      }
    }
  }
}

# TYPE III ERRORS ##################################################################################

for(alpha in salpha(.ALPHAS)) {
  path_out_alpha <- file.path(path_out, "type_3_by_delta", paste0("alpha", alpha))
  for(measure in .MEASURES) {
    for(n_topics in .N_TOPICS) {
      dat <- import(file.path("output/type_3_by_delta/", paste0("alpha", alpha),
                              paste0("type_3_by_delta_", measure, "_", n_topics,
                                     "_alpha", alpha, ".csv")))

      my.dev.new(file = file.path(path_out_alpha, paste0("type_3_by_delta_", measure,
                                                         "_", n_topics, "_alpha", alpha, ".pdf")),
                 num = 5, ratio = .9)

      plot(NA, xlim = range(.DELTAS), ylim = c(0,.014), xaxs = "i", yaxs = "i", axes = FALSE,
           xlab = expression("Effect size "*delta), ylab = "Type III error rate",
           main = bquote(bold(.(.MEASURE_NAMES[measure])*" n="*.(n_topics)*
                                " tails="*.(tails)~alpha*"="*.(alpha))))
      abline(h = seq(.002,.012,.002), v = seq(.02,.09,.01), lty = 3, col = "darkgrey")
      axis(2)
      for(x in drop0(.DELTAS)) {
        axis(1, at = as.numeric(x), labels = x)
      }

      lines(dat$delta, dat$w2, lwd = 1, col = .TEST_COLORS["w"])
      lines(dat$delta, dat$s2, lwd = 1, col = .TEST_COLORS["s"])
      lines(dat$delta, dat$b2, lwd = 1, col = .TEST_COLORS["b"])
      lines(dat$delta, dat$p2, lwd = 1, col = .TEST_COLORS["p"])
      lines(dat$delta, dat$t2, lwd = 1, col = .TEST_COLORS["t"])
      # re-plot permutation with dashes, because it tends to overlap with t-test
      lines(dat$delta, dat$p2, lwd = 1, col = .TEST_COLORS["p"], lty = 2)


      if(measure == "ap" && n_topics == 25) {
        legend("topright", col = .TEST_COLORS, .TEST_NAMES, lwd = 1, bg = "white",
               cex = .7, box.lty = 0)
      }
      box()
      my.dev.off()
    }
  }
}

for(delta in sdelta(.DELTAS)) {
  path_out_delta <- file.path(path_out, "type_3_by_alpha", paste0("delta", delta))
  for(measure in .MEASURES) {
    for(n_topics in .N_TOPICS) {
      dat <- import(file.path("output/type_3_by_alpha/", paste0("delta", delta),
                              paste0("type_3_by_alpha_", measure, "_", n_topics,
                                     "_delta", delta, ".csv")))

      my.dev.new(file = file.path(path_out_delta, paste0("type_3_by_alpha_", measure,
                                                         "_", n_topics, "_delta", delta, ".pdf")),
                 num = 5, ratio = .9)

      plot(NA, xlim = range(.ALPHAS), ylim = c(0,.014), xaxs = "i", yaxs = "i", axes = FALSE, log = "x",
           xlab = expression("Significance level "*alpha), ylab = "Type III error rate",
           main = bquote(bold(.(.MEASURE_NAMES[measure])*" n="*.(n_topics)*
                                " tails="*.(tails)~delta*"="*.(delta))))
      abline(h = seq(.002,.012,.002), v = c(.005,.01,.05), lty = 3, col = "darkgrey")
      axis(1, at = dat$alpha, rep("", length(dat$alpha)), tck = -.025)
      axis(2)
      for(x in drop0(c(.001, .005, .01, .05, .1))) {
        axis(1, at = as.numeric(x), labels = x)
      }

      lines(dat$alpha, dat$w2, lwd = 1, col = .TEST_COLORS["w"])
      lines(dat$alpha, dat$s2, lwd = 1, col = .TEST_COLORS["s"])
      lines(dat$alpha, dat$b2, lwd = 1, col = .TEST_COLORS["b"])
      lines(dat$alpha, dat$p2, lwd = 1, col = .TEST_COLORS["p"])
      lines(dat$alpha, dat$t2, lwd = 1, col = .TEST_COLORS["t"])
      # re-plot permutation with dashes, because it tends to overlap with t-test
      lines(dat$alpha, dat$p2, lwd = 1, col = .TEST_COLORS["p"], lty = 2)

      if(measure == "ap" && n_topics == 25) {
        legend("topright", col = .TEST_COLORS, .TEST_NAMES, lwd = 1, bg = "white",
               cex = .7, box.lty = 0)
      }
      box()
      my.dev.off()
    }
  }
}
