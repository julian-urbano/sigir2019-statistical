library(stringr)

.COLLECTIONS <- c("adhoc5", "adhoc6", "adhoc7", "adhoc8",
                  "web2010", "web2011", "web2012", "web2013")
.MEASURES <- c("ap", "p10", "rr", "ndcg20", "err20")

.N_TOPICS <- c(25, 50, 100) # topic set sizes
.DELTAS <- seq(.01, .1, .01) # effect sizes
.ALPHAS <- c(1:9*.001, 1:9*.01, .1) # significance levels

.B <- 1e6 # number of bootstrap/permutation samples
.SIGNIF <- 6 # number of decimal digits to save in scratch files

# computations will run in batches of a certain size, parallelizing over batches
.CORES <- parallel::detectCores()
.BATCHES <- 1667
.N_PER_BATCH <- 1000

# Utils ############################################################################################

#' Turn an alpha value into a fixed-width string
salpha <- function(alpha) {
  m <- max(str_length(.ALPHAS))
  str_pad(alpha, m, "right", "0")
}
#' Turn a delta vlaue into a fixed-width string
sdelta <- function(delta) {
  m <- max(str_length(.DELTAS))
  str_pad(delta, m, "right", "0")
}

#' Remove the bottom p% of runs
removeBottom <- function(dat, p = .25) {
  mu <- colMeans(dat)
  i <- mu >= quantile(mu, p)
  dat[,i]
}

#' Remove duplicate runs by their per-topic score
removeDuplicates <- function(dat, tol = 1e-5) {
  toremove <- integer(0)
  for(i in seq(ncol(dat)-1)) {
    x <- dat[,i]
    for(j in seq(i+1,ncol(dat))) {
      y <- dat[,j]
      if(all(abs(x-y)<=tol))
        toremove <- c(toremove, j)
    }
  }
  if(length(toremove) > 0)
    dat[-toremove]
  else
    dat
}

# Plots ############################################################################################

my.dev.width <- function(num = 1){
  return(16 / num)
}
my.dev.par <- function(mar = 0, mgp = 0, ...){
  par(mar = c(2.5,2.5,1.8,0.3) + mar, mgp = c(1.5,.5,0) + mgp, ...)
}
my.dev.abline <- function(col="darkgrey", lwd=1, lty=2, ...){
  abline(col=col, lwd=lwd, lty=lty, ...)
}
my.dev.set.pdf <- function() {
  .GlobalEnv$my.dev.new <- function(file, num, ratio=.82, ...){
    width <- my.dev.width(num)
    height <- width*ratio
    pdf(file=file, width=width, height=height)
    my.dev.par(...)
  }
  .GlobalEnv$my.dev.off <- function(...) { off <- capture.output(dev.off(...)) }
}
my.dev.set.win <- function() {
  .GlobalEnv$my.dev.new <- function(file, num, ratio=.82, ...){
    width <- my.dev.width(num)
    height <- width*ratio
    #dev.new(width=width, height=height)
    my.dev.par(...)
  }
  .GlobalEnv$my.dev.off <- function(){}
}
my.dev.set.pdf()
#my.dev.set.win()
my.axis <- function(side, at, labels, ...) {
  if(missing(labels))
    labels <- as.character(at)
  for(i in seq_along(at))
    axis(side = side, at = at[i], labels = labels[i], tick = FALSE, ...)
  axis(side = side, at = at, labels = NA, ...)
}
