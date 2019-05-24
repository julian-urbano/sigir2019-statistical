source("R/common.R")

library(rio)
library(simIReff)
library(VineCopula)
library(tools)

#' Import the marginal distributions for the given measure and collections.
#' If collection is NULL, it imports from all collections available for the given measure.
import_margins <- function(measure, collection = NULL, path = "scratch/01-margins/") {
  if(missing(collection)) # if no collection given, import them all
    collection <- .COLLECTIONS

  effs <- sapply(collection, function(coll) { # for every collection
    collmeas <- paste0(coll, "_", measure)
    if(!file.exists(file.path("data/", paste0(collmeas, ".csv")))) # don't have this combination?
      return(NULL)
    runs <- file_path_sans_ext(list.files(file.path(path, collmeas))) # runs with existing margin

    sapply(runs, function(run) { # for each run
      import(file.path(path, collmeas, paste0(run, ".rds")))
    }, simplify = FALSE)
  }, simplify = FALSE)

  effs <- effs[!sapply(effs, is.null)] # remove collections without that measure
  effs
}

#' Import the transformed marginal distributions for the given measure and collections.
#' If collection is NULL, it imports from all collections available for the given measure.
import_margins_transform <- function(measure, delta, collection = NULL,
                                         path = "scratch/02-margins_transform/") {
  if(missing(collection)) # if no collection given, import them all
    collection <- .COLLECTIONS

  effs_t <- sapply(collection, function(coll) { # for every collection
    collmeas <- paste0(coll, "_", measure)
    if(!file.exists(file.path("data/", paste0(collmeas, ".csv")))) # don't have this combination?
      return(NULL)
    delta <- as.character(delta)

    f <- file_path_sans_ext(list.files(file.path(path,collmeas), pattern = paste0("_", delta, "_")))
    runs1 <- unique(gsub("_.+_run.+$", "", f)) # all runs in 1st margin
    effs_t <- sapply(runs1, function(run1) {
      runs2 <- file_path_sans_ext(list.files(file.path(path, collmeas),
                                             pattern = paste0(run1, "_", delta, "_")))
      runs2 <- gsub(paste0(run1, "_", delta, "_"), "", runs2, fixed = TRUE) # all runs in 2nd margin
      sapply(runs2, function(run2) {
        import(file.path(path, collmeas, paste0(run1, "_", delta, "_", run2, ".rds")))
      }, simplify = FALSE)
    }, simplify = FALSE)
  }, simplify = FALSE)

  effs_t <- effs_t[!sapply(effs_t, is.null)] # remove collections without that measure
  effs_t
}

#' Import the copulas for the given measure and collections.
#' If collection is NULL, it imports from all collections available for the given measure.
import_bicops <- function(measure, collection = NULL, path = "scratch/03-bicops/") {
  if(missing(collection)) # if no collection given, import them all
    collection <- .COLLECTIONS

  cops <- sapply(collection, function(coll) { # for every collection
    collmeas <- paste0(coll, "_", measure)
    if(!file.exists(file.path("data/", paste0(collmeas, ".csv")))) # don't have this combination?
      return(NULL)

    f <- file_path_sans_ext(list.files(file.path(path, collmeas)))
    runs1 <- unique(gsub("_run.+$", "", f)) # all runs in 1st margin
    sapply(runs1, function(run1) {
      runs2 <- file_path_sans_ext(list.files(file.path(path, collmeas),
                                             pattern = paste0("^", run1, "_")))
      runs2 <- gsub("^[^_]+_", "", runs2) # all runs in 2nd margin
      sapply(runs2, function(run2) {
        import(file.path(path, collmeas, paste0(run1, "_", run2, ".rds")))
      }, simplify = FALSE)
    }, simplify = FALSE)
  }, simplify = FALSE)

  cops <- cops[!sapply(cops, is.null)] # remove collections without that measure
  cops
}

# import_model <- function(measure, collection, check = FALSE) {
#   collmeas <- paste0(collection, "_", measure)
#   if(!file.exists(file.path("data/", paste0(collmeas, ".csv"))))
#     return(NULL)
#   effs <- import_margins(collection, measure)
#   effs_t <- sapply(as.character(.DELTAS), function(delta) {
#     import_margins_transform(collection, measure, delta)
#   }, simplify = FALSE)
#   cops <- import_bicops(collection, measure)
# 
#   # checks
#   deltas <- numeric(0)
#   if(check) {
#     stopifnot(length(effs)*(length(effs)-1)/2 == sum(sapply(cops, length)))
# 
#     for(delta in .DELTAS) {
#       sdelta <- as.character(delta)
# 
#       deltad <- effs_t[[sdelta]]
#       for(e1 in names(deltad)) {
#         eff1 <- effs[[e1]]
#         deltade <- deltad[[e1]]
#         for(e2 in names(deltade)) {
#           deltas <- c(deltas, eff1$mean + delta - deltade[[e2]]$mean)
#         }
#       }
#     }
#   }
# 
#   list(effs = effs, effs_t = effs_t, cops = cops, deltas = deltas)
# }
