utils::globalVariables(c("var", "cluster_name", "size", "k",
  "pct_cluster", "val", "nn", "k", "wss", "y", "denom", "z"))

#' @import rbokeh
#' @import lattice
#' @import dplyr
#' @importFrom stats median setNames var
NULL

#' seriesclust.
#'
#' @name seriesclust
#' @docType package
#' @example man-roxygen/ex-clust.R
NULL

#' "Nasdaq 2016" dataset
#'
#' @name nasd16
#' @docType data
#' @description
#' 2016 monthly median closing price for 2801 stocks in the Nasdaq exchange.
#' @usage nasd16
#' @keywords data
#' @example man-roxygen/ex-clust-norun.R
NULL

tableau10 <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
  "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")
