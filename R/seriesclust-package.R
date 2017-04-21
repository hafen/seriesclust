utils::globalVariables(c("var", "cluster_name", "size", "k",
  "pct_cluster", "val", "nn", "k", "wss", "y", "denom"))

#' @import rbokeh
#' @import lattice
#' @import dplyr
#' @importFrom stats median setNames var
NULL

#' seriesclust.
#'
#' @name seriesclust
#' @docType package
#' @examples
#' library(dplyr)
#'
#' # scale the monthly median close price so that we are clustering on general shape
#' d <- nasd16 %>%
#'   group_by(symbol) %>%
#'   mutate(close_scl = scale(med_close)) %>%
#'   select(-company, -med_close)
#'
#' set.seed(1234)
#' # k-means clustering with 2, 5, and 9 clusters
#' km <- get_kmeans(d, x = "month", y = "close_scl", k = c(2, 5, 9))
#' plot_scree(km)
#' plot_clust(km, 9)
#' plot_heat(km, 9, col = "sector")
#' plot_heat(km, 9, col = "industry", cutoff = 10)
NULL

#' "Nasdaq 2016" dataset
#'
#' @name nasd16
#' @docType data
#' @description
#' 2016 monthly median closing price for 2801 stocks in the Nasdaq exchange.
#' @usage nasd16
#' @keywords data
#' @example man-roxygen/ex-clust.R
NULL
