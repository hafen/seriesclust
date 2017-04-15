#' Get a set k-means++ clusters for a collection of series
#'
#' @param dat data frame
#' @param x string name of x variable
#' @param y string name of y variable
#' @param groups a character vector of grouping variable names
#' @param nk number of clusters to run through (result be a set of clusters wit k = 2:nk)
#' @export
#' @importFrom purrr by_row
#' @importFrom LICORS kmeanspp
get_kmeans <- function(dat, x, y, groups = NULL, nk = 20) {
  res <- list(kmeans = list())

  dat2 <- dat
  if (!is.null(groups))
    dat2 <- select(dat, one_of(c(groups, x, y)))

  groups <- setdiff(names(dat2), c(x, y))

  tmp <- tidyr::spread_(dat2, x, y, sep = "_")

  X <- as.matrix(select(tmp, setdiff(seq_len(ncol(tmp)), one_of(groups))))
  gps <- select(tmp, one_of(groups))

  wss <- rep(NA, nk)
  wss[1] <- (nrow(X) - 1) * sum(apply(X, 2, var))
  for (k in 2:nk) {
    message("k = ", k)
    res$kmeans[[k]] <- LICORS::kmeanspp(X, k)
    wss[k] <- sum(res$kmeans[[k]]$withinss)
  }
  res$wss <- wss
  res$group_vars <- groups
  res$groups <- gps
  res$dat <- dat
  res$x <- x
  res$y <- y
  class(res) <- c("sc_kmeans", "list")

  res
}

#' Scree plot
#'
#' @param x object obtained from \code{\link{get_kmeans}}
#' @param log plot the y-axis on the log scale?
#' @export
plot_scree <- function(x, log = FALSE) {
  if (!inherits(x, "sc_kmeans"))
    stop("Expecting 'x' to be an object from get_kmeans()", call. = FALSE)

  kres <- data_frame(k = seq(length(x$wss)), wss = x$wss)
  p <- rbokeh::figure(
    xlab = "Number of Clusters",
    ylab = "Within groups sum of squares") %>%
    rbokeh::ly_points(k, wss, data = kres, hover = kres)
  if (log)
    p <- y_axis(p, log = TRUE)

  p
}

# internal
join_data_with_cluster <- function(x, k) {
  gps <- x$groups

  clust_meds <- as.numeric(apply(x$kmeans[[k]]$centers, 1, function(x) median(x, na.rm = TRUE)))

  gps$cluster <- x$kmeans[[k]]$cluster
  gps$cluster_name <- rank(clust_meds)[x$kmeans[[k]]$cluster]
  gps$size <- x$kmeans[[k]]$size[x$kmeans[[k]]$cluster]
  gps$id <- seq_len(nrow(gps))

  suppressMessages(inner_join(x$dat, gps))
}

#' Plot clustered series
#'
#' @param x object obtained from \code{\link{get_kmeans}}
#' @param k number of clusters to plot
#' @param centroid should centroids be overlaid?
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param \ldots additional parameters passed on to \code{\link{xyplot}}
#' @export
#' @importFrom tidyr unnest
plot_clust <- function(x, k, centroid = TRUE, xlab = NULL, ylab = NULL, ...) {

  dat <- join_data_with_cluster(x, k) %>%
    rename_(.dots = c(setNames(x$x, "x"), setNames(x$y, "y")))

  clustdat <- dat %>%
    dplyr::select(one_of(c("x", "y", "cluster", "cluster_name", "size", "id"))) %>%
    dplyr::mutate(cluster2 = sprintf("%d (n=%d)", cluster_name, size))

  xrange <- range(clustdat$x, na.rm = TRUE)
  yrange <- range(clustdat$y, na.rm = TRUE)
  xlim <- xrange + c(-1, 1) * diff(xrange) * 0.07
  ylim <- yrange + c(-1, 1) * diff(yrange) * 0.07
  sizemax <- max(clustdat$size)

  lattice::xyplot(y ~ x | factor(cluster_name), groups = id, data = clustdat,
    type = c("l", "g"), alpha = 0.3, col = "black",
    panel = function(x, y, subscripts, groups, ...) {
      txt <- clustdat$cluster2[subscripts][1]
      sz <- clustdat$size[subscripts][1]
      lattice::panel.rect(xrange[1], yrange[1],
        xrange[2], yrange[1] + diff(yrange) * sz / sizemax,
        border = NA, col = "#dddddd", alpha = 0.5)
      lattice::panel.xyplot(x, y, subscripts = subscripts, groups = groups, ...)
      lattice::panel.abline(h = 0, lty = 2, alpha = 0.5)
      if (centroid) {
        tmp <- dplyr::data_frame(x = x, y = y) %>%
          dplyr::group_by(x) %>%
          dplyr::summarise(y = median(y, na.rm = TRUE))
        lattice::panel.lines(tmp$x, tmp$y, col = "red", lwd = 3, alpha = 0.5)
      }
      lattice::panel.text(xrange[1], yrange[2], txt, cex = 0.7, pos = 4)
    },
    as.table = TRUE,
    strip = FALSE,
    between = list(x = 0.25, y = 0.25),
    xlim = xlim,
    ylim = ylim,
    xlab = ifelse(is.null(xlab), x$x, xlab),
    ylab = ifelse(is.null(ylab), x$y, ylab),
    ...
  )
}

# get_centroid_data <- function(x, k) {
#   tmp <- dplyr::data_frame(x = x, y = y) %>%
#     dplyr::group_by(x) %>%
#     dplyr::summarise(y = median(y, na.rm = TRUE))

# }

# plot_centroids <- function(dat, km) {

#   centdat <- map(seq(length(gp1_km$kmeans[[25]]$withinss)),
#     ~ data_frame(pc_ratio = km$centers[.x, ], p = ppoints(50),
#       cluster = .x)) %>%
#     bind_rows()

# }

