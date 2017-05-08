#' Get a set k-means++ clusters for a collection of series
#'
#' @param dat data frame
#' @param x string name of x variable
#' @param y string name of y variable
#' @param groups a character vector of grouping variable names - if not specified, all columns that are not \code{x} or \code{y} will be treated as grouping variables
#' @param k vector of number of clusters to run through
#' @export
#' @example man-roxygen/ex-clust-norun.R
#' @importFrom purrr by_row
#' @importFrom LICORS kmeanspp
get_kmeans <- function(dat, x, y, groups = NULL, k = 2:20) {
  res <- list(kmeans = list())

  dat2 <- dplyr::ungroup(dat)
  if (!is.null(groups)) {
    dat2 <- select(dat, one_of(c(groups, x, y)))
  } else {
    groups <- setdiff(names(dat2), c(x, y))
    message("Using groups: ", paste(groups, collapse = ", "))
  }

  tmp <- tidyr::spread_(dat2, x, y, sep = "_")

  X <- as.matrix(select(tmp, setdiff(seq_len(ncol(tmp)), one_of(groups))))
  gps <- select(tmp, one_of(groups))

  wss <- rep(NA, length(k))
  wss[1] <- (nrow(X) - 1) * sum(apply(X, 2, var))
  for (ii in seq_along(k)) {
    ck <- k[ii]
    message("k = ", ck)
    res$kmeans[[ck]] <- LICORS::kmeanspp(X, ck)
    wss[ii] <- sum(res$kmeans[[ck]]$withinss)
  }
  res$k <- k
  res$wss <- wss
  res$group_vars <- groups
  res$groups <- gps
  res$dat <- dat
  res$x <- x
  res$y <- y
  class(res) <- c("sc_kmeans", "list")

  res
}

# df_to_clust_matrix <- function(dat, x, y, groups = NULL) {
# }

#' Scree plot
#'
#' @param x object obtained from \code{\link{get_kmeans}}
#' @param log plot the y-axis on the log scale?
#' @example man-roxygen/ex-clust-norun.R
#' @export
plot_scree <- function(x, log = FALSE) {
  if (!inherits(x, "sc_kmeans"))
    stop("Expecting 'x' to be an object from get_kmeans()", call. = FALSE)

  kres <- data_frame(k = x$k, wss = x$wss)
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

  if (is.null(x$kmeans[[k]]))
    stop("Must use k in ", paste(x$k, collapse = ", "), call. = FALSE)

  clust_meds <- as.numeric(apply(x$kmeans[[k]]$centers, 1, function(x) median(x, na.rm = TRUE)))

  gps$cluster <- x$kmeans[[k]]$cluster
  gps$cluster_name <- rank(clust_meds, ties.method = "first")[x$kmeans[[k]]$cluster]
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
#' @param alpha alpha to use for lines
#' @param use_median should median be used instead of mean to compute the centroid?
#' @param \ldots additional parameters passed on to \code{\link{xyplot}}
#' @export
#' @example man-roxygen/ex-clust-norun.R
#' @importFrom tidyr unnest
plot_clust <- function(x, k, centroid = TRUE, xlab = NULL, ylab = NULL, alpha = 0.3,
  use_median = FALSE, ...) {

  dat <- join_data_with_cluster(x, k) %>%
    rename_(.dots = c(setNames(x$x, "x"), setNames(x$y, "y")))

  clustdat <- dat %>%
    dplyr::select(one_of(c("x", "y", "cluster", "cluster_name", "size", "id"))) %>%
    dplyr::mutate(cluster2 = paste0(cluster_name, " (n=", size, ")"))
    # %>%
    # group_by(id) %>% do({
    #   tmp <- utils::tail(., 1); tmp$x <- NA; tmp$y <- NA
    #   rbind(., tmp)
    # })

  xrange <- range(clustdat$x, na.rm = TRUE)
  yrange <- range(clustdat$y, na.rm = TRUE)
  xlim <- xrange + c(-1, 1) * diff(xrange) * 0.07
  ylim <- yrange + c(-1, 1) * diff(yrange) * 0.07
  sizemax <- max(clustdat$size)

  lattice::xyplot(y ~ x | factor(cluster_name), groups = id, data = clustdat,
    type = c("l", "g"), alpha = alpha, col = "black",
    panel = function(x, y, subscripts, groups, ...) {
      txt <- clustdat$cluster2[subscripts][1]
      sz <- clustdat$size[subscripts][1]
      lattice::panel.rect(as.numeric(xrange[1]), yrange[1],
        as.numeric(xrange[2]), yrange[1] + diff(yrange) * sz / sizemax,
        border = NA, col = "#dddddd", alpha = 0.5)
      lattice::panel.xyplot(x, y, subscripts = subscripts, groups = groups, ...)
      lattice::panel.abline(h = 0, lty = 2, alpha = 0.5)
      if (centroid) {
        fn <- mean
        if (use_median)
          fn <- median
        tmp <- dplyr::data_frame(x = x, y = y) %>%
          dplyr::group_by(x) %>%
          dplyr::summarise(y = fn(y, na.rm = TRUE))
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

#' Compute the centroids for a given clustering
#'
#' @param x object obtained from \code{\link{get_kmeans}}
#' @param k number of clusters to use
#' @param use_median should median be used instead of mean to compute the centroid?
#' @export
get_centroid_data <- function(x, k, use_median = FALSE) {
  dat <- join_data_with_cluster(x, k) %>%
    rename_(.dots = c(setNames(x$x, "x"), setNames(x$y, "y"))) %>%
    select_("x", "y", "cluster", "cluster_name")

  fn <- ifelse(use_median, median, mean)

  res <- dat %>%
    dplyr::group_by_("cluster", "cluster_name", "x") %>%
    dplyr::summarise(y = fn(y, na.rm = TRUE)) %>%
    rename_(.dots = c(setNames("x", x$x), setNames("y", x$y))) %>%
    dplyr::ungroup()

  attr(res, "xyvars") <- list(x = x$x, y = x$y)
  class(res) <- c("sc_centroids", class(res))

  res
}

#' Compute the centroids for a given clustering
#'
#' @param cents object obtained from \code{\link{get_centroid_data}}
#' @param heat object obtained from \code{\link{plot_heat}}
#' @param xlab,ylab axis labels (if NULL, will use variable name)
#' @param mod a function that can modify the resulting ggplot before it is printed
#' @export
plot_centroid_groups <- function(cents, heat, xlab = NULL, ylab = NULL, mod = identity) {
  if (!inherits(cents, "sc_centroids"))
    stop("Argument 'cents' must come from get_centroid_data()")
  if (!inherits(heat, "pheatmap_plot"))
    stop("Argument 'heat' must come from plot_heat(..., interactive = FALSE, ...)")

  gps <- heat$cluster_groups
  if (is.null(gps))
    stop("Argument 'heat' must come from plot_heat() with 'annotation_labs' set")

  if (length(unique(cents$cluster_name)) != nrow(gps))
    stop("Arguments 'cents' and 'heat' must come from objects with the same number of centroids")

  if (is.integer(cents$cluster_name))
    gps$cluster_name <- as.integer(gps$cluster_name)
  dat <- suppressMessages(dplyr::left_join(cents, gps))

  xyvars <- attr(cents, "xyvars")

  if (is.null(xlab))
    xlab <- xyvars$x
  if (is.null(ylab))
    ylab <- xyvars$y

  tail1 <- function(x) utils::tail(x, 1)
  dat_lab <- data.frame(dat) %>%
    dplyr::group_by(cluster_name) %>%
    dplyr::summarise_all(tail1)

  rng <- range(dat[[xyvars$x]], na.rm = TRUE)
  drng <- diff(rng)
  p <- ggplot2::ggplot(dat, ggplot2::aes_string(
    x = xyvars$x, y = xyvars$y, group = "cluster_name")) +
    ggplot2::geom_line(size = 1, alpha = 0.9) +
    ggrepel::geom_label_repel(ggplot2::aes_string(x = xyvars$x, y = xyvars$y,
      group = "cluster_name", label = "cluster_name"), data = dat_lab, nudge_x = 0.08 * drng,
      segment.color = ggplot2::alpha("black", 0.3), size = 4)  +
    # ggplot2::geom_text(ggplot2::aes_string(x = xyvars$x, y = xyvars$y, group = "cluster_name",
    #   label = "cluster_name"), data = dat_lab, nudge_x = 0.05 * drng, size = 6)  +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::facet_wrap("group", nrow = 1) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::xlim(rng + c(-0.04, 0.12) * drng)

  p <- mod(p)

  ## stuff to color strip labels according to group
  dm <- ggplot2::ggplot(dat, ggplot2::aes_string(
    x = xyvars$x, y = xyvars$y, group = "cluster_name")) +
    ggplot2::facet_wrap("group", nrow = 1) +
    ggplot2::geom_rect(ggplot2::aes_string(fill = "group"),
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    ggplot2::scale_fill_manual(values = tableau10) +
    ggplot2::theme_minimal(base_size = 18)

  g1 <- ggplot2::ggplotGrob(p)
  g2 <- ggplot2::ggplotGrob(dm)

  gtable_select <- function (x, ...) {
    matches <- c(...)
    x$layout <- x$layout[matches,, drop = FALSE] # nolint
    x$grobs <- x$grobs[matches]
    x
  }

  panels <- grepl(pattern = "panel", g2$layout$name)
  strips <- grepl(pattern = "strip-t", g2$layout$name)
  g2$layout$t[panels] <- g2$layout$t[panels] - 1
  g2$layout$b[panels] <- g2$layout$b[panels] - 1

  new_strips <- gtable_select(g2, panels | strips)
  # grid.newpage()
  # grid.draw(new_strips)

  gtable_stack <- function(g1, g2) {
    g1$grobs <- c(g1$grobs, g2$grobs)
    g1$layout <- transform(g1$layout, z = z - max(z), name = "g2")
    g1$layout <- rbind(g1$layout, g2$layout)
    g1
  }

  new_plot <- gtable_stack(g1, new_strips)
  grid::grid.newpage()
  grid::grid.draw(new_plot)
}

# plot_centroids <- function(dat, km) {

#   centdat <- map(seq(length(gp1_km$kmeans[[25]]$withinss)),
#     ~ data_frame(pc_ratio = km$centers[.x, ], p = ppoints(50),
#       cluster = .x)) %>%
#     bind_rows()

# }
