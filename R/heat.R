#' Plot heat map of variable distributions within each cluster
#'
#' @param x object obtained from \code{\link{get_kmeans}}
#' @param k number of clusters to use
#' @param col name of variable to plot (either categorical or sliced numeric variable in the data)
#' @param cutoff minimum number of series that must be present for a level of the variable to be included in the plot
#' @param color a vector of colors or a color ramp function
#' @param xaxis_height,yaxis_width size of axes, in pixels (ignored if \code{interactive = FALSE})
#' @param interactive if \code{TRUE}, \code{\link[d3heatmap]{d3heatmap}} will be used, else \code{\link[pheatmap]{pheatmap}}
#' @param annotation_labs a vector of labels to use to annotate column groupings if using \code{\link[pheatmap]{pheatmap}}
#' @param \ldots parameters passed on to \code{\link[d3heatmap]{d3heatmap}} or \code{\link[pheatmap]{pheatmap}}
#' @export
#' @example man-roxygen/ex-clust.R
#' @importFrom tidyr crossing_
#' @importFrom d3heatmap d3heatmap
#' @importFrom pheatmap pheatmap
#' @importFrom viridis magma
#' @importFrom stats cutree
plot_heat <- function(x, k, col, cutoff = 1, color = viridis::magma,
  xaxis_height = NULL, yaxis_width = NULL, interactive = TRUE,
  annotation_labs = NULL, ...) {

  dat <- join_data_with_cluster(x, k)
  clmns <- c(col, "cluster_name")

  # if (!(scale %in% c("row", "column")))
  #   stop("scale argument should take values 'row' or 'column'")
  # scale_var <- ifelse(scale == "row", col, "cluster_name")

  tmp <- dat %>%
    dplyr::group_by_(.dots = union(clmns, x$group_vars)) %>%
    dplyr::mutate(denom = n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_(.dots = clmns) %>%
    dplyr::summarise(n = n() / denom[1]) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_(col) %>%
    dplyr::mutate_(pct_cluster = ~ 100 * n / sum(n)) %>%
    dplyr::arrange_(col, "cluster_name")

  tmp2 <- tidyr::crossing_(as.list(tmp[clmns]))
  tmp <- suppressMessages(dplyr::left_join(tmp2, tmp)) %>%
    dplyr::mutate_(val = col) %>%
    dplyr::group_by_(col) %>%
    dplyr::mutate(
      nn = sum(n, na.rm = TRUE),
      pct_cluster = ifelse(is.na(pct_cluster), 0, pct_cluster),
      val = paste0(val, " (n=", nn, ")")) %>%
    dplyr::arrange_(.dots = rev(clmns)) %>%
    dplyr::filter(nn >= cutoff)

  mt <- matrix(tmp$pct_cluster, nrow = length(unique(tmp[[col]])))
  rownames(mt) <- as.character(unique(tmp$val))
  colnames(mt) <- as.character(unique(tmp$cluster_name))

  if (is.function(color))
    color <- color(100)

  mt2 <- matrix(paste0(tmp$n, " of ", tmp$nn, " '", tmp[[col]], "' series (",
    round(tmp$pct_cluster, 1), "%) are in cluster ",
    tmp$cluster_name), nrow = length(unique(tmp[[col]])))

  if (interactive) {
    if (is.null(yaxis_width)) {
      mnc_row <- max(nchar(rownames(mt)))
      yaxis_width <- mnc_row ^ (1 / 3) * 100
    }

    if (is.null(xaxis_height)) {
      mnc_col <- max(nchar(colnames(mt)))
      xaxis_height <- mnc_col * 40
    }

    res <- d3heatmap::d3heatmap(round(mt, 1), colors = color, cellnote = mt2,
      yaxis_width = yaxis_width, xaxis_height = xaxis_height, ...)
  } else {
    pars <- list(...)
    if (is.null(pars$annotation_colors))
      pars$annotation_colors <- "red"
    if (is.null(pars$border_color))
      pars$border_color <- "#ffffff50"
    if (is.null(pars$number_color))
      pars$number_color <- "#ffffff50"
    if (is.null(pars$number_format))
      pars$number_format <- "%.0f%%"
    if (is.null(pars$color))
      pars$color <- color
    if (is.null(pars$xlab))
      pars$xlab <- "cluster"
    if (is.null(pars$ylab))
      pars$ylab <- col
    pars$silent <- TRUE
    pars$mat <- mt
    obj <- do.call(pheatmap::pheatmap, pars)
    pars$silent <- FALSE

    cluster_groups <- NULL
    if (!is.null(annotation_labs)) {
      if (is.null(pars$cutree_cols)) {
        message("Cannot use 'annotation_labs' if 'cutree_cols' is not specified")
      } else if (length(annotation_labs) == pars$cutree_cols) {
        tmp <- cutree(obj$tree_col, pars$cutree_cols)
        # roundabout way to reorder the tree indices to the order they appear in the plot
        tmp <- tmp[as.character(obj$tree_col$order)]
        tmp2 <- as.integer(factor(tmp, levels = unique(tmp)))
        acol <- data.frame(group = annotation_labs[tmp2], stringsAsFactors = FALSE)
        rownames(acol) <- names(tmp)
        pars$annotation_col <- acol
        tableau10 <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
          "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")
        cols <- tableau10[(seq_along(annotation_labs) - 1) %% 10 + 1]
        names(cols) <- annotation_labs
        pars$annotation_colors <- list(group = cols)

        cluster_groups <- data.frame(
          group = acol$group,
          cluster_name = rownames(acol),
          stringsAsFactors = FALSE)
      } else {
        message(
          "Did not add annotations because 'annotation_labs' ",
          "was not the same length as 'cutree_cols'")
      }
    }

    res <- list(obj = obj, pars = pars, cluster_groups = cluster_groups)
    class(res) <- c("pheatmap_plot", class(res))
  }
  res
}

#' Print trellis plot to servr
#'
#' @param x pheatmap_plot
#' @param \ldots additional parameters
#' @importFrom grid grid.newpage grid.draw
#' @export
print.pheatmap_plot <- function(x, ...) {
  ww <- hh <- 1
  xlab <- x$pars$xlab
  ylab <- x$pars$ylab
  x$pars$xlab <- NULL
  x$pars$ylab <- NULL
  setHook("grid.newpage", function()
    grid::pushViewport(grid::viewport(x = 1, y = 1,
      width = ifelse(is.null(ylab), 1, 0.95),
      height = ifelse(is.null(xlab), 1, 0.95),
      name = "vp", just = c("right", "top"))),
    action = "prepend")
  do.call(pheatmap::pheatmap, x$pars)
  setHook("grid.newpage", NULL, "replace")
  if (!is.null(xlab))
    grid::grid.text(xlab, y = -0.02, gp = grid::gpar(fontsize = 14))
  if (!is.null(ylab))
    grid::grid.text(ylab, x = -0.02, rot = 90, gp = grid::gpar(fontsize = 14))
}
