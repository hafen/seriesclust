#' Plot heat map of variable distributions within each cluster
#'
#' @param x object obtained from \code{\link{get_kmeans}}
#' @param k number of clusters to use
#' @param col name of variable to plot (either categorical or sliced numeric variable in the data)
#' @param cutoff minimum number of series that must be present for a level of the variable to be included in the plot
#' @param color a vector of colors or a color ramp function
#' @param xaxis_height,yaxis_width size of axes, in pixels (ignored if \code{interactive = FALSE})
#' @param \ldots parameters passed on to \code{\link[d3heatmap]{d3heatmap}} or \code{\link[pheatmap]{pheatmap}}
#' @param interactive if \code{TRUE}, \code{\link[d3heatmap]{d3heatmap}} will be used, else \code{\link[pheatmap]{pheatmap}}
#' @export
#' @example man-roxygen/ex-clust.R
#' @importFrom tidyr crossing_
#' @importFrom d3heatmap d3heatmap
#' @importFrom pheatmap pheatmap
#' @importFrom viridis magma
plot_heat <- function(x, k, col, cutoff = 1, color = viridis::magma,
  xaxis_height = NULL, yaxis_width = NULL, interactive = TRUE, ...) {

  dat <- join_data_with_cluster(x, k)

  clmns <- c(col, "cluster_name")

  tmp <- dat %>%
    dplyr::group_by_(.dots = union(clmns, x$group_vars)) %>%
    dplyr::mutate(denom = n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_(.dots = clmns) %>%
    dplyr::summarise(n = n() / denom[1]) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_(col) %>%
    dplyr::mutate_(pct_cluster = ~ 100 * n / sum(n)) %>%
    dplyr::arrange_(col)

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

    d3heatmap::d3heatmap(round(mt, 1), colors = color, cellnote = mt2,
      yaxis_width = yaxis_width, xaxis_height = xaxis_height, ...)
  } else {
    pheatmap::pheatmap(mt,
      color = color,
      annotation_colors = "red",
      border_color = "#ffffff50", ...)
  }
}
