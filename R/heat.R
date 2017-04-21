#' Plot heat map of variable distributions within each cluster
#'
#' @param x object obtained from \code{\link{get_kmeans}}
#' @param k number of clusters to use
#' @param col name of variable to plot (either categorical or sliced numeric variable in the data)
#' @param cutoff minimum number of series that must be present for a level of the variable to be included in the plot
#' @param cluster_rows should the rows be clustered in the \code{\link[d3heatmap]{d3heatmap}} plot?
#' @param cluster_cols should the columns be clustered in the \code{\link[d3heatmap]{d3heatmap}} plot?
#' @param color a vector of colors or a color ramp function
#' @export
#' @example man-roxygen/ex-clust.R
#' @importFrom tidyr crossing_
#' @importFrom d3heatmap d3heatmap
#' @importFrom viridis magma
plot_heat <- function(x, k, col, cutoff = 1, cluster_rows = TRUE, cluster_cols = TRUE,
  color = viridis::magma) {

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

  mnc_row <- max(nchar(rownames(mt)))
  mnc_col <- max(nchar(colnames(mt)))
# browser()
  d3heatmap::d3heatmap(round(mt, 1), colors = color, cellnote = mt2,
    yaxis_width = mnc_row ^ (1 / 3) * 100, xaxis_height = mnc_col * 40)

  # pheatmap::pheatmap(mt,
  #   color = color,
  #   # color = grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 7, name = "YlOrRd"))(100),
  #   cluster_cols = cluster_cols,
  #   cluster_rows = cluster_rows,
  #   annotation_colors = "red",
  #   border_color = "#ffffff50")
}
