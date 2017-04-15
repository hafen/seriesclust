#' Plot heat map of variable distributions within each cluster
#'
#' @param x object obtained from \code{\link{get_kmeans}}
#' @param col name of variable to plot (either categorical or sliced numeric variable in the data)
#' @param cutoff minimum number of series that must be present for a level of the variable to be included in the plot
#' @param cluster_rows should the rows be clustered in the \code{\link{pheatmap}} plot?
#' @param cluster_cols should the columns be clustered in the \code{\link{pheatmap}} plot?
#' @export
#' @importFrom tidyr crossing_
#' @importFrom pheatmap pheatmap
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
plot_heat <- function(x, col, cutoff = 1, cluster_rows = TRUE, cluster_cols = FALSE) {

  dat <- join_data_with_cluster(x, k)

  clmns <- c(col, "cluster_name")

  tmp <- dat %>%
    dplyr::group_by_(.dots = clmns) %>%
    dplyr::summarise(n = n()) %>%
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

  pheatmap::pheatmap(mt,
    color = grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 7, name = "YlOrRd"))(100),
    cluster_cols = cluster_cols,
    cluster_rows = cluster_rows,
    annotation_colors = "red")
}
