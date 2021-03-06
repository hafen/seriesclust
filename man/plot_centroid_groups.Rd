% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster.R
\name{plot_centroid_groups}
\alias{plot_centroid_groups}
\title{Compute the centroids for a given clustering}
\usage{
plot_centroid_groups(cents, heat, xlab = NULL, ylab = NULL,
  mod = identity)
}
\arguments{
\item{cents}{object obtained from \code{\link{get_centroid_data}}}

\item{heat}{object obtained from \code{\link{plot_heat}}}

\item{xlab, ylab}{axis labels (if NULL, will use variable name)}

\item{mod}{a function that can modify the resulting ggplot before it is printed}
}
\description{
Compute the centroids for a given clustering
}
