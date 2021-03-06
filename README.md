# seriesclust

A few simple tools to cluster series of data and investigate the cluster results with respect to cluster membership across other variables in the data.

## Installation

You can install seriesclust from github with:

```r
# install.packages("devtools")
devtools::install_github("hafen/seriesclust")
```

## Example

```r
library(seriesclust)
library(dplyr)

# scale the monthly median close price so that we are clustering on general shape
d <- nasd16 %>%
  group_by(symbol) %>%
  mutate(close_scl = as.numeric(scale(med_close))) %>%
  select(-company, -med_close)

set.seed(1234)
# k-means clustering with 2, 5, and 9 clusters
km <- get_kmeans(d, x = "month", y = "close_scl", k = c(2, 5, 9))
plot_scree(km)
plot_clust(km, 9)
plot_heat(km, 9, col = "sector")
plot_heat(km, 9, col = "industry", cutoff = 10)
```
