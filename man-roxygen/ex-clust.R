\dontrun{
library(dplyr)

# scale the monthly median close price so that we are clustering on general shape
d <- nasd16 %>%
  group_by(symbol) %>%
  mutate(close_scl = scale(med_close)) %>%
  select(-company, -med_close)

set.seed(1234)
# k-means clustering with 2, 5, and 9 clusters
km <- get_kmeans(d, x = "month", y = "close_scl", k = c(2, 5, 9, 25))
plot_scree(km)
plot_heat(km, 9, col = "sector")
plot_heat(km, 9, col = "sector", interactive = FALSE,
  display_numbers = TRUE, cutree_cols = 3, cutree_rows = 3)
plot_heat(km, 9, col = "industry", cutoff = 20)
}
