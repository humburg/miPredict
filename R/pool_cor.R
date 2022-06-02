
fisher.trans <- function(x) 1/2 * log((1 + x) / (1 - x))
fisher.backtrans <- function(x) (exp(2 * x) - 1) / (exp(2 * x) + 1)
pool_cor <- function(r) r %>% fisher.trans() %>% mean() %>% fisher.backtrans()
