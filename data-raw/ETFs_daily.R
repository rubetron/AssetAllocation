library(quantmod)
library(PerformanceAnalytics)


allocation_names <- names(basic_asset_alloc)

tickers <- character()
for (i in 1:length(basic_asset_alloc)){
  tickers <- c(tickers, basic_asset_alloc[[allocation_names[i]]]$tickers)
}

tickers <- unique(tickers)

ETFs_daily <- get_return_data_from_tickers(tickers,
                                           starting_date = "2007-06-01")

usethis::use_data(ETFs_daily, overwrite = TRUE)


