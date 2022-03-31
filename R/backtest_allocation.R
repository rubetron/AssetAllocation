#' @export
#' @import xts
#' @importFrom PerformanceAnalytics table.AnnualizedReturns
#' @importFrom PerformanceAnalytics table.DownsideRiskRatio
#' @importFrom PerformanceAnalytics table.DownsideRisk
backtest_allocation <- function(strat, R, riskfree_returns = 0){

  # some checks
  rf_len <- length(riskfree_returns)
  if (rf_len > 1){
    if (rf_len != nrow(R)){
      stop("riskfree_returns must be the same length as the number of rows in R.")
    }
  }

  # check if R is an xts object. If not, throw error
  if (!any(class(R)=="xts")){
    stop("R must be an xts object.")
  }

  # check that R contains columns matching the tickers in strat
  n_assets <- length(strat$tickers)
  for (i in 1:n_assets){
    if (!(strat$tickers[i] %in% colnames(R))){
      stop(paste0("Ticker ", strat$tickers[i], " not found in R"))
    }
  }

  # get dates, number of assets, rebalancing dates
  dates <- index(R)

  rebal_dates <- get_rebalance_dates(dates, strat$rebalance_frequency, )

  # starting date
  returns_strat <- R[, strat$tickers]
  first_date <- dates[which.max((!is.na(rowSums(returns_strat))))]
  rebal_dates <- rebal_dates[rebal_dates >= first_date]

  # figure out allocations on rebal_dates
  weights <- xts(matrix(0, length(rebal_dates), n_assets),
                 order.by = rebal_dates)

  for (i_date in 1:length(rebal_dates)){
    switch(strat$portfolio_rule_fn,
           identity = {weights[i_date, ] <- strat$default_weights}
    )
  }

  # calculation of daily returns
  strat_returns <- xts(rep(NA, length(dates)), order.by = dates)
  colnames(strat_returns) <- make.names(strat$name)

  for (i_date in 1:(length(rebal_dates)-1)){
    # find dates between this and the next rebalance date
    dates_between <- dates[dates > rebal_dates[i_date] & dates <= rebal_dates[i_date+1]]
    strat_returns[dates_between] <- daily_ret_calc(weights[i_date], returns_strat[dates_between,])
  }

  # calculate some statistics
  table1 <- table.AnnualizedReturns(strat_returns, Rf = riskfree_returns)
  table2 <- table.DownsideRiskRatio(strat_returns, MAR = mean(riskfree_returns))
  table3 <- table.DownsideRisk(strat_returns, Rf = mean(riskfree_returns))

  table_metrics <- rbind(table1,
                         table2,
                         table3)

  return(list(strat = strat,
              returns = strat_returns,
              table_performance = table_metrics,
              rebalance_dates = rebal_dates,
              rebalance_weights = weights))
}
