# helper function to determine rebalancing dates
get_rebalance_dates <- function(dates, reb_freq, k = 1){
  # for now, we rebalance at the end of period in reb_freq
  # reb_freq must be one of the following:
  # "days", "weeks", "months", "quarters", and "years"
  return(dates[xts::endpoints(dates, on = reb_freq, k)])
}

# helper function to calculate compounded daily returns from initial allocation
daily_ret_calc <- function(w, R){
  # calculate daily portfolio returns based on begining of period weights
  # inputs:
  # w: n_assets x 1 vector of initial weights
  # R: n_periods x n_assets matrix of returns
  # where N = number of stocks, T = number of days

  n_assets <- ncol(R)
  n_periods <- nrow(R)

  PortMatrix <- matrix(0, nrow = n_periods + 1, ncol = n_assets)

  # Assume initial position in each stock equals the initial weight
  PortMatrix[1, 1:n_assets] <- w
  PortMatrix[2:nrow(PortMatrix), 1:n_assets] <- 1 + R
  PortMatrix <- apply(PortMatrix, 2, cumprod)
  port_notional <- rowSums(PortMatrix[, 1:n_assets])
  return(port_notional[2:length(port_notional)]/port_notional[1:(length(port_notional) - 1)] - 1)
}

# helper function to determine ivy portfolio weights on one date
ivy_port_weights <- function(w_0, prices, prices_MA){

  # initial allocation is w_0 (equally weighted)
  # if current price of an asset < moving average, shift that allocation to cash
  w_0[which(prices < prices_MA)] <- 0
  return(w_0)
}


backtest_allocation <- function(strat, R, riskfree_returns = 0){

  # some checks
  rf_len <- length(riskfree_returns)
  if (rf_len > 1){
    if (rf_len != nrow(R)){
      stop("riskfree_returns must be the same length as the number of rows in R")
    }
  }

  # get dates, number of assets, rebalancing dates
  dates <- index(R)
  n_assets = length(strat$tickers)
  rebal_dates <- get_rebalance_dates(dates, strat$rebalance_frequency)


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
    strat_returns[dates_between] <- daily_ret_calc(weights[i_date], R[dates_between,])
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


