#' @export
#' @import xts
#' @importFrom PerformanceAnalytics table.AnnualizedReturns
#' @importFrom PerformanceAnalytics table.DownsideRiskRatio
#' @importFrom PerformanceAnalytics table.DownsideRisk
backtest_allocation <- function(strat, P, R, riskfree_returns = 0){

  # some checks
  rf_len <- length(riskfree_returns)
  if (rf_len > 1){
    if (rf_len != nrow(R)){
      stop("riskfree_returns must be the same length nrows(R).")
    }
  } else {
    riskfree_returns <- xts(rep(riskfree_returns, nrow(R)),
                            order.by = index(P))
  }

  # check if R is an xts object. If not, throw error
  if (!any(class(R)=="xts")){
    stop("R must be an xts object.")
  }

  # check if P is an xts object. If not, throw error
  if (!any(class(P)=="xts")){
    stop("P must be an xts object.")
  }

  # check dimensions of R and P match
  if (any(dim(R) != dim(P))){
    stop("Dimensions of P and R don't match.")
  }

  # check that P contains columns matching the tickers in strat
  n_assets <- length(strat$tickers)
  for (i in 1:n_assets){
    if (!(strat$tickers[i] %in% colnames(P))){
      stop(paste0("Ticker ", strat$tickers[i], " not found in P"))
    }
  }
  # check that R contains columns matching the tickers in strat
  n_assets <- length(strat$tickers)
  for (i in 1:n_assets){
    if (!(strat$tickers[i] %in% colnames(R))){
      stop(paste0("Ticker ", strat$tickers[i], " not found in R"))
    }
  }

  # check if user provided params. If not, initialize
  if (!"params" %in% names(strat)){
    strat$params <- list()
  }

  # get dates, number of assets, rebalancing dates
  dates <- index(R)

  rebal_dates <- get_rebalance_dates(dates, strat$rebalance_frequency)

  # starting date
  R <- R[, strat$tickers]
  P <- P[, strat$tickers]
  first_date <- dates[which.max((!is.na(rowSums(R))))]
  rebal_dates <- rebal_dates[rebal_dates >= first_date]

  # figure out allocations on rebal_dates
  weights <- xts(matrix(0, length(rebal_dates), n_assets),
                 order.by = rebal_dates)

  for (i_date in seq(from = 1, to = length(rebal_dates))){
    this_reb_date <- rebal_dates[i_date]
    weights[i_date, ] <- strat$portfolio_rule_fn(strat,
                                                 this_reb_date,
                                                 P,
                                                 R)
  }

  # calculation of daily returns
  strat_returns <- xts(rep(NA, length(dates)), order.by = dates)
  colnames(strat_returns) <- make.names(strat$name)

  for (i_date in seq(from = 1, to = length(rebal_dates)-1)){
    # find dates between this and the next rebalance date
    dates_between <- dates[dates > rebal_dates[i_date] &
                           dates <= rebal_dates[i_date+1]]

    weight_risk_assets <- sum(weights[i_date])
    weight_risk_free <- 1 - weight_risk_assets
    risk_free_account <- cumprod(c(weight_risk_free,
                            1 + riskfree_returns[dates_between]))
    risk_account <- daily_account_calc(weights[i_date],
                                        R[dates_between,])
    total_account <- risk_free_account + risk_account
    strat_returns[dates_between] <- total_account[2:length(total_account)]/
                                    total_account[1:length(total_account) -1 ] - 1

  }
  strat_returns <- strat_returns[paste0(as.character(first_date), "/")]
  riskfree_returns <- riskfree_returns[paste0(as.character(first_date), "/")]

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
