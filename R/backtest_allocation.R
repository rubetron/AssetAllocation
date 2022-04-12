#' Backtesting of asset allocation strategies
#'
#' \code{backtest_allocation} computes a backtest of a given portfolio
#' allocation rule.
#'
#' The function first determines the rebalancing dates based
#' on \code{strat$rebalance_frequency}. Then, it cycles through intermediate
#' dates and calculates daily returns based on the allocation.
#'
#' @param strat A list representing an asset allocation strategy.
#' @param P An xts object with daily prices of the tickers in strat.
#' @param R An xts object with daily returns of the tickers in strat.
#' @param risk_free Either an xts object with daily returns of the risk-free
#' asset, or a scalar numeric with the annual risk-free rate in decimals.
#'
#' @examples
#' # Example 1: backtesting one of the asset allocations in the package
#' us_60_40 <- asset_allocations$static$us_60_40
#' bt_us_60_40 <- backtest_allocation(us_60_40,
#'                                   ETFs$Prices,
#'                                   ETFs$Returns,
#'                                   ETFs$risk_free)
#'
#' # show table with performance metrics
#' bt_us_60_40$table_performance


#' # Example 2: creating and backtesting an asset allocation from scratch
#'
#' # create a strategy that invests equally in momentum (MTUM), value (VLUE),
#' # low volatility (USMV) and quality (QUAL) ETFs.
#'
#' factor_strat  <- list(name = "EW Factors",
#'                       tickers = c("MTUM", "VLUE", "USMV", "QUAL"),
#'                       default_weights = c(0.25, 0.25, 0.25, 0.25),
#'                       rebalance_frequency = "month",
#'                       portfolio_rule_fn = constant_weights)
#'
#' # get data for tickers using getSymbols
#' factor_ETFs <- get_data_from_tickers(factor_strat$tickers,
#'                                      starting_date = "2020-01-01")

#' # backtest the strategy
#' bt_factor_strat <- backtest_allocation(factor_strat,
#'                                        factor_ETFs$P,
#'                                        factor_ETFs$R)

#' # show table with performance metrics
#' bt_factor_strat$table_performance
#' @return An object of class \code{"List"} with the following elements:
#' \item{strat}{The strat provided to the function}
#' \item{returns}{An xts object with the daily returns of the strategy}
#' \item{table_performance}{A table with performance metrics}
#' \item{rebalance_dates}{Vector of rebalancing dates}
#' \item{rebalance_weights}{Vector of rebalancing dates}
#' @export
#' @import xts
#' @importFrom PerformanceAnalytics table.AnnualizedReturns
#' @importFrom PerformanceAnalytics table.DownsideRiskRatio
#' @importFrom PerformanceAnalytics table.DownsideRisk
backtest_allocation <- function(strat, P, R, risk_free = 0){

  # some checks
  rf_len <- length(risk_free)
  if (rf_len > 1){
    if (rf_len != nrow(R)){
      stop("risk_free must be the same length nrows(R).")
    }
  } else {
    risk_free <- xts(rep(risk_free/252, nrow(R)),
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

  # check if portfolio rule different from identity.
  # in this case, increase start date by one year
  if (!any(grepl("identity", deparse(strat$portfolio_rule_fn)))){
    first_date <- dates[which.max((!is.na(rowSums(R)))) + 252]
  } else{
    first_date <- dates[which.max((!is.na(rowSums(R))))]
  }

  rebal_dates <- rebal_dates[rebal_dates >= first_date]

  # figure out allocations on rebal_dates
  weights <- xts(matrix(0, length(rebal_dates), n_assets),
                 order.by = rebal_dates)
  colnames(weights) <- strat$tickers

  for (i_date in seq(from = 1, to = length(rebal_dates))){
    this_reb_date <- rebal_dates[i_date]
    weights[i_date, ] <- strat$portfolio_rule_fn(strat,
                                                 this_reb_date,
                                                 P,
                                                 R,
                                                 risk_free)
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
                            1 + risk_free[dates_between]))
    risk_account <- daily_account_calc(weights[i_date],
                                        R[dates_between,])
    total_account <- risk_free_account + risk_account
    strat_returns[dates_between] <- total_account[2:length(total_account)]/
                                    total_account[1:length(total_account) -1 ] - 1

  }
  strat_returns <- strat_returns[paste0(as.character(first_date), "/")]
  risk_free <- risk_free[paste0(as.character(first_date), "/")]

  # calculate some statistics
  table1 <- table.AnnualizedReturns(strat_returns, Rf = risk_free)
  table2 <- table.DownsideRiskRatio(strat_returns, MAR = mean(risk_free))
  table3 <- table.DownsideRisk(strat_returns, Rf = mean(risk_free))

  table_metrics <- rbind(table1,
                         table2,
                         table3)

  return(list(strat = strat,
              returns = strat_returns,
              table_performance = table_metrics,
              rebalance_dates = rebal_dates,
              rebalance_weights = weights))
}
