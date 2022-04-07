\name{backtest_allocation}
\alias{backtest_allocation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{backtest_allocation
}
\description{
This function computes a backtest of a given portfolio allocation rule.
}
\usage{
backtest_allocation(strat, R, riskfree_returns = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{strat}{A list describing an asset allocation rule with the fields: name, tickers, default_weights, rebalance_frequency, and portfolio_rule_fn. Refer to the data set basic_asset_alloc for examples}
  \item{R}{An xts matrix of returns. The names of the columns must match the tickers in strat.}
  \item{riskfree_returns}{An xts vector of returns for the risk-free asset. If absent, a risk-free rate of zero is applied to calculate performance metrics.}
}
\details{
The function first determines the rebalancing dates based on strat$rebalance_frequency. Then, it cycles through intermediate dates and calculates daily returns based on the allocation.
}
\value{
A list with the following elements:
\item{strat}{The strat provided to the function}
\item{returns}{An xts object with the daily returns of the strategy}
\item{table_performance}{A table with performance metrics}
\item{rebalance_dates}{Vector of rebalancing dates}
\item{rebalance_weights}{Vector of rebalancing dates}

}
\author{
Alexandre Rubesam
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

## Example 1: backtesting one of the asset allocations in the package
us_60_40 <- basic_asset_alloc$us_60_40

# test using the data set provided in the package
bt_us_60_40 <- backtest_allocation(us_60_40, ETFs_daily)

# show table with performance metrics
bt_us_60_40$table_performance


## Example 2: creating and backtesting an asset allocation from scratch

# create a strategy that invests equally in momentum (MTUM), value (VLUE),
# low volatility (USMV) and quality (QUAL) ETFs.

factor_strat  <- list(name = "EW Factors",
                      tickers = c("MTUM", "VLUE", "USMV", "QUAL"),
                      default_weights = c(0.25, 0.25, 0.25, 0.25),
                      rebalance_frequency = "month",
                      portfolio_rule_fn = "constant_weights")

# get data for tickers using getSymbols
returns_ETFs <- get_return_data_from_tickers(factor_strat$tickers, starting_date = "2020-01-01")

# backtest the strategy
bt_factor_strat <- backtest_allocation(factor_strat,
                                       returns_ETFs)

# show table with performance metrics
bt_factor_strat$table_performance
}
