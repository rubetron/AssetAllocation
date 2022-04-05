
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AssetAllocation

<!-- badges: start -->
<!-- badges: end -->

The goal of AssetAllocation is to perform backtesting of customizable
asset allocation strategies. The main function that the user interacts
with is backtest\_allocation().

## Installation

You can install the development version of AssetAllocation from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rubetron/AssetAllocation")
```

## Example

Simple example using preloaded strategy:

``` r
library(AssetAllocation)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
## Example 1: backtesting one of the asset allocations in the package
us_60_40 <- basic_asset_alloc$us_60_40

# test using the data set provided in the package
bt_us_60_40 <- backtest_allocation(us_60_40, ETFs_daily)

# plot returns
library(PerformanceAnalytics)
#> Loading required package: xts
#> Loading required package: zoo
#> Warning: package 'zoo' was built under R version 4.0.5
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
#> 
#> Attaching package: 'PerformanceAnalytics'
#> The following object is masked from 'package:graphics':
#> 
#>     legend
chart.CumReturns(bt_us_60_40$returns,
                 main = paste0("Backtest of the ",
                               bt_us_60_40$strat$name,
                               " portfolio"),
                 ylab = "Cumulative returns"
)
```

<img src="man/figures/README-example1-1.png" width="100%" />

``` r
# show table with performance metrics
bt_us_60_40$table_performance
#>                               United.States.60.40
#> Annualized Return                          0.0813
#> Annualized Std Dev                         0.1107
#> Annualized Sharpe (Rf=0%)                  0.7349
#> daily downside risk                        0.0049
#> Annualised downside risk                   0.0783
#> Downside potential                         0.0021
#> Omega                                      1.1602
#> Sortino ratio                              0.0679
#> Upside potential                           0.0024
#> Upside potential ratio                     0.5862
#> Omega-sharpe ratio                         0.1602
#> Semi Deviation                             0.0051
#> Gain Deviation                             0.0050
#> Loss Deviation                             0.0057
#> Downside Deviation (MAR=210%)              0.0103
#> Downside Deviation (Rf=0%)                 0.0049
#> Downside Deviation (0%)                    0.0049
#> Maximum Drawdown                           0.3258
#> Historical VaR (95%)                      -0.0105
#> Historical ES (95%)                       -0.0170
#> Modified VaR (95%)                        -0.0097
#> Modified ES (95%)                         -0.0117
```

Another example creating a strategy from scratch, retrieving data from
Yahoo Finance, and backtesting:

``` r
library(AssetAllocation)
# create a strategy that invests equally in momentum (MTUM), value (VLUE), low volatility (USMV) and quality (QUAL) ETFs.

factor_strat  <- list(name = "EW Factors",
                      tickers = c("MTUM", "VLUE", "USMV", "QUAL"),
                      default_weights = c(0.25, 0.25, 0.25, 0.25),
                      rebalance_frequency = "month",
                      portfolio_rule_fn = "identity")

# get data for tickers using getSymbols
returns_ETFs <- get_return_data_from_tickers(factor_strat$tickers, starting_date = "2013-08-01")
#> 'getSymbols' currently uses auto.assign=TRUE by default, but will
#> use auto.assign=FALSE in 0.5-0. You will still be able to use
#> 'loadSymbols' to automatically load data. getOption("getSymbols.env")
#> and getOption("getSymbols.auto.assign") will still be checked for
#> alternate defaults.
#> 
#> This message is shown once per session and may be disabled by setting 
#> options("getSymbols.warning4.0"=FALSE). See ?getSymbols for details.

# backtest the strategy
bt_factor_strat <- backtest_allocation(factor_strat,
                                       returns_ETFs)

# plot returns
library(PerformanceAnalytics)
chart.CumReturns(bt_factor_strat$returns,
                 main = paste0("Cumulative returns of the ",
                               bt_factor_strat$strat$name,
                               " portfolio"),
                 ylab = "Cumulative returns"
)
```

<img src="man/figures/README-example2-1.png" width="100%" />

``` r
# show table with performance metrics
bt_factor_strat$table_performance
#>                               EW.Factors
#> Annualized Return                 0.1380
#> Annualized Std Dev                0.1659
#> Annualized Sharpe (Rf=0%)         0.8322
#> daily downside risk               0.0075
#> Annualised downside risk          0.1191
#> Downside potential                0.0030
#> Omega                             1.1894
#> Sortino ratio                     0.0757
#> Upside potential                  0.0036
#> Upside potential ratio            0.5628
#> Omega-sharpe ratio                0.1894
#> Semi Deviation                    0.0077
#> Gain Deviation                    0.0074
#> Loss Deviation                    0.0090
#> Downside Deviation (MAR=210%)     0.0122
#> Downside Deviation (Rf=0%)        0.0075
#> Downside Deviation (0%)           0.0075
#> Maximum Drawdown                  0.3499
#> Historical VaR (95%)             -0.0150
#> Historical ES (95%)              -0.0256
#> Modified VaR (95%)               -0.0142
#> Modified ES (95%)                -0.0142
```
