library(testthat)              # load testthat package
library(AssetAllocation)       # load our package

# Test whether the output from daily_account_calc has correct dimension
test_that("daily_account_calc() returns a vector with the correct size", {
  # generate some random data
  set.seed(12345)
  R <- matrix(rnorm(21*10, 0.01, 0.025), nrow = 21, ncol = 10)
  w <- runif(10)
  w <- w/sum(w)
  output <- daily_account_calc(w, R)
  expect_equal(length(output), 22)
})

# Test that daily_account_calc() stops in case of wrong dimensions
test_that("daily_account_calc() stops if length(w) != ncol(R)", {
  # generate some random data
  set.seed(12345)
  R <- matrix(rnorm(21*10, 0.01, 0.025), nrow = 21, ncol = 10)
  w <- runif(9)
  w <- w/sum(w)
  expect_error(daily_account_calc(w, R))
})

# test that backtest_allocation stops if strategy is missing expected elements
test_that("backtest_allocation() stops if missing elements", {
  # create a simple strategy
  us_60_40 <- asset_allocations$static$us_60_40
  # remove the tickers.
  us_60_40 <- within(us_60_40, rm("tickers"))
  # test using the data set provided in the package
  expect_error(backtest_allocation(us_60_40,
                                   ETFs$Prices,
                                   ETFs$Returns,
                                   ETFs$risk_free))
})


# Test that function backtest_allocation produces a list
test_that("backtest_allocation() returns a list", {
  ## Example 1: backtesting one of the asset allocations in the package
  us_60_40 <- asset_allocations$static$us_60_40

  # test using the data set provided in the package
  output <- backtest_allocation(us_60_40,
                                ETFs$Prices,
                                ETFs$Returns,
                                ETFs$risk_free)
  expect_type(output, "list")
})

# Test that function backtest_allocation produces a list with correct fields
test_that("backtest_allocation() returns a list with correct fields", {
  ## Example 1: backtesting one of the asset allocations in the package
  us_60_40 <- asset_allocations$static$us_60_40

  # test using the data set provided in the package
  output <- backtest_allocation(us_60_40,
                                ETFs$Prices,
                                ETFs$Returns,
                                ETFs$risk_free)

  output_names <- names(output)

  expected <- c("strat",
                "returns",
                "table_performance",
                "rebalance_dates",
                "rebalance_weights")
  expect_named(output, expected)
})

# Test that function backtest_allocation produces error if risk-free is of wrong dimension +
test_that("backtest_allocation() stops if risk-free is of larger dimension", {
  ## Example 1: backtesting one of the asset allocations in the package
  us_60_40 <- asset_allocations$us_60_40

  n_rows_R <- nrow(ETFs$Returns)
  rf <- rep(0, n_rows_R + floor(runif(1)*100))

  expect_error(backtest_allocation(us_60_40,
                                   ETFs$Prices,
                                   ETFs$Returns,
                                   rf))
})

# Test that function backtest_allocation produces error if risk-free is of wrong dimension -
test_that("backtest_allocation() stops if risk-free is of lower dimension", {
  ## Example 1: backtesting one of the asset allocations in the package
  us_60_40 <- asset_allocations$us_60_40

  n_rows_R <- nrow(ETFs$Returns)
  rf <- rep(0, n_rows_R - floor(runif(1)*100))

  expect_error(backtest_allocation(us_60_40,
                                   ETFs$Prices,
                                   ETFs$Returns,
                                   rf))
})

# Test that function backtest_allocation stops if R is not xts
test_that("backtest_allocation() stops if R is not of class xts", {
  ## Example 1: backtesting one of the asset allocations in the package
  us_60_40 <- asset_allocations$us_60_40

  # get only numeric data from ETFs
  R <- zoo::coredata(ETFs$Returns)

  expect_error(backtest_allocation(us_60_40,
                                   ETFs$Prices,
                                   R,
                                   rf))
})

# Test that function backtest_allocation stops if P is not xts
test_that("backtest_allocation() stops if R is not of class xts", {
  ## Example 1: backtesting one of the asset allocations in the package
  us_60_40 <- asset_allocations$us_60_40

  # get only numeric data from ETFs
  P <- zoo::coredata(ETFs$Prices)

  expect_error(backtest_allocation(us_60_40,
                                   P,
                                   ETFs$Returns,
                                   rf))
})

# Test that function backtest_allocation stops if tickers not found in R
test_that("backtest_allocation() produces error if tickers not found", {
  ## Example 1: backtesting one of the asset allocations in the package
  us_60_40 <- asset_allocations$us_60_40

  # remove one of the assets in the strategy being tested
  R <- ETFs$Returns
  ind_rm <- which(colnames(R) == "SPY")
  R <- R[, -ind_rm]

  expect_error(backtest_allocation(us_60_40,
                                   ETFs$Prices,
                                   R,
                                   rf))
})

# Test that function backtest_allocation stops if tickers not found in P
test_that("backtest_allocation() produces error if tickers not found", {
  ## Example 1: backtesting one of the asset allocations in the package
  us_60_40 <- asset_allocations$us_60_40

  # remove one of the assets in the strategy being tested
  P <- ETFs$Prices
  ind_rm <- which(colnames(P) == "SPY")
  P <- P[, -ind_rm]

  expect_error(backtest_allocation(us_60_40,
                                   P,
                                   ETFs$Returns,
                                   rf))
})

