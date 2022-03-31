library(testthat)              # load testthat package
library(AssetAllocation)       # load our package

# Test whether the output from daily_ret_calc has correct dimension
test_that("daily_ret_calc() returns a vector with the correct size", {
  # generate some random data
  set.seed(12345)
  R <- matrix(rnorm(21*10, 0.01, 0.025), nrow = 21, ncol = 10)
  w <- runif(10)
  w <- w/sum(w)
  output <- daily_ret_calc(w, R)
  expect_equal(length(output), 21)
})

# Test that function backtest_allocation produces a list
test_that("backtest_allocation() returns a list", {
  ## Example 1: backtesting one of the asset allocations in the package
  us_60_40 <- basic_asset_alloc$us_60_40

  # test using the data set provided in the package
  output <- backtest_allocation(us_60_40, ETFs_daily)
  expect_type(output, "list")
})

# Test that function backtest_allocation produces a list with correct fields
test_that("backtest_allocation() returns a list with correct fields", {
  ## Example 1: backtesting one of the asset allocations in the package
  us_60_40 <- basic_asset_alloc$us_60_40

  # test using the data set provided in the package
  output <- backtest_allocation(us_60_40, ETFs_daily)

  output_names <- names(output)

  expected <- c("strat", "returns", "table_performance", "rebalance_dates", "rebalance_weights")
  expect_named(output, expected)
})

# Test that function backtest_allocation produces error if risk-free is of wrong dimension +
test_that("backtest_allocation() produces error if risk-free is of wrong (higher) dimension", {
  ## Example 1: backtesting one of the asset allocations in the package
  us_60_40 <- basic_asset_alloc$us_60_40

  n_rows_R <- nrow(ETFs_daily)
  rf <- rep(0, n_rows_R + floor(runif(1)*100))

  expect_error(backtest_allocation(us_60_40, ETFs_daily, rf))
})

# Test that function backtest_allocation produces error if risk-free is of wrong dimension -
test_that("backtest_allocation() produces error if risk-free is of wrong (lower) dimension", {
  ## Example 1: backtesting one of the asset allocations in the package
  us_60_40 <- basic_asset_alloc$us_60_40

  n_rows_R <- nrow(ETFs_daily)
  rf <- rep(0, n_rows_R - floor(runif(1)*100))

  expect_error(backtest_allocation(us_60_40, ETFs_daily, rf))
})

# Test that function backtest_allocation stops if R is not xts
test_that("backtest_allocation() stops if R is not of class xts", {
  ## Example 1: backtesting one of the asset allocations in the package
  us_60_40 <- basic_asset_alloc$us_60_40

  # get only numeric data from ETFs_daily
  R <- coredata(ETFs_daily)

  expect_error(backtest_allocation(us_60_40, R))
})

# Test that function backtest_allocation if tickers not found in R
test_that("backtest_allocation() produces error if tickers not found", {
  ## Example 1: backtesting one of the asset allocations in the package
  us_60_40 <- basic_asset_alloc$us_60_40

  # remove one of the assets in the strategy being tested
  ind_rm <- which(colnames(R) == "SPY")
  R <- ETFs_daily[, -ind_rm]

  expect_error(backtest_allocation(us_60_40, R))
})


