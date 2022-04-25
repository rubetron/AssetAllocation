# AssetAllocation 1.0.0

-   This is a major update to the package which includes changes to allow dynamic (tactical) asset allocation strategies.

-   The `backtest_allocation` function now expects a list containing an element (a function) `portfolio_rule_fn` which contains the logic used in determining the weights on each rebalancing date.

-   The `backtest_allocation` function now can take an optional input `start_date` in date format. If it is provided, the backtest starts from that date. Otherwise, it starts from the date from which data on all assets becomes available.

-   Pre-loaded data is now in a list called `ETFs`, which has replaced the previous object `ETFs_daily`. Type `?ETFs` to see details and `ETFs$Description` to see more information about the assets.

-   Changed logic to calculate portfolio returns. The previous function `daily_ret_calc` has been replaced by the function `daily_account_cal`.

-   Added a wrapper function get_data_from_tickers which retrieves adjusted prices from Yahoo Finance and calculates returns. The previous function `get_return_data_from_tickers`, which only returned the returns of the assets, has been replaced by the function `get_data_from_tickers` .

-   The pre-loaded asset allocation strategies are now in an object called `asset_allocations`. It contains one list with static asset allocations, and one with tactical asset allocations.

-   Added the following tactical asset allocation strategies:

    -   Ivy portfolio (rebalance function: `tactical_ivy)`

    -   Robust Asset Allocation portfolio (rebalance function: `tactical_RAA)`

    -   Dual Momentum (rebalance function: `tactical_DualMomentum)`

    -   Adaptive Asset Allocation (rebalance function: `tactical_AAA`)

-   Added generic functions to calculate some portfolios that rely on optimization:

    -   risk parity portfolios (rebalance function: `risk_parity)`

    -   minimum variance portfolios (rebalance function: `min_variance`, uses `minvar` from `NMOF` package)

# AssetAllocation 0.1.0

-   Added a `NEWS.md` file to track changes to the package.
-   Changed the package description to include reference and URL for webservices used by the package.
