# AssetAllocation 1.0.0

-   Major update with changes to allow dynamic (tactical) asset allocation strategies.
-   The `backtest_allocation` function now expects a list containing an element (a function) `portfolio_rule_fn` which contains the logic used in determining the weights on each rebalancing date.
-   Pre-loaded data is now in a list called `ETFs`. Type `?ETFs` to see details and `ETFs$Description` to see more information about the assets.
-   Changed logic to calculate portfolio returns. The previous function `daily_ret_calc` no longer exists.
-   Added a wrapper function get_data_from_tickers which retrieves adjusted prices from Yahoo Finance and calculates returns. The previous function `get_return_data_from_tickers`, which only returned the returns of the assets, was removed.
-   The pre-loaded asset allocation strategies are now in an object called `asset_allocations`. It contains one list with static asset allocations, and one with tactical asset allocations.
-   Added three tactical asset allocation strategies: the Ivy portfolio, the Robust Asset Allocation, and the Dual Momentum. The corresponding rebalancing functions are `tactical_ivy`, `tactical_RAA`, and `tactical_DualMomentum`.
-   Added functions to calculate risk parity portfolios. The corresponding rebalancing function is `risk_parity`.
-   Added functions to calculate minimum variance portfolios. The corresponding rebalancing function is `min_variance`.
-   

# AssetAllocation 0.1.0

-   Added a `NEWS.md` file to track changes to the package.
-   Changed the package description to include reference and URL for webservices used by the package.
