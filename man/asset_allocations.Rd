\name{asset_allocations}
\alias{asset_allocations}
\docType{data}
\title{Pre-loaded Static and Tactical Asset Allocations
%%   ~~ data name/kind ... ~~
}
\description{
Basic static and tactical asset allocation strategies that work with the pre-loaded data in the object \code{ETFs}. Each element is itself a list with the following fields: name, tickers, defaukt_weights, rebalance_frequency, portfolio_rule_fn. The static allocations included are:
- United States 60/40 portfolio
- Golden Butterfly portfolio
- Rob Arnott Portfolio
- Global Asset Allocation
- Permanent Portfolio
- Desert Portfolio
- Larry Portfolio
- Big Rocks Portfolio
- Sandwich Portfolio
- Balanced Tax Aware Portfolio
- Balanced Portfolio
- Income with Growth Portfolio
- Income with Growth Tax Aware Portfolio
- Conservative Income
- Conservative Income Tax Aware
- All Weather Portfolio
The tactical asset allocations included are:
- Ivy Portfolio
- Robust Asset Allocation
- Dual Momentum
}
\usage{data("asset_allocations")}
\format{
Object of class \code{"List"} with two fields containing static and tactical sset allocations, respectively. Each asset allocation is represented by a list with the following fields:
  ..$ name               : chr
  ..$ tickers            : chr
  ..$ default_weights    : num
  ..$ rebalance_frequency: chr (default is "month")
  ..$ portfolio_rule_fn  : chr (default is "identity")
}
\examples{
data(asset_allocations)
# basic static allocation is the U.S. 60/40 portfolio:
us_60_40 <- asset_allocations$static$us_60_40

# basic tactical allocation is the Ivy portfolio:
ivy <- asset_allocations$tactical$ivy
}
\keyword{datasets}
