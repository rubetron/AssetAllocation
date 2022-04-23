\name{asset_allocations}
\alias{asset_allocations}
\docType{data}
\title{Pre-loaded Static and Tactical Asset Allocations
%%   ~~ data name/kind ... ~~
}
\description{
Basic static and tactical asset allocation strategies that work with the pre-loaded data in the object \code{ETFs}. Each element is itself a list with the following fields: name, tickers, default_weights, rebalance_frequency, portfolio_rule_fn.

The static allocations included are:
\itemize{
\item United States 60/40 portfolio
\item Golden Butterfly portfolio
\item Rob Arnott Portfolio
\item Global Asset Allocation
\item Permanent Portfolio
\item Desert Portfolio
\item Larry Portfolio
\item Big Rocks Portfolio
\item Sandwich Portfolio
\item Balanced Tax Aware Portfolio
\item Balanced Portfolio
\item Income with Growth Portfolio
\item Income with Growth Tax Aware Portfolio
\item Conservative Income
\item Conservative Income Tax Aware
\item All Weather Portfolio
}

The tactical asset allocations included are:
\itemize{
\item Ivy Portfolio
\item Robust Asset Allocation
\item Dual Momentum
\item Adaptive Asset Allocation
}

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
