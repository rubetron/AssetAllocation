\name{basic_asset_alloc}
\alias{basic_asset_alloc}
\docType{data}
\title{basic_asset_alloc
%%   ~~ data name/kind ... ~~
}
\description{
List of basic asset allocation strategies. Each element is itself a list with the following fields: name, tickers, defaukt_weights, rebalance_frequency, portfolio_rule_fn. The allocations included are: - United States 60/40 portfolio
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
}
\usage{data("basic_asset_alloc")}
\format{
  The format is:
List of 16 lists, each representing an asset allocation strategy with the following fields:
  ..$ name               : chr
  ..$ tickers            : chr
  ..$ default_weights    : num
  ..$ rebalance_frequency: chr (default is "month")
  ..$ portfolio_rule_fn  : chr (default is "identity")
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(basic_asset_alloc)
## maybe str(basic_asset_alloc) ; plot(basic_asset_alloc) ...
}
\keyword{datasets}
