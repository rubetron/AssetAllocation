\name{get_return_data_from_tickers}
\alias{get_return_data_from_tickers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{get_return_data_from_tickers
%%  ~~function to do ... ~~
}
\description{
The function builds a return matrix in xts format from a list of tickers.
}
\usage{
get_return_data_from_tickers(tickers, starting_date = "2007-01-01")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tickers}{Vector containing a number of stock of ETF tickers.
%%     ~~Describe \code{tickers} here~~
}
  \item{starting_date}{Starting date in YYYY-MM-dd format. Default is "2007-01-01".
}
}
\details{
The function retrieves data from Yahoo Finance using the getSymbols function from the quantmod package. It calculates returns from adjusted prices. The ticker names must correspond to those found in Yahoo Finance.
}
\value{
An xts return matrix with the daily returns calculated from the adjusted prices. The column names correspond to the tickers.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Alexandre Rubesam
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## build a return matrix for the following exchange-traded-funds: MTUM, VLUE, USMV, and QUAL.
tickers <- c("MTUM", "VLUE", "USMV", "QUAL")
returns_ETFs <- get_return_data_from_tickers(tickers, starting_date = "2013-08-01")

# display first 10 rows
head(returns_ETFs, 10)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
