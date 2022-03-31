\name{get_rebalance_dates}
\alias{get_rebalance_dates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{get_rebalance_dates
%%  ~~function to do ... ~~
}
\description{
This is a helper function used by backtest_allocation to determine the rebalancing dates based on user choices. This function is not intended to be directly executed by users of the package.
}
\usage{
get_rebalance_dates(dates, reb_freq, k = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dates}{vector of dates corresponding to the returns matrix}
  \item{reb_freq}{rebalance frequency with one of the following values: "days", "weeks", "months", "quarters", or "years"}
  \item{k}{number of periods to skip. For example, if reb_freq is "months" and k = 2, the rebalancing will be done every 2 months.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
# get dates for the provided ETF data set
dates <- zoo::index(ETFs_daily)
(reb_dates_monthly <- get_rebalance_dates(dates, "months"))

# rebalancing every 3 months
(reb_dates_monthly <- get_rebalance_dates(dates, "months", k=3))

# rebalancing weekly
(reb_dates_monthly <- get_rebalance_dates(dates, "weeks"))
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
