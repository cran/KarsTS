\name{diffKTS}
\alias{diffKTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
diffKTS: calculate differences of a time series
}
\description{
This function creates a new time series by differencing the input time series.It is used through the button differences in the Time Series Menu
}
%- maybe also 'usage' for other objects documented here.
\details{
The inputs are: time series, lag and center yes/not. In this context, center the time series means that the differenced time series will be interpolated, so that the output time series has same length and dates as the original time series. 
}
\value{
The differenced time series
}

\author{
Marina Saez Andreu
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
