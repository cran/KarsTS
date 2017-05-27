\name{embedData}
\alias{embedData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
embedData: embeds a time series
}
\description{
This function embeds a time series and adds NAs so that the embedded data have the same length as the original time series.It is used internally
}
\usage{
embedData(TSData, embDim, embDelay)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TSData}{
Values of the time series
}
  \item{embDim}{
Embedding dimension
}
  \item{embDelay}{
Delay
}
}

\value{
A matrix of embedded data, with as many NAs as necessary so that it has the same length as the original time series.
}

\author{
Marina Saez Andreu
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
