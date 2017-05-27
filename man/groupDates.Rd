\name{groupDates}
\alias{groupDates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
groupDates: groups dates
}
\description{
This function transforms the output of the function groupIndices from indices to dates.It is used internally.
}
\usage{
groupDates(rawIndices, TimSer)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawIndices}{
The indices
}
  \item{TimSer}{
The time series to which the indices belong.
}
}

\author{
Marina Saez Andreu
}

\examples{

X <- sort(sample(1:50,25))
TS <- genTSExample(InKTSEnv = FALSE)
groupIndices(X)
groupDates(X, TS)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
