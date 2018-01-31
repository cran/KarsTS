\name{getNAsGaps}
\alias{getNAsGaps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getNAsGaps: get the gaps in a time series
}
\description{
This function identifies the gaps existing in a time series and returns a table.It is used internally.
}
\usage{
getNAsGaps(y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
The time series (only values, not times)
}
}

\value{
A matrix listing the initial and final indices of each gap
}

\author{
Marina Saez Andreu
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
