\name{getDelayCharTimes}
\alias{getDelayCharTimes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getDelayCharTimes: transforms dates from character to numeric. 
}
\description{
This function transforms a set of dates (given as character) to numeric after testing that their format is allowed by Susana.
}
\usage{
getDelayCharTimes(initialTimes, tz = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initialTimes}{
The dates
}
  \item{tz}{
The time zone
}
}

\value{
A vector with the times in numeric form
}
\author{
Marina Saez Andreu
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
