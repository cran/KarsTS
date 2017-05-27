\name{getGapsAfterFill}
\alias{getGapsAfterFill}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getGapsAfterFill: get the remaining gaps
}
\description{
This function identifies which gaps remain in a time series after applying a filling method. It is used internally.
}
\usage{
getGapsAfterFill(filledTS, selGap, envir = KTSEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filledTS}{
The filled time series
}
  \item{selGap}{
The gap set that was meant to be filled
}
  \item{envir}{
The environment
}
}

\value{
It returns a list containing two data frames: one of them lists the remaining gaps and the other, the filled gaps.
}

\author{
Marina Saez Andreu
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
