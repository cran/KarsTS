\name{areTsRmTimeCompatible}
\alias{areTsRmTimeCompatible}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
areTsRmTimeCompatible: test time compatibility between a time series and a recurrence matrix
}
\description{
It checks for three types of compatibility: the initial dates are the same, the sampling period is the same and the length is the same. It is used internally to check whether the inputs chosen by the user are appropiated.
}
\usage{
areTsRmTimeCompatible(TS1, RM1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TS1}{
A time series
}
  \item{RM1}{
A recurrence matrix
}
}

\value{
A logical vector, which is c(TRUE, TRUE, TRUE) when all the time compatibilty conditions are met.
}

\author{
Marina Saez Andreu
}
