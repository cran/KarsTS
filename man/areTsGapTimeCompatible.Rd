\name{areTsGapTimeCompatible}
\alias{areTsGapTimeCompatible}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
areTsGapTimeCompatible: tests time compatibility between a time series and a gap set
}
\description{
It checks for three types of compatibility: the initial dates are the same, the sampling period is the same and the length is the same. It is used internally to check whether the inputs chosen by the user are appropiated.
}
\usage{
areTsGapTimeCompatible(TS1, GAP1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TS1}{
A time series
}
  \item{GAP1}{
A gap set
}
}

\value{
A logical vector, which is c(TRUE, TRUE, TRUE) when all the time compatibilty conditions are met.
}

\author{
Marina Saez Andreu
}

\examples{
## Generate a time series
TS <- genTSExample(InKTSEnv = FALSE)
## Generate a gap set consisiting of four gaps of 3 NAs each
GS <- genGapExample(TS, 3, 4, InKTSEnv = FALSE)

## They are obviously compatible since we use TS to create GS
timeComp <- areTsGapTimeCompatible (TS, GS)
timeComp

## We modifify the time of TS. The initial dates are not compatible anymore,
## although the sampling period and the length remain untouched.
TS$time <- TS$time + 1
timeComp <-  areTsGapTimeCompatible (TS, GS)
timeComp

}
