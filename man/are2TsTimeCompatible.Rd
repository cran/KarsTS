\name{are2TsTimeCompatible}
\alias{are2TsTimeCompatible}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
are2TsTimeCompatible: test time compatibility between two time series
}
\description{
It checks for three types of compatibility: the initial dates are the same, the sampling period is the same and the final date is the same. It is used internally to check whether the inputs chosen by the user are appropiated.
}
\usage{
are2TsTimeCompatible (TS1, TS2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TS1}{
A time series
}
  \item{TS2}{
Another time series
}
}

\value{
A logical vector, which is c(TRUE, TRUE, TRUE) when all the time compatibilty conditions are met.
}

\author{
Marina Saez Andreu
}

\examples{

## Generate two time series
TS1 <- genTSExample(InKTSEnv = FALSE)
TS2 <- genTSExample(InKTSEnv = FALSE)

## Their times are identical
timeComp <- are2TsTimeCompatible (TS1, TS2)
timeComp

## We modifify the time of TS2. Now their starts are different,
## although they still have same sampling period and length
TS2$time <- TS2$time + 1
timeComp <- are2TsTimeCompatible (TS1,  TS2)
timeComp

}
