\name{getNewGapsInd}
\alias{getNewGapsInd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to create random gaps
}
\description{
This function creates nGaps gaps of length lGaps in the time series timSer. It is used internally. The gaps do not overlay amongst them; they do not overlay pre-existing gaps either.
}
\usage{
getNewGapsInd(timSer, lGaps, nGaps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timSer}{
The time series where the gaps will be created.
}
  \item{lGaps}{
The length of the gaps
}
  \item{nGaps}{
The number of gaps.
}
}

\value{
A vector containing the indices of the gaps
}

\author{
Marina Saez Andreu
}

\examples{

# Create time series
TS <- genTSExample(InKTSEnv = FALSE)[1:50,]

# Create 3 gaps of 7 NAs each
gInd <- getNewGapsInd(TS, 7, 3)

# Create time series duplicate and apply the gaps
TS1 <- TS
TS1$value[gInd] <- NA

#Compare
cbind(TS, TS1$value)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
