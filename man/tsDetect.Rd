\name{tsDetect}
\alias{tsDetect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
tsDetect: identifies the time series currently loaded
}
\description{
This function identifies the time series currently loaded in the environment susEnv
It is used internally
}
\usage{
tsDetect()
}
%- maybe also 'usage' for other objects documented here.

\value{
A vector containing the names of the time series
}

\author{
Marina Saez Andreu
}

\examples{

## Generate a time series
genTSExample(name = "TS", InKTSEnv = TRUE)
## Generate some gap sets consisiting of four gaps of 3 NAs each
genGapExample(KTSEnv$TS, 3, 4, name = "NewGapSet1", InKTSEnv = TRUE)
genGapExample(KTSEnv$TS, 3, 4, name = "NewGapSet2", InKTSEnv = TRUE)
genGapExample(KTSEnv$TS, 3, 4, name = "NewGapSet3", InKTSEnv = TRUE)

# Detect time series in the environment KTSEnv
detectedTS <- tsDetect()
detectedTS

# Detect gap sets in the environment KTSEnv
detectedGapSets <- gapDetect()
detectedGapSets

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
