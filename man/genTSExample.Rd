\name{genTSExample}
\alias{genTSExample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to generate an example time series
}
\description{
This function generates an example time series. The dates range from 2015 to 2016 and the sampling period is half an hour. The time series can be white noise or a random walk. It can be assigned to the KTSEnv environment. 
}
\usage{
genTSExample(stationary = TRUE, name = "TS", InKTSEnv = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stationary}{
Generate white noise (TRUE) or a random walk (FALSE)
}
  \item{name}{
A name for the time series. It defaults to TS. When InKTSEnv is FALSE, the name is not necessary.
}
  \item{InKTSEnv}{
Assign to the environment KTSEnv (TRUE) or not (FALSE)
}
}

\value{
The time series
}

\author{
Marina Saez Andreu
}

\examples{

# Generate white noise and assign to the KTSEnv environment
genTSExample(stationary = TRUE, name = "WN")
graphics::plot(KTSEnv$WN, type = "l")

# Generate random walk and not assign to the KTSEnv environment
RW <- genTSExample(stationary = FALSE, InKTSEnv = FALSE)
graphics::plot(RW, type = "l")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
