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