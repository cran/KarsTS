\name{aggregateKTS}
\alias{aggregateKTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
aggregateKTS: creation of time series of aggregated values
}
\description{
This function creates time series of aggregated values.The user choses the inputs via interface: time series to aggregate, aggregation period, statistic, name for the output and NA treatment (see details).
}
%- maybe also 'usage' for other objects documented here.
\details{
The statistics available are: median, mean, minimum, maximum, standard deviation and sum.
The NA treatment can be: ignore or propagate. In the first case, the statistic is computed using the observations available in the window, as long as the window is not completely missing.In the second case, incomplete windows are assigned NA.
}
\value{
A time series of aggregated values appears in the environment KTSEnv
}

\author{
Marina Saez Andreu
}
