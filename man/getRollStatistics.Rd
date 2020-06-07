\name{getRollStatistics}
\alias{getRollStatistics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getRollStatistics: get rolling statistics
It is used internally
}
\description{
This function calculates a certain statistic in centered sliding windows along a time series. The available statistics are: minimum, first quartile,median, mean, third quartile, maximum and standard deviation. Incomplete windows can be assigned NA (option tailsTS = FALSE); alternatively, the statistic can be calculated with the available values.
}
\usage{
getRollStatistics(selTs, selTsName, slidingWin, tailsTS = FALSE, selStatisTF)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{selTs}{
The input time series
}
  \item{selTsName}{
The input time series name
}
  \item{slidingWin}{
The sliding window size
}
  \item{tailsTS}{
The action regarding the tails (TRUE or FALSE)
}
  \item{selStatisTF}{
A data frame containing seven columns. Each column corresponds to a statistic. When the user selects a statistic, its value is TRUE; otherwise, it is FALSE. 
}
}

\value{
For each selected statistic, the function creates a time series in the environment susEnv (for example, a time series of mean values).
}

\author{
Marina Saez Andreu
}
