\name{getSamPerTable}
\alias{getSamPerTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getSamPerTable: get sampling periods table
}
\description{
This function divides a time series in pieces with a single sampling period and either values or missing values. Internally, this function separates true missing values from missing values that are the result of the existence of different sampling periods in the time series. Note that KarsTS time series are regular; when a time series with different time steps is loaded, NAs are added to achieve regularity. It is used internally. 
}
\usage{
getSamPerTable(timSer, sampPer)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timSer}{
The input time series
}
  \item{sampPer}{
A vector that contains the existing sampling periods.
}
}

\value{
It returns a data frame listing all the pieces.
}

\author{
Marina Saez Andreu
}
