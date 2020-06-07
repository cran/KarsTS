\name{getFANRM2}
\alias{getFANRM2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getFANRM2: creates a fixed amount of neighbours (FAN) matrix via console.
}
\description{
This function creates a fixed amount of neighbours (FAN) matrix via console from a time series. To create a FAN matrix via interface, see the function createFAN.
}
\usage{
getFANRM2(selTs, embedDim, lagDelay, threshold, theilerWin, fan)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{selTs}{
A univarate time series in KarsTS format.
}
  \item{embedDim}{
The embedding dimension. 
}
  \item{lagDelay}{
The delay (in lags).
}
  \item{threshold}{
The threshold. 
}
  \item{theilerWin}{
The Theiler's window (in lags). 
}
  \item{fan}{
The fixed amount of neighbours. 
}
}
\details{
In case you do not want to embed your time series, set embedDim to 1 and lagDelay to 0. 
If you do not want to apply a Theiler's window, set theilerWin to 0.
The threshold will determine in the first place the number of neighbors of each point; then, the fixed amount of neighbours will determine how many of those are kept. For example, if a point A has 10 neighbors (according to the threshold) and the fan is 5, the point A will have 5 neighbors in the output matrix. However, if a point B has only 4 neighbors (according to the threshold), the number of neighbors of B in the output matrix will be 4. If you want a matrix with a truly fixed amount of neighbors, give the threshold a value high enough to render it useless. 
}
\value{
A list containing the X and Y positions of the recurrent points in the FAN recurrence matrix.
}
\references{
Marwan,R., Romano, M.C., Thiel,M., Kurths,J.(2007): Recurrence plots for the analysis of complex systems. Physics Reports 438, 237-329.
}
\author{
Marina Saez Andreu
}
