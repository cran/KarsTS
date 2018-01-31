\name{getProTaos}
\alias{getProTaos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getProTaos:calculate the probability of recurrence
}
\description{
This function calculates the probability of recurrence of a system, given by a recurrence matrix in KarsTS format. The RP is calculated for each diagonal (upper triangle).
}
\usage{
getProTaos(RecMat, xlim = NULL, main = NULL, doPlot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RecMat}{
A recurrence matrix in KarsTS format
}
  \item{xlim}{
X range of the plot.
}
  \item{main}{
Plot title
}
  \item{doPlot}{
Get the plot or not
}
}

\value{
A list containing
\item{Tao }{The diagonals where the RP was calculated}
\item{Prob }{The RP for each diagonal}

}
\references{
Romano, M. C. (2004). Synchronization Analysis by Means of Recurrences in Phase Space, Universitat Postdam. Doctoral dissertation.
}
\author{
Marina Saez Andreu
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
