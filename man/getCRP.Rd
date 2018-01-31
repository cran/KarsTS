\name{getCRP}
\alias{getCRP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getCRP: gets the correlation probability of recurrence
}
\description{
This function calculates the correlation probability of recurrence of two systems, given by their recurrece matrices. 
}
\usage{
getCRP(prob1, prob2, xLims, doPlot = FALSE, main = "plotTitle")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prob1}{
Probability of recurrence of the first matrix
}
  \item{prob2}{
Probability of recurrence of the second matrix
}
  \item{xLims}{
X range where the correlation probability will be calculated. The diagonals very close or very far from the main diagonal are usually discarded for estimating the CPR.
}
  \item{doPlot}{
Get a plot (TRUE) or not (FALSE)
}
  \item{main}{
Title for the plot
}
}

\value{
Correlation probability of recurrence (CPR)
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
