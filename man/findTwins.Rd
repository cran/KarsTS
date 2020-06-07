\name{findTwins}
\alias{findTwins}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
findTwins: finds twin points in a recurrence matrix
}
\description{
This function finds twin points (that is, identical columns) in a recurrence matrix.It is used internally
}
\usage{
findTwins(recMat, pointsToFind = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{recMat}{
The recurrence matrix
}
  \item{pointsToFind}{
The points (columns) whose twins are to be found (defaults to all)
}
}
\details{
The function classifies the columns in families of twins and assigns an integer number to each family. The numbers are mere codes to identify the families and they do not have any meaning themselves.
}
\value{
A vector of integers. Each number represents a family of twins. Positions corresponding to columns with no twins are assigned NA. When the search of twins is limited to a subset of points, the uninteresting points are assigned NA also.  
}
\references{
Marwan,R., Romano, M.C., Thiel,M., Kurths,J.(2007): Recurrence plots for the analysis of complex systems. Physics Reports 438, 237-329.
}
\author{
Marina Saez Andreu
}

\examples{

# Generate time series and recurrence matrix
res <- genRmExample(name = "SRM", InKTSEnv = FALSE, plotRM = FALSE)
TS <- res$TS
SRM <- res$newSimpRM

# Find twins
SRMTwins <- findTwins(SRM)

# Interpretation example
aFamily <- SRMTwins[which(is.finite(SRMTwins))][1]
twinsInFamily <- which(SRMTwins == aFamily)
TS[twinsInFamily,] 
# The values in TS are very similar
# The columns in the recurrence matrix are identical,
# although this cannot be observed directly
# because of the way KarsTS stores recurrence matrices
}
