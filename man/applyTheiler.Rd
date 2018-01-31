\name{applyTheiler}
\alias{applyTheiler}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
applyTheiler: apply Theiler's window
}
\description{
It is used to apply a Theiler's window to a matrix.
}
\usage{
applyTheiler(RM, thW)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RM}{
The recurrence matrix, in KarsTS format
}
  \item{thW}{
The window, in lags from the diagonal
}
}

\value{
A recurrence matrix, in KarsTS format, where the diagonals from the main diagonal up to the distance given by the window have been removed.
}

\author{
Marina Saez Andreu
}

\examples{

RM <- genRmExample(InKTSEnv = FALSE,plotRM = FALSE)

RM1 <- applyTheiler(RM,10)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
