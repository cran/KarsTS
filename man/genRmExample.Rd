\name{genRmExample}
\alias{genRmExample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to generate a recurrence matrix example
}
\description{
A function to generate a recurrence matrix example. It can be assigned to the KTSEnv environment.
}
\usage{
genRmExample(name = "SRM", InKTSEnv = TRUE, plotRM = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
A name for the recurrence matrix. When the local environment is used instead of KTSEnv, the name is still necessary for the recurrence plot title.
}
  \item{InKTSEnv}{
Assign the output to the KTSEnv (TRUE) or to the local  environment (FALSE)
}
  \item{plotRM}{
Plot the recurrence matrix (TRUE) or not (FALSE)
}
}
\details{
The recurrence matrix is calculated on a sinusoidal time series composed with a random walk. The embedding dimension is 2, the delay is 290 and the threshold is 20.
}
\value{
The recurrence matrix and the time series
}

\author{
Marina Saez Andreu
}

\examples{

 res <- genRmExample(name = "RMExample", InKTSEnv = FALSE, plotRM = FALSE)
 res$newSimpRM
 res$TS
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
