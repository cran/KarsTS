\name{myLinModel}
\alias{myLinModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
myLinModel: performs a linear fit
}
\description{
This function is used to perform a linear model between observed and predicted values. It also prepares the outputs to be writen on the output window and plots different  graphics.It is the core of the goodnessFilling function.
}
\usage{
myLinModel(observed, predicted)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{observed}{
Observed values
}
  \item{predicted}{
Predicted values
}
}

\author{
Marina Saez Andreu
}

\examples{

observed <- genTSExample(stationary = FALSE, InKTSEnv = FALSE)$value
predicted <- observed + stats::rnorm(stats::rnorm(length(observed)))
myLinModel(observed, predicted)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
