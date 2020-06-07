\name{myApplyVector}
\alias{myApplyVector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
myApplyVector: apply type function
}
\description{
This function applies a function to the elements of a vector so that the result for each element is located in a row in a matrix. It is used internally.
}
\usage{
myApplyVector(FUN = NULL, dataVector = NULL, out.ncols = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FUN}{
the function to apply
}
  \item{dataVector}{
The vector to which elements apply the function
}
  \item{out.ncols}{
Number of columns for the output matrix
}
  \item{\dots}{
Further arguments to passed
}
}


\author{
Marina Saez Andreu
}

\examples{

exampleF <- function(X){c(X + 5, X*5)}
myApplyVector(FUN = exampleF, dataVector = 1:10, out.ncols = 2)

}
