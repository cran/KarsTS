\name{myScale}
\alias{myScale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
myScale: scaling function
}
\description{
This function is used to scale one or more variables with the possibility of removing the attributes from the base::scale output. It is also possible to perform a robust scaling using the median and the median absolute deviation instead of the mean and the standard deviation. It is used internally.
}
\usage{
myScale(inputMatrix, scaleType = "Robust", outputType = c("outDef", "outList", "outNo"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputMatrix}{
Matrix containing the variables to scale.
}
  \item{scaleType}{
Robust scaling or not
}
  \item{outputType}{
Three types of outputs: output as in base::scale, attributes removed but stored in a list and no attributes at all. 
}
}

\author{
Marina Saez Andreu
}
