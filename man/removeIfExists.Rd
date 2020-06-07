\name{removeIfExists}
\alias{removeIfExists}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
removeIfExists: remove if exists
}
\description{
This function checks whether a variable exists and removes it in case it does. It is used to clean KTSEnv from global variables created as by-products of the procedures.  It is used internally.
}
\usage{
removeIfExists(candidates, envir = KTSEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{candidates}{
The names of the variables to remove.
}
  \item{envir}{
The environment
}
}


\author{
Marina Saez Andreu
}
\examples{

# Create a vector in the KTSEnv environment
KTSEnv$ThisExists <- 1:30

# It does not throw an error when the element to remove does not exist
removeIfExists(c("ThisExists", "ThisDoesNot"), envir = KTSEnv)


}
