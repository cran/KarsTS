\name{assignMultiple}
\alias{assignMultiple}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
assignMultiple: assign multiple
}
\description{
This function applies the function assign multiple times
}
\usage{
assignMultiple(namesVector, valuesList, envir = KTSEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{namesVector}{
A vector containing the names to be assigned
}
  \item{valuesList}{
The values to which the names will be assigned
}
  \item{envir}{
The environment
}
}

\author{
Marina Saez Andreu
}

\examples{

assignMultiple(c("One", "Two", "Three"), list(1:10,2,3), envir = KTSEnv)
KTSEnv$One
KTSEnv$Two
KTSEnv$Three

}
