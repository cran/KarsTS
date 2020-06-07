\name{verifyRealEntry}
\alias{verifyRealEntry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
verifyRealEntry: verify character entries
}
\description{
This function reads a variable coming from a text entry and checks whether it is real or not.It is used internally.
}
\usage{
verifyRealEntry(x, noValid = "isNoValid")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The element to check
}
  \item{noValid}{
The output the function will return when the input is not character.
}
}

\author{
Marina Saez Andreu
}

\examples{

verifyRealEntry("Strawberry", noValid = "isNoValid")
verifyRealEntry(235, noValid = NA)
verifyRealEntry(0235, noValid = NA)
verifyRealEntry(235.6, noValid = NA)

}
