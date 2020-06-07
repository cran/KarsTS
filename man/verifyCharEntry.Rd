\name{verifyCharEntry}
\alias{verifyCharEntry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
verifyCharEntry: verify character entries
}
\description{
This function reads a variable coming from a text entry and checks whether it is character or not.It is used internally.
}
\usage{
verifyCharEntry(x, noValid = "isNoValid")
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
\details{
One-character strings are not allowed.
}

\author{
Marina Saez Andreu
}

\examples{

verifyCharEntry("Strawberry", noValid = "isNoValid")
verifyCharEntry(235, noValid = "isNoValid")
verifyCharEntry(235, noValid = NA)

}
