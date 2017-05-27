\name{verifyIntEntry}
\alias{verifyIntEntry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
verifyIntEntry: verify integer entries
}
\description{
This function reads a variable coming from a text entry and checks whether it is integer or not.It is used internally.
}
\usage{
verifyIntEntry(x, noValid = "isNoValid")
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

verifyIntEntry("Strawberry", noValid = "isNoValid")
verifyIntEntry(235.6, noValid = NA)
verifyIntEntry(235, noValid = NA)
verifyIntEntry(0235, noValid = NA)
verifyIntEntry(235.0, noValid = NA)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
