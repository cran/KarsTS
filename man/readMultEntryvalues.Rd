\name{readMultEntryvalues}
\alias{readMultEntryvalues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
readMultEntryvalues: verifies multiple entry values
}
\description{
This function verifies that the values of multiple text entries are valid.
}
\usage{
readMultEntryvalues(nElements, prefix = "entValue", type = "character")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nElements}{
Number of entries
}
  \item{prefix}{
A prefix that has been previously used to name the variables associated to the entries.
}
  \item{type}{
The required type of element: character, integer or real.
}
}

\value{
The text entry values transformed in integer or real if necessary. If the value is not valid the function returns NA.
}

\author{
Marina Saez Andreu
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
