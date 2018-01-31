\name{createRandName}
\alias{createRandName}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createRandName: creates a random name
}
\description{
This function creates a random name consisting of a prefix and a random number from 10000 to 99999.It is used internally
}
\usage{
createRandName(prefix = "panel")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prefix}{
The prefix to which the random number will be added
}
}

\author{
Marina Saez Andreu
}

\examples{
createRandName("Rodrigo")
createRandName("Rodrigo")
createRandName("Diaz")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
