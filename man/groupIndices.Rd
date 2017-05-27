\name{groupIndices}
\alias{groupIndices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
groupIndices: group indices
}
\description{
This function groups a set of sorted indices. For example: 1,2,3,50,100,101,102,103 would be grouped as follows: 1-3, 50, 100-103.It is used internally.
}
\usage{
groupIndices(rawIndices)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawIndices}{
The indices
}
}

\value{
It returns a data frame. Each row corresponds to a group. The columns are: initial index, final index and group length.
}

\author{
Marina Saez Andreu
}

\examples{

X <- sort(sample(1:50,25))
X
groupIndices(X)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
