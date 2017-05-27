\name{compareVecVec}
\alias{compareVecVec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compareVecVec: compare the elements of two vectors
}
\description{
Checks whether there are any shared elements between two vectors.It is used internally
}
\usage{
compareVecVec(VA, VB)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{VA}{
One vector
}
  \item{VB}{
Another vector
}
}

\value{
A logical matrix showing the coincidences
}

\author{
Marina Saez Andreu
}

\examples{

compareVecVec(1:10, 7:12)

V1 <- c("Apples", "Strawberries","Watermelon")
V2 <- c("Ananas", "Apples", "Strawberries", "Coconut")
compareVecVec(V1,V2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
