\name{writeMethodSummary}
\alias{writeMethodSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
writeMethodSummary: writes a summary of the filling method
}
\description{
This function is internally used when any of the filling methods that Susana offers is used. It writes on Susana output window information that is shared by all filling methods: which time series and gap set were to fill, which gaps were effectively filled and which remained empty. Filling-method-specific information is writen trhough other functions. It is used internally.
}
\usage{
writeMethodSummary(filledNasTable, remainingNAsInGap, selTsName, selGapName, selGap)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filledNasTable}{
Table of filled gaps
}
  \item{remainingNAsInGap}{
Table of gaps that could not be filled. It does not include gaps whose filling was not attempted. 
}
  \item{selTsName}{
Name of the time series that was filled.
}
  \item{selGapName}{
Name of the gap set that was meant to be filled
}
  \item{selGap}{
Gap set that was meant to be filled
}
}

\author{
Marina Saez Andreu
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
