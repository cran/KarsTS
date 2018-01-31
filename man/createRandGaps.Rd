\name{createRandGaps}
\alias{createRandGaps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createRandGaps: create random gaps in a time series
}
\description{
This function creates N gaps of length M, randomly distributed through a time series. The inputs are the time series, the number of gaps and their length. It is used through the Random Gaps button in the Gap Sets menu.
}
%- maybe also 'usage' for other objects documented here.
\details{
The new gaps will not overlay previously existing gaps.
}
\value{
The function creates a gap set and applies it to a copy of the input time series.Both the gap set and the new time series appear in the environment susEnv.   
}

\author{
Marina Saez Andreu
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
