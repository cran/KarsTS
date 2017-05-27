\name{rmSlopeOutliers}
\alias{rmSlopeOutliers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
rmSlopeOutliers: slope filter
}
\description{
This function filters the slope (difference) outliers in a time series. It finds the greater outlier, it removes the value causing it and it tries to re-fill value by linear interpolation. Then the greater outlier in the modified time series is found and so on. It is used internally.
}
\usage{
rmSlopeOutliers(tS = NULL, origMxPosSlope = NULL, origMxNegSlope = NULL, filling = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tS}{
The time series
}
  \item{origMxPosSlope}{
The maximum allowed positive slope. Greater slopes are considered as outliers.
}
  \item{origMxNegSlope}{
The maximum allowed negative slope. Greater slopes (in absolute value) are considered as outliers.
}
  \item{filling}{
The positions in the time series corresponding to filled values(that is, not actual observations).When this argument is not null, the actual observations remain untouched whether if they cause outliers or not. 
}
}

\value{
The smoothed time series. The function stops when there are no more outliers or when it reaches 100000 iterations. This is a safety measure to avoid infinite loops.If the time series still contains outliers, run the function again.  
}

\author{
Marina Saez andreu
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
