\name{removePoints}
\alias{removePoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
removePoints: graphically remove points from a time series
}
\description{
This function is used to remove from a time series a set of graphically selected points. It is useful to remove outliers.It is used through the Remove Points button in the Plots menu.
}

%- maybe also 'usage' for other objects documented here.
\details{
The points are actually removed from a copy of the time series, which is called by default timeSeriesName_pr. The points to remove are selected by dragging the mouse over them. All points within a radius are selected; the user can control this radius (or threshold) via interface. 
}
\value{
A new time series where the selected points values have been replaced by NAs.
}

\author{
Marina Saez Andreu
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
