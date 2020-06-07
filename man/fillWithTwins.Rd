\name{fillWithTwins}
\alias{fillWithTwins}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
fillWithTwins: fills missing values using twin points
}
\description{
This function finds twin points in a recurrence matrix and uses them to fill adjacent missing values. Many points do not have twins, therefore it is unlikely that all the missing values are filled. It is used through the button twins in the Filling Menu
}
%- maybe also 'usage' for other objects documented here.
\details{
The inputs to this function are: the time series to fill (TSF), a recurrence matrix that represents the system (RM), the maximum distance (MD) and, optionally, the gap set to fill. A missing value is replaced by the median of its twin points.
Case A: TSF was used to build RM. In this case, RM lacks information at the points to fill; therefore, it is necessary to find the twins of the adjacent points, that is, MD must be greater than 0.
Case B: the system was reconstructed using other time series, that is, RM was calculated without TSF. In this case, it is possible to set MD to 0, although it can be greater than 0, as well. 
}
\value{
The filled time series in susEnv and a summary of the procedure on the output window.
}

\author{
Marina Saez Andreu
}
