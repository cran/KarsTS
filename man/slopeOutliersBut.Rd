\name{slopeOutliersBut}
\alias{slopeOutliersBut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
slopeOutliersBut: remove slope outliers
}
\description{
This function handles the inputs and outputs from function rmSlopeOutliers. It removes, one by one, points that produce abnormally steep slopes and replaces them by linear interpolation. The peaks are progresively filed down and the result is a smoothed time series. Usually, these outliers are caused by wrong measurements; filling missing values can produce them also.It is used through the button Rm slope Outliers in the Filling menu.
}

%- maybe also 'usage' for other objects documented here.
\details{
In the first step, the user must choose the time series to smooth and whether if the outliers must be removed from a filling or from the whole time series. In the second step, the user must provide the maximum positive and negative slopes or, alternatively, a reference time series from which calculate them. When the outliers are removed from a filling, the reference time series is the non-filled time series.
 
}
\value{
The smoothed time series. In order to avoid infinite loops, there is a maximum of 100000 iterations. If, by then, the time series has not been completely smoothed, it will necessary to apply the function again.
}

\author{
Marina Saez Andreu
}
