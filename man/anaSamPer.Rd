\name{anaSamPer}
\alias{anaSamPer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
anaSamper: analyzing sampling periods and gaps
}
\description{
Time series need to have a homogeneous sampling period for most calculations.This function divides a time series in homogeneous-sampling-period pieces that contain either observations or NAs.This information allows to decide how to resample the time series. Note that it also provides a list of gaps. 
}
%- maybe also 'usage' for other objects documented here.
\details{
 Once chosen the time serie to analyze, KarsTS offers a list of possible sampling periods (time jumps in the time series), but a human decision is necessary to separate safely true sampling periods from gaps.Tipically, time jumps correspoding to sampling periods appear many times. 
}
\value{
A table containing the aforementioned information appears on KarsTS output window.
}

\author{
Marina Saez Andreu
}
