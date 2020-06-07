\name{gamKTS}
\alias{gamKTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to fill values by means of a generalized additive model
}
\description{
This function is used to fill gaps using a generalized additive model (gam) with tensor smoothing. It is used through the Multivariate Splines in the Filling Menu. Although the gam model is not necessarily a multivariate spline, the button has this name because we think is more familiar to the users. 
}

%- maybe also 'usage' for other objects documented here.
\details{
The user selects the time series to fill and a set of predictor time series. The function fits a generalized linear model a gap and uses the model to fill the gap. This is done for every gap in the gap set. It is used through the Multivariate Splines in the Filling Menu. The user can choose a fixed d.f. regression spline or a penalized regression spline. It is also possible to choose any of the smoothing bases allowed by the function mgcv::te. 
}
\value{
The filled time series in the environment KTSEnv and a summary of the results on the output window.
}

\author{
Marina Saez Andreu
}
