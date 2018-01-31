\name{arimaKalman}
\alias{arimaKalman}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
arimaKalman: ARIMA + Kalman smoother
}
\description{
This function fits an ARIMA model to a univariate time series and uses the model to feed a Kalman smoother, which is used to fill missing values in the time series. It is used through the ARIMA button in the Filling Menu.
}
%- maybe also 'usage' for other objects documented here.
\details{
This function input panel contains a button called "Estimate ARIMA parameters"; this button calls the function forecast::auto.arima to provide an automatic estimation of the ARIMA parameters.These parameters can also be directly introduced by the user.
Optionally, the filling can be applied only to a set of gaps in the time series.
If the time series does not contain any NAs, Susana will fit the ARIMA model anyway and return the parameters. 

}
\value{
The filled time series appears in the environment susEnv
}

\author{
Marina Saez Andreu
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
