\name{arimaXKalman}
\alias{arimaXKalman}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
arimaXKalman: ARIMAX + Kalman smoother
}
\description{
This function fits an ARIMAX model to a time series and uses the model to feed a Kalman smoother, which is used to fill missing values in the time series. It is used through the ARIMAX button in the Filling Menu.
}
%- maybe also 'usage' for other objects documented here.
\details{
The only difference between this function and arimaKalman is that arimaXKalman allows the introduction of regressor variables in the model.
See arimaKalman for more details.
}
\value{
The filled time series appears in the environment susEnv
}

\author{
Marina Saez Andreu
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
