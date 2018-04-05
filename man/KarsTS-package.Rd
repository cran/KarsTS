\name{KarsTS-package}
\alias{KarsTS-package}
\docType{package}
\title{
\packageTitle{KarsTS}
}
\description{
\packageDescription{KarsTS}
}
\details{

\pkg{KarsTS} is a package for microclimate time series, with an emphasis on undergroung environments, such as caves. Microclimate research typically includes CO2 and Rn concentrations, temperature and humidity time series, amongst others. Many of these time series have a strong nonlinear behavior and they often contain significant gaps. \pkg{KarsTS} provides linear and non-linear analysis and filling methods, as well as tools to manipulate easily time series and gap sets. \cr
\cr
The interface \pkg{KarsTS} is opened by running the function \code{KarsTS} on the R or RStudio console. It has five menus: Time Series, Gap Sets, Analysis, Plots and Filling. \cr
\bold{Time Series menu}: time series basic manipulation (loading, saving, resampling, scaling, rounding, etc.). \cr
\bold{Gap Sets menu}: gap sets basic manipulation (loading, saving, selection, MCAR Little's test etc.). Gap sets manipulation allows to apply a filling method to a subset of gaps in a time series (for example, gaps smaller than a certain length). \cr
\bold{Analysis menu}: linear and non-linear analytic procedures (statistics, rolling statistics, loess decomposition,invariants, recurrence matrices, stationarity and linearity tests etc.). \cr
\bold{Plots menu}: tools for plotting recurrence matrices, time series, phase portraits, manual removal of points etc. It contains also analytic procedures with mainly graphical results (linear correlation, mutual information, false nearest neighbors etc.). \cr
\bold{Filling menu}: univariate and multivariate methods to fill missing values in time series (interpolation, ARIMA, random forest algorithm etc.).\cr
\cr
See the \bold{User's Guide} for more information.
}
\author{
\packageAuthor{KarsTS}

Maintainer: \packageMaintainer{KarsTS}
}
\note{
Please, cite this package as: \cr
Marina Saez (2018). KarsTS: An interface for microclimate time series. R package version 2.1.
}

\keyword{ package }

