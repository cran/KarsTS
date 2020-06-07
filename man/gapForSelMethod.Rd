\name{gapForSelMethod}
\alias{gapForSelMethod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
gapForSelMethod: gap set to fill
}
\description{
This function finds which gap the user selected to apply a filling method on it. If none was selected, it creates a gap set containing all the gap sets in the time series.
}
\usage{
gapForSelMethod(selTsName, selTs, envir = KTSEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{selTsName}{
The time series name
}
  \item{selTs}{
The time series
}
  \item{envir}{
The environment
}
}

\value{
A list containing the selected gap set name and the gap set. If the user did not select any gap set or there is no gap set in the environment, the function creates a gap set containing all the gaps in the time series. It is name is All plus the time series name.
}

\author{
Marina Saez Andreu
}
