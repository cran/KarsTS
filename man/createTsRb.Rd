\name{createTsRb}
\alias{createTsRb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createTsRb: creates a radiobutton of time series
}
\description{
This function creates a radiobutton listing the time series that exist in the environment KTSEnv.
}
\usage{
createTsRb(labTitle = "Time series", variableName = "selTsP", envir = KTSEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labTitle}{
Radiobutton title
}
  \item{variableName}{
Name of the variable to be assigned to the radiobutton
}
  \item{envir}{
Environment (defaults to KTSEnv)
}
}

\value{
A variable that will be read in the corresponding onOk function
}

\author{
Marina Saez Andreu
}

