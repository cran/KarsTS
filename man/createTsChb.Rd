\name{createTsChb}
\alias{createTsChb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createTsChb: creates a check box of time series
}
\description{
This function creates a check box listing the time series that exist in the environment KTSEnv.
}
\usage{
createTsChb(labTitle = "Time series", envir = KTSEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labTitle}{
Check box title
}
  \item{envir}{
Environment (defaults to KTSEnv)
}
}

\value{
Variables that will be read in the corresponding onOk function
}

\author{
Marina Saez Andreu
}
