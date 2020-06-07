\name{tsCheckedTF}
\alias{tsCheckedTF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
tsCheckedTF: identifies checked time series
}
\description{
This function identifies which time series have been checked from a check box. It is used internally.
}
\usage{
tsCheckedTF(prefix = "scbValue", envir = KTSEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prefix}{
A prefix to reconstruct the names that were assigned to the variables in the check box.
}
  \item{envir}{
The environment
}
}

\value{
A logical vector
}

\author{
Marina Saez Andreu
}
