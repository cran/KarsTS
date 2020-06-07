\name{createRmRb}
\alias{createRmRb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createRmRb: creates a radiobutton of recurrence matrices
}
\description{
This function creates a radiobutton listing the recurrence matrices that exist in the environment KTSEnv.
}
\usage{
createRmRb(labTitle = "Recurrence matrices", envir = KTSEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labTitle}{
Radiobutton title
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
