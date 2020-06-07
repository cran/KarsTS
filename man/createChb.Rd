\name{createChb}
\alias{createChb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createChb: creates a check box on the main screen
}
\description{
This function creates a check box on the main screen
}
\usage{
createChb(labTitle = NULL, variableName = NULL, defaultVal = "0")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labTitle}{
A title for the check box
}
  \item{variableName}{
A name for the variable associated to the check box
}
  \item{defaultVal}{
Default value (defaults to not selected)
}
}

\value{
A variable that will be passed to the corresponding OnOk function
}

\author{
Marina Saez Andreu
}
