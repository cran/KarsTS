\name{createEachRb}
\alias{createEachRb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createEachRb: creates a radiobutton on the main screen
}
\description{
This function creates a radiobutton. It is used internally
}
\usage{
createEachRb(labTitle = NULL, variable = NULL, panel = KTSEnv$subPanR4C1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labTitle}{
A title for the check box
}
  \item{variable}{
The variable associated to the radiobutton
}
  \item{panel}{
The panel where the radiobutton is to be placed (defaults to the input panel in the main screen)
}
}

\value{
A variable that will be passed to the corresponding OnOk function
}

\author{
Marina Saez Andreu
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line


