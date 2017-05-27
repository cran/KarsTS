\name{createChbChb}
\alias{createChbChb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createChbChb: two column check box
}
\description{
createChbChb: creates a two column check box on the main screen
}
\usage{
createChbChb(ind, elements, prefix1 = NULL, prefix2 = NULL, envir = KTSEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ind}{
Number of rows of the check box
}
  \item{elements}{
Row labels
}
  \item{prefix1}{
A prefix to assign a name to the output variables (column 1)
}
  \item{prefix2}{
A prefix to assign a name to the output variables (column 2)
}
  \item{envir}{
Enviroment to which the output variables will be assigned
}
}

\value{
An output variable for each box. They are be passed to the corresponding onOk function
}

\author{
Marina Saez Andreu
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

