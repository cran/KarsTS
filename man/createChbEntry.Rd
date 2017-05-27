\name{createChbEntry}
\alias{createChbEntry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createChbEntry: creates a check box with associated text entries
}
\description{
This function creates an element in the input panel consisting of two columns: a check box and the corresponding text entries. It is used internally
}
\usage{
createChbEntry(ind, elements, prefix = "scbValue", envir = KTSEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ind}{
Number of rows
}
  \item{elements}{
Row labels
}
  \item{prefix}{
A prefix to assign names to the output variables
}
  \item{envir}{
Environment (defaults to the environment KTSEnv)
}
}

\value{
Variables that will be passed to the corresponding OnOk function
}

\author{
Marina Saez Andreu
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
