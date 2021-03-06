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
createChbEntry(ind, elements, prefix = "scbValue", envir = KTSEnv, dCh = "0", dEn = "")
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
  \item{dCh}{
Default value for the checkbox
}
  \item{dEn}{
Default value for the text entry
}
}

\value{
Variables that will be passed to the corresponding OnOk function
}

\author{
Marina Saez Andreu
}
