\name{separateEntry}
\alias{separateEntry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
separateEntry: separates and checks comma-separated entry values
}
\description{
Some text entries in Susana consist of a list of values separated by commas. This function separates the values and checks whether they have the appropiate format.It is used internally
}
\usage{
separateEntry(y, class1 = verifyIntEntry, class2 = verifyCharEntry, noValid = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
The value to check
}
  \item{class1}{
The class the first element should be
}
  \item{class2}{
The class the second element should be
}
  \item{noValid}{
The output to return when the input is not valid
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
