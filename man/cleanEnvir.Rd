\name{cleanEnvir}
\alias{cleanEnvir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
cleanEnvir: removes a list of variables from KTSEnv
}
\description{
In order to communicate the functions in an interface, it is often unavoidable to create global variables. This function cleans the global variables that are mere intermediate results of the prodecures.It is used internally 
}
\usage{
cleanEnvir(envir = KTSEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{envir}{
The environment where the intermediate objects are to be removed.It defaults to KTSEnv. 
}
}


\author{
Marina Saez Andreu
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
