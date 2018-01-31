\name{functToExport}
\alias{functToExport}
\alias{KTSEnv}
\alias{makeGlobal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
functToExport: functions to export
}
\description{
This function exports the function KarsTS::KarsTS. It is used internally. 
}
\usage{
functToExport()
}
%- maybe also 'usage' for other objects documented here.
\details{
Other objects documente here: \cr
makeGlobal is a vector containing the variables to declare global.It is used internally. \cr
KTSEnv is the environment where the loaded data sets are and it can be accesed directly from the R console. We reccomend to use KarsTS to manipulate the data sets; however, the user can handle them directly from R in order to apply functionalities not included in KarsTS. If that is the case, be careful to produce data sets with the right format; otherwise, KarsTS will not recognize them. See the User's Guide for more information. 

}
\author{
Marina Saez Andreu
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
