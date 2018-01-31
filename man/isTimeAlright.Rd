\name{isTimeAlright}
\alias{isTimeAlright}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
isTimeAlright: checks whether a set of dates fits KarsTS format
}
\description{
This function checks whether a set of dates format is correct according to KarsTS specifications.It is used internally.
}
\usage{
isTimeAlright(timeCharacter, tz = KTSEnv$timeZone)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timeCharacter}{
A vector containg the dates as character.
}
  \item{tz}{
The time zone.
}
}

\author{
Marina Saez Andreu
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
