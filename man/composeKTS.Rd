\name{composeKTS}
\alias{composeKTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
composeKTS: perform operations on time series
}
\description{
This function allows to perform a number of operations on the values of the time series.The operations currently available are: sum, multiplication, opposite, reciprocal and natural logarithm. It is used through the Operations button in the Analysis Menu
}
%- maybe also 'usage' for other objects documented here.
\details{
Opposite, inverse and logarithm can be applied simultaneously to N time series. The output consists of N output time series the names of which are assigned by default (original names plus Opp, Recip or Ln).
On the contrary, the output of sum and multiplication is a single time series and the user must choose its name.
}
\value{
The output time series in the environment KTSEnv
}

\author{
Marina Saez Andreu
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
