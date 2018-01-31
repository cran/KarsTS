\name{mainScreen}
\alias{mainScreen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mainScreen: launches the main screen
}
\description{
This function launches Susana main screen when the welcome screen is destroyed.It is used internally.
}
\usage{
mainScreen()
}

\author{
Marina Saez Andreu
}

\examples{

#Opens interface
KarsTS(skipWelcome = TRUE)
destroyMainScreen()
# When the interface has been opened once, the main screen can be opened directly
mainScreen()
destroyMainScreen()

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
