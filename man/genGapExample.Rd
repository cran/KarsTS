\name{genGapExample}
\alias{genGapExample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to generate a gap set example
}
\description{
A function to generate a gap set example.It can be assigned to the KTSEnv environment. 
}
\usage{
genGapExample(timSer, lGaps, nGaps, name = "GS", InKTSEnv = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timSer}{
The time series where the gaps are generated
}
  \item{lGaps}{
The length of the gaps
}
  \item{nGaps}{
The number of gaps
}
  \item{name}{
A name for the gap set. It defaults to GS. When InKTSEnv is FALSE, the name is not necessary.
}
  \item{InKTSEnv}{
Assign to the environment KTSEnv (TRUE) or not (FALSE)
}
}

\value{
The gap set
}

\author{
Marina Saez Andreu
}


\examples{

# Generate a time series

RW <- genTSExample(stationary = FALSE, InKTSEnv = FALSE)

# Generate 2 gaps of 50 NAs each
G.2.50 <- genGapExample(RW, 50, 2, InKTSEnv = FALSE)

RW1 <- RW
RW1$value[G.2.50$gaps] <- NA
graphics::plot(RW, type = "l", col = "red")
graphics::points(RW1, type = "l", col = "blue")

# The same in the environment KTSEnv
removeIfExists(c("G.2.50","RW1"), envir = environment())
genGapExample(RW, 50, 2, name = "G.2.50")

RW1 <- RW
RW1$value[KTSEnv$G.2.50$gaps] <- NA
graphics::plot(RW, type = "l", col = "red")
graphics::points(RW1, type = "l", col = "blue")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line