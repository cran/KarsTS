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
