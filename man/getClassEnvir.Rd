\name{getClassEnvir}
\alias{getClassEnvir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getClassEnvir: get objects of a class from an environment
}
\description{
This function identifies objects of a class in an environment 
}
\usage{
getClassEnvir(classGet = "list", envir = KTSEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{classGet}{
The class
}
  \item{envir}{
The environment
}
}

\value{
A vector with the names of the objects
}

\author{
Marina Saez Andreu
}

\examples{

## Generate a time series
genTSExample(name = "TS", InKTSEnv = TRUE)
## Generate some gap sets consisiting of four gaps of 3 NAs each
genGapExample(KTSEnv$TS, 3, 4, name = "NewGapSet1", InKTSEnv = TRUE)
genGapExample(KTSEnv$TS, 3, 4, name = "NewGapSet2", InKTSEnv = TRUE)
genGapExample(KTSEnv$TS, 3, 4, name = "NewGapSet3", InKTSEnv = TRUE)

KTSEnv$NoGapSet <- list(1:6, 3:4, "Lemon")

# Get the lists in KTSEnv (gap sets and other)
listsInKTSEnvget <- getClassEnvir(classGet = "list", envir = KTSEnv)
# Detect gap sets in the environment KTSEnv
detectedGapSets <- gapDetect()

listsInKTSEnvget
detectedGapSets

}
