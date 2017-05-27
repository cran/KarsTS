\name{getRecurrencePoints}
\alias{getRecurrencePoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to get recurrent points in a time series
}
\description{
This function is the core of the function createSimpleRm. It finds the recurrence points in a time series, possibly embedded. 
}
\usage{
getRecurrencePoints(timSer, embedDim, lagDelay, threshold)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timSer}{
The time series
}
  \item{embedDim}{
The embedding dimension
}
  \item{lagDelay}{
The delay for the embedding (in lags)
}
  \item{threshold}{
The threshold
}
}
\details{
Two points in the phase space are recurrent when the distance between them (infinite norm) is less than the threshold. \cr
In a recurrence matrix the X positions read rightwards and the Y positions read upwards. Recurrence matrices are symmetric and their diagonal (line X = Y) cannot contain not-recurrent points. For this reason, only the upper triangle is stored.  
}
\value{
A list containing the X and Y positions of the recurrent points in the recurrence matrix (upper triangle)
}

\author{
Marina Saez Andreu
}

\examples{

# # Generate time series
timSer <- genTSExample(stationary = FALSE, InKTSEnv = FALSE)
graphics::plot(timSer)

# # Calculate recurrence matrix
RP <- getRecurrencePoints(timSer, 1, 0, threshold = 0.1)
X <- RP$recPointsX
Y <- RP$recPointsY

# # Recurrence plot
LT <- nrow(timSer)
graphics::par(pty = "s")
# Upper triangle
graphics::plot(timSer$time[X], timSer$time[Y], cex = 0.3, col = 4, xlab = "", ylab = "")
# Lower triangle
graphics::points(timSer$time[Y], timSer$time[X], cex = 0.3, col = 4)
# Diagonal
graphics::points(1:LT, 1:LT, cex = 0.3, col = 4)

graphics::par(pty = "m")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
