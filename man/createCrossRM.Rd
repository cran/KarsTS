\name{createCrossRM}
\alias{createCrossRM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createCrossRM: creates a cross recurrence matrix
}
\description{
This function creates a cross recurrence matrix. It is used through the Cross Recurrence Matrix button in the Analysis Menu
}
%- maybe also 'usage' for other objects documented here.
\details{
The inputs are two time series representing similar variables (for example, two air temperature time series). They must have the same sampling period, although they can have different lengths.
The matrix can be visualized by means of the Plot Cross Recurrence Matrix button in the Plots Menu
}
\value{
A cross recurrence matrix in the environment susEnv. Cross recurrence matrices are not symmetrical, therefore the entire matrix is stored (not only a triangle).
}
\references{
Marwan,R., Romano, M.C., Thiel,M., Kurths,J.(2007): Recurrence plots for the analysis of complex systems. Physics Reports 438, 237-329.
}
\author{
Marina Saez Andreu
}
