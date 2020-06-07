\name{createFAN}
\alias{createFAN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createFAN: creates a fixed amount of neighbors (FAN) matrix.
}
\description{
This function creates a fixed amount of neighbours (FAN) recurrence matrix. It is used through the FAN Recurrence Matrix button in the Analysis Menu.
}
%- maybe also 'usage' for other objects documented here.
\details{
The FAN recurrence matrix can be created from an existing matrix; in this case, only the closest neighbors are kept.The FAN recurrence matrix can be created directly from a time series. In this case, the embedding dimension, delay, tolerance, Theiler's window and fixed amount of neighbours must be provided via interface.
The matrix can be visualized by means of the FAN Recurrence Plot button in the Plots Menu.
Note that FAN recurrence matrices are not symmetric.
}
\references{
Marwan,R., Romano, M.C., Thiel,M., Kurths,J.(2007): Recurrence plots for the analysis of complex systems. Physics Reports 438, 237-329.
}
\author{
Marina Saez Andreu
}
