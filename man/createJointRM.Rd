\name{createJointRM}
\alias{createJointRM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createJointRM: creates a joint recurrence matrix
}
\description{
This function creates a joint recurrence matrix. It is used through the Joint Recurrence Matrix button in the Analysis Menu
}
%- maybe also 'usage' for other objects documented here.
\details{
The inputs are two time series. They must have the same sampling period. The shorter one will determine the size of the recurrence matrix.
The matrix can be visualized by means of the Plot Joint Recurrence Matrix button in the Plots Menu
}
\value{
A joint recurrence matrix in the environment susEnv
}
\references{
Marwan,R., Romano, M.C., Thiel,M., Kurths,J.(2007): Recurrence plots for the analysis of complex systems. Physics Reports 438, 237-329.
}
\author{
Marina Saez Andreu
}
