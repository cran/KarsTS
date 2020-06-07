\name{createSimpleRM}
\alias{createSimpleRM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createSimpleRM: creates a recurrence matrix
}
\description{
This function creates a recurrence matrix. It is used through the Recurrence Matrix button in the Analysis Menu.
}
%- maybe also 'usage' for other objects documented here.
\details{
The inputs are a time series, the delay, the embedding dimension and a name for the output matrix.
The matrix can be visualized by means of the Recurrence Plot button in the Plots Menu
Note that there are specific buttons to create and plot cross and joint recurrence matrices.

}
\value{
The recurrence points are represented in a two-column data frame by their positions in the recurrence matrix. Only the upper triangle is stored.
}
\references{
Marwan,R., Romano, M.C., Thiel,M., Kurths,J.(2007): Recurrence plots for the analysis of complex systems. Physics Reports 438, 237-329.
}
\author{
Marina Saez Andreu
}
