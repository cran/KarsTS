\name{refreshDataSetsList}
\alias{refreshDataSetsList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
refreshDataSetsList: refreshes Susana data set list
}
\description{
This function identifies the time series, gap sets and recurrence matrices present in the environment susEnv.It returns a list of data sets, which is assigned to susEnv so that it is accesible from all the package functions. 
}
\usage{
refreshDataSetsList(outp = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outp}{
when outp = TRUE, it shows a summary of the data sets on the output window.
}
}

\value{
It returns a list containing: the time series names, the number of time series, the gap sets names, the number of gap sets, the recurrence matrices names and the number of recurrence matrices. 
}

\details{
The function is used internally every time a new data set is created in order to refresh the data set list. From this list, Susana extracts information every time it creates a check box or radiobutton of gap sets. Optionally, information about the data sets appears on the output window (when outp = TRUE ). This happens when the user runs the function by means of the button List in the Time Series or Gap Sets menus. In this case, the user gets to see the information. 
}

\author{
Marina Saez Andreu
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
