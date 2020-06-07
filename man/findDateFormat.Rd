\name{findDateFormat}
\alias{findDateFormat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
findDateFormat: finds the format of a set of dates
}
\description{
This function identifies the format of a set of dates from a list of allowed formats.
}
\usage{
findDateFormat(X, tz = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
The date (given as a character string)
}
  \item{tz}{
The time zone
}
}
\details{
The allowed formats are: "\%m/\%d/\%Y \%H:\%M", "\%Y/\%m/\%d \%H:\%M","\%Y-\%m-\%d \%H:\%M" and "\%m-\%d-\%Y \%H:\%M"
}
\value{
It returns the date format, ready to use in the funtion strptime. If the dates have different formats, the function returns "variousFormats". If the format is not one of the allowed ones, it returns "notAllowedformat".
}

\author{
Marina Saez Andreu
}
