\name{setCorrectDate}
\alias{setCorrectDate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
setCorrectDate: verifies date entry and returns proper output
}
\description{
This function is used inside the verifyDateEntry function, whic is used to verify if a date is correct. The function setCorrectDate verifies if the year, month, day, hour, minutes or seconds are correct. For example, the day has to be an integer between 1 and 31, the minutes and seconds must be integers between 0 an 59 etc.
}
\usage{
setCorrectDate(x, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The element to test
}
  \item{type}{
The type of element: year, month, day, hour, minute or second
}
}

\value{
The element if it is valid; NA otherwise.
}

\author{
Marina Saez Andreu
}
