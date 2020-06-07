\name{selectionGaps}
\alias{selectionGaps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
selectionGaps: select gaps in a time series
}
\description{
This function is used create a gap set from a time series. The new gap set contains the gaps that meet some criteria. The available criteria are: all gaps, minimum length, maximum length and specific gaps (selected from a list). The user can choose one or more criteria.Through the button Gap selection in the Gap Sets menu.
}
%- maybe also 'usage' for other objects documented here.
\details{
The criteria can be combined in two ways: a. (minimum length and maximum length) and specific gaps; b. (minimum length or maximum length) and specific gaps. When the user chooses only one length criterion, the combination must be the second (the first one would produce and empty gap). 
}
\value{
A gap set gathering the required gaps. 
}

\author{
Marina Saez Andreu
}
