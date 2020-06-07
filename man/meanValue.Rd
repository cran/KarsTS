\name{meanValue}
\alias{meanValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
meanValue: mean value filling
}
\description{
This function replaces each missing value by the mean (or median) of the values located in equivalent positions in the periods around. It is used through the button Mean Value in the Filling menu.
}

%- maybe also 'usage' for other objects documented here.
\details{
The user needs to enter the following inputs: time series, period, number of surrounding periods to consider, maximum number of iterations, minimum number of observations ar one side and statistic (median or mean). Note that the surrounding periods can have missing values also; they can even be completely missing.The method can be applied iteratively, although it is not advisable to use many iterations because filling missing values does not increases the real amount of information available. In case the missing values concentrate at one side, the filling might be biased. The minimum number of observations at one side is useful to discard these biased fillings. 

}
\value{
The filled time series in the environment susEnv and a summary of the results on the output window.
}

\author{
Marina Saez Andreu
}
