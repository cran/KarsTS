\name{stlplusKTS}
\alias{stlplusKTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
stlplusKTS: loess seasonal decomposition
}
\description{
This function performs a loess seasonal decomposition using the function stlplus::stlplus.
Through the button Loess.decomp. in the Analysis menu.
}
%- maybe also 'usage' for other objects documented here.
\details{
The user enters via interface the time series, the period, the seasonal window, the trend window and the type of decomposition (additive or multiplicative). Optionally, the trend can be decomposed using more windows.
}
\value{
The trend, the seasonal and the irregular components.These bear the name of the original time series plus Tr, Sea and Rem, respectively. If the trend is further decomposed, the user must enter names for the output components. 
}

\author{
Marina Saez Andreu
}
