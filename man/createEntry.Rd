\name{createEntry}
\alias{createEntry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createChb: creates a text entry on the main screen
}
\description{
This function creates a text entry on the input panel (on the main screen)
}
\usage{
createEntry(labTitle, textVariableName, defaultVal = "", font = KTSEnv$KTSFonts$T1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labTitle}{
The text entry title
}
  \item{textVariableName}{
The name of the variable associated to the text entry
}
  \item{defaultVal}{
Default value (empty entry)
}
  \item{font}{
Title font
}
}

\value{
A variable that will be passed to the corresponding OnOk function

}

\author{
Marina Saez Andreu
}
