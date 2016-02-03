\name{SQLCompatibility}
\alias{SQLCompatibility}
\title{Converting data frame to SQL compatible names}
\description{This function converts a data frame with SQL more compatible names.}
\usage{SQLCompatibility(X)}
\arguments{
  \item{X}{a data frame.}
}
\details{}
\value{Returns a data frame with SQL compatible names.
It handles ".", "\", "*", "/", "(", ")".
If X is not a data frame, it is returned unchanged.
}
\author{IAZI}
\examples{}




