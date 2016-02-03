\name{breakpoint}
\alias{breakpoint}

\title{
Creates the cuts based on the breakpoints passed
}
\description{
creates the cuts based on the breakpoints and return the (breakpoints+1) cut values .
}
\usage{
breakpoint(Data=NULL,breakpoints=NULL, style=c("fixed","sd","unique"),precision=NULL)
}
\arguments{
  \item{Data}{Vector to find the break points}
  \item{breakpoints}{the number of breaks needed}
  \item{style}{either "fixed" or "sd" or "unique". "unique" is used only for the color code maps.}
  \item{precision}{Decimal point precision required while creating breaks.}
}
\details{
This is used internally for iAZIMAP function. But, can be used as a general function also.
}
\value{
returns the cut points as a vector with breakpoints+1 elements.
}

\author{
IAZI
}

\examples{
set.seed(2)
Data<-(rnorm(100)*100)
breakpoint(Data,8,"sd",2)
}

