\name{ModPass}
\alias{ModPass}

\title{Calculate model for each Pass in IIT.ModPass.
}
\description{ This function helps to Organise IIT.ModPass in a better way.}
\usage{
ModPass(Pass, form = "", regdata = "", Outliers = "")
}

\arguments{
  \item{Pass}{the specification of the regression technique in the pass. E.g. Pass1=list(Meth="lm",Crit="res",Cut=2.5).}
  \item{form}{the model formula.}
  \item{regdata}{the data on which the model calculation is performed.}
  \item{Outliers}{the initial outlier vector.}
  }
\details{This function choose a method for performing regression based on the different Pass'i'.}
\value{
Internal to IIT.ModPass
}
\author{IAZI}