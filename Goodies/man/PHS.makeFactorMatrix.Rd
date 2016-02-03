\name{PHS.makeFactorMatrix}
\alias{PHS.makeFactorMatrix}
\title{Transforms a factor variable to a dummy factor matrix }
\description{This function transforms a multi-level factor into a dummy variable factor matrix usable for regression.}
\usage{PHS.makeFactorMatrix(x, ref = 1, tol = .Machine$double.eps^0.5)}
\arguments{
  \item{x}{a factor variable to be converted.}
  \item{ref}{indicates which level is used as a reference. In case of inconsistent data, the first level is used as reference. If the reference is NULL, no level is removed.}
  \item{tol}{.Machine$double.eps^0.5}
}
\details{}
\value{Converts to a dummy matrix.}
\author{IAZI}
\examples{
regdata<-cbind.data.frame(V1=rnorm(50), V2=rnorm(50), V3=rnorm(50),
V4= abs(rnorm(50)*100),V5= as.factor(rep(seq(1,5),10)))
attach(regdata)
PHS.makeFactorMatrix(regdata$V5, ref=NULL, tol=.Machine$double.eps^0.5)
}




