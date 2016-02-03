\name{hist.factorP}
\alias{hist.factorP}
\title{Draws the histogram of a factor variable}
\description{This function calculates the counts associated with the histogram of a factor variable.}
\usage{hist.factorP(X, DispVal = F, ...)}
\arguments{
  \item{X}{the variable or data set.}
  \item{DispVal}{if TRUE print the statistics.}
  \item{\dots}{other arguments.}
}
\details{}
\value{Returns the number fo observations associated with each factor.}
\references{}
\author{IAZI}
\examples{
dat <- cbind.data.frame(V1=rnorm(50), V2=rnorm(50), 
V3=rnorm(50),V4= abs(rnorm(50)*100),
V5= as.factor(rep(seq(1,5),10)))
hist.factorP(dat$V5,col=IAZIColor("IAZIGreen2"),DispVal=TRUE)
}
