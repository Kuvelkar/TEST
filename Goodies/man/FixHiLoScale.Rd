\name{FixHiLoScale}
\alias{FixHiLoScale}
\title{Calculates the limits for boxplots keeping the upper limit fixed}
\description{This function calculates the quantiles, lower and upper limits. Used as a subsidiary in all the boxplot functions resboxchar, resboxcont etc. Here, the upper limit is fixed.}
\usage{
FixHiLoScale(X, HiLim, probs = c(lowCut = 0.1, hiCut = 0.9), ...)
}
\arguments{
  \item{X}{object to calculate the limits.}
  \item{HiLim}{value of the upper limit(fixing the upper limit).}
  \item{probs}{c(lowCut=0.1,hiCut=0.9), fixed quantile limits.}
  \item{\dots}{other arguments.}
}
\details{Used in drawing boxplots while calculating the associated statistics.}
\value{Returns a vector with lower limit and fixed upper limit for drawing the box-pot.}
\author{IAZI}
\examples{V1<-rep(seq(1,5),10)
V2<-round(abs(rnorm(50))*100)
df<-data.frame(V1,V2)
FixHiLoScale  (df$V2, 220, probs=c(lowCut=0.1,hiCut=0.9))
}
