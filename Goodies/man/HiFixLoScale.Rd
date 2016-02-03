\name{HiFixLoScale}
\alias{HiFixLoScale}
\title{Calculates the limits for drawing boxplot keeping the lower limit fixed}
\description{This function calculates the quantiles, lower and upper limits. Used as a subsidiary in all the boxplot functions resboxchar, resboxcont etc. Here, lower limit fixed.}
\usage{
HiFixLoScale(X, LowLim, probs = c(lowCut = 0.1, hiCut = 0.9), ...)
}
\arguments{
  \item{X}{object to calculate the limits.}
  \item{LowLim}{value of the lower limit(fixing the lower limit).}
  \item{probs}{c(lowCut=0.1,hiCut=0.9), fixed quantile limits.}
  \item{\dots}{other arguments.}
}
\details{Used in drawing boxplots while calculating the associated statistics.}
\value{Returns a vector with fixed lower limit value and upper limit for drawing the box-plot.}
\author{IAZI}

\examples{
V1<-rep(seq(1,5),10)
V2<-round(abs(rnorm(50))*100)
df<-data.frame(V1,V2)
HiFixLoScale(df$V2, 25, probs=c(lowCut=0.1,hiCut=0.9))
}
