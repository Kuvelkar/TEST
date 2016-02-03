\name{HiLoScale}
\alias{HiLoScale}
\title{calculates the upper and lower lilmits for drawing a boxplot.}
\description{This function calculates the quantiles, lower and upper limits. Used as a subsidiary in all the boxplot functions resboxchar, resboxcont etc.}
\usage{
HiLoScale(X, probs = c(lowCut = 0.1, hiCut = 0.9), ...)
}
\arguments{
  \item{X}{object to calculate the limits.}
  \item{probs}{c(lowCut=0.1, hiCut=0.9).}
  \item{\dots}{other arguments.}
}
\details{Used in drawing boxplots while calculating the associated statistics.}
\value{Returns a vector with lower and upper limit for drawing the box-plot.}
\author{IAZI}

\examples{V1<-rep(seq(1,5),10)
V2<-round(abs(rnorm(50))*100)
df<-data.frame(V1,V2)
HiLoScale(df$V2, probs=c(lowCut=0.1,hiCut=0.9))
}