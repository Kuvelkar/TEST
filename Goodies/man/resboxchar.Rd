\name{resboxchar}
\alias{resboxchar}
\title{Draws boxplot of residuals for a discrete character type variable}
\description{This function draws a box-plot of the residuals Res for a discrete char-type variable XVar. NAs are not allowed.}
\usage{resboxchar(Res, XVar, Weight = F, MyMain = "", savPlt = "", DispVal = T,
           confint = T, SignLim = 5, ...)}
\arguments{
  \item{Res}{residual vector to be plotted.}
  \item{XVar}{the variable to be plotted.}
  \item{Weight}{either logical or a value.}
  \item{MyMain}{the title of the box-plot.}
  \item{savPlt}{path to be saved.}
  \item{DispVal}{logical. If T, print the output.}
  \item{confint}{logical. If T, print the confidence intervals.}
  \item{SignLim}{the minimum number of observations required to plot. 5 by default.}
  \item{\dots}{other arguments.}
}
\value{This plots the box-plot of the residuals of the variable given and saves in the path specified in savPlt. This also use bowplot ( ).}
\author{IAZI}
\examples{
Year<-2006:2010
Price<-c(round(abs(rnorm(50)*100)))
df<-data.frame(Year,Price)
splus <- list(boxwex=0.4, staplewex=1, outwex=0, boxfill=rgb(220,225,151, alpha = 255,
maxColorValue = 255),medlwd=3, medcol= rgb(0,86,132, alpha = 255, maxColorValue = 255), 
whisklty=1, outlty=1, outpch=NA)
FilePath=getwd()
resboxchar(df$Price,df$Year, MyMain="Histogram",pars=splus)
}