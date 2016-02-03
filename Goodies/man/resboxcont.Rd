\name{resboxcont}
\alias{resboxcont}
\title{Draws the boxplpot of reiduals for continous variables}
\description{This function draws a box plot of the residuals Res for a continuous variable XVar. The limits are calculated on the bases of deciles. NAs are not allowed.}
\usage{resboxcont(Res, XVar, Weight = F, MyMain = "", savPlt = "", 
           DispVal = T, confint = T, SignLim = 5, ...)}
\arguments{
  \item{Res}{residual vector to be plotted.}
  \item{XVar}{the variables for which the residuals are to be plotted.}
  \item{Weight}{either logical or a value.}
  \item{MyMain}{the title of the box-plot.}
  \item{savPlt}{path to be saved.}
  \item{DispVal}{logical. If T, print the statistics.}
  \item{confint}{logical. If T, print the confidence intervals.}
  \item{SignLim}{minimum number of observations required to make the plot. 5 by default.}
  \item{\dots}{other arguments.}
}
\details{}
\value{This plots the box-plot of the residuals of the variable given and saves in the path specified in savPlt. This also use resboxcontfix ( ) and bowplot( ).}
\author{IAZI}
\examples{
V1<-1:50
V2<-abs(rnorm(50)*100)
Data<-data.frame(V1,V2)
splus <- list(boxwex=0.4, staplewex=1, outwex=0, boxfill=rgb(220,225,151, alpha = 255, 
maxColorValue = 255), medlwd=3,  medcol= rgb(0,86,132, alpha = 255,
maxColorValue = 255),whisklty=1, outlty=1, outpch=NA)
FilePath=getwd()
resboxcont(Data$V2, Data$V1,MyMain="Histogram",pars=splus)
}
