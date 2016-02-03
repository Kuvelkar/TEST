\name{resboxcontfix}
\alias{resboxcontfix}
\title{Draws the boxplpot of reiduals for continous variables with fixed limits}
\description{This function draws a box-plot of the residuals Res for a continuous variable XVar. The limits are passed manually. NAs are not allowed.}
\usage{resboxcontfix(Res, XVar, newlim, Weight = F, MyMain = "", savPlt = "", 
              DispVal = T, confint = T, SignLim = 5, ...)}
\arguments{
  \item{Res}{residual vector to be plotted.}
  \item{XVar}{the variables for which the residuals are to be plotted.}
  \item{newlim}{the passed limits.}
  \item{Weight}{either logical or a value.}
  \item{MyMain}{the title of the box-plot.}
  \item{savPlt}{path to be saved.}
  \item{DispVal}{logical. If T print the statistics.}
  \item{confint}{logical. If T print the confidence intervals.}
  \item{SignLim}{by default 5.}
  \item{\dots}{other arguments.}
}
\value{This plots the box-plot of the residuals of the variable given and save in the path specified in  savPlt. This also use bowplot ( ).}
\author{IAZI}
\examples{V1<-1:50
V2<-abs(rnorm(50)*100)
Data<-data.frame(V1,V2)
splus <- list(boxwex=0.4, staplewex=1, outwex=0, boxfill=rgb(220,225,151, alpha = 255,
maxColorValue = 255), medlwd=3,  medcol= rgb(0,86,132, alpha = 255,
maxColorValue = 255),whisklty=1, outlty=1, outpch=NA)
FilePath=getwd()
resboxcontfix(Data$V2, Data$V1,newlim=c(0,10,20,30,40,50,60),
MyMain="Histogram",pars=splus)}




