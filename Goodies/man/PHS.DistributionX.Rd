\name{PHS.DistributionX}
\alias{PHS.DistributionX}
\title{Histogram and qqnorm of a variable together}
\description{This function plots the histograms and the qqnorm of the variable name in a dataset or matrix.}
\usage{PHS.DistributionX(X, name, label = "", graphfolder = "", savepdf = TRUE, 
                  savepng = TRUE)}
\arguments{
  \item{X}{ a data set or matrix.}
  \item{name}{ variable to plot.}
  \item{label}{ label  prefixed to the graph.}
  \item{graphfolder}{ folder to which the graph is being saved.}
  \item{savepdf}{ logical. If TRUE, the graph is saved in the specified graphfolder as pdf.}
  \item{savepng}{ logical. If TRUE, the graph is saved in the specified graphfolder as png.}
}
\details{This function uses FName( ) to save the plot in a specified path. So, the argument FilePath must be specified before executing this function in order to save the plot.}
\value{Plot the histogram and qqnorm.}
\author{IAZI}
\examples{
## Define the  FilePath to save the plot
## For example: FilePath<-getwd()

dat <- cbind.data.frame(V1=rnorm(50), V2=rnorm(50), V3= abs(rnorm(50)*100),
V4=rnorm(50),V5= as.factor(rep(seq(1,5),10)))
attach(dat)
PHS.DistributionX (dat, "V3", label ="Hist", graphfolder="", savepdf=FALSE,
savepng=FALSE)
}
