\name{PHS.DistributionLog}
\alias{PHS.DistributionLog}
\title{Plot histogram for shifted variables}
\description{This function plots the histograms of the log of the shifted variable name in a dataset or matrix X. }
\usage{PHS.DistributionLog(X, name, Shift = c(1, 0.01, 1e-04), graphfolder = "", 
savepdf = TRUE, savepng = TRUE)}
\arguments{
  \item{X}{a data frame or matrix of shifted variables.}
  \item{name}{the name of the variable.}
  \item{Shift}{threes values are defined in parameter Shift. The default is c (1, 0.01, and 0.0001).}
  \item{graphfolder}{the folder name to be saved.}
  \item{savepdf}{logical. If TRUE, the graph is saved in the specified graphfolder as pdf.}
  \item{savepng}{logical. If TRUE, the graph is saved in the specified graphfolder as png.}
}
\details{This function uses FName( ) to save the plot in a specified path. So, the argument FilePath must be specified before executing this function in order to save the plot.
If the folder is not specified it creates the plot in the FilePath not in the folder.}
\value{Plot the histograms.}
\author{IAZI}
\examples{
## Define the  FilePath to save the plot
## For example: FilePath<-getwd()

dat <- cbind.data.frame(V1=rnorm(50), V2=rnorm(50), V3=rnorm(50),
V4= abs(rnorm(50)*100),V5= as.factor(rep(seq(1,5),10)))
attach(dat)
PHS.DistributionLog(dat, name="V4", Shift=c(1,0.01,0.0001), graphfolder="", 
savepdf=FALSE,savepng=FALSE)
}
