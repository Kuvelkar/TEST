\name{PHS.DistributionPosX}
\alias{PHS.DistributionPosX}
\title{Plot histogram of the log and sqrt of a variable together}
\description{This function plots the histograms of the variable name in a dataset or matrix X, of its log and of its square root.}
\usage{PHS.DistributionPosX(X, name, graphfolder = "", savepdf = TRUE, savepng = TRUE)}
\arguments{
  \item{X}{a data frame or matrix of variables.}
  \item{name}{variable name.}
  \item{graphfolder}{the folder name to be saved.}
  \item{savepdf}{logical. If TRUE, the graph is saved in the specified graphfolder as pdf.}
  \item{savepng}{logical. If TRUE, the graph is saved in the specified graphfolder as png.}
}
\details{This function uses FName( ) to save the plot in a specified path. So, the argument FilePath must be specified before executing this function in order to save the plot. }
\value{Plot the histograms.}
\author{IAZI}
\examples{
## Define the  FilePath to save the plot.
## For example: FilePath<-getwd()

regdata <- cbind.data.frame(X1=rnorm(50), X2=rnorm(50), X3=rnorm(50),
X4= abs(rnorm(50)*100),X5= as.factor(rep(seq(1,5),10)))
attach(regdata)
PHS.DistributionPosX(regdata, name="X4", graphfolder=" ", savepdf=FALSE, savepng=FALSE)
}
