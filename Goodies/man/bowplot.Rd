\name{bowplot}
\alias{bowplot}
\title{Calculates the statistics associated with boxplot}
\description{This is used as the subsidiary function for drawing box-plots using resboxcont, resboxchar etc.}
\usage{bowplot(X, SW = NULL, probs = c(lowCut = 0.1, hiCut = 0.9),DispVal = T,
        Export = NULL, confcol = 13, TitClaVis = T, DigTab = 1, ...)}
\arguments{
  \item{X}{variable to draw the box-plot.}
  \item{SW}{specifies which group of variable.}
  \item{probs}{fixed.c(lowCut=0.1,hiCut=0.9).}
  \item{DispVal}{if TRUE, prints the matrix of statistics}
  \item{Export}{if not NULL exports the statistics to the   SQL table using modExport ().}
  \item{confcol}{fixed to 13.}
  \item{TitClaVis}{logical. If TRUE, the plot will be displayed.}
  \item{DigTab}{for rounding digits and formating parameter.Default is 1.}
  \item{\dots}{other parameters.}
}
\details{This function does not plot anything. It only returns a list with the following statistics.
It also uses FName( ). In order to send the output to a file or save the plot it is necessary to define the FilePath before running this function. it also uses PHS.bxp( )}
\value{Returns a list of sort, plot, names, n, max, uplim, Q1, median, Q3, lowlim, min, upconf, lowconf, mean, stdev, wtmean.}
\author{IAZI}
\examples{
## FName ( ) is required while running. Specify FilePath
## For Example:
## FilePath<-getwd()
}
