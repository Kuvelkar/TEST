\name{boxclass}
\alias{boxclass}
\title{Boxplot for a discrete factor variable}
\description{Draws a boxplot of the YVar for a discrete factor variable XVar. NAs are not allowed.}
\usage{boxclass(XVar, YVar, Weight = F, MyMain = "", savPlt = NULL, DispVal = F, 
         confint = T, YLIMMan = F, SignLim = 5, OtherLbl = "Other", 
         TitClaVis = T, DigTab = 1, ...)}
\arguments{
  \item{XVar}{the discrete factor variable.}
  \item{YVar}{the variable to make the boxplot.}
  \item{Weight}{either logical or a value.}
  \item{MyMain}{title of the boxplot.}
  \item{savPlt}{logical. If TRUE, saves the plot.}
  \item{DispVal}{logical. If TRUE, displays the output.}
  \item{confint}{logical. If TRUE, displays the confidence intervals.}
  \item{YLIMMan}{limits for the confidence interval. Can be logical also.}
  \item{SignLim}{minimum number of observations.}
  \item{OtherLbl}{if the population of a class is smaller than SignLim, the class is renamed OtherLbl.}
  \item{TitClaVis}{logical. If TRUE, the plot will be visible.}
  \item{DigTab}{rounding and formatting option to print the statistics.}
  \item{\dots}{other arguments.}
}
\details{It also uses FName( ). In order to send the output to a file or save the plot its necessary to define the FilePath before running this function.}
\value{Creates the boxplots}
\author{IAZI}
\examples{
## FName ( ) is required. Specify FilePath
## For Example:
## FPath<-getwd()
}




