\name{box2class}
\alias{box2class}
\title{Draws a double boxplot}
\description{Draws a double boxplot of the first with IP, the second with Ref }
\usage{box2class(XVarIP, YVarIP, WeightIP = F, LegendIP = "IP", XVarRef, YVarRef,
          WeightRef = F, LegendRef = "Reference", MyMain = "", 
          savPlt = "",YLIMMan = F, DispVal = F, confint = T, SignLim = 5, 
          OtherLbl = "Other",TitClaVis = T, DigTab = 1, ...)}
\arguments{
  \item{XVarIP}{X Variable.}
  \item{YVarIP}{Y Variable.}
  \item{WeightIP}{weight.}
  \item{LegendIP}{legend of the plot.}
  \item{XVarRef}{reference X variable.}
  \item{YVarRef}{reference X variable.}
  \item{WeightRef}{reference weight.}
  \item{LegendRef}{reference legend.}
  \item{MyMain}{title of the boxplot.}
  \item{savPlt}{path to be saved.}
  \item{YLIMMan}{manual limiys for Y value.}
  \item{DispVal}{logical. If TRUE, displays the statistics associated.}
  \item{confint}{logical. If TRUE, print the confidence intervals.}
  \item{SignLim}{minimum number of observations to draw the boxplot.}
  \item{OtherLbl}{label.}
  \item{TitClaVis}{logical. If TRUE, the plot will be displayed.}
  \item{DigTab}{rounding and formatting option to print the statistics.}
  \item{\dots}{other arguments.}
}
\details{
Use bowplot( ),FName( ) etc.
}
\value{Draws a double boxplot.}
\author{IAZI}
\examples{}




