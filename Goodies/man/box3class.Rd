\name{box3class}
\alias{box3class}
\title{Draws boxplots for various classes}
\description{Draws a boxplot with the Reference statistics and with points in the various classes for every property.}
\usage{box3class(XVarIP, YVarIP, IDIP, WeightIP = F, LegendIP = "IP", StatRef, 
          LegendRef = "Reference",MyMain = "",savPlt = "", YLIMMan = F, 
          DispVal = F, confint = T, SignLim = 5, OtherLbl = "Other", 
          TitClaVis = T, DigTab = 1, ...)}
\arguments{
  \item{XVarIP}{X Variable.}
  \item{YVarIP}{Y Variable.}
  \item{IDIP}{ID.}
  \item{WeightIP}{weight.}
  \item{LegendIP}{legend of the plot.}
  \item{StatRef}{reference to calculate the statistics.}
  \item{LegendRef}{reference legend.}
  \item{MyMain}{title of the boxplot.}
  \item{savPlt}{path to be saved.}
  \item{YLIMMan}{manual limiyt for Y value.}
  \item{DispVal}{logical. If TRUE, displays the statistics associated.}
  \item{confint}{logical. If TRUE, print the confidence intervals.}
  \item{SignLim}{minimum number of observations to draw the boxplot.}
  \item{OtherLbl}{label.}
  \item{TitClaVis}{logical. If TRUE, the plot will be visible.}
  \item{DigTab}{rounding and formatting option to print the statistics.}
  \item{\dots}{other arguments.}
}
\details{A maximum of five properties per graph is allowed.
If a class for the units is not present in the benchmark, the class is set to "other".
If the class "other" is not present for the benchmark, the points in "other" are not drawn. 
(Alternative strategy: create the class in benchmark and leave it without box)
NAs are not allowed.
It also uses FName( ). In order to send the output to a file or save the plot its necessary to define the FilePath before running this function.}
\value{Draws boxplot}
\author{IAZI}
\examples{## FName ( ) is required while running. Specify FilePath before running the function.
## For Example:
## FilePath<-getwd()}




