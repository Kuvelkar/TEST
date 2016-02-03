\name{boxplotSQL}
\alias{boxplotSQL}
\title{Create the statistics associated with boxplot taking data from SQL}
\description{This function produces statistics like function bowplot, but takes the data from an SQL table.}
\usage{boxplotSQL(TabName, SW = F, probs = c(lowCut = 0.1, hiCut = 0.9),DispVal = T,
           Export = NULL, confcol = 13, TitClaVis = T, DigTab = 1, ...)}
\arguments{
  \item{TabName}{the table from which data to be taken.}
  \item{SW}{choose for which group of variables the calcualtion is being done.}
  \item{probs}{c(lowCut=0.1,hiCut=0.9).}
  \item{DispVal}{logical. If TRUE, print the statistics.}
  \item{Export}{logical. If TRUE, export the results.}
  \item{confcol}{default 13. }
  \item{TitClaVis}{logical.If TRUE, displays the plot.}
  \item{DigTab}{sets the rounding and formating options, 1 by default.}
  \item{\dots}{other arguments.}
}
\details{It also uses FName( ). In order to send the output to a file or save the plot its necessary to define the FilePath before running this function.}
\value{This function displays or export (if specified) the list of statistics (as in bowplot) to some table in SQL.}
\author{IAZI}
\examples{
## FName ( ) is required whle running. Specify FilePath ##before running the function 
##in order to avoid the error.
## For Example:
## FilePath<-getwd()
}

