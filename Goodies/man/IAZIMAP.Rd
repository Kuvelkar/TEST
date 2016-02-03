\name{IAZIMAP}
\alias{IAZIMAP}

\title{Generates Map}
\description{
This function creates the map based on the variable at Gemeinde level or Kanton level.
}
\usage{
IAZIMAP(TabName="",ColName="", VarName="",ODBCDB="",breakpoints=NULL,
style=c("fixed","sd","unique"),precision=NULL, colr="", QueryCond="",
Title="",MapType="",FilePath="",SavPlt=FALSE)
}


\arguments{
  \item{TabName}{the table name from which data is taken}
  \item{ColName}{the column name on which the data is joined with the shapefile date. Usually BFS_NR for Gemeinde and KANT_NR for Kanton or any other representatives of the same.}
  \item{VarName}{the variable for which the map is to be plotted.}
  \item{ODBCDB}{the DSN name of the ODBC connection. Make sure that the ODBC connection with the given DSN is existing}
  \item{breakpoints}{the number of classes to which the data should be splitted}
  \item{style}{either "fixed" or "sd" or "unique". "unique" is used only for the color code maps.}
  \item{precision}{precision of the digits used.}
  \item{colr}{either a predefined vector of colors assigned to a variable or leave it empty- it will popup a window yo choose color while running. fro that its possible to select a set of pre defined colors or manual colors}
  \item{QueryCond}{usually, the where condition part of the select data query.}
  \item{Title}{the title to be assigned to the Map legend.}
  \item{MapType}{either "Gemeinde" or "Kanton". If it Gemeinde, it plot a Gemeinde level map else plot the Kanton level map.}
  \item{FilePath}{the path at which the maps is to be saved.}
  \item{SavPlt}{logical. If TRUE save the map in the specified path given by FilePath}
  
}
\details{

}
\value{
creates the map
}

\author{
IAZI
}

\examples{
require(RColorBrewer)
newcol <- c(brewer.pal(9,"Blues")[7:4],
brewer.pal(8,"BuPu")[[3]],brewer.pal(9,"Oranges")[3:5])

IAZIMAP(TabName="IF_R_Map",ColName="BFS_NR", VarName="IAZI_Fakt0713",
ODBCDB="IAZISYS",breakpoints=8,precision=4,style="fixed",Title="Hedo",
MapType="Gemeinde",FilePath="",SavPlt=FALSE, colr=newcol)
}
