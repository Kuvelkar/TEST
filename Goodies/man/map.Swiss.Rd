\name{map.Swiss}
\alias{map.Swiss}
\title{
map.Swiss function(internal to IAZIMAP function) to create map}
\description{This function creates the map for either Gemeinde or Kanton}
\usage{
map.Swiss(Level,lak,z,title=NULL,breaks=NULL,cex.legend=.8,bw=.2,
col.vec=NULL,plot.legend=TRUE,rounding=NULL,style="")
}

\arguments{
  \item{Level}{the name of the shapefile which shows at which level (either kanton or Gemeinde) the plotting is.}
  \item{lak}{denotes the name of shapefile to plot the lakes}
  \item{z}{the variable into which the variable whose map is to be generated.}
  \item{title}{title of the Legend of the Map.}
  \item{breaks}{number of breaks. ie., vector of cuts}
  \item{cex.legend}{graphical parameter.}
  \item{bw}{graphical parameter.}
  \item{col.vec}{the name of the color vector. Takes intrnally the colors while chosing from the GUI or while passingfrom IAZIMAP function.}
  \item{plot.legend}{logical. If TRUE, plots the legend.}
  \item{rounding}{precision of the digits.}
  \item{style}{either "fixed" or "sd"and "unique" for color code plotting.}
}
\details{
This is an internal function that plays the leading role while creating maps using IAZIMAP function.
}


\author{
IAZI
}

