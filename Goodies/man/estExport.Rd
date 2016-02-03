\name{estExport}
\alias{estExport}
\title{Export the estimated values from the model }
\description{This function exports the estimated values from the model to an SQL table or a CSV file.}
\usage{estExport(X, Table = "", File = "", varTypes = varTypes, ...)}

\arguments{
  \item{X}{the object to be exported.}
  \item{Table}{destination table name.}
  \item{File}{destination CSV file name.}
  \item{varTypes}{type of the variable to create variables with proper data type in SQL.}
  \item{\dots}{other arguments.}
}
\details{Use DBExport() and also FName( ).}
\value{Exports the object to the specified SQL table or as a CSV file.}
\author{IAZI}
\examples{
## FName ( ) is required while running. Specify FilePath
## For Example:
## FilePath<-getwd()
}
