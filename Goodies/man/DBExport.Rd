\name{DBExport}
\alias{DBExport}
\title{Export Tables}
\description{This function export tables to SQL or to a CSV file}
\usage{DBExport(X, Table="", File = "", colNames=TRUE, rowNames=FALSE, varTypes="", ...)}
\arguments{
  \item{X}{data set to be exported.}
  \item{Table}{SQL table name to export.}
  \item{File}{CSV file name to export.}
  \item{colNames}{logical.If TRUE, export with column names.}
  \item{rowNames}{logical.If TRUE, export with row names.}
  \item{varTypes}{type of the variable to create variables with proper data type in SQL.}
  \item{\dots}{other arguments.}
}
\details{Use SQLCompatibility() to return the data frame with more SQL compatible names.
 It also use FName( ). So the FilePath should be specified before running the function.}
\value{Exports data frame into the respective DB or CSV file in to the specified path.}

\author{IAZI}
\examples{
## FName ( ) is required while running. Specify FilePath
## For Example:
## FilePath<-getwd()
}