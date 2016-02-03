\name{modExport}
\alias{modExport}
\title{Export the estimated betas or covariance matrix from the model}
\description{This function is used to export the model betas or covariance matrices.}
\usage{modExport(X, Table = "", File = "", varTypes = "", ...)}
\arguments{
  \item{X}{the object to be exported.}
  \item{Table}{destination table name.}
  \item{File}{destination file.}
  \item{varTypes}{type of the variable to create variables with proper data type in SQL.}
  \item{\dots}{other arguments.}
}
\details{Use DBExport()}
\value{Exports the object to the specified SQL table or as a CSV file.}
\author{IAZI}
\examples{
## FName ( ) is required while running. Specify FilePath
## For Example:
## FPath<-FilePath<-getwd()
}




