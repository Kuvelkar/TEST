\name{SQLExport}
\alias{SQLExport}
\title{Export an object as an SQL table}
\description{This function saves the object X on SQL server as a table}
\usage{SQLExport(X, Table = "", colNames = TRUE, rowNames = FALSE, ...)}
\arguments{
  \item{X}{object to be saved.}
  \item{Table}{destination table name.}
  \item{colNames}{logical. If TRUE, include the column names during export.}
  \item{rowNames}{logical. If TRUE, include the row names during export.}
  \item{\dots}{other arguments.}
}
\details{}
\value{Exports the object to SQL.}
\author{IAZI}
\examples{}