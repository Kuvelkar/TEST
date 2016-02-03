\name{PHS.match}
\alias{PHS.match}
\title{Searching for character match}
\description{This function is an intuitive improvement of pmatch, using grep for incomplete match.
The parameters are first converted to a character vector by as.character( ).Empty strings do not match/are not matched.}
\usage{PHS.match(tosearch, incharvector)}
\arguments{
  \item{tosearch}{the character vector to be matched.}
  \item{incharvector}{the character vector in which the search to be done.}
}
\details{}
\value{Return the position number(s) of the incharvector where the match is found.}
\author{IAZI}
\examples{PHS.match(c("", "abc", "a"), c("", "ab", "abc"))}
