\name{IAZIColor}
\alias{IAZIColor}
\title{IAZI color combinations}
\description{to get standard IAZI color.}
\usage{IAZIColor(colr="")}
\arguments{
  \item{colr}{the color name.}
  }
\details{The color should be any one of IAZIBlue1, IAZIBlue6, IAZIGreen1, IAZIGreen2.}
\value{Returns the specified color.}
\author{IAZI}
\examples{
hist(1:10,col=IAZIColor("IAZIBlue1"))
hist(1:10,col=IAZIColor("IAZIBlue6"))
hist(1:10,col=IAZIColor("IAZIGreen1"))
hist(1:10,col=IAZIColor("IAZIGreen2"))
}