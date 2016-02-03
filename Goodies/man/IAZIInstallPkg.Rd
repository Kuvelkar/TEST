\name{IAZIInstallPkg}
\alias{IAZIInstallPkg}
\title{IAZIInstallPkg Package Installation}
\description{This function automatically install the IAZI standard packages or the speccified packages.}
\usage{
IAZIInstallPkg(pkg="")
}

\arguments{
  \item{pkg}{a vector of package names }
}
\details{
This will by default install the IAZI Standard packages "MASS", "RODBC", "Hmisc", "stringr", "corrgram","gtools"and "Goodies". It will also install the packages if specified.
}
\value{
This does not return any value. Install packages to R library.
}
\references{

}
\author{
IAZI
}
\note{
}


\seealso{

}
\examples{IAZIInstallPkg(pkg=c("polyr","gplots"))

}


