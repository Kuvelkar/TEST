\name{hist.factor}
\alias{hist.factor}
\title{Histogram for a factor variable}
\description{Creates a histogram on the current graphics device.}
\usage{hist.factor(x, plot=TRUE,probability=FALSE, include.lowest=T, 
            names = levels(x),...,xlab=deparse(substitute(x)))
}

\arguments{
  \item{x}{numeric or factor vector of data for histogram. If x is a factor (or category) hist will call hist.factor with all but the nclass and breaks arguments. If x is integer-valued hist.factor(x) may give you better results than hist(x). Missing values (NA) are allowed. 
}
  \item{plot}{logical, if TRUE, the histogram will be plotted; if FALSE, a list giving breakpoints and counts will be returned.}
  \item{probability}{logical flag: if TRUE, the histogram will be scaled as a probability density; the sum of the bar heights times bar widths will equal 1. If FALSE, the heights of the bars will be counts.}
  \item{include.lowest}{if TRUE (the default), the lowest bar will include data points equal to the lowest break, otherwise it will act like the other bars (see the description of the breaks argument).}
  \item{names}{levels(x)}
  \item{\dots}{additional arguments to barplot. The hist function uses the function barplot to do the actual plotting; consequently, arguments to the barplot function that control shading, etc., can also be given to hist. See the barplot documentation for arguments angle, density, col, and inside. Do not use the space or histo arguments. 
}
\item{xlab}{label for the plot x-axis. By default, this will be x. }

}
\details{}
\value{if plot=TRUE, a vector containing the coordinate of the center of each box is returned. 
If plot=FALSE, hist.factor returns a list with components
}
\author{IAZI}
\examples{
dat <- cbind.data.frame(V1=rnorm(50), V2=rnorm(50),
V3=rnorm(50),V4= abs(rnorm(50)*100),
V5= as.factor(rep(seq(1,5),10)))
hist.factor(dat$V5)
hist.factor(dat$V5,col=IAZIColor("IAZIGreen2"))
hist.factor(dat$V5,plot=FALSE)
}
