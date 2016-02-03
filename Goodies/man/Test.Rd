\name{Test}
\alias{Test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adding two numbers
}
\description{
Adds two numbers
}
\usage{
x(x,y)
}

\arguments{
  \item{x}{
a single numeic value
}
  \item{y}{
a single numeic value
}
}
\details{
This function sums up the two values passed ot it.
}
\value{
The value is also a numeric value which is the sum of the two parameters passed
}
\references{
http://127.0.0.1:20442/library/base/html/mean.html
}
\author{
TVK
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
mean, median
}
\examples{

## The function is currently defined as
function (x,y) 
{
  c=x+y
	return(c)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sum}
\keyword{add}% __ONLY ONE__ keyword per line
