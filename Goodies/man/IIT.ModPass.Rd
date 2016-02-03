\name{IIT.ModPass}
\alias{IIT.ModPass}

\title{
Advanced model building technique with user flexibility to perform different regression methodologies simultaneously
}
\description{
This function allows user to insert multiple techniques to perform regressions with user defined parameters simultaneously. 
e.g. it is possible to run a robust regression to identify the outliers from its robust weights and then perform a linear model excluding those outliers simultaneously using this function. The function accepts this techniques through different passes
}
\usage{
IIT.ModPass(form, regdata, OutInit, Instp = TRUE, stp = TRUE, CutPass = NULL, 
Pass1 = list(Meth = "lm", Crit = "res", Cut = 2.5),
Pass2 = list(Meth = "lm", Crit = "res", Cut = 2.5), 
Pass3 = list(Meth = "lm", Crit = "res", Cut = 2.5), 
Pass4 = list(Meth = "rlm", Crit = "w", Cut = 0.02, Alt = "cook", CAlt = 0.02), 
Pass5 = list(Meth = "rlm", Crit = "w", Cut = 0.02, Alt = "cook", CAlt = 0.02))
}

\arguments{
  \item{form}{The model formula.}
  \item{regdata}{the data on which the model calculation is performed.}
  \item{OutInit}{the initial outlier vector.}
  \item{Instp}{logical. If TRUE, performs stepwise in linear pass.}
  \item{stp}{logical. If TRUE, performs stepwise as a final step after executing all passes.}
  \item{CutPass}{number of passes the user need to perform. Maximum  five.}
  \item{Passi}{regression technique with user defined parameters. i=1:5.}
  \item{Meth}{either "lm" or "rlm".}
  \item{Crit}{either "res" or "w".}
  \item{Cut}{the value of the cut.} 
}
\details{}
\value{}

\author{
IAZI
}

\examples{
require(MASS)
regdata<-cbind.data.frame(V1=rnorm(50), V2=rnorm(50), V3=rnorm(50),
V4= abs(rnorm(50)*100),V5= as.factor(rep(seq(1,5),10)))
attach(regdata)
form<-V4~V1+V2+V3
OutInit<-NULL 
Reg<-IIT.ModPass(form, regdata, OutInit, Instp = TRUE, stp = TRUE, CutPass = 2, 
Pass1 = list(Meth = "lm", Crit = "res", Cut = 2.5),
Pass2 = list(Meth = "rlm", Crit = "w", Cut = 0.01,
Alt = "cook", CAlt = 0.02))
summary(Reg)
}
