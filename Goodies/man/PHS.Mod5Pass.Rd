\name{PHS.Mod5Pass}
\alias{PHS.Mod5Pass}
\title{Regression with 5 passes and outlier elimination}
\description{This function calculates the regression with 5 passes and outlier elimination based on the criterian(s) specified.}
\usage{PHS.Mod5Pass(form, regdata, OutInit, 
             Pass1 = list(Meth = "lm", Crit = "res", Cut = 2.5), 
             Pass2 = list(Meth = "lm", Crit = "res", Cut = 2.5), 
             Pass3 = list(Meth = "lm", Crit = "res", Cut = 2.5), 
             Pass4 = list(Meth = "rlm", Crit = "w", Cut = 0.02,
             Alt = "cook", CAlt = 0.02))}
\arguments{
  \item{form}{regression formula.}
  \item{regdata}{data set.}
  \item{OutInit}{initial outlier vector.}
  \item{Pass1}{Pass1 criteria.}
  \item{Pass2}{Pass2 criteria.}
  \item{Pass3}{Pass3 criteria.}
  \item{Pass4}{Pass4 criteria.}
}
\details{}
\value{Return the statistics and summary in the IAZI documentation format.}
\author{IAZI}
\examples{
##  Require library MASS
regdata<-cbind.data.frame(V1=rnorm(50), V2=rnorm(50), V3=rnorm(50),
V4= abs(rnorm(50)*100),V5= as.factor(rep(seq(1,5),10)))
attach(regdata)
formula<-V4~V1+V2+V3
Cut1<-2.3000
Outliers<-br<-NULL 
if(length(Outliers)==0) {
form0 <- lm(formula, regdata, na.action = na.omit)
} else {
form0 <- lm(formula, regdata, subset = -Outliers, na.action = na.omit)
}
step0 <- step(form0, trace=FALSE)
Cut1 <- Cut1*summary(step0)$sigma
Resform0 <- as.integer(rep(NA, length(V4)))
Resform0[as.integer(names(residuals(step0)))] <- residuals(step0)
br0 <- as.integer(names(residuals(step0)))[(abs(residuals(step0))>Cut1)]
Outliers <- c(Outliers,br0)  
Cut1<-0.012419331
PHS.Mod5Pass(formula, regdata, Outliers, 
Pass1 = list(Meth = "lm", Crit = "res", Cut = 2.5),
Pass2 = list(Meth = "lm", Crit = "res", Cut = 2.5),
Pass3 = list(Meth = "lm", Crit = "res", Cut = 2.5),
Pass4 = list(Meth = "rlm", Crit = "w", Cut = 0.02, 
Alt = "cook", CAlt = 0.02))
}