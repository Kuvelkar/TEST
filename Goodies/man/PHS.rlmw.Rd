\name{PHS.rlmw}
\alias{PHS.rlmw}
\title{Regression and elimination of outliers using robust regression}
\description{This function calculates a robust regression.}
\usage{PHS.rlmw(form, regdata, Outliers, Cut, fAlt, Alt = "cook", CAlt, Silent = TRUE)}
\arguments{
  \item{form}{a regression formula.}
  \item{regdata}{the data used.}
  \item{Outliers}{the outlier vector.}
  \item{Cut}{value of the cut to remove observations.}
  \item{fAlt}{alternate form if the first regression form fails.}
  \item{Alt}{the method for removing observations. cook, res or hat.}
  \item{CAlt}{alternate value for cut.}
  \item{Silent}{logical. If TRUE, donot show the output.}
}
\details{This function calculates a robust regression, providing it is not limited by the 50 terms limit or any robust regression limitation. Outliers are then identified as the observations with the weights lower than the quantile defined with Cut. If the robust regression is not possible, the alternate stepwise backward elimination is performed and used for identifying new outliers as the observations exceeding the alternative criteria (Alt) & cuts (CAlt). 
An alternate starting formula fAlt can be defined.}
\value{It returns a list with ModStart (the starting lm), ModFinal (the final stepwise backward elimination model) and ModOut (the observations identified as outliers in this step).
If the robust regression was possible, the return parameter Robust is TRUE, if stepwise was used, it is FALSE.
}
\author{IAZI}
\examples{regdata<-cbind.data.frame(V1=rnorm(50), V2=rnorm(50),
V3=rnorm(50),V4= abs(rnorm(50)*100),
V5= as.factor(rep(seq(1,5),10)))
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
PHS.rlmw(formula, regdata, Outliers, Cut1, Silent = FALSE)}
