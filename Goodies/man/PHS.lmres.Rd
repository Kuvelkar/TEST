\name{PHS.lmres}
\alias{PHS.lmres}
\title{Regression and elimination of outliers based on residuals}
\description{This function calculates a stepwise backward OLS identifying new outliers as the observations having a absolute residual larger than Cut*sd(residuals).}
\usage{PHS.lmres(form, regdata, Outliers, Cut, Silent = TRUE, 
                                                  Instp = TRUE)}
\arguments{
  \item{form}{the regression model.}
  \item{regdata}{the data from which the form is derived.}
  \item{Outliers}{the outlier vector.}
  \item{Cut}{the value of the cut.}
  \item{Silent}{logical. If TRUE, remove observations without showing any message.}
  \item{Instp}{logical. If, FALSE it doesnt perform the stepwise regression in lm Pass. }
}
\details{}
\value{Returns a list with ModStart (the starting lm), ModFinal (the final stepwise backward elimination model) and ModOut (the observations identified as outliers in this step).}
\author{IAZI}
\examples{
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
PHS.lmres(formula, regdata, Outliers, Cut1, Silent = FALSE)
}