\name{PHS.CheckFactorsAfterCut}
\alias{PHS.CheckFactorsAfterCut}
\title{Check singularity of factor variables in the model.}
\description{This function checks that the factors in the model are not singular, i.e. no factor over determination takes place.}
\usage{PHS.CheckFactorsAfterCut(mymodel, mydata)}
\arguments{
  \item{mymodel}{model to be used.}
  \item{mydata}{outlier filtered data.}
}
\details{}
\value{Returns a list of problematic cases or returns NULL.}
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
PHS.CheckFactorsAfterCut(form0, regdata[-Outliers,])
}
