#source("C:\\Documents and Settings\\nair\\Desktop\\PHSFuncs\\PHS.Mod5Pass.r")

PHS.CheckFactorsAfterCut <- function (mymodel, mydata) 
########################################################################################################
# FUNCTION PHS.CheckFactorsAfterCut
# created  20121219/PHS
#
# This function checks that the factors in the model are not singular, i.e. no factor overdetermination takes place
# The parameters are the model and the dataset with Outliers filtered
#
# PHS.CheckFactorsAfterCut(MAnv000, regdata[-Outliers,])
# The Analysis process was terminated because of factor overdetermination:
#                tDSee0                tDSee1     tUrbanization_Cd1     tUrbanization_Cd2     tUrbanization_Cd3     tSCMedicalSchool0     tSCMedicalSchool1 
#                 2240                    78                     0                   360                  1958                  2261                    57 
# tSCMiddleSchoolTeach0 tSCMiddleSchoolTeach1       tSCOtherSchool0       tSCOtherSchool1   tSTHighSchoolTeach0   tSTHighSchoolTeach1    tSTOtherSchoolVoc0 
#                  2318                     0                  2250                    68                  2299                    19                  2291 
#   tSTOtherSchoolVoc1 
#                    27 
# The problematic factors are : tUrbanization_Cd1 tSCMiddleSchoolTeach1 
# Error in PHS.CheckFactorsAfterCut(MAnv000, regdata[-Outliers, ]) : 
#   Analysis process terminated
########################################################################################################
{
sury <- summary(mymodel, corr=F)																# get the summary of the model
fnam <- names(attr(sury$terms,"dataClasses"))[grep("factor",attr(sury$terms,"dataClasses"))]	# names of the factors in the model
if (length(fnam)==0) return()																	# no factor present in model
ctt <- NULL																						# ctt is the cumulative table of factor counts
for (ii in seq(along=fnam))
{
	tt <- table(mydata[,fnam[ii]])
	names(tt) <- paste(fnam[ii], names(tt), sep="")
	ctt <- c(ctt, tt)
}
if (any(ctt==0)) 
{
	cat("The Analysis process was terminated because of factor overdetermination:\n")
	print (ctt)
	if(length((probl <- names(ctt)[ctt==0]))==1) cat("The problematic factor is :",probl,"\n") else
												 cat("The problematic factors are :", toString(fnam),"\n")
	return(list(Valid="PHS.CheckFactorsAfterCut"))
} else return(NULL)
}

PHS.DistributionLog <- function(X, name, Shift=c(1,0.01,0.0001), graphfolder="", savepdf=TRUE, savepng=TRUE) 
########################################################################################################
# FUNCTION PHS.DistributionLog
# created 20121207/PHS
#
# This function plots the histograms of the log of the shifted variable name in a dataset or matrix X. 3 values are defined in parameter Shift. Default is Shift=c(1,0.01,0.0001)
#
# for (i in seq(along=AT_MacroALTnames)) PHS.DistributionLog(.AT_MacroALT,AT_MacroALTnames[i],graphfolder=graphfolder, savepdf=FALSE)	
{
if ((nShift<-length(Shift))!=3) {cat("Shift has a length=",nShift,", must have length=3\n", sep=""); return(-1)}
if (!(is.matrix(X) | is.data.frame(X))) {cat("X must be a matrix or ad data.frame\n", sep=""); return(-2)}
myrows <- which(X[,name]>0)
par(mfrow=c(3,2))
Title<-paste("log(",Shift[1],"+",name,")",sep=""); hist(log(Shift[1]+X[myrows,name]), col=IAZIColor("IAZIBlue6"), xlab=Title, main=Title); qqnorm(log(Shift[1]+X[myrows,name]), main=Title)
Title<-paste("log(",Shift[2],"+",name,")",sep=""); hist(log(Shift[2]+X[myrows,name]), col=IAZIColor("IAZIBlue6"), xlab=Title, main=Title); qqnorm(log(Shift[2]+X[myrows,name]), main=Title)
Title<-paste("log(",Shift[3],"+",name,")",sep=""); hist(log(Shift[3]+X[myrows,name]), col=IAZIColor("IAZIBlue6"), xlab=Title, main=Title); qqnorm(log(Shift[3]+X[myrows,name]), main=Title)
if (savepdf) dev.copy2pdf(width=8, height=12, file= FName(paste(graphfolder,"\\",  name, " DistributionLog.pdf", sep=""),FilePath))
if (savepng) dev.print(device = png, units="in",width=8, height=12, res=600, file= FName(paste(graphfolder,"\\",  name, " DistributionLog.png", sep=""),FilePath))
}

PHS.DistributionPosX <- function(X, name, graphfolder="", savepdf=TRUE, savepng=TRUE) 
########################################################################################################
# FUNCTION PHS.DistributionPosX
# created 20121207/PHS
#
# This function plots the histograms of the variable name in a dataset or matrix X, of its log and of its sqrt
#
# for (i in seq(along=AT_MacroALTnames)) PHS.DistributionPosX(.AT_MacroALT,AT_MacroALTnames[i],graphfolder=graphfolder, savepdf=FALSE)	
{
myrows <- which(X[,name]>0)
par(mfrow=c(3,2))
Title<-paste("",name,"",sep=""); hist(X[myrows,name], col=IAZIColor("IAZIBlue6"), xlab=Title, main=Title); qqnorm(X[myrows,name], main=Title)
Title<-paste("log(",name,")",sep=""); hist(log(X[myrows,name]), col=IAZIColor("IAZIBlue6"), xlab=Title, main=Title); qqnorm(log(X[myrows,name]), main=Title)
Title<-paste("sqrt(",name,")",sep=""); hist(sqrt(X[myrows,name]), col=IAZIColor("IAZIBlue6"), xlab=Title, main=Title); qqnorm(sqrt(X[myrows,name]), main=Title)
if (savepdf) dev.copy2pdf(width=8, height=12, file= FName(paste(graphfolder, "\\", name, " Distribution.pdf", sep=""),FilePath))
if (savepng) dev.print(device = png, units="in",width=8, height=12, res=600, file= FName(paste(graphfolder,"\\", name, " Distribution.png", sep=""),FilePath))
}

PHS.DistributionX <- function(X, name, label ="", graphfolder="", savepdf=TRUE, savepng=TRUE) 
########################################################################################################
# FUNCTION PHS.DistributionsX
# created 20121213PHS
#
# This function plots the histograms and the qqnorm of the variable name in a dataset or matrix X
#
# for (i in seq(along=AT_MacroTransfALTnames)) PHS.DistributionX(.AT_MacroTransfALT,AT_MacroTransfALTnames[i], label="Transf", graphfolder=graphfolder, savepdf=FALSE)	
{
myrows <- which(!is.na(X[,name])&!is.infinite(X[,name]))
par(mfrow=c(1,2))
Title<-paste("",name,"",sep=""); hist(X[myrows,name], col=IAZIColor("IAZIBlue6"), xlab=Title, main=Title); qqnorm(X[myrows,name], main=Title)
if (savepdf) dev.copy2pdf(width=12, height=8, file= FName(paste(graphfolder,"\\", name, label, " Distribution.pdf", sep=""),FilePath))
if (savepng) dev.print(device = png, units="in",width=12, height=8, res=600, file= FName(paste(graphfolder,"\\", name, label, " Distribution.png", sep=""),FilePath))
}

PHS.getcex <- function(n) 
########################################################################################################
# FUNCTION PHS.getcex
# created 20121208/PHS
#
# This function returns the caracter size used in the panel.cor function depending on the number of variables in the cor analysis
# It improves the readability of the numbers
# sapply(1:20, PHS.getcex) 	
{
	cex <- 22;	
	if(n>1) for (i in 2:n) cex <- round(cex*(i-1)/i-0.006*i,1); 
	cex
}

PHS.makeFactorMatrix <- function(x, ref=1, tol=.Machine$double.eps^0.5)  
########################################################################################################
# FUNCTION PHS.makeFactorMatrix
# created 20121215/PHS
#
# This function transforms a multi-level factor into a dummy variable factor matrix usable for regression. 
# The reference indicates which level is used as a reference. In case of inconsistent data, the first level is used as reference.
# If the reference is NULL, no level is removed
#
# head(fUrbanization_Cd <- PHS.makeFactorMatrix());head(fUrbanization_Cd <- PHS.makeFactorMatrix(tUrbanization_Cd,2));head(fUrbanization_Cd <- PHS.makeFactorMatrix(tUrbanization_Cd,NULL));
{
	if (!is.factor(x)) return(NULL)															# Input parameter must be a factor
	ix <- as.integer(x);ix[is.na(x)]<-0;x<-as.factor(ix)									# Assign level 0 to NA
	if ((nc <- length(f<-levels(x)))<2)	return(NULL)										# nc is the number of levels
	if (!is.null(ref)) if ((abs(ref - round(ref))>tol)|(ref<1)|(ref>nc)) ref<-1				# in case of inconsistent reference, default ref=1 is applied
	nm<-length(x)																			# nm is the length of vector x
	mx <- matrix(0, nrow=nm, ncol=nc)														# mx is a factor matrix with each column being a dummy variable
	dimnames(mx)[[2]] <- f																	# The label of the columns are the levels of the original factor
	for (i in 1:nc) {mx[as.numeric(x)==i,i] <- 1}											# Set the 1 for the corresponding dummy variables
	if (is.null(ref)) (fx <- mx) else (fx <- mx[,-ref])										# reference (default 1st column <=> 1) is excluded from the regression
}

PHS.match <- function(tosearch, incharvector)
########################################################################################################
# FUNCTION PHS.match
# created	20121216/PHS
# modified	20121219/PHS	modified the algorithm for the elimination of multiple matches
# 			20121226/PHS	simplified code using generic unique function
#
# This function is an intuitive improvement of pmatch, using grep for incomplete match. 
# The parameters are first converted to a character vector by as.character. Empty strings do not match/are not matched.
#
# PHS.match(c("", "abc", "a"), c("", "ab", "abc"))
# [1] 3 2
# PHS.match(.AT_MacroVariableGroup[.AT_MacroVariableGroup[,2]=="ALT",1],dimnames(.AT_MacroTransfRatios)[[2]])
########################################################################################################
{
	tosearch <- as.character(tosearch)[nchar(as.character(tosearch))>0]						# remove empty strings from tosearch after making it a character
	incharvector <- as.character(incharvector)												# transform the table to search to a character
	mycol <- NULL																			# initialize the position found to NULL
	for (i in seq(along=tosearch)) mycol <- c(mycol, grep(tosearch[i], incharvector))		# loop the match search on the input
	(mycol <- unique(mycol))																# keep the first
}

PHS.Mod5Pass <- function (form, regdata, OutInit, 
					Pass1=list(Meth="lm",Crit="res",Cut=2.5), 
					Pass2=list(Meth="lm",Crit="res",Cut=2.5), 
					Pass3=list(Meth="lm",Crit="res",Cut=2.5), 
					Pass4=list(Meth="rlm",Crit="w",Cut=0.02,Alt="cook",CAlt=0.02))
########################################################################################################
# FUNCTION PHS.Mod5Pass
# created  20121220/PHS
#
# This function generalizes the calculation of the Vx uncentered model MF starting from M0
# MAnv00hathathatwhat <- PHS.Mod5Pass(fMAnv00, regdata, 
#			Pass1=list(Meth="lm",Crit="hat",Cut=0.012419331), 
#			Pass2=list(Meth="lm",Crit="hat",Cut=0.012419331), 
#			Pass3=list(Meth="lm",Crit="hat",Cut=0.012419331), 
#			Pass4=list(Meth="rlm",Crit="w",Cut=0.02,Alt="hat",CAlt=0.02))
#						Following steps are performed:				
#						1. lm(subset) / step / cuts at hatvalues>quantile(1-Pass1$Cut) (cuts are defined in table HedoSinglemodelCut)
#						2. lm(subset) / step / cuts at hatvalues>quantile(1-Pass2$Cut)
#						3. lm(subset) / step / cuts at hatvalues>quantile(1-Pass3$Cut)
#						4. rlm(formula, subset) / cuts at weights<quantile(Pass4$Cut) if max 50 factors or hatvalues>quantile(1-Pass4$Cut)
#						5. lm(subset) / step
########################################################################################################
{
start <- REPMDate()
cat(start, " - starting PHS.Mod5Pass\n")

# 1. Validation of calling parameters
#####################################
{ Valid <- NULL
if (!is.matrix(regdata)&!is.data.frame(regdata))
{	cat("PHS.Mod5Pass: Validation error! regdata must be a matrix or a data frame\n")
	cat("regdata:\n")
	print(str(regdata))
	Valid <- c(Valid, "regdata")

}
Valid <- c(Valid, PHS.ValidPass(Pass1, 1))	# Validates the parameters for Pass 1
Valid <- c(Valid, PHS.ValidPass(Pass2, 2))	# Validates the parameters for Pass 2
Valid <- c(Valid, PHS.ValidPass(Pass3, 3))	# Validates the parameters for Pass 3
Valid <- c(Valid, PHS.ValidPass(Pass4, 4))	# Validates the parameters for Pass 4
if (length(Valid)>0) return(list(Valid=Valid, start=c(start)))
}

# 2. Calculation of 5 pass model
################################
assign("Outliers", OutInit, envir= globalenv())
regdata <- as.data.frame(regdata)								# Make sure regdata is a data.frame
{
cat("Begin calculation.\n")
cat("The starting model is: \n")
print(form)

## ------------------------------------------------------------------------------------
## Model 1 pass, subset filters Outliers
##
start1 <- REPMDate()
cat(start1, " - starting pass 1 (",toString(rbind(Pass1)),")\n")
if ((Pass1$Meth=="lm")&(Pass1$Crit=="res")) Pass1$Model <- PHS.lmres(form, regdata, Outliers, Pass1$Cut) else
if ((Pass1$Meth=="lm")&(Pass1$Crit=="hat")) Pass1$Model <- PHS.lmhat(form, regdata, Outliers, Pass1$Cut) else
if ((Pass1$Meth=="lm")&(Pass1$Crit=="cook"))Pass1$Model <- PHS.lmcook(form, regdata, Outliers, Pass1$Cut) else
if ((Pass1$Meth=="rlm")&(Pass1$Crit=="w"))  
{
	Pass1$Model <- PHS.rlmw(form, regdata, Outliers, Pass1$Cut, form, Pass1$Alt, Pass1$CAlt) 
	if (length(PHS.match(c("ModStart", "ModFinal", "ModOut", "Robust"), names(Pass1$Model))) != 4)
	{
		cat ("PHS.Mod5Pass - Pass1: Robust regression failed,", Pass1$Model$Valid, "\n")
		return(list(Valid = c(Valid, Pass1$Model$Valid), Outliers=Outliers, start=c(start, start1)))
	}
} else
{
	cat ("PHS.Mod5Pass - Pass1: Method / Criteria combination not implemented\n")
	Valid <- c(Valid,"Pass1$MethCrit")
	return(list(Valid=Valid, start=c(start, start1)))
}
M0 <- Pass1$Model$ModStart
anyfactor <- (length(grep("factor",attr(summary(M0)$terms,"dataClasses")))>0)
stepM0 <- Pass1$Model$ModFinal
assign("Outliers", c(Outliers,Pass1$Model$ModOut), envir= globalenv())					# Append Pass Outliers to global Outliers
if (anyfactor) if(!is.null(Valid <- PHS.CheckFactorsAfterCut(M0, regdata[-Outliers,])))	# Check that elimination of Outliers does not make the factors singular
{
	cat ("PHS.Mod5Pass - Pass1: Factor singularity\n")
	return(list(Valid=Valid, Pass1=Pass1$Model, Outliers=Outliers, start=c(start, start1)))
}											
## ------------------------------------------------------------------------------------
## Model 2 pass, subset filters Outliers
##
start2 <- REPMDate()
cat(start2, " - starting pass 2\ (",toString(rbind(Pass2)),")\n")
if ((Pass2$Meth=="lm")&(Pass2$Crit=="res")) Pass2$Model <- PHS.lmres(form, regdata, Outliers, Pass2$Cut) else
if ((Pass2$Meth=="lm")&(Pass2$Crit=="hat")) Pass2$Model <- PHS.lmhat(form, regdata, Outliers, Pass2$Cut) else
if ((Pass2$Meth=="lm")&(Pass2$Crit=="cook"))Pass2$Model <- PHS.lmcook(form, regdata, Outliers, Pass2$Cut) else
if ((Pass2$Meth=="rlm")&(Pass2$Crit=="w")) 
{
	Pass2$Model <- PHS.rlmw(formula(Pass1$Model$ModFinal), regdata, Outliers, Pass2$Cut, form, Pass2$Alt, Pass2$CAlt) 
	if (length(PHS.match(c("ModStart", "ModFinal", "ModOut", "Robust"), names(Pass2$Model))) != 4)
	{
		cat ("PHS.Mod5Pass - Pass2: Robust regression failed,", Pass2$Model$Valid, "\n")
		return(list(Valid = c(Valid, Pass2$Model$Valid), Pass1=Pass1$Model, Outliers=Outliers, start=c(start, start1, start2)))
	}
} else
{
	cat ("PHS.Mod5Pass - Pass2: Method / Criteria combination not implemented\n")
	Valid <- c(Valid,"Pass2$MethCrit")
	return(list(Valid=Valid, Pass1=Pass1$Model, Outliers=Outliers, start=c(start, start1, start2)))
}
assign("Outliers", c(Outliers,Pass2$Model$ModOut), envir= globalenv())					# Append Pass Outliers to global Outliers
if (anyfactor) if(!is.null(Valid <- PHS.CheckFactorsAfterCut(M0, regdata[-Outliers,])))		# Check that elimination of Outliers does not make the factors singular
{
	cat ("PHS.Mod5Pass - Pass2: Factor singularity\n")
	return(list(Valid=Valid, Pass1=Pass1$Model, Pass2=Pass2$Model, Outliers=Outliers, start=c(start, start1, start2)))
}
## ------------------------------------------------------------------------------------
## Model 3 pass, subset filters Outliers
##
start3 <- REPMDate()
cat(start3, " - starting pass 3 (",toString(rbind(Pass3)),")\n")
if ((Pass3$Meth=="lm")&(Pass3$Crit=="res")) Pass3$Model <- PHS.lmres(form, regdata, Outliers, Pass3$Cut) else
if ((Pass3$Meth=="lm")&(Pass3$Crit=="hat")) Pass3$Model <- PHS.lmhat(form, regdata, Outliers, Pass3$Cut) else
if ((Pass3$Meth=="lm")&(Pass3$Crit=="cook"))Pass3$Model <- PHS.lmcook(form, regdata, Outliers, Pass3$Cut) else
if ((Pass3$Meth=="rlm")&(Pass3$Crit=="w"))  
{
	Pass3$Model <- PHS.rlmw(formula(Pass2$Model$ModFinal), regdata, Outliers, Pass3$Cut, form, Pass3$Alt, Pass3$CAlt)
	if (length(PHS.match(c("ModStart", "ModFinal", "ModOut", "Robust"), names(Pass3$Model))) != 4)
	{
		cat ("PHS.Mod5Pass - Pass3: Robust regression failed,", Pass3$Model$Valid, "\n")
		return(list(Valid = c(Valid, Pass3$Model$Valid), Pass1=Pass1$Model, Pass2=Pass2$Model, 
					Outliers=Outliers, start=c(start, start1, start2, start3)))
	}
} else
{
	cat ("PHS.Mod5Pass - Pass3: Method / Criteria combination not implemented\n")
	Valid <- c(Valid,"Pass3$MethCrit")
	return(list(Valid=Valid, Pass1=Pass1$Model, Pass2=Pass2$Model, Outliers=Outliers, start=c(start, start1, start2, start3)))
}
assign("Outliers", c(Outliers,Pass3$Model$ModOut), envir= globalenv())					# Append Pass Outliers to global Outliers
if (anyfactor) if(!is.null(Valid <- PHS.CheckFactorsAfterCut(M0, regdata[-Outliers,])))		# Check that elimination of Outliers does not make the factors singular
{
	cat ("PHS.Mod5Pass - Pass3: Factor singularity\n")
	return(list(Valid=Valid, Pass1=Pass1$Model, Pass2=Pass2$Model, Pass3=Pass3$Model, 
				Outliers=Outliers, start=c(start, start1, start2, start3)))
}
## ------------------------------------------------------------------------------------
## Model 4 pass, Robust, subset filters Outliers
##
start4 <- REPMDate()
cat(start4, " - starting pass 4 (",toString(rbind(Pass4)),")\n")
if ((Pass4$Meth=="lm")&(Pass4$Crit=="res")) Pass4$Model <- PHS.lmres(form, regdata, Outliers, Pass4$Cut) else
if ((Pass4$Meth=="lm")&(Pass4$Crit=="hat")) Pass4$Model <- PHS.lmhat(form, regdata, Outliers, Pass4$Cut) else
if ((Pass4$Meth=="lm")&(Pass4$Crit=="cook"))Pass4$Model <- PHS.lmcook(form, regdata, Outliers, Pass4$Cut) else
if ((Pass4$Meth=="rlm")&(Pass4$Crit=="w"))  
{
	Pass4$Model <- PHS.rlmw(formula(Pass3$Model$ModFinal), regdata, Outliers, Pass4$Cut, form, Pass4$Alt, Pass4$CAlt)
	if (length(PHS.match(c("ModStart", "ModFinal", "ModOut", "Robust"), names(Pass4$Model))) != 4)
	{
		cat ("PHS.Mod5Pass - Pass4: Robust regression failed,", Pass4$Model$Valid, "\n")
		return(list(Valid = c(Valid, Pass4$Model$Valid), Pass1=Pass1$Model, Pass2=Pass2$Model, Pass3=Pass3$Model, 
					Outliers=Outliers, start=c(start, start1, start2, start3, start4)))
	}
} else
{
	cat ("PHS.Mod5Pass - Pass4: Method / Criteria combination not implemented\n")
	Valid <- c(Valid,"Pass4$MethCrit")
	return(list(Valid=Valid, Pass1=Pass1$Model, Pass2=Pass2$Model, Pass3=Pass3$Model, 
				Outliers=Outliers, start=c(start, start1, start2, start3, start4)))
}
assign("Outliers", c(Outliers,Pass4$Model$ModOut), envir= globalenv())					# Append Pass Outliers to global Outliers
if (anyfactor) if(!is.null(Valid <- PHS.CheckFactorsAfterCut(M0, regdata[-Outliers,])))		# Check that elimination of Outliers does not make the factors singular
{
	cat ("PHS.Mod5Pass - Pass4: Factor singularity\n")
	return(list(Valid=Valid, Pass1=Pass1$Model, Pass2=Pass2$Model, Pass3=Pass3$Model, Pass4=Pass4$Model, 
				Outliers=Outliers, start=c(start, start1, start2, start3, start4)))
}
## ------------------------------------------------------------------------------------
## Model 5 pass, stepwise OLS
start5 <- REPMDate()
cat(start5, " - starting pass 5 (final)\n")

MF <- lm(form, regdata, subset = -Outliers, na.action = na.omit)
stepMF <- step(MF, trace=F)
print(summary(stepMF,corr=F))
}

# 3. Return all the results
###########################
return(list(Valid=NULL, Pass1=Pass1$Model, Pass2=Pass2$Model, Pass3=Pass3$Model, Pass4=Pass4$Model, 
			ModFinal=stepMF, Outliers=Outliers, start=c(start, start1, start2, start3, start4, start5)))
}

PHS.panel.cor <- function(x, y, digits=2, prefix="", use = "pair", ...)
########################################################################################################
# FUNCTION PHS.panel.cor
# created 20121208/PHS
#
# panel FUNCTIONS for pairs scatterplots
# put (absolute) correlations on the upper panels, with size proportional to the correlations.
# filter the cases with NA without causing an error
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y, use = use))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(!exists("cex.cor")) cex.cor <- 2.0/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

PHS.panel.hist <- function(x, ...)
########################################################################################################
# FUNCTION PHS.panel.hist
# created 20121208/PHS
#
# panel FUNCTIONS for pairs scatterplots
# put histograms on the diagonal
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

PHS.ValidPass <- function (Pass, Step)
########################################################################################################
# FUNCTION PHS.ValidPass
# created  20121220/PHS
#
# This function validates the parameter constistence of a Pass
# It returns a NULL vector if the validation is successful, a vector containing error code to facilitate debugging otherwise
# 
# Valid <- NULL; Valid <- c(Valid, PHS.ValidPass(Pass1, 1)
########################################################################################################
{
Valid <- NULL
PS <-paste("Pass",Step, sep="")
if (length(PHS.match(c("Meth", "Crit", "Cut"), names(Pass))) != 3)
{
	cat("PHS.ValidPass: Validation error!",PS," - Parameter must be a list containing '$Meth', '$Crit' and '$Cut'\n", PS, ":\n")
	print(str(Pass))
	Valid <- c(Valid, PS)
} else 
{
	if(is.na(match(Pass$Meth, c("lm", "rlm")))) 
	{
		cat("PHS.ValidPass: Validation error!",PS," - Allowed methods are 'lm' and 'rlm', passed:",Pass$Meth,"\n") 
		Valid <- c(Valid, paste(PS,"$Meth",sep=""))
	}
	if(is.na(match(Pass$Crit, c("res", "cook", "hat", "w")))) 
	{
		cat("PHS.ValidPass: Validation error!",PS," - Allowed criterias are 'res', 'cook', 'hat' and 'w', passed:",Pass$Crit,"\n")
		Valid <- c(Valid, paste(PS,"$Crit",sep=""))
	}
	if(!is.numeric(Pass$Cut)) 
	{
		cat("PHS.ValidPass: Validation error!",PS," - Cut must be a positive number, passed:", Pass$Cut,"\n")
		Valid <- c(Valid, paste(PS,"$Cut.num",sep=""))
	} else
	{
		if(Pass$Cut<0) 
		{
			cat("PHS.ValidPass: Validation error!",PS," - Cut must be a positive number, passed:", Pass$Cut,"\n")
			Valid <- c(Valid, paste(PS,"$Cut.neg",sep=""))
		}
		if((Pass$Cut>1)&!is.na(match(Pass$Crit, c("cook", "hat", "w")))) 
		{
			cat("PHS.ValidPass: Validation error!",PS," - Cut is a quantile, must be in [0,1], passed:", Pass$Cut,"\n"); 
			Valid <- c(Valid, paste(PS,"$Cut.quant",sep=""))
		}
	}
	if(Pass$Crit=="w")
	{
		if(Pass$Meth!="rlm") 
		{
			cat("PHS.ValidPass: Validation error!",PS," - Criteria 'w' only allowed for method 'rlm', passed:",Pass$Meth,"\n")
			Valid <- c(Valid, paste(PS,"$Meth.w",sep=""))
		}
		if (length(PHS.match(c("Alt", "CAlt"), names(Pass))) != 2)
		{
			cat("PHS.ValidPass: Validation error!",PS," - Robust parameters must include '$Alt' and '$CAlt'\n", PS,":\n")
			print(str(Pass))
			Valid <- c(Valid, paste(PS,"$Alt",sep=""))
		} else 
		{
			if(is.na(match(Pass$Alt, c("res", "cook", "hat")))) 
			{
				cat("PHS.ValidPass: Validation error!",PS," - Alternate criterias allowed are 'res', 'cook' and 'hat', passed:",Pass$Alt,"\n")
				Valid <- c(Valid, paste(PS,"$CAlt",sep=""))
			}
			if(!is.numeric(Pass$CAlt)) 
			{
				cat("PHS.ValidPass: Validation error!",PS," - CAlt must be a positive number, passed:", Pass$CAlt,"\n")
				Valid <- c(Valid, paste(PS,"$CAlt.num",sep=""))
			} else
			{
				if(Pass$CAlt<0) 
				{
					cat("PHS.ValidPass: Validation error!",PS," - CAlt must be a positive number, passed:", Pass$CAlt,"\n")
					Valid <- c(Valid, paste(PS,"$CAlt.neg",sep=""))
				}
				if((Pass$CAlt>1)&!is.na(match(Pass$CAlt, c("cook", "hat")))) 
				{
					cat("PHS.ValidPass: Validation error!",PS," - CAlt is a quantile, must be in [0,1], passed:", Pass$CAlt,"\n")
					Valid <- c(Valid, paste(PS,"$CAlt.quant",sep=""))
					}
			}
		}
	}
}
return(Valid)
}

#                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 