########################################################################################################
## Info            : IIT.ModPass: Modified VErsion of PHS.Mod5Pass
## Created		   : PHS
## Generation      : 
## Modified        : 20130610/TVK/RPO : Created IIT.ModPass & ModPass to Make it flexible for users to define Multiple cuts as per their requirement.		
##				     20130812/TVK/NMA : Modified IIT.ModPass & ModPass to get Stepwise and Robust Regression output properly.
##					 20131005/RPO/PVN :	Introduced Instp parameter in IIT/ModPass to control step wise regression under PHS.lmres
## Call            : source("E:\\IAZI\\2_Tasks\\16_MCR405_IITModPass\\Test_ModPass_Instp\\IIT.ModPass.r")
########################################################################################################

PHS.lmcook <- function (form, regdata, Outliers, Cut, Silent = TRUE)
########################################################################################################
# FUNCTION PHS.lmcook
# created  20121220/PHS
#
# This function calculates a stepwise backward OLS identifying new outliers as the observations having a Cook's distance larger than quantile(1-Cut).
# It returns a list with ModStart (the starting lm), ModFinal (the final stepwise backward elimination model) and ModOut (the observationss identified as outliers in this step)
# Outliers <- NULL; 
# Cut1<-0.012419331; Pass1$Model <- PHS.lmcook(fMAnv00, regdata, Outliers, Cut1); MAnv00 <- Pass1$Model$ModStart; summary(stepMAnv000 <- Pass1$Model$ModFinal); Outliers <- c(Outliers,Pass1$Model$ModOut)
# Cut2<-0.012419331; Pass2$Model <- PHS.lmcook(fMAnv00, regdata, Outliers, Cut2); summary(stepMAnv001 <- Pass2$Model$ModFinal); Outliers <- c(Outliers,Pass2$Model$ModOut)
########################################################################################################
{
if(length(Outliers)==0) M0 <- lm(form, regdata, na.action = na.omit) else M0 <- lm(form, regdata, subset = -Outliers, na.action = na.omit)
stepM0 <- step(M0, trace=F)															# do the stepwise backard elimination using starting model M0
if (!Silent) cat("Cutting off the",Cut,"observations having the highest Cook's distances\n")
CDM0 <- as.integer(rep(NA, dim(regdata)[1]))										# CDM0 contains NA for Outliers
CDM0[as.integer(names(cooks.distance(stepM0)))] <- cooks.distance(stepM0)			# CDM0 contains the Cook's distances of the observations in the final model StepM0
CutNew <- round(quantile(CDM0, probs = 1-Cut, na.rm=T),2)									# Set CutNew so that the Cut higest Cook's distances are eliminated
brM0 <- as.integer(names(cooks.distance(stepM0)))[(cooks.distance(stepM0)>CutNew)]	# brM0 containes the rows of regdata that are cut out here
return(list(ModStart=M0, ModFinal=stepM0, ModOut=brM0, Cut=CutNew))					# returns all information
}

PHS.lmhat <- function (form, regdata, Outliers, Cut, Silent = TRUE)
########################################################################################################
# FUNCTION PHS.lmhat
# created  20121220/PHS
#
# This function calculates a stepwise backward OLS identifying new outliers as the observations having a leverage larger than quantile(1-Cut).
# It returns a list with ModStart (the starting lm), ModFinal (the final stepwise backward elimination model) and ModOut (the observationss identified as outliers in this step)
# Outliers <- NULL; 
# Cut1<-0.012419331; Pass1$Model <- PHS.lmhat(fMAnv00, regdata, Outliers, Cut1); MAnv00 <- Pass1$Model$ModStart; summary(stepMAnv000 <- Pass1$Model$ModFinal); Outliers <- c(Outliers,Pass1$Model$ModOut)
# Cut2<-0.012419331; Pass2$Model <- PHS.lmhat(fMAnv00, regdata, Outliers, Cut2); summary(stepMAnv001 <- Pass2$Model$ModFinal); Outliers <- c(Outliers,Pass2$Model$ModOut)
########################################################################################################
{
if(length(Outliers)==0) M0 <- lm(form, regdata, na.action = na.omit) else M0 <- lm(form, regdata, subset = -Outliers, na.action = na.omit)
stepM0 <- step(M0, trace=F)															# do the stepwise backard elimination using starting model M0
if (!Silent) cat("Cutting off the",Cut,"observations having the highest leverage\n")
CDM0 <- as.integer(rep(NA, dim(regdata)[1]))														# CDM0 contains NA for Outliers
CDM0[as.integer(names(hatvalues(stepM0)))] <- hatvalues(stepM0)						# CDM0 contains the leverages of the observations in the final model StepM0
CutNew <- round(quantile(CDM0, probs = 1-Cut, na.rm=T),2)									# Set CutNew so that the Cut higest leverages are eliminated
brM0 <- as.integer(names(hatvalues(stepM0)))[(hatvalues(stepM0)>CutNew)]			# brM0 containes the rows of regdata that are cut out here
return(list(ModStart=M0, ModFinal=stepM0, ModOut=brM0, Cut=CutNew))					# returns all information
}

PHS.lmres <- function (form, regdata, Outliers, Cut, Silent = TRUE, Instp = TRUE, Qt = FALSE)
  ########################################################################################################
# FUNCTION PHS.lmres
# created  : 20121220/PHS 
# modified : 20140102/NMA/ Defined CutNew based on quantiles and added new parameter Qt. 
# This function calculates a stepwise backward OLS identifying new outliers as the observations having a absolute residual larger than Cut*sd(residuals).
# It returns a list with ModStart (the starting lm), ModFinal (the final stepwise backward elimination model) and ModOut (the observationss identified as outliers in this step)
# Outliers <- NULL; 
# Cut1<-0.012419331; Pass1$Model <- PHS.lmres(fMAnv00, regdata, Outliers, Cut1); MAnv00 <- Pass1$Model$ModStart; summary(stepMAnv000 <- Pass1$Model$ModFinal); Outliers <- c(Outliers,Pass1$Model$ModOut)
# Cut2<-0.012419331; Pass2$Model <- PHS.lmres(fMAnv00, regdata, Outliers, Cut2); summary(stepMAnv001 <- Pass2$Model$ModFinal); Outliers <- c(Outliers,Pass2$Model$ModOut)
########################################################################################################
{
  if(length(Outliers)==0) M0 <- lm(form, regdata, na.action = na.omit) else M0 <- lm(form, regdata, subset = -Outliers, na.action = na.omit)
  ifelse(Instp == FALSE,stepM0 <- M0,stepM0 <- step(M0, trace = F))  				# do the stepwise backward elimination using starting model M0
  if (!Silent) cat("Cutting off the",Cut,"observations having the highest absolute residuals\n")
  CDM0 <- as.integer(rep(NA, dim(regdata)[1]))										# CDM0 contains NA for Outliers
  CDM0[as.integer(names(residuals(stepM0)))] <- residuals(stepM0)						# CDM0 contains the Residuaks of the observations in the final model StepM0
  # ifelse(Qt==FALSE, CutNew <- round(Cut*summary(stepM0)$sigma,2), CutNew <- quantile(abs(residuals(stepM0)), Cut, na.rm=T)) # Set CutNew so that the abslute residuals farther than Cut*sd are eliminated
  ifelse(Qt==FALSE, CutNew <- round(Cut*summary(stepM0)$sigma,2), CutNew <- quantile(abs(CDM0), Cut, na.rm=T)) # Set CutNew so that the abslute residuals farther than Cut*sd are eliminated
  brM0 <- as.integer(names(residuals(stepM0)))[(abs(residuals(stepM0))>CutNew)]		# brM0 containes the rows of regdata that are cut out here
  return(list(ModStart=M0, ModFinal=stepM0, ModOut=brM0, Cut=CutNew))					# returns all information
}

PHS.rlmw <- function (form, regdata, Outliers, Cut, fAlt, Alt="cook", CAlt, Silent = TRUE)
########################################################################################################
# FUNCTION PHS.rlmw
# created	20121220/PHS
# modified	20121226/PHS	Capture case when robust fails or all parameters are 0
# modified	20130503/TVK/RPO While using rlm in the first pass the Outlier vector is NULL so we remove it in the first pass.
# modified	20130503/TVK/RPO While using rlm in the first pass the Outlier vector is NULL so we added a ifelse statement.
# This function calculates a robust regression, providing it is not limited by the 50 terms limit or any robust regression limitation. Outliers are then identified as the
# observations with the weights lower than the quantile defined with Cut. If the robust regression is not possible, the alternate stepwise backward elimination is performed
# and used for identifying new outliers as the observations exceeding the alternative criteria (Alt) & cuts (CAlt). An alternate starting formula fAlt can be defined.
#
# It returns a list with ModStart (the starting lm), ModFinal (the final stepwise backward elimination model) and ModOut (the observationss identified as outliers in this step).
# If the robust regression was possible, the return parameter Robust is TRUE, if stepwise was used, it is FALSE.
# Pass4$Model <- PHS.rlmw(formula(Pass3$Model$ModFinal), regdata, Outliers, Pass4$Cut, form, Pass4$Alt, Pass4$CAlt)
########################################################################################################
{
	if(length(Outliers)==0) M0 <- lm(form, regdata, na.action = na.omit) else M0 <- lm(form, regdata, subset = -Outliers, na.action = na.omit)
	options(show.error.messages = FALSE)
	if(length(Outliers)==0) msg <- try(MR<-rlm(form, regdata, na.action = na.omit, method = "M", psi = psi.bisquare)) else 
		msg <- try(MR<-rlm(form, regdata, subset = -Outliers, na.action = na.omit, method = "M", psi = psi.bisquare))
	if (is.character(msg)) MRProblem <- (grep("Error",msg) > 0) else MRProblem <- !any(MR$coeff!=0)
	options(show.error.messages = TRUE)
	
	if (!MRProblem) 
	{
		cat("!!! robust regression \n")
		CutNew <- round(quantile(MR$w, probs = Cut),2)												# Set cut so that the lowest weights are elimitated,
		ifelse(length(Outliers) == 0,brMR<-(1:dim(regdata)[1])[which(MR$w<CutNew)],brMR<-(1:dim(regdata)[1])[-Outliers][which(MR$w<CutNew)]) # (RPO/TVK) To avoid error in First pass as rlm (DR-GIR)
		# brMR<-(1:dim(regdata)[1])[-Outliers][which(MR$w<CutNew)]							# Weight are only available for the subset used (1:dim(regdata)[1])[-Outliers]
		# brMR<-(1:dim(regdata)[1])[which(MR$w<CutNew)]										# (RPO/TVK) Weight are only available for the subset used (1:dim(regdata)[1])[-Outliers]
		return(list(ModStart=M0, ModFinal=MR, ModOut=brMR, Cut=CutNew, Robust=!MRProblem))	# returns all information, including Robust
	} else
	{
		cat ("!!! robust regression replaced with stepwise OLS because \n")
		print(msg)
		if (missing(fAlt)) fAlt <- form
		if (missing(CAlt)) CAlt <- Cut
		if (Alt=="res")  rlmw <- PHS.lmres(fAlt, regdata, Outliers, CAlt) else
		if (Alt=="hat")  rlmw <- PHS.lmhat(fAlt, regdata, Outliers, CAlt) else
		if (Alt=="cook") rlmw <- PHS.lmcook(fAlt, regdata, Outliers, CAlt) else
		{
			cat ("PHS.rlmw - Alternate Method / Criteria combination not implemented\n")
			Valid <- c(Valid,"PHS.rlmw$MethCrit")
			return(list(Valid=Valid))
		}
																					# returns all information from Stepwise
		return(list(ModStart=rlmw$ModStart, ModFinal=rlmw$ModFinal, ModOut=rlmw$ModOut, Cut=rlmw$Cut, Robust=!MRProblem))	
	}
}

IIT.ModPass <- function (form, regdata, OutInit,Instp = TRUE,stp=TRUE, CutPass=NULL,  
					Pass1=list(Meth="lm",Crit="res",Cut=2.5), 
					Pass2=list(Meth="lm",Crit="res",Cut=2.5), 
					Pass3=list(Meth="lm",Crit="res",Cut=2.5), 
					Pass4=list(Meth="rlm",Crit="w",Cut=0.02,Alt="cook",CAlt=0.02),
					Pass5=list(Meth="rlm",Crit="w",Cut=0.02,Alt="cook",CAlt=0.02))
########################################################################################################
# FUNCTION IIT.ModPass
# created  20130610/TVK/RPO
# Modified : 20130812/TVK/NMA : Modified IIT.ModPass : Added stp function
#
# This function generalizes PHS.MOD5Pass Function & Allows user to be flexible with No. of Cuts to be passed & 
# option for stepwise regression to be executed or not.
# IIT.ModPass <- function (form, regdata, OutInit,step=TRUE, CutPass=4,  
#					  Pass1=list(Meth="lm",Crit="res",Cut=2.5), 
#					  Pass2=list(Meth="lm",Crit="res",Cut=2.5), 
#					  Pass3=list(Meth="lm",Crit="res",Cut=2.5), 
#					  Pass4=list(Meth="rlm",Crit="w",Cut=0.02,Alt="cook",CAlt=0.02))
#						Following steps are performed:				
#						1. lm(subset) / step / cuts at hatvalues>quantile(1-Pass1$Cut) (cuts are defined in table HedoSinglemodelCut)
#						2. lm(subset) / step / cuts at hatvalues>quantile(1-Pass2$Cut)
#						3. lm(subset) / step / cuts at hatvalues>quantile(1-Pass3$Cut)
#						4. rlm(formula, subset) / cuts at weights<quantile(Pass4$Cut) if max 50 factors or hatvalues > quantile(1-Pass4$Cut)
#						5. lm(subset) / step
########################################################################################################
{
start <- REPMDate()
cat(start, " - starting IIT.ModPass\n")

# 1. Validation of calling parameters
{ #####################################
Valid <- NULL
if (!is.matrix(regdata)&!is.data.frame(regdata))
{	cat("IIT.ModPass: Validation error! regdata must be a matrix or a data frame\n")
	cat("regdata:\n")
	print(str(regdata))
	Valid <- c(Valid, "regdata")
}
for(i in 1:CutPass)
{
 cat(start, " - Validates the parameters for Pass", i,"\n")
 Valid <<- c(Valid, PHS.ValidPass(get(paste("Pass",i,sep="")),i))
 if (length(Valid)>0) return(list(Valid=Valid, start=c(start)))
}
}
# 2. Calculation of  Pass model
{ #####################################
Instp <<- Instp
assign("Outliers", OutInit, envir= globalenv())
regdata <- as.data.frame(regdata)								# Make sure regdata is a data.frame
cat("Begin calculation.\n")
cat("The starting model is: \n")
print(form)
## ------------------------------------------------------------------------------------
## Model pass, subset filters Outliers
##
for(i in 1:CutPass)
{
 i <<- i # Assigning Globally
 assign(paste("start",i,sep=""),REPMDate())			
 cat(get(paste("start",i,sep ="")), "----- Starting Pass : ",i, "\n")

 assign(paste("Pass",i,sep=""),get(paste("Pass",i,sep="")), envir=globalenv())	
 assign("P",ModPass(get(paste("Pass",i,sep="")),form, regdata, Outliers), envir=globalenv())
 assign(paste("P",i,sep=""),P, envir=globalenv())											# Assigning Reg Object
 assign("Outliers", c(Outliers,get(paste("P",i,sep=""))$Pass$ModOut),envir=globalenv())	# Append Pass Outliers to global 
 print(length(Outliers))
 assign(paste("start",i,sep=""),get(paste("P",i,sep=""))$strt)			# Assigning Reg Execution Time.
 cat(get(paste("start",i,sep ="")), "----- Finished ModPass : ",i, "\n")
 assign(paste("start",i,sep=""),REPMDate())			
 cat(get(paste("start",i,sep ="")), "----- Finishing Pass : ",i, "\n")
} 
## Model FInal pass, Stepwise/OLS
startFinal <- REPMDate()
cat(startFinal, " --- starting Final Pass\n")
MF <- lm(form, regdata, subset = -Outliers, na.action = na.omit)
ifelse(stp == TRUE, SMF <- step(MF, trace=F), SMF <- MF)
print(summary(SMF,corr=F))
}
# Return all the results
###########################
mylist.names <- c(paste("P", 1:CutPass, sep=""))
mylist <- vector("list", length(mylist.names))
names(mylist) <- mylist.names

for( i in 1:CutPass)
{
	
	mylist[[i]] <- get(paste("P",i,sep=""))
}

return(c(mylist, list(ModFinal = SMF,Outliers = Outliers)))
# return(list(P1 = P1,P2 = P2,P3 = P3,P4 = P4,ModFinal = SMF,Outliers = Outliers))
}

ModPass <- function(Pass, form="", regdata="", Outliers="")
########################################################################################################
# FUNCTION ModPass
# created  20130610/TVK
# Modified : 20130812/TVK/NMA : Modified ModPass to get Robust Regression output.
# ModPass(Pass1,form, regdata, Outliers)
# This function is used under IIT.ModPass to calculate model for each Pass. It helps to Organise IIT.ModPass in a better way.
# ModPass <- function(Pass, form="", regdata="", Outliers="")
########################################################################################################
{
strt <- REPMDate()
cat(strt, " - Starting ModPass  ",i,"\ (",toString(rbind(get(paste("Pass",i,sep="")))),")\n")
Pass <- get(paste("Pass",i,sep=""))
if ((Pass$Meth=="lm")&(Pass$Crit=="res")) Pass$Model <- PHS.lmres(form, regdata, Outliers, Pass$Cut,Instp = Instp) else
if ((Pass$Meth=="lm")&(Pass$Crit=="hat")) Pass$Model <- PHS.lmhat(form, regdata, Outliers, Pass$Cut) else
if ((Pass$Meth=="lm")&(Pass$Crit=="cook"))Pass$Model <- PHS.lmcook(form, regdata, Outliers, Pass$Cut) else 
if ((Pass$Meth=="rlm")&(Pass$Crit=="w")) 
{
#	Pass$Model <- ifelse(Pass == "Pass1",PHS.rlmw(form, regdata, Outliers, Pass$Cut, form, Pass$Alt, Pass$CAlt),PHS.rlmw(formula(get(paste("P",i-1,sep=""))$Pass$ModFinal), regdata, Outliers, Pass$Cut, form, Pass$Alt, Pass$CAlt))
	if(paste("Pass",i,sep="") == "Pass1") Pass$Model <- PHS.rlmw(form, regdata, Outliers, Pass$Cut, form, Pass$Alt, Pass$CAlt) else Pass$Model<-PHS.rlmw(formula(get(paste("P",i-1,sep=""))$Pass$ModFinal), regdata, Outliers, Pass$Cut, form, Pass$Alt, Pass$CAlt)
	if (length(PHS.match(c("ModStart", "ModFinal", "ModOut", "Robust"), names(Pass$Model))) != 4)	
	{
		cat ("ModPass - Pass: Robust regression failed,", Pass$Model$Valid, "\n")
		return(list(Valid = c(Valid, Pass$Model$Valid), Pass=Pass$Model, Outliers=Outliers, strt=strt))
	}
} else
{
	cat ("ModPass - Pass: Method / Criteria combination not implemented\n")
	Valid <- c(Valid,"Pass$MethCrit")
	return(list(Valid=Valid, Pass=Pass$Model, Outliers=Outliers, strt=c(strt)))
}
M0 <- Pass$Model$ModStart
anyfactor <- (length(grep("factor",attr(summary(M0)$terms,"dataClasses")))>0)
stepM0 <- Pass$Model$ModFinal
if (anyfactor) if(!is.null(Valid <- PHS.CheckFactorsAfterCut(M0, regdata[-Outliers,])))		# Check that elimination of Outliers does not make the factors singular
{
	cat ("ModPass - Pass: Factor singularity\n")
	return(list(Valid=Valid, Pass=Pass$Model, Outliers=Outliers, strt=c(strt)))
}
cat(strt, " - Finished Pass",i,"\ (",toString(rbind(get(paste("Pass",i,sep="")))),")\n")
return(list(Valid = c(Valid, Pass$Model$Valid), Pass=Pass$Model, Outliers=Outliers, strt=strt))
}

