##
## Loads the following R files into the R working memory
##
## calling sequence: 	source("C:\\Documents and Settings\\nair\\Desktop\\PHSFuncs\\LIB_R\\sourcein.r")
##
## Modified 20070726/PhS		Added function CrashWarning()
## Modified 20080821/PhS&ESU	Modified REPMDate() for R
## Modified 20100219/PHS		Activated Summary.r, changed all extensions to .r
## Modified 20100223/PHS		Commented unnecessary sources
## Modified 20110125/PHS		Removed commented items, CrashWarning modified
##

REPMDate<-function(...)
## REPMDate returns the date & time in the format YYYYMMDD.hhmmss
{
return(format(Sys.time(), "%Y%m%d.%H%M%S"))
}

CrashWarning <- function () {
sink()
cat("###################################################################################\n")
cat("## Please close R in order to free dynamic memory before executing the next step ##\n")
cat("## otherwise the calculation may crash although the program logic is correct!    ##\n")
cat("###################################################################################\n")
}
