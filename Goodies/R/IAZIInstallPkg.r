IAZIInstallPkg <- function(pkg="")
{
 packs <- installed.packages()
 exc <- names(packs[,'Package'])
 if (is.element("Goodies", installed.packages()[,1])==FALSE)
 {
   install.packages("H:\\1_Allgemein\\0_R\\LIB_R\\Goodies.zip", repos=NULL)
 }
 ifelse(nchar(pkg)==0, av <- c("MASS", "RODBC", "Hmisc", "stringr", "corrgram","gtools", "Goodies") , av <- c("MASS", "RODBC", "Hmisc", "stringr", "corrgram","gtools", "Goodies",pkg))
 ins <- av[!av %in% exc]
 if(length(ins) > 0)
 {
  for(i in 1:length(ins))
  {
  a <- ins[i]
  install.packages(a, repos="http://stat.ethz.ch/CRAN/")
  }
 }
cat("Following packages have been installed - ", av,"\n")
}