########################################################################################################
## Info       : Function for creating a map by passing the table name,variable name and the column name for inner join.
##						  IAZIMAP() uses map.Swiss() function which plots the map for Gemeinde level and Kanton level respectively.
## Created on : 29.07.2013
## Created by : TVK/PVN
## Call       :  source("N:\\4_Produkte\\43_Produkte-Entwicklung\\435_Modelle\\7_Operation\\ChangeRequests\\Model\\HedoPrice\\2013_Q1\\Resources\\MCR374\\GeneralizedMap\\20130821GenMapFinal.r")
##						  IAZIMAP(TabName="IF_R_Map",ColName="BFS_NR", VarName="IAZI_Fakt0713",ODBCDB="IAZISYS",breakpoints=8,precision=4,style="fixed",Title="Hedo",MapType="Gemeinde",FilePath="",SavPlt=FALSE, colr="")
## Modified		: 29.07.2013/TVK	Add a GUI interface for selecting colors and created a function for breakpoints for maps legend	
##						: 05.08.2013/PVN Added fixed breaks to the breakfunction, separated the manual color selection function
##						: 21.08.2013/PVN Added style="unique" to the breakpoint() and map.Swiss () in order to plot the color code also. NB : This parameter should be used only for plotting color code or varaibles with a few unique values.
##						: 23.08.2013/PVN/RPO Added LEFT JOIN INSTEAD OF INNER JOIN in addition of new varaiable into the shapefile, as there are only less than 400 gemeinde for DR.
##						: 26.08.2013/TVK	Added a parameter "colr" in thh function to pass any user defined colors. The colors need to be pre-defined before calling the function
##						: 28.08.2013/PVN	the coloring will be in such a way that the first color will be given to the smallest interval and in legend it appears at the bottom. so the color order should be half of the colors are from darkest to lightest and the other lightest to darkest
########################################################################################################
#====================================================================================
{	# IAZIMAP FUNCTION DEFINITION
#====================================================================================

IAZIMAP<-function(TabName="",ColName="", VarName="",ODBCDB="",breakpoints=NULL,style=c("fixed","sd","unique"),precision=NULL, colr="", QueryCond="",Title="",MapType="",FilePath="",SavPlt=FALSE)
{
sink() ## to visualise the log, just in case of an abnormal termination before this step while sinking to a log file
sink() ## to visualise the log, just in case of an interupted multiple sinking
sink() ## ditto
ptm <- proc.time()

#====================================================================================
{	# INSTALL MISSING LIBRARY'S
#====================================================================================

packs <- installed.packages()
exc <- names(packs[,'Package'])
av <- c("RODBC", "RColorBrewer", "maptools", "sqldf")
ins <- av[!av %in% exc]
if(length(ins) > 0)
{
	for(i in 1:length(ins))
	{
	a <- ins[i]
	install.packages(a, repos="http://stat.ethz.ch/CRAN/")
	}
} else { 
print("All Necessary Packages Available")}


if (is.element("Goodies", installed.packages()[,1])==FALSE)
{
  install.packages("H:\\1_Allgemein\\0_R\\LIB_R\\Goodies.zip", repos=NULL)
}else
{
 cat("Goodies is installed!!! \n")
}
}


#====================================================================================
{	# INIT LIBRARY
#====================================================================================
library(Goodies)
library(RODBC)
library(RColorBrewer)
library(maptools)
library(sqldf)
library(tcltk)

}


#====================================================================================
{	# INIT ODBC
#====================================================================================

.ODBC.IAZISYS <- ODBCDB               # ODBC connection to the IAZISYS DB
# 0.2 ODBC Connection to IAZISYS DB
.ODBCSYSChannel <- .ODBCChannel.IAZISYS <- odbcConnect(.ODBC.IAZISYS)
if(.ODBCChannel.IAZISYS==-1) { cat("Error: ODBC connection to .ODBC.IAZISYS=\"", .ODBC.IAZISYS, "\" failed\n", sep="") ; odbcCloseAll() ; return (-0.2) }

}


#====================================================================================
{	# INIT FilePath
#====================================================================================
ShpFilePath <- "N:\\4_Produkte\\43_Produkte-Entwicklung\\435_Modelle\\7_Operation\\ChangeRequests\\Model\\HedoPrice\\2013_Q1\\Resources\\MCR374\\GeneralizedMap\\"
if (nchar(FilePath)==0)
{FilePath<-"N:\\4_Produkte\\43_Produkte-Entwicklung\\435_Modelle\\7_Operation\\ChangeRequests\\Model\\HedoPrice\\2013_Q1\\Resources\\MCR374\\GeneralizedMap\\"
} else { FilePath <- FilePath }

}


#====================================================================================
{	# SHAPE FILES
#====================================================================================

Kanton.map <- readShapeSpatial(FName(FileName="G3K09_region.shp",paste(ShpFilePath,"ShapeFiles\\",sep="")))
Gemeinde.map <- readShapeSpatial(FName(FileName="SKA_Macro_Analyse_region.shp",paste(ShpFilePath,"ShapeFiles\\",sep="")))
# Gemeinde.map <- readShapeSpatial(FName(FileName="G3G12_region.shp",paste(ShpFilePath,"ShapeFiles\\",sep="")))
Lake.map <- readShapeSpatial(FName(FileName="G3See07_region.shp",paste(ShpFilePath,"ShapeFiles\\",sep="")))

}

#====================================================================================
{	# TYPE OF MAP - KANTON OR GEMEINDE
#====================================================================================

if(MapType=="Kanton")
	{	
	#================================================================================
	{	# DATA 
	#================================================================================
	# First check is there any condition except "SELECT * FROM 'TabName'". 
	
	if(nchar(QueryCond)==0)	{
	IF1<- sqlQuery(.ODBCChannel.IAZISYS, paste("SELECT * FROM ", TabName ,sep=""))
	} else {
	IF1<- sqlQuery(.ODBCChannel.IAZISYS, paste("SELECT * FROM ", TabName ," ",QueryCond,sep="")) 
	}
	attach(IF1)
	# Create a data frame from the shape file and join the variable to be plotted to it.
	Kant<-as.data.frame(Kanton.map)
	if(nchar(ColName)==0) { 
	cat("\n Error...The field ","ColName"," cannot be empty \n")
		} else { Kanton.map@data$IF<-as.matrix(sqldf(paste("SELECT A.",VarName," FROM Kant X  LEFT JOIN IF1 A ON A.",ColName,"=X.KURZ",sep="")))
	}
	}
	
	
	# Number of breaks to the data.
	breakpoints<-min(breakpoints,8)		# Fixing The maximum breaks to 8
	cuts <- breakpoint(Data=Kanton.map@data$IF, breakpoints,style=style, precision)
	#Color Range
	IAZICol(brkpts=breakpoints,MapType=MapType)	
	dev.cur()
	if(nchar(colr)==0)
	{
	ReturnVal <- tkmessageBox(title = "Color Selection Help",  message = "Select the color name from the displayed image(Press Ctrl+Tab) OR to choose your own colors type N. If N is chosen then the colors selected by you will be displayed in reverse, i.e. assign colors from negative to positive", icon = "Color Selection Info", type = "OK")
	color <<- readline("Please select your color -")
	ifelse(color == "N", mapcolor<-colorsel(length(cuts)-1), assign("mapcolor",get(color)))
	} else mapcolor <- colr
		
	#================================================================================
	{	# Call to the map.Swiss () function to create the Kantonwise map
	#================================================================================
	
	CAT<-as.matrix(sqlQuery(.ODBCChannel.IAZISYS,paste("select DISTINCT(cat) from ",TabName,QueryCond,sep="")))
	map.Swiss(Level=Kanton.map,lak=Lake.map,z="IF",title=Title,breaks=cuts,col.vec=mapcolor,rounding=precision)
	
	# Saving The Map

	if(SavPlt)	
	{
	savePlot(FName(paste(FilePath,Title,sep="")),"wmf")
	cat("The map is saved at ",paste("'",FilePath," 'with name ", Title,".wmf '",sep=""), "\n")
	}
	detach(IF1)
	}
}
else if(MapType=="Gemeinde")
	{	
	#====================================================================================
	{	# DATA 
	#====================================================================================
	
	if(nchar(QueryCond)==0)	{
	IF1<- sqlQuery(.ODBCChannel.IAZISYS, paste("SELECT * FROM ", TabName ,sep=""))
	} else {
	IF1<- sqlQuery(.ODBCChannel.IAZISYS, paste("SELECT * FROM ", TabName ," ",QueryCond,sep=""))	
	}
	attach(IF1)
	Gemeinde<-as.data.frame(Gemeinde.map)
	Gemeinde.map@data$IF<-as.matrix(sqldf(paste("SELECT A.",VarName," FROM Gemeinde X  LEFT JOIN IF1 A ON A.",ColName,"=X.BFS_NR",sep="")))
	}
	
	# Number of breaks to the data.
	
	cuts <- breakpoint(Data=Gemeinde.map@data$IF, breakpoints,style=style, precision)
	#Color Range
	IAZICol(brkpts=breakpoints,MapType=MapType)
		
	dev.cur()
	if(nchar(colr)==0)
	{
	ReturnVal <- tkmessageBox(title = "Color Selection Help",  message = "Select the color name from the displayed image(Press Ctrl+Tab) OR to choose your own colors type N. If N is chosen then the colors selected by you will be displayed in reverse, i.e. assign colors from negative to positive", icon = "info", type = "ok")
	color <<- readline("Please select your color -")
	if(style=="unique")
	{
	ifelse(color == "N", mapcolor<-colorsel(length(cuts)), assign("mapcolor",get(color)))
	} else {ifelse(color == "N", mapcolor<-colorsel(length(cuts)-1), assign("mapcolor",get(color)))}
	}else mapcolor<- colr
	
	#================================================================================
	{	# Call to the map.Swiss() function in order to plot for Gemeinde level
	#================================================================================
	
	map.Swiss(Level=Gemeinde.map,lak=Lake.map,z="IF",title=Title,breaks=cuts,col.vec=mapcolor,rounding=precision,style=style)
	
	if(SavPlt)	
	{
	savePlot(FName(paste(FilePath,Title,sep="")),"wmf")
	cat("The map is saved at ",paste(FilePath,"' ", " with name '", Title,".wmf '",sep=""), "\n")
	}
	detach(IF1)
	}
}
}
	
	#================================================================================
	{	# CALCULATIO OF PROCESS TIME
	#================================================================================
	fptm<-(proc.time()[3] - ptm[3])/60
	fptm1<-floor(fptm)
	cat("The total time taken is",floor(fptm1),"min(s):",round(fptm%%1*100), "sec(s)\n")
	}
}

}


#=============================================================================================
{	# MAP.SWISS() DEFINITION
#######################################################################################################
# FUNCTION 		: map.Swiss
# CREATED 		: 20130528/PVN
# DESCRIPTION	: This function creates the map of either Kanton or Gemeinde.
#				 map.Swiss(Level=Gemeinde.map,lak=Lake.map,z="IF",title=paste("HEDO_",VarName,sep=""),breaks=cuts, 	   	col.vec=mapcolor, rounding=precision)
#				 Level=either Kanton or Gemeinde, lak= adding the lakes to the map,z="IF" the column from the data(name kept fixed), title= Title to the legend of the map,breaks=the cuts are directly passed, col.vec=the colors to the map,rounding=the precision required in legend or cuts
# MODOFIED		: 20130822/PVN added the parameters style="unique" for plotting color code.
########################################################################################################

map.Swiss <- function(Level,lak,z,title=NULL,breaks=NULL,cex.legend=.8,bw=.2,col.vec=NULL,plot.legend=TRUE,rounding=NULL,style="")
{ 	
	if(style=="unique")
	{
	cuts=breaks
	n<-length(cuts)
	# Level@data$zCat <- cut(Level@data[,z],cuts,include.lowest=TRUE,dig.lab=rounding)
	Level@data$zCat <- factor(Level@data[,z])
	cutpoints<-table(Level@data$z)
	cts1=tt=cnt=matrix(nrow=n, ncol=1)	
	precision=rounding
	for (p in 1:n)
	{
	cts1[[p]]<-sprintf(paste("%2.",precision,"f",sep=""),cuts[[p]])
	}
	
	for(s in 1:n)
	{
	tt[[s]]<-as.character(paste(" (",cutpoints[[s]],")",sep=""))
	}
	
	for(s in 1:n)
	{
	cnt[[s]]<-sprintf(paste("%",precision+3,"s",sep=""),tt[[s]])
	}
	cts<-cbind(cts1,cnt)
	ctsn<-do.call(paste,as.data.frame(cts))
	cutpointsColors <- col.vec
	levels(Level@data$zCat)<-cutpointsColors
	if (!is.null(Level)) 
	{
		plot(Level,lwd=bw,axes = FALSE, las = 1,col=as.character(Level@data$zCat))
	}
	if (!is.null(lak)) 
	{
	plot(lak,add=TRUE,col="blue")
	}
	if (plot.legend) legend("topleft", ctsn,fill = cutpointsColors,bty="n",title=title,cex=cex.legend,inset=0.05)
		}
	else {
	cuts=breaks
	n<-length(cuts)-1
	Level@data$zCat <- cut(Level@data[,z],breaks,include.lowest=TRUE,dig.lab=rounding)
	cutpoints<-table(Level@data$zCat)
	cts1=cts2=cts3=cnt=cts4=tt=cts5=mm=nn=matrix(nrow=n, ncol=1)
	precision=rounding
	for (p in 1:n)
	{
	mm[[p]]<-sprintf(paste("%0.",precision,"f",sep=""),cuts[[p]])
	}
	for(p in 1:n)
	{
	cts1[[p]]<-sprintf(paste("%",precision+4,"s",sep=""),mm[[p]])
	}
	for (r in 1:n)
	{
	cts2[[r]]<-" to"
	}
	for (q in 1:n)
	{
	nn[[q]]<-sprintf(paste("%0.",precision,"f",sep=""),cuts[[q+1]])
	}
	for(q in 1:n)
	{
	cts3[[q]]<-sprintf(paste("%",precision+4,"s",sep=""),nn[[q]])
	}
	for(s in 1:n)
	{
	tt[[s]]<-as.character(paste(" (",cutpoints[[s]],")",sep=""))
	}
	
	for(s in 1:n)
	{
	cnt[[s]]<-sprintf(paste("%",precision+3,"s",sep=""),tt[[s]])
	}
	cts<-cbind(cts1,cts2,cts3,cnt)
	ctsn<-do.call(paste,as.data.frame(cts))
	cutpointsColors <- col.vec
	levels(Level@data$zCat)<-cutpointsColors
	if (!is.null(Level)) 
	{
		plot(Level,lwd=bw,axes = FALSE, las = 1,col=as.character(Level@data$zCat))
	}
	if (!is.null(lak)) 
	{
	plot(lak,add=TRUE,col="blue")
	}
	if (plot.legend) legend("topleft", rev(ctsn),fill = rev(cutpointsColors),bty="n",title=title,cex=cex.legend)
}

}

}


#=======================================================================================
{	# FUNCTION TO CHOOSE MANUAL COLORS WHILE PLOTTING
#=======================================================================================
# FUNCTION 		: IAZICol
# CREATED 		: 20130805/TVK/PVN
# DESCRIPTION	: This function creates the manual colors for maps depending on the breaks and sttyle of the breakpoints.
# This is used inside the IAZIMAP() function
# IAZICol(brkpts=breakpoints,MapType=MapType)	
# Depending on the breakpoints and MapType given displays 4 set of colors to choose one among them.
# For Kanton maximum breakpoints allowed is 5 only. 
# MODIFIED		: 20130822/PVN added the condition for the number of breakpoints to restrict at 8 and print a messsage
#=======================================================================================
IAZICol<-function(brkpts=NULL, MapType=MapType)
{
	opar <<- par() 
	par(mfrow=c(2,2))
	if(brkpts>8)
	{
	dev.cur()
		cat("\n")
		cat("##================================================================================##\n")
		cat("## Maximum allowed colors or intervals is fixed to 8 and setting to 8 by default. ## \n")
		cat("##================================================================================##\n")
		colr81 <- c(brewer.pal(9,"Blues")[7:4],brewer.pal(8,"BuPu")[[3]],brewer.pal(9,"Oranges")[3:5])
		colr82 <- c(brewer.pal(9,"Blues")[6:5],brewer.pal(8,"BuPu")[[3]],brewer.pal(9,"Oranges")[2:6])
		colr83 <- c(brewer.pal(9,"Blues")[8:5],brewer.pal(9,"Oranges")[3:6])
		colr84 <- c(brewer.pal(9,"Blues")[6:4],brewer.pal(8,"BuPu")[[3]],brewer.pal(9,"Oranges")[3:6])
		n <- 8
		image(1:n, 1, as.matrix(1:n), col = colr81, xlab="colr81")
		image(1:n, 1, as.matrix(1:n), col = colr82, xlab="colr82")
		image(1:n, 1, as.matrix(1:n), col = colr83, xlab="colr83")
		image(1:n, 1, as.matrix(1:n), col = colr84, xlab="colr84")
		par(opar)
	}
	else if(brkpts<2)
	{
	dev.cur()
	stop("breakpoints cannot be less than 2")
	}
	else if (brkpts>5 && MapType=="Kanton")
	{
	dev.cur()
	stop("For Kantons maximum allowed breakpoints is 5 only.")
	}else if(brkpts==8 && MapType=="Gemeinde")
	{
		colr81 <- c(brewer.pal(9,"Blues")[7:4],brewer.pal(8,"BuPu")[[3]],brewer.pal(9,"Oranges")[3:5])
		colr82 <- c(brewer.pal(9,"Blues")[6:5],brewer.pal(8,"BuPu")[[3]],brewer.pal(9,"Oranges")[2:6])
		colr83 <- c(brewer.pal(9,"Blues")[8:5],brewer.pal(9,"Oranges")[3:6])
		colr84 <- c(brewer.pal(9,"Blues")[6:4],brewer.pal(8,"BuPu")[[3]],brewer.pal(9,"Oranges")[3:6])
		n <- 8
		image(1:n, 1, as.matrix(1:n), col = colr81, xlab="colr81")
		image(1:n, 1, as.matrix(1:n), col = colr82, xlab="colr82")
		image(1:n, 1, as.matrix(1:n), col = colr83, xlab="colr83")
		image(1:n, 1, as.matrix(1:n), col = colr84, xlab="colr84")
		par(opar)

	}else if(brkpts==7 && MapType=="Gemeinde")
	{
		colr71 <- c(brewer.pal(9,"Blues")[7:4],brewer.pal(9,"Oranges")[3:5])
		colr72 <- c(brewer.pal(9,"Blues")[5:4],brewer.pal(8,"BuPu")[[3]],brewer.pal(9,"Oranges")[2:5])
		colr73 <- c(brewer.pal(9,"Blues")[6:3],brewer.pal(9,"Oranges")[3:5])
		colr74 <- c(brewer.pal(9,"Blues")[6:4],brewer.pal(8,"BuPu")[[3]],brewer.pal(9,"Oranges")[3:5])	
		n <- 7
		image(1:n, 1, as.matrix(1:n), col = colr71, xlab="colr71")
		image(1:n, 1, as.matrix(1:n), col = colr72, xlab="colr72")
		image(1:n, 1, as.matrix(1:n), col = colr73, xlab="colr73")
		image(1:n, 1, as.matrix(1:n), col = colr74, xlab="colr74")
		par(opar)

	}else if(brkpts==6 && MapType=="Gemeinde")
	{
		colr61 <- c(brewer.pal(9,"Blues")[6:5],brewer.pal(8,"BuPu")[[3]],brewer.pal(9,"Oranges")[3:5])
		colr62 <- c(brewer.pal(9,"Blues")[[7]],brewer.pal(9,"Blues")[[5]],brewer.pal(8,"BuPu")[[3]],brewer.pal(9,"Oranges")[3:5])
		colr63 <- c(brewer.pal(9,"Blues")[6:4],brewer.pal(8,"BuPu")[[3]],brewer.pal(9,"Oranges")[4:5])
		colr64 <- c(brewer.pal(9,"Blues")[[7]],brewer.pal(9,"Blues")[5:4],brewer.pal(8,"BuPu")[[3]],brewer.pal(9,"Oranges")[3:4])
		n <- 6
		image(1:n, 1, as.matrix(1:n), col = colr61, xlab="colr61")
		image(1:n, 1, as.matrix(1:n), col = colr62, xlab="colr62")
		image(1:n, 1, as.matrix(1:n), col = colr63, xlab="colr63")
		image(1:n, 1, as.matrix(1:n), col = colr64, xlab="colr64")
		par(opar)
		
	}else if(brkpts==5 && (MapType=="Gemeinde" || MapType=="Kanton"))
	{
		colr51 <- rev(c("#804000", "#ffbc79", "#ffdfbf", "#808000", "#0080ff"))
		colr52 <- rev(c("#804000", "#ffb062", "#ffe0c1", "#ffffff", "#c1ecee"))
		colr53 <- c("#804000", "#ff8000", "#76ceeb", "#d7fd64", "#ffffff")
		colr54 <- c("#004000", "#80ff80", "#ff8040", "#07c1f8", "#c1ecee")
		n <- 5
		image(1:n, 1, as.matrix(1:n), col = colr51, xlab="colr51")
		image(1:n, 1, as.matrix(1:n), col = colr52, xlab="colr52")
		image(1:n, 1, as.matrix(1:n), col = colr53, xlab="colr53")
		image(1:n, 1, as.matrix(1:n), col = colr54, xlab="colr54")
		par(opar)
		
	}else if(brkpts==4 && (MapType=="Gemeinde" || MapType=="Kanton"))
	{
		colr41 <- c(brewer.pal(9,"Blues")[6:5],brewer.pal(9,"Oranges")[4:5])
		colr42 <- c(brewer.pal(9,"Blues")[[7]],brewer.pal(9,"Blues")[[5]],brewer.pal(9,"Oranges")[4:5])
		colr43 <- c(brewer.pal(9,"Blues")[6:5],brewer.pal(9,"Oranges")[2:3])
		colr44 <- c(brewer.pal(9,"Blues")[5:4],brewer.pal(9,"Oranges")[3:4])
		n <- 4
		image(1:n, 1, as.matrix(1:n), col = colr41, xlab="colr41")
		image(1:n, 1, as.matrix(1:n), col = colr42, xlab="colr42")
		image(1:n, 1, as.matrix(1:n), col = colr43, xlab="colr43")
		image(1:n, 1, as.matrix(1:n), col = colr44, xlab="colr44")
		par(opar)
		
	}else if(brkpts==3 && (MapType=="Gemeinde" || MapType=="Kanton"))
	{
		colr31 <- rev(c("#ff8000", "#ffe0c1", "#408080"))
		colr32 <- rev(c("#008000", "#ffffff", "#ff0000"))
		colr33 <- c("#008000", "#ff8000", "#ff0000")
		colr34 <- c("#ff8000", "#c1ecee", "#804000")
		n <- 4
		image(1:n, 1, as.matrix(1:n), col = colr31, xlab="colr31")
		image(1:n, 1, as.matrix(1:n), col = colr32, xlab="colr32")
		image(1:n, 1, as.matrix(1:n), col = colr33, xlab="colr33")
		image(1:n, 1, as.matrix(1:n), col = colr34, xlab="colr34")
		par(opar)
		
	}else
	{
		colr21 <- c("#ff0000", "#008000")
		colr22 <- c("#ff8000", "#c1ecee")
		colr23 <- c("#ff8080", "#80ff80")
		colr24 <- c("#ff0000", "#ffffff")
		n <- 4
		image(1:n, 1, as.matrix(1:n), col = colr21, xlab="colr21")
		image(1:n, 1, as.matrix(1:n), col = colr22, xlab="colr22")
		image(1:n, 1, as.matrix(1:n), col = colr23, xlab="colr23")
		image(1:n, 1, as.matrix(1:n), col = colr24, xlab="colr24")
		par(opar)		
	}
	
	}
}


##====================================================================================
{	# INIT BREAKPOINT FUNCTION
#=======================================================================================
# FUNCTION		: breakpoint
# CREATED		: 20130730/TVK
# DESCRIPTION	: This function creates the number of cuts(breakpoints) that should be passed to the map.Swiss function.
# set.seed(5); x<-rnorm(25);
# > breakpoint(Data=x, breakpoints=4, style="sd",precision=2)
# [1] -2.19 -2.97 -0.03  2.90  1.72
# 
# These values will be taken as the cutpoints for mapSwiss() function.
#=======================================================================================

breakpoint <- function(Data=NULL,breakpoints=NULL, style=c("fixed","sd","unique"),precision=NULL)
{	
	breakpoints=min(breakpoints,8)
	mu<-mean(Data,na.rm=TRUE)
	sig<-sd(Data,na.rm=TRUE)
	min<-min(Data,na.rm=TRUE)
	max<-max(Data,na.rm=TRUE)
	if(missing(style)){ style="fixed"}
	if(style=="sd")
	{
	sigp3 <- mu+3*sig
	sigm3 <- mu-3*sig
	sigp2 <- mu+2*sig
	sigm2 <- mu-2*sig
	sigp1 <- mu+sig
	sigm1 <- mu-sig
	ifelse(precision==4, p <- 0.0001, ifelse(precision==3, p<- 0.001, ifelse(precision==2, p<-0.01, ifelse(precision==1, p<- 0.1, p <- 1))))
	cuts <- round(c(min-p,sigm3,sigm2,sigm1,mu,sigp1,sigp2,sigp3,max+p),precision)
	if(breakpoints>=8){ cuts < - cuts} 
	else if(breakpoints==7) { cuts <- cuts[-2]}
	else if(breakpoints==6) { cuts <- cuts[c(-3,-7)]}
	else if(breakpoints==5) { cuts <- cuts[c(-3,-5,-7)]}
	else if(breakpoints==4) { cuts <- cuts[c(-3,-4,-6,-7)]}
	else if(breakpoints==3) { cuts <- cuts[c(-3,-4,-5,-6,-7)]}
	else{ cuts <- cuts[c(-2,-3,-4,-6,-7,-8)]}
	}
	else if(style=="fixed")
	{
	ifelse(precision==4, p <- 0.0001, ifelse(precision==3, p<- 0.001, ifelse(precision==2, p<-0.01, ifelse(precision==1, p<- 0.1, p <- 1))))
	cuts<-round(seq(min-p, max+p,length.out=breakpoints+1),precision)
	# cuts<-seq(min, max,length.out=breakpoints+1)
	}
	else if (style=="unique")
	{
	cuts<-sort(unique(Data))
	}
	else
	{
	stop("style must be either sd or fixed or unique.")
	}
	return(cuts)
	# return(round(cuts,precision))
}
	}


##====================================================================================
{	# DIALOG BOX TO SELECT COLORS
#=======================================================================================
# FUNCTION : colorsel
# CREATED  : 20130730/TVK
#
# This function adds the UI to choose the color based on the breakpoints passed.
# colorsel(breakpoints)
# 
#=======================================================================================

colorsel <- function(breakpoints=NULL)
{
	breakpoints<-min(breakpoints,8)
	colr <- matrix(nrow=breakpoints, ncol=1)
	for(i in 1:breakpoints)
	{
		colr[i,] <- as.character(tcl("tk_chooseColor"))
	}
return(colr)
}

}




##====================================================================================
{# FName Definition
##====================================================================================
FName<-function(FileName="",FilePath="")
{
 paste(FilePath, FileName, sep="") 
}
}
