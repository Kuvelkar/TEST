## source("H:\\1_Allgemein\\0_R\\LIB_R\\DBExport.r")
##
## 20050925/PhS Modified the date to display yyyymmdd.HHMMSS with timeDate(date(), in.format="%w %m %d %H:%M:%S %Z %Y",format="%Y%02m%02d.%02H%02M%02S")
## 20060313/PhS Modified the ODBC export introducing a "dummy" file to work around an error message
## 20060713/PhS Modified the time assignment to call REPMDate()
## 20061005/PhS Modified DB_, est_, mod_ and sigExport using a cbind.data.frame instead of cbind to export the correct data type, and 
##		Changed the order of Table and File, so that by default (only one string) the table is created in the ODBC, but not the excel.
## 20080728/PhS&ESU Edited comment
## 20080821/PhS&ESU Commented exportData out and replaced by calls to write.xls and by sqlSave
## 20100218/PHS     Use the R ODBC channel defined during Initialization: .ODBCChannel
## 20100219/PHS     Modified function DBExport for R, replaced Excel export with CSV export, added test that action was performed
## 20100422/PHS		Created function SQLCompatibility to avoid Export problems due to improper field names, Modified DBExport
## 20120216/PHS		Modified DBExport to export file correctly, replaced generic T and F by TRUE and FALSE
## 20120221/TVK		Modified modExport,DBExport with new parameter varTypes(mentioned in bowplot) to create variables with proper data type in SQL
##
## Calls	DBExport(Obj, "tabName")		Creates (overwrites) table tabName with object Obj, no MS-Excel file created
##		DBExport(Obj, "tabName", "filName")	Creates (overwrites) table tabName and excel file filName.XLS with object Obj	
##

SQLCompatibility <- function (X)
# FUNCTION SQLCompatibility
# created 20100422/PHS
#
# This function returns a data frame with SQL more compatible names. It handles ".", "\", "*", "/", "(", ")".
# If X is not a data frame, it is returned unchanged
#
# dfr <- SQLCompatibility(dfr)
{
	if (is.data.frame(X)) 
	{
	XCols <- gsub("\\.", "_", names(X))
	XCols <- gsub("\\\\", "_", XCols)
	XCols <- gsub("\\*", "m", XCols)
	XCols <- gsub("\\/", "d", XCols)
	XCols <- gsub("\\(", "_", XCols)
	XCols <- gsub("\\)", "_", XCols)
	names(X) <- XCols
	} else if(is.character(X))
	{
	XCols <- gsub("\\.", "_", X)
	XCols <- gsub("\\\\", "_", XCols)
	XCols <- gsub("\\*", "m", XCols)
	XCols <- gsub("\\/", "d", XCols)
	XCols <- gsub("\\(", "_", XCols)
	X <- gsub("\\)", "_", XCols)
	}
	return(X)
}

DBExport<-function(X, Table="", File="", colNames=TRUE, rowNames=FALSE, varTypes="", ...)
{
##
## Sauvetage sur disque des moyennes et de la matrice de covariance pour ce mod?le
## ====================
## 20100219/PHS     Modified function DBExport for R, replaced Excel export with CSV export, added test that action was performed
## 20100422/PHS		Introduced call to SQLCompatibility
## 20120216/PHS		Removed col.names from write.csv call, changed tosave to X, removed col.names from call (obsolete, causes a warning)
  n <- 0
  if (File!="")
  {
    cat("Saving ", .FilePath, File, ".csv\n", sep="")
	write.csv(X, file = FName(paste(File,".csv",sep=""),FilePath), row.names = rowNames)
	n <- n+1
  }

  if (Table!="")
  {
	X <- SQLCompatibility(X)
	Table <- SQLCompatibility(Table)
	sqlQuery(.ODBCChannel, paste("DROP TABLE ",Table), errors = FALSE) 
	#sqlSave(.ODBCChannel, X, tablename = Table, rownames = rowNames)
	sqlSave(.ODBCChannel, X, tablename = Table, rownames = rowNames, varTypes=varTypes)
	NbRows<-sqlQuery(.ODBCChannel, paste("SELECT count(*) FROM ",Table), errors = FALSE, rows_at_time = 1) [1,]

    cat("Created ",NbRows, " records in table", Table, " on ODBC server ", .ODBCServer, "\n")
	n <- n+1
  }
  if (n==0) cat("Warning: Data was neiter exported to a file nor inserted into a DB\n")
}

SQLExport<-function(X, Table="", colNames=TRUE, rowNames=FALSE, ...)
{
##
## Save on SQL server .ODBCServer of objet X as Table 
## ==================================================
## 20100422/PHS		Introduced call to SQLCompatibility
  if (Table!="")
  {
## 20080821/PhS&ESU Commented exportData out and replaced by calls to write.xls and by sqlSave
##    NbRows<-exportData(X, file="dummy", server=.ODBCServer, type="ODBC", odbcConnection = .ODBCConnect, odbcTable = Table, colNames=colNames, rowNames=rowNames, use.locale=T)
 	X <- SQLCompatibility(X)
	Table <- SQLCompatibility(Table)	
    sqlQuery(.ODBCChannel, paste("DROP TABLE ",Table), errors = FALSE) 
    sqlSave(.ODBCChannel, X, tablename = Table, rownames = rowNames)
    NbRows<-sqlQuery(.ODBCChannel, paste("SELECT count(*) FROM ",Table), errors = FALSE, rows_at_time = 1) [1,]
    cat("Creating ",NbRows, " records in table", Table, " on ODBC server ", .ODBCServer, "\n")
  }
}


estExport<-function(X, Table="", File="", varTypes=varTypes, ...)
{
##
## Sauvetage sur disque des estimation 
## ====================
  RecNo<-rep(1:length(X[,1]), 1)
  time<-REPMDate()
  Model<-rep(FName(Table,FilePath), length(X[,1]))
  toExport <- cbind.data.frame(RecNo, Model,time, X)
  DBExport(toExport, Table, File, rowNames=F, varTypes=varTypes)
}

modExport<-function(X, Table="", File="", varTypes="",...)
#modExport<-function(X, Table="", File="",...)
{
##
## Sauvetage sur disque du mod?le X (sum????$coef, sum????$cov)
## ====================
  RecNo<-rep(1:length(X[,1]), 1)
  time<-REPMDate()
  Model<-rep(FName(File,FilePath), length(X[,1]))
  toExport <- cbind.data.frame(RecNo, Model,time,X)
  #DBExport(toExport, Table, File, rowNames=TRUE)
  DBExport(toExport, Table, File, rowNames=TRUE,varTypes=varTypes)
  }

sigExport<-function(X, Table="", File="", ...)
{
##
## Sauvetage sur disque du sigma du mod?le X (sum????$sigma)
## ====================
  RecNo<-rep(1:length(X), 1)
  time<-REPMDate()
  Model<-rep(FName(Table,FilePath), length(X))
  toExport <- cbind.data.frame(RecNo, Model,time,X)
  DBExport(toExport, Table, File, rowNames=F)
}
