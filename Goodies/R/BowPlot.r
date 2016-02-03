## S-PLUS Functions for Boxplot with Percentile cuts and other utilities
## ---------------------------------------------------------------------
##
## 20050925/PhS 	Modified the date to display yyyymmdd.HHMMSS with REPMDate()
## 20051001/PhS 	Modified resboxcont histlim: added as.numeric(...), set histlim[length(histlim)] dynamically (was fix 11)
## 20060713/PhS 	Added test to stop if no data is left for boxplot after filtering,
##					Introduced usage of REPMDate()
## 20061009/PhS/ESu/SKa	Added as.data.frame for export of the boxplot table in order to savec the data type
## 20061020/PhS		Modified the cut and setting the limit for significance due to the maximum length of summary (100)
## 20061211/PhS/SKa	Removed commenting of export of WMF
## 20070407/PhS		Corrected the treatment when number of classes >0 .
##						1. Introduced a test if(sa[101]+1>SignLim)
##						2. After new limits are set, reset the original order to the classes otherwise it causes a mix-up
## 20070413/PhS&ESu	Removed boxcol=0, to solve color problem
## 20070727/PhS		Added functions bxpMake (prepare bxp container) and boxplotSQL (generate boxplot from SQL boxplot table)
##					modified the DBExport to modExport, in order to have the proper data type and sort order
## 20100421/PHS&CSE	Modified bowplot, boxclass & boxplotSQL for R
## 20100422/PHS&CSE	Modified bowplot to reflect different order applied in boxplot by R and by S-Plus, commented the margins, replaced default for SW to NULL
## 20110329/PHS		Replaced oldCut by cut function
## 20110405/PHS		Added PHS.bxp and use call to the new function instead of bxp, added unique to levels assignment to avoid warning
## 20120218/TVK,CSE	Introduced IAZI colors for the residual boxplots
## 20120221/TVK		Introduced variable varTypes in bowplot to send the data from R to SQL with data type as float or varchar
##
## boxplot function usile probs as percentile cuts
## 20130507/PVN   added IAZIColor () with the IAZIColor definitions
###########################################################################################################


PHS.bxp <- function (z, notch = FALSE, width = NULL, varwidth = FALSE, outline = TRUE, 
    notch.frac = 0.5, log = "", border = par("fg"), pars = NULL, 
    frame.plot = axes, horizontal = FALSE, add = FALSE, at = NULL, 
    show.names = NULL, 
    medlwd=5, confint=FALSE, confcol=2, boxwex = 0.5, staplewex = 1, ...) 
# Modified version of bxp, in order to draw the boxplots like in S-plus
# Modified 20110405/PHS
# The use of the following parameters are inspired by the S-plus implementation of bxp
# medlwd      median line width. Setting this parameter implicitly sets the medline parameter to TRUE. The special value, NA, is used to indicate the current line width ( par("lwd")). The default is 5, but the "old" and "att" styles set the it to 5. 
# confint     confidence interval logical flag. If TRUE, use z$conf to display confidence intervals. How the confidence intervals are shown is determined by the notch, confcol, confangle and confdensity parameters. 
# confcol     confidence interval color. If supplied, confidence intervals will be filled with the indicated color. The default is 2. 
{
    pars <- c(list(...), pars)
    pars <- pars[unique(names(pars))]
    bplt <- function(x, wid, stats, out, conf, notch, xlog, i) {
        ok <- TRUE
        if (!any(is.na(stats))) {
            xP <- if (xlog) 
                function(x, w) x * exp(w)
            else function(x, w) x + w
            wid <- wid/2
            if (notch) {
                ok <- stats[2L] <= conf[1L] && conf[2L] <= stats[4L]
                xx <- xP(x, wid * c(-1, 1, 1, notch.frac, 1, 
                  1, -1, -1, -notch.frac, -1))
                yy <- c(stats[c(2, 2)], conf[1L], stats[3L], 
                  conf[2L], stats[c(4, 4)], conf[2L], stats[3L], 
                  conf[1L])
				if (confint) {
					xconf <- xP(x, wid * c(-1, 1, notch.frac, 1, -1, -notch.frac))
					yconf <- c(conf[c(1, 1)], stats[3L], conf[c(2, 2)], stats[3L])
				}
            }
            else {
                xx <- xP(x, wid * c(-1, 1, 1, -1))
                yy <- stats[c(2, 2, 4, 4)]
				if (confint) {
					xconf <- xP(x, wid * c(-1, 1, 1, -1))
					yconf <- c(conf[c(1, 1)], conf[c(2, 2)])
				}
            }
            if (!notch) 
                notch.frac <- 1
            wntch <- notch.frac * wid
            xypolygon(xx, yy, lty = "blank", col = boxfill[i])						# draw a box without border
			if (confint) xypolygon(xconf, yconf, lty = "blank", col = confcol)		# draw the confidence intervall
            xysegments(xP(x, -wntch), stats[3L], xP(x, +wntch), 					# draw the median
                stats[3L], lty = medlty[i], lwd = medlwd[i], 
                col = medcol[i], lend = 0)											# lend = 0 instead of 1
            xypoints(x, stats[3L], pch = medpch[i], cex = medcex[i], 				# point the median
                col = medcol[i], bg = medbg[i])
            xysegments(rep.int(x, 2), stats[c(1, 5)], rep.int(x, 					# draw the wiskers
                2), stats[c(2, 4)], lty = whisklty[i], lwd = whisklwd[i], 
                col = whiskcol[i])
            xysegments(rep.int(xP(x, -wid * staplewex[i]), 2), 						# draw the staples
                stats[c(1, 5)], rep.int(xP(x, +wid * staplewex[i]), 
                  2), stats[c(1, 5)], lty = staplelty[i], lwd = staplelwd[i], 
                col = staplecol[i])
            xypolygon(xx, yy, lty = boxlty[i], lwd = boxlwd[i], 					# draw the border of the box
                border = boxcol[i])
            if ((nout <- length(out))) { 
                xysegments(rep(x - wid * outwex, nout), out, 						# draw the outliers segment
                  rep(x + wid * outwex, nout), out, lty = outlty[i], 
                  lwd = outlwd[i], col = outcol[i])
                xypoints(rep.int(x, nout), out, pch = outpch[i], 					# point the outliers
                  lwd = outlwd[i], cex = outcex[i], col = outcol[i], 
                  bg = outbg[i])
            }
            if (any(inf <- !is.finite(out))) {
                warning(sprintf(ngettext(length(unique(out[inf])), 
                  "Outlier (%s) in boxplot %d is not drawn", 
                  "Outliers (%s) in boxplot %d are not drawn"), 
                  paste(unique(out[inf]), collapse = ", "), x), 
                  domain = NA)
            }
        }
        return(ok)
    }
    if (!is.list(z) || 0L == (n <- length(z$n))) 
        stop("invalid first argument")
    if (is.null(at)) 
        at <- 1L:n
    else if (length(at) != n) 
        stop("'at' must have same length as 'z$n', i.e. ", n)
    if (is.null(z$out)) 
        z$out <- numeric()
    if (is.null(z$group) || !outline) 
        z$group <- integer()
    if (is.null(pars$ylim)) 
        ylim <- range(z$stats[is.finite(z$stats)], if (outline) z$out[is.finite(z$out)], 
            if (notch) z$conf[is.finite(z$conf)])
    else {
        ylim <- pars$ylim
        pars$ylim <- NULL
    }
    if (is.null(pars$xlim)) 
        xlim <- c(0.5, n + 0.5)
    else {
        xlim <- pars$xlim
        pars$xlim <- NULL
    }
    if (length(border) == 0L) 
        border <- par("fg")
    if (!add) {
        plot.new()
        if (horizontal) 
            plot.window(ylim = xlim, xlim = ylim, log = log, 
                xaxs = pars$yaxs)
        else plot.window(xlim = xlim, ylim = ylim, log = log, 
            yaxs = pars$yaxs)
    }
    xlog <- (par("ylog") && horizontal) || (par("xlog") && !horizontal)
    pcycle <- function(p, def1, def2 = NULL) rep(if (length(p)) p else if (length(def1)) def1 else def2, 
        length.out = n)
    p <- function(sym) pars[[sym, exact = TRUE]]
    boxlty <- pcycle(pars$boxlty, p("lty"), par("lty"))
    boxlwd <- pcycle(pars$boxlwd, p("lwd"), par("lwd"))
    boxcol <- pcycle(pars$boxcol, border)
    boxfill <- pcycle(pars$boxfill, par("bg"))
    boxwex <- pcycle(pars$boxwex, boxwex * {								# boxwex instead of 0.8 for compatibility with S-plus
        if (n <= 1) 
            1
        else stats::quantile(diff(sort(if (xlog) 
            log(at)
        else at)), 0.1)
    })
    medlty <- pcycle(pars$medlty, p("lty"), par("lty"))
    medlwd <- pcycle(pars$medlwd, medlwd, 3 * par("lwd"))				# medlwd instead of 3 * p("lwd") for S-plus compatibility
    medpch <- pcycle(pars$medpch, NA_integer_)
    medcex <- pcycle(pars$medcex, p("cex"), par("cex"))
    medcol <- pcycle(pars$medcol, border)
    medbg <- pcycle(pars$medbg, p("bg"), par("bg"))
    whisklty <- pcycle(pars$whisklty, p("lty"), "dashed")
    whisklwd <- pcycle(pars$whisklwd, p("lwd"), par("lwd"))
    whiskcol <- pcycle(pars$whiskcol, border)
    staplelty <- pcycle(pars$staplelty, p("lty"), par("lty"))
    staplelwd <- pcycle(pars$staplelwd, p("lwd"), par("lwd"))
    staplecol <- pcycle(pars$staplecol, border)
    staplewex <- pcycle(pars$staplewex, staplewex)						# staplewex instead of 0.5 for compatibility with S-plus
    outlty <- pcycle(pars$outlty, "blank")
    outlwd <- pcycle(pars$outlwd, p("lwd"), par("lwd"))
    outpch <- pcycle(pars$outpch, p("pch"), par("pch"))
    outcex <- pcycle(pars$outcex, p("cex"), par("cex"))
    outcol <- pcycle(pars$outcol, border)
    outbg <- pcycle(pars$outbg, p("bg"), par("bg"))
    outwex <- pcycle(pars$outwex, 0.5)
    width <- if (!is.null(width)) {
        if (length(width) != n | any(is.na(width)) | any(width <= 
            0)) 
            stop("invalid boxplot widths")
        boxwex * width/max(width)
    }
    else if (varwidth) 
        boxwex * sqrt(z$n/max(z$n))
    else if (n == 1) 
        0.5 * boxwex
    else rep.int(boxwex, n)
    if (horizontal) {
        xypoints <- function(x, y, ...) points(y, x, ...)
        xypolygon <- function(x, y, ...) polygon(y, x, ...)
        xysegments <- function(x0, y0, x1, y1, ...) segments(y0, 
            x0, y1, x1, ...)
    }
    else {
        xypoints <- points
        xypolygon <- polygon
        xysegments <- segments
    }
    ok <- TRUE
    for (i in 1L:n) ok <- ok & bplt(at[i], wid = width[i], stats = z$stats[, 
        i], out = z$out[z$group == i], conf = z$conf[, i], notch = notch, 
        xlog = xlog, i = i)
    if (!ok) 
        warning("some notches went outside hinges ('box'): maybe set notch=FALSE")
    axes <- is.null(pars$axes)
    if (!axes) {
        axes <- pars$axes
        pars$axes <- NULL
    }
    if (axes) {
        ax.pars <- pars[names(pars) %in% c("xaxt", "yaxt", "xaxp", 
            "yaxp", "las", "cex.axis", "col.axis", "format")]
        if (is.null(show.names)) 
            show.names <- n > 1
        if (show.names) 
            do.call("axis", c(list(side = 1 + horizontal, at = at, 
                labels = z$names), ax.pars))
        do.call("Axis", c(list(x = z$stats, side = 2 - horizontal), 
            ax.pars))
    }
    do.call("title", pars[names(pars) %in% c("main", "cex.main", 
        "col.main", "sub", "cex.sub", "col.sub", "xlab", "ylab", 
        "cex.lab", "col.lab")])
    if (frame.plot) 
        box(col=IAZIColor("IAZIBlue1"))
    invisible(at)
}
###########################################################################################################
###########################################################################################################
###########################################################################################################

bowplot <- function(X, SW=NULL, probs=c(lowCut=0.1,hiCut=0.9), DispVal=T, Export=NULL, confcol=13, TitClaVis=T, DigTab=1, ...)
{
	start<-REPMDate()
	bxp.X <- boxplot(X, plot=F)
	if (length(dimnames(summary(X))[[1]])==1) { bxp.X$names <- dimnames(summary(X))[[1]] } else { if(length(grep(" thru",  bxp.X$names))==0) { bxp.X$names <- dimnames(summary(X))[[1]] } else {  bxp.X$names[grep(" thru",  bxp.X$names)]<-paste(signif(as.numeric(substring(dimnames(summary(X))[[1]],1,regexpr(" thru", dimnames(summary(X))[[1]])-2)),4),"+", sep="") } }

	## calculate quantiles
	quant <- sapply(X, quantile, probs=probs, na.rm = T)

	## change the whisker end points in bxp.X$stats
	bxp.X$stats[c(1,5),] <- quant					# Order is inversed compared to S-Plus
	
	a7<-matrix(nrow=16,ncol=length(bxp.X$names))
	a7[1,]<-1:length(bxp.X$names)
	a7[2,]<-rep(if(is.null(Export)) "bowplot" else Export,length(bxp.X$names))
	a7[3,]<-bxp.X$names
	a7[4,]<-as.integer(bxp.X$n)
	a7[5,]<-cbind(sapply(X,max))
	a7[6:10,]<-as.numeric(bxp.X$stats[(5:1),])		# Inverse order to get the same data structure as with S-Plus
	a7[11,]<-cbind(sapply(X,min))
	a7[12:13,]<-as.numeric(bxp.X$conf[(2:1),])		# Inverse order to get the same data structure as with S-Plus
	a7[14,]<-as.numeric(cbind(sapply(X,mean)))
	a7[15,]<-as.numeric(cbind((sapply(X,var))^0.5))
	if(is.null(SW)) { a7[16,]<-as.numeric(cbind(sapply(X,mean))) } else { for(i in 1:length(SW[dimnames(summary(X))[[1]]])) a7[16,i]<-sum(cbind(c(t(SW[dimnames(summary(X))[[1]][i]]))[[1]])*cbind(c(t(X[dimnames(summary(X))[[1]][i]]))[[1]]))/sum(cbind(c(t(SW[dimnames(summary(X))[[1]][i]]))[[1]])) }
	dimnames(a7) <-list(c("sort","plot","names","n","max","uplim","Q1","median","Q3","lowlim","min","upconf","lowconf","mean","stdev", "wtmean"),NULL)
	varTypes <- c("float", "varchar(255)", "varchar(255)", "float", "float", "float", "float", "float", "float", "float", "float", "float", "float", "float", "float", "float")
	names(varTypes)<-list("sort","plot","names","n","max","uplim","Q1","median","Q3","lowlim","min","upconf","lowconf","mean","stdev", "wtmean")

	a8<-matrix(nrow=8,ncol=length(bxp.X$names))
	a8[1,]<-a7[3,]
 	a8[2,]<-as.numeric(a7[16,])
	a8[3,]<-a7[4,]
	a8[4,]<-a7[6,]
	a8[5,]<-a7[7,]
	a8[6,]<-a7[8,]
	a8[7,]<-a7[9,]
	a8[8,]<-a7[10,]
	dimnames(a8) <-list(c("names", "wtmean","n","uplim","Q1","median","Q3","lowlim"),NULL)

	if (DispVal!=F) 
	{
		a6<-matrix(nrow=8,ncol=length(bxp.X$names))
		a6[1,]<-as.integer(bxp.X$n)
		a6[2,]<-as.numeric(bxp.X$stats[3,])
		a6[3:4,]<-as.numeric(bxp.X$conf)
		a6[5,]<-as.numeric(a7[16,])
		a6[6,]<-as.numeric(a7[15,])
 		a6[7,]<-as.numeric(a7[11,])
		a6[8,]<-as.numeric(a7[5,])
		dimnames(a6) <-list(c("n","median","upconf","lowconf","wtmean","stdev","min","max"),bxp.X$names)
		print(t(a6),digits=5)
	}
	if(!is.null(Export))
	{
		ASCIIFile<-FName(paste(Export,".txt",sep=""),FilePath)
		#modExport(as.data.frame(t(a7)), Export)
		modExport(as.data.frame(t(a7)), Export, varTypes=varTypes)
		write.csv(t(a7), file = ASCIIFile, row.names = FALSE)
		
		ASCIIFile<-FName(paste(Export,".bxp.txt",sep=""),FilePath)
		cat(format(a8[1,]),	  			                                      "\n", sep="\t", file=ASCIIFile)
		cat(format(round(as.numeric(a8[2,]),DigTab),nsmall=DigTab,digits=15), "\n", sep="\t", file=ASCIIFile,append=T)
		cat(a8[3,],           					   	                          "\n", sep="\t", file=ASCIIFile,append=T)
		cat(format(round(as.numeric(a8[4,]),DigTab),nsmall=DigTab,digits=15), "\n", sep="\t", file=ASCIIFile,append=T)
		cat(format(round(as.numeric(a8[5,]),DigTab),nsmall=DigTab,digits=15), "\n", sep="\t", file=ASCIIFile,append=T)
		cat(format(round(as.numeric(a8[6,]),DigTab),nsmall=DigTab,digits=15), "\n", sep="\t", file=ASCIIFile,append=T)
		cat(format(round(as.numeric(a8[7,]),DigTab),nsmall=DigTab,digits=15), "\n", sep="\t", file=ASCIIFile,append=T)
		cat(format(round(as.numeric(a8[8,]),DigTab),nsmall=DigTab,digits=15), "\n", sep="\t", file=ASCIIFile,append=T)
	}
  	## draw boxplots
	
	if (TitClaVis!=F) {invisible(PHS.bxp(bxp.X, confcol=confcol, ...))}
	else
	{
		bxp.X$names<-rep("",length(bxp.X$names))
		invisible(PHS.bxp(bxp.X, confcol=confcol, main="", ...))
	}  
	return(a7)
}

###########################################################################################################
###########################################################################################################
###########################################################################################################
bxpMake <- function (n=4) 
## bxpMake 
##########
## This function returns a "container" with the proper structure to draw a boxplot with n (default=4) classes
## example: bxp.X <- bxpMake(10) ; invisible(PHS.bxp(bxp.X,outline=F,medcol=1,whisklty=1,confint=confint,main="Test"))
#############################################################################################################
{
x <- 1:(5*n) 
return( boxplot(split(x, cut(x, n, labels=paste("class", 1:n))), plot=F))
}

###########################################################################################################
###########################################################################################################
###########################################################################################################
boxplotSQL <- function(TabName, SW=F, probs=c(lowCut=0.1,hiCut=0.9), DispVal=T, Export=NULL, confcol=13, TitClaVis=T, DigTab=1, ...)
##########
## This function produces a boxplot like function bowplot, but takes the data from a SQL table
## example: # par(mfrow=c(1,1),mar=c(5,4,4,2)+.1) ; a7 <- boxplotSQL("D2_GRAPHS_D2kv40ResTime", outline=F,medcol=1,whisklty=1,confint=T,main="D2kv40ResTime loaded from SQL",DispVal=T) ; axis(2,tck=1,lty=2)
###########################################################################################################
{
	start<-REPMDate()

	if(exists("bxpSQLData")) rm(bxpSQLData)
##	guiImportODBC(sDataSource = .ODBCServer,sConnectionString = .ODBCConnect,sTargetStartCol = "<END>",sTargetInsertOverwrite = "Insert at start col", sTableName = TabName, sSQLQuery = paste("SELECT * FROM ",TabName, " ORDER BY RecNo"), sTargetDataFrame = "bxpSQLData")
##	guiClose("data.frame", "bxpSQLData")
	bxpSQLData <- sqlQuery(.ODBCChannel, paste("SELECT * FROM ", TabName, " ORDER BY RecNo\n", sep=""))

	NClasses <- length(bxpSQLData$names)
	ClassLabels <- as.character(bxpSQLData$names)
	bxp.X <- bxpMake(length(bxpSQLData$names))
	bxp.X$names <- as.character(bxpSQLData$names)
	bxp.X$stats[1,] <- as.numeric(as.character(bxpSQLData$uplim))
	bxp.X$stats[2,] <- as.numeric(as.character(bxpSQLData$Q1))
	bxp.X$stats[3,] <- as.numeric(as.character(bxpSQLData$median))
	bxp.X$stats[4,] <- as.numeric(as.character(bxpSQLData$Q3))
	bxp.X$stats[5,] <- as.numeric(as.character(bxpSQLData$lowlim))
	bxp.X$n <- as.numeric(as.character(bxpSQLData$n))
	bxp.X$conf[1,] <- as.numeric(as.character(bxpSQLData$upconf))
	bxp.X$conf[2,] <- as.numeric(as.character(bxpSQLData$lowconf))

	a7<-matrix(nrow=16,ncol=length(bxp.X$names))
	a7[1,]<-as.character(rep(start,length(bxp.X$names)))
	a7[2,]<-rep(if(is.null(Export)) "bxpSQLData" else Export,length(bxp.X$names))
	a7[3,]<-bxp.X$names
	a7[4,]<-as.integer(bxp.X$n)
	a7[5,]<-as.numeric(as.character(bxpSQLData$max))
	a7[6:10,]<-as.numeric(bxp.X$stats)
	a7[11,]<-as.numeric(as.character(bxpSQLData$min))
	a7[12:13,]<-as.numeric(bxp.X$conf)
	a7[14,]<-as.numeric(as.character(bxpSQLData$mean))
	a7[15,]<-as.numeric(as.character(bxpSQLData$stdev))
	a7[16,]<-as.numeric(as.character(bxpSQLData$wtmean))
  
	dimnames(a7) <-list(c("sort","plot","names","n","max","uplim","Q1","median","Q3","lowlim","min","upconf","lowconf","mean","stdev", "wtmean"),NULL)

	a8<-matrix(nrow=8,ncol=length(bxp.X$names))
	a8[1,]<-a7[3,]
 	a8[2,]<-as.numeric(a7[16,])
	a8[3,]<-a7[4,]
	a8[4,]<-a7[6,]
	a8[5,]<-a7[7,]
	a8[6,]<-a7[8,]
	a8[7,]<-a7[9,]
	a8[8,]<-a7[10,]
	dimnames(a8) <-list(c("names", "wtmean","n","uplim","Q1","median","Q3","lowlim"),NULL)

	if (DispVal!=F) 
	{
		a6<-matrix(nrow=8,ncol=length(bxp.X$names))
		a6[1,]<-as.integer(bxp.X$n)
		a6[2,]<-as.numeric(bxp.X$stats[3,])
		a6[3:4,]<-as.numeric(bxp.X$conf)
		a6[5,]<-as.numeric(a7[16,])
		a6[6,]<-as.numeric(a7[15,])
 		a6[7,]<-as.numeric(a7[11,])
		a6[8,]<-as.numeric(a7[5,])
		dimnames(a6) <-list(c("n","median","upconf","lowconf","wtmean","stdev","min","max"),bxp.X$names)
		print(t(a6),digits=5)
	}
	if(!is.null(Export))
	{
		ASCIIFile<-FName(paste(Export,".txt",sep=""),FilePath)
		modExport(as.data.frame(t(a7)), Export)
		write.csv(t(a7), file = ASCIIFile, row.names = FALSE)
		
		ASCIIFile<-FName(paste(Export,".bxp.txt",sep=""),FilePath)
		cat(format(a8[1,]),	  			                                      "\n", sep="\t", file=ASCIIFile)
		cat(format(round(as.numeric(a8[2,]),DigTab),nsmall=DigTab,digits=15), "\n", sep="\t", file=ASCIIFile,append=T)
		cat(a8[3,],           					   	                          "\n", sep="\t", file=ASCIIFile,append=T)
		cat(format(round(as.numeric(a8[4,]),DigTab),nsmall=DigTab,digits=15), "\n", sep="\t", file=ASCIIFile,append=T)
		cat(format(round(as.numeric(a8[5,]),DigTab),nsmall=DigTab,digits=15), "\n", sep="\t", file=ASCIIFile,append=T)
		cat(format(round(as.numeric(a8[6,]),DigTab),nsmall=DigTab,digits=15), "\n", sep="\t", file=ASCIIFile,append=T)
		cat(format(round(as.numeric(a8[7,]),DigTab),nsmall=DigTab,digits=15), "\n", sep="\t", file=ASCIIFile,append=T)
		cat(format(round(as.numeric(a8[8,]),DigTab),nsmall=DigTab,digits=15), "\n", sep="\t", file=ASCIIFile,append=T)
	}
  	## draw boxplots
	if (TitClaVis!=F) {invisible(PHS.bxp(bxp.X, confcol=confcol, ...))}
	else
	{
		bxp.X$names<-rep("",length(bxp.X$names))
		invisible(PHS.bxp(bxp.X, confcol=confcol, main="", ...))
	}  
	return(a7)
	
}

###########################################################################################################
###########################################################################################################
###########################################################################################################

resboxcont<- function(Res, XVar, Weight=F, MyMain="", savPlt="", DispVal=T, confint=T, SignLim=5, ...)
## resboxcont
#############
## Draws a boxplot of the residuals Res for a continuous variable XVar. The limits are 
## calculated on the bases of deciles. NAs are not allowed.
######################################################################################
{
	histlim<-as.numeric(levels(factor(quantile(XVar, probs = seq(0,1,.1), na.rm = T))))
	mode(histlim)<-"numeric"
	histlim[1]<-histlim[1]-1
	histlim[length(histlim)]<-histlim[length(histlim)]+1
	newlim<-round(histlim,5)
	resboxcontfix(Res, XVar, newlim, YLIMMan=c(min(newlim), max(newlim)),Weight=Weight, MyMain=MyMain, savPlt=savPlt, DispVal=DispVal, confint=confint, SignLim=SignLim,...)
}

###########################################################################################################
###########################################################################################################
###########################################################################################################

resboxcontfix<- function(Res, XVar, newlim, Weight=F, MyMain="", savPlt="", DispVal=T, confint=T, SignLim=5,...)
## resboxcontfix
################
## Draws a boxplot of the residuals Res for a continuous variable XVar. The limits are passed.
## NAs are not allowed.
##############################################################################################
{
	Class<-cut(XVar,breaks=newlim)
	sa<-summary(Class)
	useClass<-rep(1,length(sa))
	for(i in 1:length(sa)) if(sa[i]<SignLim) useClass[i]<-0

	Class<-cut(XVar,breaks=newlim[c(1,useClass)==1])
	ScatData<-split(Res, Class)
	if (class(Weight)!="logical") {Wgt<-Weight} else {Wgt<-rep(1,length(Res))}
	ScatW<-split(Wgt, Class)

	YLIM<-HiLoScale(ScatData)
	if (DispVal!=F) 
	{
		cat("\n", MyMain, "\n")
		noCut<-length(useClass)-sum(useClass)
		if(noCut>0)
		{
			print(sa)
			if(noCut==1) cat("\n Following class is not significant (less than",SignLim,"observations): ") else cat("\n Following",noCut,"classes are not significant (less than",SignLim,"observations):\n") 
			cat(names(sa[useClass==0]), "\n\n")
		}
	}

#	if(savPlt!="") win.metafile(FName(paste(savPlt, ".wmf",sep=""),FilePath), restoreConsole = TRUE) else x11()	
	x11()	
	# par(mfrow=c(1,1),mar=c(5,4,4,2)+.1)
	par(mfrow=c(1,1),lab=c(10,10,1))

	a7 <- bowplot(ScatData,ScatW,ylim=YLIM,outline=F,medcol=1,whisklty=1,confint=confint,Export=savPlt,main=MyMain,DispVal=DispVal,...)
	# axis(2,tck=1,lty=2) old for S-Plus
	axis(2,tck=1,lty="18")
#	if(savPlt!="") dev.off()															# Close the window to save the plot
	if(savPlt!="") dev.print(device = png, units="in",width=12, height=8, res=600, file=FName(paste(savPlt, ".png",sep=""),FilePath))

	return (a7)
}

###########################################################################################################
###########################################################################################################
###########################################################################################################

resboxchar<- function(Res, XVar, Weight=F, MyMain="", savPlt="", DispVal=T, confint=T, SignLim=5,...)
## resboxchar
#############
## Draws a boxplot of the residuals Res for a discrete char-type variable XVar. 
## NAs are not allowed.
###############################################################################
{
	Xv<-c(as.character(XVar))
	nokt<-as.character(c())
	sa<-rev(sort(tapply(XVar, XVar, length)))
## Limit the number of classes to 100
	if(length(sa)>100)
	{
		SignLimOld <- SignLim
		SignLim <- sa[101]+1
		cat(paste("Too many classes, increased the significance limit from ",SignLimOld," to ",SignLim, " in order to reduce the number\n",sep=""))
	}
	for(i in 1:length(sa)){if(sa[i]<SignLim) nokt<-c(nokt,names(sa[i]))}
	recs<-rep(1,length(Res))
	if(length(nokt)>0) for(i in 1:length(nokt))
	{
		recs[Xv==nokt[i]]<-rep(0,length(recs[Xv==nokt[i]]))
	}

	ScatData<-split(Res[recs==1], factor(XVar[recs==1]))
	if (class(Weight)!="logical") {Wgt<-Weight} else {Wgt<-rep(1,length(Res))}
	ScatW<-split(Wgt[recs==1], factor(XVar[recs==1]))

	YLIM<-HiLoScale(ScatData)
	if (DispVal!=F) 
	{
		cat("\n", MyMain, "\n")
		if(length(nokt)>0)
		{
			print(summary(factor(XVar)))
			if(length(nokt)==1) cat("\n Following class is not significant (less than",SignLim,"observations): ") else cat("\n Following",length(nokt),"classes are not significant (less than",SignLim,"observations):\n") 
			cat(nokt, "\n\n")
		}
	}
	# if(savPlt!="") win.metafile(FName(paste(savPlt, ".wmf",sep=""),FilePath), restoreConsole = TRUE) else x11()	
	x11()
	# par(mfrow=c(1,1),mar=c(5,4,4,2)+.1)
	par(mfrow=c(1,1),lab=c(10,10,1))
	a7 <- bowplot(ScatData,ScatW,ylim=YLIM,outline=F,medcol=1,whisklty=1,confint=confint,Export=savPlt,main=MyMain,DispVal=DispVal,...)
	# axis(2,tck=1,lty=2) old for S-Plus
	axis(2,tck=1,lty="18")
	# if(savPlt!="") dev.off()															# Close the window to save the plot
	if(savPlt!="") dev.print(device = png, units="in",width=12, height=8, res=600, file=FName(paste(savPlt, ".png",sep=""),FilePath))

	return(a7)
}

###########################################################################################################
###########################################################################################################
###########################################################################################################

## resboxdummy
##############
## Draws a boxplot of the residuals Res for a discrete numeric variable XVar. 
## NAs are not allowed.
#############################################################################
resboxdummy<- function(Res, XVar, Weight=F, MyMain="", savPlt="", DispVal=T, confint=T, SignLim=5,...)
{
	Xv<-c(as.character(XVar))
	nokt<-c()
	sa<-rev(sort(tapply(XVar, XVar, length)))
## Limit the number of classes to 100
	if(length(sa)>100)
	{
		SignLimOld <- SignLim
		SignLim <- sa[101]+1
		cat(paste("Too many classes, increased the significance limit from ",SignLimOld," to ",SignLim, " in order to reduce the number\n",sep=""))
	}
	for(i in 1:length(sa)){if(sa[i]<SignLim) nokt<-c(nokt,names(sa[i]))}
	recs<-rep(1,length(Res))
	if(length(nokt)>0) for(i in 1:length(nokt))
	{
		recs[Xv==nokt[i]]<-rep(0,length(recs[Xv==nokt[i]]))
	}

	ScatData<-split(Res[recs==1], factor(XVar[recs==1]))
	if (class(Weight)!="logical") {Wgt<-Weight} else {Wgt<-rep(1,length(Res))}
	ScatW<-split(Wgt[recs==1], factor(XVar[recs==1]))

	YLIM<-HiLoScale(ScatData)
	if(DispVal) 
	{
		cat("\n", MyMain, "\n")
		if(length(nokt)>0)
		{
			print(summary(factor(XVar)))
			if(length(nokt)==1) cat("\n Following class is not significant (less than",SignLim,"observations): ") else cat("\n Following",length(nokt),"classes are not significant (less than",SignLim,"observations):\n") 
			cat(nokt, "\n\n")
		}
	}
	
	# if(savPlt!="") win.metafile(FName(paste(savPlt, ".wmf",sep=""),FilePath), restoreConsole = TRUE) else x11()	
	x11()
	# par(mfrow=c(1,1),mar=c(5,4,4,2)+.1)
	par(mfrow=c(1,1),lab=c(10,10,1))
	a7 <- bowplot(ScatData,ScatW,ylim=YLIM,outline=F,medcol=1,whisklty=1,confint=confint,Export=savPlt,main=MyMain,DispVal=DispVal,...)
	# axis(2,tck=1,lty=2) old for S-Plus
	axis(2,tck=1,lty="18")
	# if(savPlt!="") dev.off()															# Close the window to save the plot
	if(savPlt!="") dev.print(device = png, units="in",width=12, height=8, res=600, file=FName(paste(savPlt, ".png",sep=""),FilePath))

	return (a7)
}

###########################################################################################################
###########################################################################################################
###########################################################################################################

## boxclass
##############
## Draws a boxplot of the YVar for a discrete factor variable XVar. 
## NAs are not allowed.
#############################################################################
boxclass<- function(XVar, YVar, Weight=F, MyMain="", savPlt=NULL, DispVal=F, confint=T, YLIMMan=F, SignLim=5, OtherLbl="Other", TitClaVis=T, DigTab=1, ...)
# Modified 20110405/PHS	New lty="18" definition for axis (see help on par)
{
##
## 1. Modifies the classes of XVar according to the significance limit SignLim. If the
##    population of a class is smaller than SignLim, the class is renamed OtherLbl.
##

	Xv<-c(as.character(XVar))
	nokt<-as.character(c())
	NewLvl<-as.character(c())
	sa<-rev(sort(tapply(Xv, Xv, length)))
## Limit the number of classes to 100
	if(length(sa)>100)
	{
		if(sa[101]+1>SignLim)
## Modify limits only if 101st class is more populated than the significance limit
		{
			SignLimOld <- SignLim
			SignLim <- sa[101]+1
			cat(paste("Too many classes, increased the significance limit from ",SignLimOld," to ",SignLim, " in order to reduce the number\n",sep=""))
		}
	}

## Reset original order to sa
	sa<-tapply(Xv, Xv, length)
	for(i in 1:length(sa))
	{
		if(sa[i]<SignLim)
		{
			nokt<-c(nokt,names(sa[i]))
			NewLvl<-c(NewLvl, OtherLbl)
		}
		else
		{
			NewLvl<-c(NewLvl, names(sa)[i])
		}
	}
	levels(XVar)<-unique(NewLvl)

	Xv<-as.character(XVar[order(as.character(XVar))])
	Yv<-as.double(YVar[order(as.character(XVar))])
	if (class(Weight)!="logical") {Wgt<-as.double(Weight[order(as.character(XVar))])} else {Wgt<-rep(1,length(YVar))}

	if(DispVal!=F) 
	{
		cat("\n", savPlt, "\n",sep="")
		cat("\n", MyMain, "\n\n",sep="")
		if(length(nokt)>0)
		{
			print(sa)
			if(length(nokt)==1) cat("\nFollowing class is not significant (less than",SignLim,"observations): ", sep="") else cat("\nFollowing ",length(nokt)," classes are not significant (less than ",SignLim," observations):\n", sep="") 
			cat(nokt, sep=", ")
			cat("\n")
			if(length(nokt)==1) cat("Not sinigificant class was renamed '", OtherLbl, "'\n\n", sep="") else cat("Not sinigificant classes were renamed '", OtherLbl, "'\n\n", sep="") 
		}
	}

##
## 2.a) Eliminates the class OtherLbl if the number of observations is smaller than SignLim
##

	nokt<-c()
	sa<-summary(factor(Xv))
	for(i in 1:length(sa)){if(sa[i]<SignLim) nokt<-c(nokt,names(sa[i]))}
	recs<-rep(1,length(Yv))
	if(length(nokt)>0) for(i in 1:length(nokt))
	{
		recs[Xv==nokt[i]]<-rep(0,length(recs[Xv==nokt[i]]))
		if(DispVal!=F) 
		{
			if(length(nokt)>0)
			{
				cat("\n")
				print(sa)
				cat("\nClass '", OtherLbl, "' is not significant (less than ",SignLim," observations)\n\n", sep="")
			}
		}
	}
	if(length(factor(Xv[recs==1]))==0) 
	{
		cat("No data after filtering for boplot ", MyMain, " \n", sep="")
		return()
	}
	ScatData<-split(Yv[recs==1], factor(Xv[recs==1]))
	ScatW<-split(Wgt[recs==1], factor(Xv[recs==1]))

##
## 3. Produce the boxplot
##

	if (length(YLIMMan)>1) {YLIM<-YLIMMan} else {YLIM<-HiLoScale(ScatData)}
## OLD ## # par(mfrow=c(1,1),mar=c(5,4,4,2)+.1)


	# if(is.null(savPlt)) x11() else
		# win.metafile(FName(paste(savPlt, ".wmf",sep=""),FilePath), width = 9, height = 9, restoreConsole = TRUE)
	#par(mfrow=c(1,1),mar=c(5,1.2,4,0.7)+.1)
	x11()
	par(mfrow=c(1,1),lab=c(10,10,1))

	a7 <- bowplot(ScatData,ScatW,ylim=YLIM,outline=F,medcol=IAZIColor("IAZIBlue1"),whiskcol=IAZIColor("IAZIBlue1"),whisklty=1,boxcol=IAZIColor("IAZIBlue1"),staplecol=IAZIColor("IAZIBlue1"),confint=confint,Export=savPlt,main=MyMain,DispVal=DispVal,TitClaVis=TitClaVis,DigTab=DigTab, ...)
	# axis(2,tck=1,lty=2) old for S-Plus
	axis(2,tck=1,col=IAZIColor("IAZIBlue1"),lty="dotted")
	# if(!is.null(savPlt)) dev.off()															# Close the window to save the plot
	if(savPlt!="") dev.print(device = png, units="in",width=12, height=8, res=600, file=FName(paste(savPlt, ".png",sep=""),FilePath))

	return (a7)
}

###########################################################################################################
###########################################################################################################
###########################################################################################################

## box2class
##############
## Draws a double boxplot of the first with IP, the second with Ref 
## NAs are not allowed.
#############################################################################
box2class<- function(XVarIP, YVarIP, WeightIP=F, LegendIP="IP", XVarRef, YVarRef, WeightRef=F, LegendRef="Reference", MyMain="", savPlt="", YLIMMan=F, DispVal=F, confint=T, SignLim=5, OtherLbl="Other", TitClaVis=T, DigTab=1, ...)
{
##
## 1.a) Modifies the classes of XVarIP according to the significance limit SignLim. If the
##      population of a class is smaller than SignLim, the class is renamed OtherLbl.
##

XvIP<-c(as.character(XVarIP))
XVarIP<-factor(XvIP)
noktIP<-c()
NewLvl<-c()
saIP<-summary(XVarIP)
for(i in 1:length(saIP))
{
  if(saIP[i]<SignLim)
  {
    noktIP<-c(noktIP,names(saIP[i]))
    NewLvl<-c(NewLvl, OtherLbl)
  }
  else
  {
    NewLvl<-c(NewLvl, levels(XVarIP)[i])
  }
}
levels(XVarIP)<-unique(NewLvl)

XvIP<-as.character(XVarIP[order(as.character(XVarIP))])
YvIP<-as.double(YVarIP[order(as.character(XVarIP))])
if (class(WeightIP)!="logical") {WgtIP<-as.double(WeightIP[order(as.character(XVarIP))])} else {WgtIP<-rep(1,length(YVarIP))}

if(DispVal!=F) 
{
	cat("\n", savPlt, "\n",sep="")
	cat("\n", MyMain, "\n\n",sep="")
	if(length(noktIP)>0)
	{
		cat(LegendIP, "\n")
		print(saIP)
		if(length(noktIP)==1) cat("\nFollowing class is not significant for ", LegendIP," (less than",SignLim,"observations): ", sep="") else cat("\nFollowing ",length(noktIP)," classes are not significant for ", LegendIP," (less than ",SignLim," observations):\n", sep="") 
		cat(noktIP, "\n")
		if(length(noktIP)==1) cat("Not sinigificant class was renamed '", OtherLbl, "'\n\n", sep="") else cat("Not sinigificant classes were renamed '", OtherLbl, "'\n\n", sep="") 
	}
}

##
## 2.a) Eliminates the class OtherLbl if the number of observations is smaller than SignLim
##

noktIP<-c()
saIP<-summary(factor(XvIP))
for(i in 1:length(saIP)){if(saIP[i]<SignLim) noktIP<-c(noktIP,names(saIP[i]))}
recsIP<-rep(1,length(YvIP))
if(length(noktIP)>0) for(i in 1:length(noktIP))
{
  recsIP[XvIP==noktIP[i]]<-rep(0,length(recsIP[XvIP==noktIP[i]]))
  if(DispVal!=F) 
  {
    if(length(noktIP)>0)
    {
	cat("\n")
	print(saIP)
	cat("\nClass '", OtherLbl, "' is not significant (less than ",SignLim," observations)\n\n", sep="")
    }
  }
}

if(length(factor(XvIP[recsIP==1]))==0) 
{
	cat("No data after filtering for boplot ", MyMain, " \n", sep="")
	return()
}
ScatIP<-split(YvIP[recsIP==1], factor(XvIP[recsIP==1]))
ScatWIP<-split(WgtIP[recsIP==1], factor(XvIP[recsIP==1]))

##
## 1.b) Modifies the classes of XVarRef according to the significance limit . If the 
##      population of a class is smaller than SignLim, the class is renamed OtherLbl.
##

levIP<-as.character(levels(factor(XvIP[recsIP==1])))
XvRef<-c(as.character(XVarRef))
XVarRef<-factor(XvRef)
noktRef<-c()
NewLvl<-c()
saRef<-summary(XVarRef)
for(i in 1:length(saRef))
{
  if(max(regexpr(names(saRef[i]), levIP))>0)
  {
    NewLvl<-c(NewLvl, levels(XVarRef)[i])
  }
  else
  {
    noktRef<-c(noktRef,names(saRef[i]))
    NewLvl<-c(NewLvl, OtherLbl)
  }
}
levels(XVarRef)<-unique(NewLvl)
if(DispVal!=F) 
{
	if(length(noktRef)>0)
	{
		cat(LegendRef, "\n")
		print(saRef)
		if(length(noktRef)==1) cat("\nFollowing class is not present for ", LegendIP," (less than",SignLim,"observations): ", sep="") else cat("\nFollowing ",length(noktRef)," classes are not present for ", LegendIP," (less than ",SignLim," observations):\n", sep="") 
		cat(noktRef, "\n")
		if(length(noktRef)==1) cat("Absent class was renamed '", OtherLbl, "'\n\n", sep="") else cat("Absent classes were renamed '", OtherLbl, "'\n\n", sep="") 
	}
}

XvRef<-c(as.character(XVarRef))
XVarRef<-factor(XvRef)
noktRef<-c()
NewLvl<-c()
saRef<-summary(XVarRef)
for(i in 1:length(saRef))
{
  if(saRef[i]<SignLim)
  {
    noktRef<-c(noktRef,names(saRef[i]))
    NewLvl<-c(NewLvl, OtherLbl)
  }
  else
  {
    NewLvl<-c(NewLvl, levels(XVarRef)[i])
  }
}
levels(XVarRef)<-unique(NewLvl)

XvRef<-as.character(XVarRef[order(as.character(XVarRef))])
YvRef<-as.double(YVarRef[order(as.character(XVarRef))])
if (class(WeightRef)!="logical") {WgtRef<-as.double(WeightRef[order(as.character(XVarRef))])} else {WgtRef<-rep(1,length(YVarRef))}

if(DispVal!=F) 
{
	if(length(noktRef)>0)
	{
		cat(LegendRef, "\n")
		print(saRef)
		if(length(noktRef)==1) cat("\nFollowing class is not significant for ", LegendRef," (less than",SignLim,"observations): ", sep="") else cat("\nFollowing ",length(noktRef)," classes are not significant for ", LegendRef," (less than ",SignLim," observations):\n", sep="") 
		cat(noktRef, "\n")
		if(length(noktRef)==1) cat("Not sinigificant class was renamed '", OtherLbl, "'\n\n", sep="") else cat("Not sinigificant classes were renamed '", OtherLbl, "'\n\n", sep="") 
	}
}

##
## 2.b) Eliminates the class OtherLbl if the number of observations is smaller than SignLim
##

noktRef<-c()
saRef<-summary(factor(XvRef))
for(i in 1:length(saRef)){if(saRef[i]<SignLim) noktRef<-c(noktRef,names(saRef[i]))}
recsRef<-rep(1,length(YvRef))
if(length(noktRef)>0) for(i in 1:length(noktRef))
{
  recsRef[XvRef==noktRef[i]]<-rep(0,length(recsRef[XvRef==noktRef[i]]))
  if(DispVal!=F) 
  {
    if(length(noktRef)>0)
    {
	cat("\n")
	print(saRef)
	cat("\nClass '", OtherLbl, "' is not significant (less than ",SignLim," observations)\n\n", sep="")
    }
  }
}
if(length(factor(XvRef[recsRef==1]))==0) 
{
	cat("No data after filtering for boplot ", MyMain, " \n", sep="")
	return()
}

ScatRef<-split(YvRef[recsRef==1], factor(XvRef[recsRef==1]))
ScatWRef<-split(WgtRef[recsRef==1], factor(XvRef[recsRef==1]))

##
## 3. Data is now ready for boxplot analysis
##

if (length(YLIMMan)>1) {YLIM<-YLIMMan}
else {
	YLIMIP<-HiLoScale(ScatIP)
	YLIMRef<-HiLoScale(ScatRef)
	YLIM<-c(min(YLIMIP,YLIMRef),max(YLIMIP,YLIMRef))
}

##
## 4.a) Produce the boxplot for IP
##

if(DispVal!=F) 
{
  	cat(LegendIP, "\n")
}

# if(savPlt!="") win.metafile(FName(paste(savPlt, ".wmf",sep=""),FilePath), restoreConsole = TRUE) else x11()	
x11()
## OLD ## par(mfrow=c(1,2),mar=c(5,4,6,2)+.1)
par(mfrow=c(1,2),mar=c(5,1.2,6,0.7)+.1)
#	par(mfrow=c(1,2))

if(regexpr("\\.", savPlt)>0) 
  { Export<-paste(substring(savPlt, 1, regexpr("\\.", savPlt)-1),"IP.",substring(savPlt, regexpr("\\.", savPlt)+1),sep="") }
else 
  { Export<-paste(savPlt, "IP",sep="") }
a7 <- bowplot(ScatIP,ScatWIP,ylim=YLIM,outline=F,medcol=1,whisklty=1,confint=confint,Export=Export,DispVal=DispVal,TitClaVis=TitClaVis,DigTab=DigTab,...)

	# axis(2,tck=1,lty=2) old for S-Plus
	axis(2,tck=1,lty="18")
if (TitClaVis!=F) {mtext(line=2, LegendIP, side=1,cex=1.3)}
else {mtext(line=0, LegendIP, side=1,cex=1)}  

##
## 4.b) Produce the boxplot for Ref and put the title 
##

if(DispVal) 
{
	cat("\n\n")
	cat(LegendRef, "\n")
}
if(regexpr("\\.", savPlt)>0) 
  { Export<-paste(substring(savPlt, 1, regexpr("\\.", savPlt)-1),"Ref.",substring(savPlt, regexpr("\\.", savPlt)+1),sep="") } 
else  
  { Export<-paste(savPlt, "Ref",sep="") }
 
bowplot(ScatRef,ScatWRef,ylim=YLIM,outline=F,medcol=1,whisklty=1,confint=confint,Export=Export,DispVal=DispVal,TitClaVis=TitClaVis,yaxt='n',DigTab=DigTab,...)
	# axis(2,tck=1,lty=2) old for S-Plus
	axis(2,tck=1,lty="18")
if (TitClaVis!=F) 
{
  mtext(MyMain,outer=T,line=-3,side=3,cex=1.6)
  mtext(line=2, LegendRef, side=1,cex=1.3)
}
else {mtext(line=0, LegendRef, side=1,cex=1)}  

##
## 5. save the plot
##

# if(savPlt!="") dev.off()															# Close the window to save the plot
if(savPlt!="") dev.print(device = png, units="in",width=12, height=8, res=600, file=FName(paste(savPlt, ".png",sep=""),FilePath))

return (a7)
}

###########################################################################################################
###########################################################################################################
###########################################################################################################

## box3class
##############
## Draws a boxplot with the Reference statistics and with points in the various classes for every property.
## A maximum of five properties per graph is allowed. 
## If a class for the units is not present in the benchmark, the class is set to "other".
## If the class "other" is not present for the benchmark, the points in "other" are not drawn. 
## 	(Alternative strategy: create the class in benchmark and leave it without box)
## NAs are not allowed.
#############################################################################
box3class<- function(XVarIP, YVarIP, IDIP, WeightIP=F, LegendIP="IP", StatRef, LegendRef="Reference", MyMain="", savPlt="", YLIMMan=F, DispVal=F, confint=T, SignLim=5, OtherLbl="Other", TitClaVis=T, DigTab=1, ...)
{
	start<-REPMDate()

	if(length(levels(factor(IDIP)))>5) return()
##
## 1. Loads the data from the boxplot statistics StatRef
##
	mstat<-t(StatRef[,2:6])
	conf<-t(StatRef[,8:9])
	bxp.X <- boxplot(StatRef[,2:6], plot=F)
	bxp.X$stats<-mstat
	bxp.X$n<-t(StatRef[,7])
	bxp.X$conf<-conf
	bxp.X$names<-t(StatRef[,1])
	maxRef<-t(StatRef[,10])
	minRef<-t(StatRef[,11])
	meanRef<-t(StatRef[,12])
	StdRef<-t(StatRef[,13])
	wmeanRef<-t(StatRef[,14])

	dimstat<-dim(mstat)
	nclassstat<-dimstat[2]
	ScatData<-mstat[,1]
	for(i in 2:nclassstat) { ScatData<-c(ScatData,mstat[,i]) }

##
## 2.1 Modifies the classes of XVarIP according to the presence in reference. If not present, the class is renamed OtherLbl.
##

	XvIP<-c(as.character(XVarIP))
	XVarIP<-factor(XvIP)
	noktIP<-c()
	NewLvl<-c()
	saIP<-summary(XVarIP)
	for(i in 1:length(saIP))
	{
		if(length(grep(names(saIP[i]),bxp.X$names))==0) 
		{
			noktIP<-c(noktIP,names(saIP[i]))
			NewLvl<-c(NewLvl, OtherLbl)
		}
		else
		{
			NewLvl<-c(NewLvl, names(saIP[i]))
		}
	}
	levels(XVarIP)<-unique(NewLvl)

	XvIP<-as.character(XVarIP[order(as.character(XVarIP))])
	YvIP<-as.double(YVarIP[order(as.character(XVarIP))])
	IvIP<-as.character(IDIP[order(as.character(XVarIP))])
	if (class(WeightIP)!="logical") {WgtIP<-as.double(WeightIP[order(as.character(XVarIP))])} else {WgtIP<-rep(1,length(YVarIP))}

	if(DispVal!=F) 
	{
		cat("\n", savPlt, "\n",sep="")
		cat(MyMain, "\n\n",sep="")
		if(length(noktIP)>0)
		{
			cat(LegendIP, "\n")
			print(saIP)
			if(length(noktIP)==1) cat("\nFollowing class of ", LegendIP,"  is not present in reference: ", sep="") else cat("\nFollowing ",length(noktIP)," classes of ", LegendIP," are not present in reference :\n", sep="") 
			cat(noktIP, "\n")
			if(length(noktIP)==1) cat("Not present class was renamed '", OtherLbl, "'\n\n", sep="") else cat("Not present classes were renamed '", OtherLbl, "'\n\n", sep="") 
		}
	}

##
## 2.2 Eliminates the class OtherLbl if not present in reference
##

	noktIP<-c()
	saIP<-summary(factor(XvIP))
	for(i in 1:length(saIP)){if(length(grep(names(saIP[i]),bxp.X$names))==0) noktIP<-c(noktIP,names(saIP[i]))}
	recsIP<-rep(1,length(YvIP))
	if(length(noktIP)>0) for(i in 1:length(noktIP))
	{
		recsIP[XvIP==noktIP[i]]<-rep(0,length(recsIP[XvIP==noktIP[i]]))
		if(DispVal!=F) 
		{
			if(length(noktIP)>0)
			{
				cat("\n")
				print(saIP)
				cat("\nClass '", OtherLbl, "' is not present in reference\n\n", sep="")
			}
		}
	}
	if(length(factor(XvIP[recsIP==1]))==0) 
	{
		cat("No data after filtering for boplot ", MyMain, " \n", sep="")
		return()
	}
	YVarIP<-YvIP[recsIP==1]
	XVarIP<-factor(XvIP[recsIP==1])
	levelIP<-levels(XVarIP)
	IDIP<-IvIP[recsIP==1]
	WVarIP<-WgtIP[recsIP==1]
	ScatWIP<-split(WVarIP, IDIP)
	ScatYIP<-split(YVarIP, IDIP)
	ScatXIP<-split(XVarIP, IDIP)

	ClassRefPresent<-c()
	for(i in 1:length(bxp.X$names)) { ClassRefPresent<-c(ClassRefPresent,(length(grep(bxp.X$names[i],levelIP))!=0)) }

	bxp.X$stats<-matrix(bxp.X$stats[1:5,ClassRefPresent],nrow=5)
	bxp.X$n<-bxp.X$n[ClassRefPresent]
	bxp.X$conf<-matrix(bxp.X$conf[1:2,ClassRefPresent],nrow=2)
	bxp.X$names<-bxp.X$names[ClassRefPresent]
	maxRef<-maxRef[ClassRefPresent]
	minRef<-minRef[ClassRefPresent]
	meanRef<-meanRef[ClassRefPresent]
	StdRef<-StdRef[ClassRefPresent]
	wmeanRef<-wmeanRef[ClassRefPresent]

	ClassRefPresent<-c()
	for(i in 1:length(bxp.X$names)) { ClassRefPresent<-c(ClassRefPresent,i) }
	dimnames(bxp.X$stats)[2]<-list(ClassRefPresent)
	dimnames(bxp.X$stats)[1]<-dimnames(mstat)[1]
	dimnames(bxp.X$conf)[2]<-list(ClassRefPresent)
	dimnames(bxp.X$conf)[1]<-dimnames(conf)[1]


##
## 3. Data is now ready for boxplot analysis
##

if (length(YLIMMan)>1) {YLIM<-YLIMMan} else {
	YLIMIP<-HiLoScale(ScatYIP)
	YLIMRef<-HiLoScale(ScatData)
	YLIM<-c(min(YLIMIP,YLIMRef),max(YLIMIP,YLIMRef))
}
YLIM2<-YLIM
YLIM2[2]<-YLIM[2]*1.2

##
## 4.1) Produce the boxplot for Ref and put the title 
##

	if(regexpr("\\.", savPlt)>0) { Export<-paste(substring(savPlt, 1, regexpr("\\.", savPlt)-1),"Ref.",substring(savPlt, regexpr("\\.", savPlt)+1),sep="") } 
	else { Export<-paste(savPlt, "Ref",sep="") }

	a7<-matrix(nrow=16,ncol=length(bxp.X$names))
	a7[1,]<-rep(start,length(bxp.X$names))
	a7[2,]<-rep(Export,length(bxp.X$names))
	a7[3,]<-bxp.X$names
	a7[4,]<-as.integer(bxp.X$n)
	a7[5,]<-maxRef
	a7[6:10,]<-as.numeric(bxp.X$stats)
	a7[11,]<-minRef
	a7[12:13,]<-as.numeric(bxp.X$conf)
	a7[14,]<-meanRef
	a7[15,]<-StdRef
	a7[16,]<-wmeanRef
	dimnames(a7) <-list(c("time","plot","names","n","max","uplim","Q1","median","Q3","lowlim","min","upconf","lowconf","mean","stdev", "wtmean"),NULL)

	a8<-matrix(nrow=8,ncol=length(bxp.X$names))
	a8[1,]<-a7[3,]
	a8[2,]<-as.numeric(a7[16,])
	a8[3,]<-a7[4,]
	a8[4,]<-a7[6,]
	a8[5,]<-a7[7,]
	a8[6,]<-a7[8,]
	a8[7,]<-a7[9,]
	a8[8,]<-a7[10,]
	dimnames(a8) <-list(c("names", "wtmean","n","uplim","Q1","median","Q3","lowlim"),NULL)

	if (DispVal!=F) 
	{
		a6<-matrix(nrow=8,ncol=length(bxp.X$names))
		a6[1,]<-as.integer(bxp.X$n)
		a6[2,]<-as.numeric(bxp.X$stats[3,])
		a6[3:4,]<-as.numeric(bxp.X$conf)
		a6[5,]<-as.numeric(a7[16,])
		a6[6,]<-as.numeric(a7[15,])
	 	a6[7,]<-as.numeric(a7[11,])
		a6[8,]<-as.numeric(a7[5,])
		dimnames(a6) <-list(c("n","median","upconf","lowconf","wtmean","stdev","min","max"),bxp.X$names)
		print(t(a6),digits=5)
	}
	if(Export!="")
	{
		ASCIIFile<-FName(paste(Export,".txt",sep=""),FilePath)
		DBExport(as.data.frame(t(a7)), Export)
		dh <- openData(ASCIIFile, type="ASCII", colNames=T, rowNames=F, openType="write") 
		writeNextDataRows(dh, t(a7))
		closeData(dh)
		ASCIIFile<-FName(paste(Export,".bxp.txt",sep=""),FilePath)
		con <- file(ASCIIFile, "") 
		cat(format(a8[1,]),	  			                                            "\n", sep="\t", file=con)
		cat(format(round(as.numeric(a8[2,]),DigTab),nsmall=DigTab,scientific= c(-4, 12),digits=15), "\n", sep="\t", file=con,append=T)
		cat(a8[3,],           					   	                            "\n", sep="\t", file=con,append=T)
		cat(format(round(as.numeric(a8[4,]),DigTab),nsmall=DigTab,scientific= c(-4, 12),digits=15), "\n", sep="\t", file=con,append=T)
		cat(format(round(as.numeric(a8[5,]),DigTab),nsmall=DigTab,scientific= c(-4, 12),digits=15), "\n", sep="\t", file=con,append=T)
		cat(format(round(as.numeric(a8[6,]),DigTab),nsmall=DigTab,scientific= c(-4, 12),digits=15), "\n", sep="\t", file=con,append=T)
		cat(format(round(as.numeric(a8[7,]),DigTab),nsmall=DigTab,scientific= c(-4, 12),digits=15), "\n", sep="\t", file=con,append=T)
		cat(format(round(as.numeric(a8[8,]),DigTab),nsmall=DigTab,scientific= c(-4, 12),digits=15), "\n", sep="\t", file=con,append=T)
		close(con, type="rw")
	}

	# if(savPlt!="") win.metafile(FName(paste(savPlt, ".wmf",sep=""),FilePath), restoreConsole = TRUE) else x11()	
	x11()
	# par(mfrow=c(1,1),mar=c(5,4,4,2)+.1)
	par(mfrow=c(1,1),lab=c(10,10,1))
	if (TitClaVis!=F) 
	{
		cx<-PHS.bxp(bxp.X, confcol=13,ylim=YLIM2,outline=F,medcol=1,whisklty=1,confint=confint)
		mtext(MyMain,outer=T,line=-3,side=3,cex=1.6)
	}
	else
	{
		bxp.X$names<-rep("",length(bxp.X$names))
		cx<-PHS.bxp(bxp.X, confcol=13,ylim=YLIM2,outline=F,medcol=1,whisklty=1,confint=confint)
	}  
		# axis(2,tck=1,lty=2) old for S-Plus
	axis(2,tck=1,lty="18")

## 
## 4.2) Produce the matrix for the property points
##

	ScatWIP<-split(WVarIP, IDIP)
	ScatYIP<-split(YVarIP, IDIP)
	ScatXIP<-split(XVarIP, IDIP)
	nimmo<-length(levels(factor(IDIP)))
	nclassimmo<-length(levelIP)
	points<-matrix(ncol=nclassimmo, nrow=nimmo)
	matrows<-levels(factor(IDIP))
	dimnames(points)[[2]] <- as.list(levelIP)
	dimnames(points)[[1]] <- matrows

	for(i in 1:nimmo)
	{
		Yval<-unlist(ScatYIP[i])
		Xval<-unlist(ScatXIP[i])
		for(j in 1:length(Xval)) { points[i,Xval[j]]<-Yval[j] }
	}

##
## 4.3) Produce a sorted matrix for value display
##
 
	points2<-points

	for(j in 1:nclassimmo)
	{
		count<-0
		maxi<-max(points[,j],na.rm=T)
		for(i in 1:nimmo)
		{
			if(is.na(points[i,j]))
			{     
				count<-count+1
				points2[i,j]<-maxi*(1+0.01*count)
			}


		}

	}

	ordrematrix<-rep(1,(nclassimmo*nimmo))
	ordrematrix<-matrix(ordrematrix,ncol=nclassimmo)

	for(k in 1:(nclassimmo))
	{
		num<-c(1:nimmo)
		matrpoints<-matrix(c(as.numeric(points[,k]),c(num)),ncol=nimmo)
		matrpoints<-rbind(as.numeric(points2[,k]),num)

		ordre<-sort(matrpoints[1,])
		for(j in 1:nimmo)
		{
			for(i in 1:nimmo)
			{
				if(!is.na(points2[j,k]))
				{
					if(ordre[i]==(points2[j,k])) { ordrematrix[j,k]=i }
				}
			}
		}
	}

##
## 4.4) Print points, labels alternatively L/R and the legend
##

	for(i in 1:nimmo) { points(cx,points[i,],pch=i,col=3,cex=min(0.9,1.8*(5/nclassimmo))) }

	for(j in 1:nclassimmo)
	{
		for(i in 1:nimmo)
		{ text((cx[j]+(0.66*((-1)^ordrematrix[i,j])*(cx[length(cx)])/8)),points[i,j],format(round(points[i,j],DigTab),nsmall=DigTab), col=3 ,cex=min(0.9,(0.9*(5/(nclassimmo))))) }
	}

	nameimmo<-levels(factor(IDIP))
	nameimmostr<-nameimmo[1]

	for(i in 2:nimmo) { nameimmostr<-c(nameimmostr,nameimmo[i]) }
	legend(0,(YLIM2[2]+0.1*(YLIM2[2]-YLIM[2])),nameimmostr,marks=1:nimmo,col=3,ncol=5,cex=0.9)

##
## 5. save the plot
##

	# if(savPlt!="") dev.off()															# Close the window to save the plot
	if(savPlt!="") dev.print(device = png, units="in",width=12, height=8, res=600, file=FName(paste(savPlt, ".png",sep=""),FilePath))
}

###########################################################################################################
###########################################################################################################
###########################################################################################################

HiLoScale <- function(X, probs=c(lowCut=0.1,hiCut=0.9), ...)
{
  bxp.X <- boxplot(X, plot=F)
  quant <- sapply(X, quantile, probs=probs, na.rm = T)
  c(min(quant), max(quant))
}

###########################################################################################################
###########################################################################################################
###########################################################################################################

HiFixLoScale <- function(X, LowLim, probs=c(lowCut=0.1,hiCut=0.9), ...)
{
  bxp.X <- boxplot(X, plot=F)
  quant <- sapply(X, quantile, probs=probs, na.rm = T)
  c(LowLim, max(quant))
}

###########################################################################################################
###########################################################################################################
###########################################################################################################

FixHiLoScale <- function(X, HiLim, probs=c(lowCut=0.1,hiCut=0.9), ...)
{
  bxp.X <- boxplot(X, plot=F)
  quant <- sapply(X, quantile, probs=probs, na.rm = T)
  c(min(quant), HiLim)
}

###########################################################################################################
###########################################################################################################
###########################################################################################################

hist.factorP<- function(X, DispVal=F, ...)
{
  HistRef<-hist.factor(X, plot=F)
  if (DispVal!=F) 
  {
	print(levels(X))
	print(HistRef$counts)
  }
  HistRef$counts<-100*HistRef$counts/length(X)
  invisible(barplot(HistRef$counts,names=levels(X), ...))
}

###########################################################################################################
###########################################################################################################
###########################################################################################################

hist.factor <- function(x, plot = TRUE, probability = FALSE, include.lowest = T, names = levels(x), ..., xlab = deparse(substitute(x)))
{
  if(!is.factor(x))
    x <- factor(x)
  x <- x[!is.na(x)]
  if(length(names) != length(levels(x)))
    stop("length(names) != length(levels(x))")
  counts <- tabulate(as.integer(unclass(x)), length(levels(x)))
  if(probability) {
    counts <- counts/sum(counts)
  }
  if(plot) {
    invisible(barplot(counts, names = names, ..., xlab = xlab))
  }
  else list(breaks = seq(0.5, length = length(levels(x)) + 1), counts = counts)
}


###########################################################################################################
###########################################################################################################
###########################################################################################################


# FName <- function (FileName="", FilePath=FilePath)
# { 
#   paste(FilePath, FileName, sep="")
# }

FName <- function (FileName = "", FPath = FilePath) 
{
  paste(FPath, FileName, sep = "")
}

###########################################################################################################
###########################################################################################################
###########################################################################################################
IAZIColor<-function(colr="")
{
  if (colr=="IAZIBlue1")
  {IAZIBlue1 <- rgb(0, 86, 132, 255, names = "IAZIBlue1", maxColorValue = 255)}
  else if (colr=="IAZIBlue6")
  {IAZIBlue6 <- rgb(230, 238, 250, 255, names = "IAZIBlue6", maxColorValue = 255)}
  else if (colr=="IAZIGreen1")
  {IAZIGreen1 <- rgb(185, 198, 0, 255, names = "IAZIGreen2", maxColorValue = 255)}
  else if (colr=="IAZIGreen2")
  {IAZIGreen2 <- rgb(221, 226, 151, 255, names = "IAZIGreen2", maxColorValue = 255)}
}

#                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                