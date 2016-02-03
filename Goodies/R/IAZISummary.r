########################################################################################################
## Info            		: Summary function for exploratory analysis
## Created on   	    : 13.06.2013
## Created by   	    : PVN
## Call           		: source("V:\\435_Modelle\\7_Operation\\ChangeRequests\\Documentation\\PVN\\20130710_Exploratory\\IAZISummary.r")
##						  IAZISummary(x)
## Modified		   		: 
########################################################################################################
IAZISummary<-function (x, na.rm = TRUE)
{
	options(digits=2)
    NonNA <- function(x) { sum(!is.na(x)) }
	uni <- function(x) { length(unique(x)) }
	Miss  <- function(x) { abs(length(x)-NonNA(x)) }
	Quant1<-function(x,probs=.25,na.rm=TRUE)
	{
	xx<-quantile(x,probs,na.rm=na.rm)
	names(xx)<-NULL
	xx
	}
	Quant2<-function(x,probs=.75,na.rm=TRUE)
	{
	xy<-quantile(x,probs,na.rm=na.rm)
	names(xy)<-NULL
	xy
	}
	if (!na.rm) x <- na.omit(x)
    if (is.null(dim(x)[2])) {
    	stats 		 <- matrix(rep(NA, 11), ncol = 11)
		rownames(stats) <- colnames(x)
		stats[1, 1]  <- length(x)
		stats[1, 2]  <- uni(x)
		stats[1, 3]  <- Miss(x)
		stats[1, 4]  <- NonNA(x)
        stats[1, 7]  <- mean(x, na.rm = na.rm)
        stats[1, 11] <- sd(x, na.rm = na.rm)
        stats[1, 8]  <- median(x, na.rm = na.rm)
        stats[1, 5]  <- min(x, na.rm = na.rm)
        stats[1, 10] <- max(x, na.rm = na.rm)
        stats[1, 6]  <- Quant1(x)
		stats[1, 9]  <- Quant2(x)
	   }   else  {
        stats = matrix(rep(NA, ncol(x) * 11), ncol = 11)
        rownames(stats) <- colnames(x)
		stats[, 1]   <- apply(x, 2, length)
		stats[, 2]   <- apply(x, 2, uni)
		stats[, 3]   <- apply(x, 2, Miss)
		stats[, 4]   <- apply(x, 2, NonNA)
        stats[, 7]   <- apply(x, 2, mean, na.rm = na.rm)
        stats[, 11]  <- apply(x, 2, sd, na.rm = na.rm)
        stats[, 8]   <- apply(x, 2, median, na.rm = na.rm)
        stats[, 5]   <- apply(x, 2, min, na.rm = na.rm)
        stats[, 10]  <- apply(x, 2, max, na.rm = na.rm)
        stats[, 6]   <- apply(x, 2, Quant1)
		stats[, 9]   <- apply(x, 2, Quant2)
	}
	colnames(stats)<-c("n","Unique","Missing","NonNA","Min.","Q1","Mean","Median","Q3","Max.","sd")
	stats
}