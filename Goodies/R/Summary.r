## Special 'summary'-functions
## ===========================
## Content:	objects.summary	Adapted version for R / 20100219 / PHS

## 20100219/PHS introduced objects.summary <- to be compatible with R
## 20100223/PHS removed obsolete S+ functions
## 20100226/PHS	removed "dataset.date" from what

## objects.summary(what = "storage.mode")
## names. = NULL; what = "storage.mode"; where = 1; frame = NULL; pattern = NULL; regexpr.pattern = NULL; 
## data.class. = NULL; storage.mode. = NULL; mode. = "any"; all.classes = F; order. = NULL; reverse = F;
## immediate = T

objects.summary <- function(names. = NULL, what = c("data.class", "storage.mode", "extent", "object.size"), 
	where = 1, frame = NULL, pattern = NULL, regexpr.pattern = NULL, data.class.
	 = NULL, storage.mode. = NULL, mode. = "any", all.classes = F, order. = NULL, reverse = F,
	immediate = T)
{
	all.fields <- c("data.class", "storage.mode", "extent", "object.size", "dataset.date")
	which.info <- pmatch(what, all.fields)
	if(any(is.na(which.info)))
		stop(paste("Elements of `what' must (partially) match", deparse(all.fields), 
			", with no duplicates."))
	what <- all.fields[which.info]
	do.class <- any(which.info == 1)
	do.smode <- any(which.info == 2)
	do.extent <- any(which.info == 3)
	do.size <- any(which.info == 4)
	do.date <- any(which.info == 5)
	if(is.null(order.)) {
		which.order <- NULL
	} else {
		no.order <- if(all.classes) c("data.class", "extent") else "extent"
		ok.order <- what[!match(what, no.order, nomatch = 0)]
		if(!length(ok.order)) {
			which.order <- NULL
			warning("No element of `what' can be used for sorting. Ignoring argument `order' and sorting on object names."
				)
		} else {
			which.order <- pmatch(order., ok.order)
			if(any(is.na(which.order)))
				stop(paste("Elements of `order' must (partially) match", deparse(
					ok.order), ", with no duplicates."))
			which.order <- match(ok.order[which.order], all.fields)
		}
	}
	explicit.names <- !is.null(names.)
	if(!explicit.names) {
		# call objects(); note if `frame' or `pattern' are (explicitly) NULL, want
		# to omit these from the call, as objects() uses missing(); so no match.call
		oargs <- if(!is.null(frame)) list(frame = frame) else list() # originally in S+: else list(where = where)
		# evaluate here, by the way
		if(!is.null(pattern)) oargs <- c(oargs, pattern = pattern)
		if(!is.null(regexpr.pattern))
			oargs <- c(oargs, regexpr.pattern = regexpr.pattern)
		names. <- do.call("objects", oargs)
		# restrict list to files containing legal S objects
		if(!is.null(names.) && length(names.) > 0) names. <- names.[sapply(names., exists)]
		if(is.null(names.) || (length(names.) == 0))
			return(NULL)
	}
	n.obj <- length(names.)
	if(is.null(frame) && is.character(where) && do.date) {
		# dataset.date() needs attached database and numeric where. Other restriction, 
		# that db is attached when explicit.names == F, has been checked already.
		# Except for these, things work with unattached db's too.
		cwhere <- where
		if(is.na(where <- database.position(cwhere)))
			stop(paste("Database", cwhere, "not on search list"))
	}
	if(do.class)
		cl <- I(vector(if(all.classes) "list" else "character", n.obj))
	if(do.smode)
		smode <- character(n.obj)
	if(do.extent)
		ext <- structure(vector("list", n.obj), class = "AsIs")
	if(do.size)
		osize <- integer(n.obj)
	if(do.date)
		ddate <- integer(n.obj)
	data.class2 <- if(all.classes) {
		function(x)
		if(is.null(clx <- oldClass(x))) data.class(x) else clx
	} else {
		function(x)
		data.class(x)[1]
	}
	get.date <- do.date && is.null(frame) && where != 0
	#
	# call dataset.date only if answer not := 0; letting non-dir db's
	# slip through, maybe dataset.date can (someday) return nonzero for them
	select.cl <- !explicit.names && !is.null(data.class.)
	select.sm <- !explicit.names && !is.null(storage.mode.)
	select.mode <- !explicit.names && !(length(mode.) == 1 && mode. == "any")
	selected <- logical(n.obj)
	j <- 0
	for(i in seq(along = names.)) {
		valid.data.file <- T
		x <- if(is.null(frame)) {
			if(exists(names.[i], where = where))
				get(names.[i])  # originally in S+: get(names.[i], where = where, immediate = immediate) 
			else valid.data.file <- F
		} else get(names.[i], frame = frame)
		if(!valid.data.file) {
			warning(paste(names.[i], "is not an Splus data file"))
			next
		}
		if(select.cl) {
			cl.i <- data.class2(x)
			if(!any(match(cl.i, data.class., nomatch = 0)))
				next
		}
		if(select.sm) {
			smode.i <- storage.mode(x)
			if(!any(match(smode.i, storage.mode., nomatch = 0)))
				next
		}
		if(select.mode) {
			if(!any(match(mode(x), mode., nomatch = 0)))
				next
		}
		# x will be part of summary
		j <- j + 1
		selected[i] <- T
		if(do.class)
			cl[[j]] <- if(select.cl) cl.i else data.class2(x)
		if(do.smode)
			smode[j] <- if(select.sm) smode.i else storage.mode(x)
		if(do.extent)
			ext[[j]] <- if(length(dx <- dim(x))) dx else length(x)
		if(do.size)
			osize[j] <- object.size(x)
		if(get.date)
			ddate[j] <- dataset.date(names.[i], where, date.format = 1)
	}
	# shorten to j <= n.obj
	eval(expression(length(cl) <- j, length(smode) <- j, length(ext) <- j, length(osize) <- j,
		length(ddate) <- j)[which.info])
	# length<- strips the AsIs class, until we get length<-.AsIs use following
	n.obj <- j
	names. <- names.[selected]
	vecs <- expression(cl, smode, ext, osize, ddate)
	if(n.obj && length(which.order)) {
		# need to reorder
		ord <- eval(as.call(c(as.name("order"), vecs[which.order])))
		if(reverse)
			ord <- rev(ord)
		names. <- names.[ord]
		eval(expression(cl <- cl[ord], smode <- smode[ord], ext <- ext[ord], osize <- osize[
			ord], ddate <- ddate[ord])[which.info])
	}
	# Now make a data frame
	eval(expression(cl <- I(cl), smode <- I(smode), ext <- I(ext), osize <- I(osize), ddate <-
		I(ddate))[which.info])
	cols <- vecs[which.info]
	names(cols) <- what
	the.list <- eval(as.call(c(as.name("list"), cols)))
	val <- as.data.frame(the.list, row.names = names.)
	oldClass(val) <- c("objects.summary", oldClass(val))
	val
}
