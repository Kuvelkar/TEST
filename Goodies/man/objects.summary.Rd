\name{objects.summary}
\alias{objects.summary}
\title{Get the summary of objects stored in the memory}
\description{This function is the SPLUS objects.summary adapted version for R.}
\usage{objects.summary(names. = NULL, what = c("data.class","storage.mode","extent",
                "object.size"), where = 1, frame = NULL, pattern = NULL, 
                regexpr.pattern = NULL, data.class. = NULL, storage.mode. = NULL,
                mode. = "any", all.classes = F, order. = NULL, reverse = F,
                immediate = T)}
\arguments{
  \item{names.}{character vector naming objects to summarize.}
  \item{what}{character vector specifying what information to return. This can be any subset of c("data.class", "storage.mode", "extent", "object.size", "dataset.date"), in any order. The default is to return all five types of information, in the order shown. what is subject to partial matching, that is, only enough initial letters of each string element are needed to guarantee unique recognition.}
  \item{where}{a number or character string identifying a database in the search list, or 0 for the session database. The default is the working database.}
  \item{frame}{a frame number. If frame is given, where is ignored.}
  \item{pattern}{character string specifying a pattern as in grep. Selects objects whose names match the pattern. This argument now deprecated in favor of the regexpr.pattern argument.}
  \item{regexpr.pattern}{character string specifying a pattern as in regexpr. Selects objects whose names match the pattern.}
  \item{data.class.}{character vector of data classes. Selects objects belonging to one of the named data classes. If all.classes=T, each element of an object's class attribute is considered, not just the first.}
  \item{storage.mode.}{character vector of storage modes. Selects objects with one of the named storage modes.}
  \item{mode.}{character vector of modes. Selects objects with one of the named modes.}
  \item{all.classes}{logical flag specifying whether the entire class vector of an object or just the first element should be used, both in selection based on argument data.class and in the returned summary. This has bearing only on objects with a class attribute. By default only the first class element is used. }
  \item{order.}{character vector controlling the sort order of the object entries (printed as rows) in the summary. For example, order="object.size" means sort the objects on the object.size component of the summary. The elements of order can be any subset of the elements of what except "extent"; in addition, "data.class" may not be requested in order if all.classes=T. order is subject to partial matching; thus order="obj" means order="object.size". If `length(order) > 1', the second and succeeding elements are used to break ties; see the order function. If order is omitted, the entries are sorted alphabetically by object name.}
  \item{reverse}{logical flag: if TRUE, the final sort order is reversed, but only if this order depends on something other than object names.}
  \item{immediate}{logical flag: if TRUE, objects in database where are not retained in the expression frame for the duration of the top-level expression (unless they were there already). This flag is TRUE by default to keep memory growth down, though FALSE could improve evaluation time if objects from where will be accessed again during the same top-level expression. See get for details.}
}
\details{}
\value{This function is used to list all the objects of the specified type in the memory while cleaning.}
\author{IAZI}
\examples{
objtodel<-objects.summary(what="data.class")
datanames<-dimnames(objtodel)[[1]]
selection<-(objtodel[,1]!="function")
}