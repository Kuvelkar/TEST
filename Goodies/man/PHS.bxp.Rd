\name{PHS.bxp}
\alias{PHS.bxp}
\title{Draw Box Plots from Summaries}
\description{Draw box plots based on the given summaries in z. It is usually called from within bowplot.}
\usage{PHS.bxp(z, notch = FALSE, width = NULL, varwidth = FALSE, outline = TRUE,
        notch.frac = 0.5, log = "", border = par("fg"), pars = NULL, 
        frame.plot = axes, horizontal = FALSE, add = FALSE, at = NULL, 
        show.names = NULL, medlwd = 5, confint = FALSE, confcol = 2,
        boxwex = 0.5, staplewex = 1, ...)}
\arguments{
  \item{z}{a list containing data summaries to be used in constructing the plots. These are usually the result of a call to boxplot, but can be generated in any fashion.}
  \item{notch}{logical if notch is TRUE, a notch is drawn in each side of the boxes. If the notches of two plots do not overlap then the medians are significantly different at the 5 percent level.}
  \item{width}{a vector giving the relative widths of the boxes making up the plot.}
  \item{varwidth}{logical if varwidth is TRUE, the boxes are drawn with widths proportional to the square-roots of the number of observations in the groups.}
  \item{outline}{logical if outline is not true, the outliers are not drawn.}
  \item{notch.frac}{numeric in (0,1). When notch = TRUE, the fraction of the box width that the notches should use.}
  \item{log}{character, indicating if any axis should be drawn in logarithmic scale.}
  \item{border}{character or numeric (vector), the color of the box borders. Is recycled for multiple boxes. Is used as default for the boxcol, medcol, whiskcol, staplecol, and outcol options (see below).}
  \item{pars}{graphical parameters.}
  \item{frame.plot}{logical, indicating if a ?frame? (box) should be drawn; defaults to TRUE, unless axes = FALSE is specified.}
  \item{horizontal}{logical indicating if the boxplots should be horizontal; default FALSE means vertical boxes.}
  \item{add}{logical, if true add boxplot to current plot.}
  \item{at}{numeric vector giving the locations where the boxplots should be drawn, particularly when add = TRUE; defaults to 1:n where n is the number of boxes.}
  \item{show.names}{set to TRUE or FALSE to override the defaults on whether an x-axis label is printed for each group.}
  \item{medlwd}{median line width. Setting this parameter implicitly sets the medline parameter to TRUE. The special value, NA, is used to indicate the current line width ( par("lwd")). The default is 5, but the "old" and "att" styles set the it to 5. }
  \item{confint}{confidence interval logical flag. If TRUE, use z$conf to display confidence intervals. How the confidence intervals are shown is determined by the confnotch, confcol, confangle and confdensity parameters.}
  \item{confcol}{confidence interval color. If supplied, confidence intervals will be filled with the indicated color. The default is 2, but the "old" and "att" styles set it to -1 (no filling). }
  \item{boxwex}{box width expansion. The width of the boxes, along with the width of the staples (whisker end caps) and outliers (if drawn as lines), are proportional to this parameter. The default is 0.5, but the "att" and "old" styles set this to 1.}
  \item{staplewex}{staple width expansion. Proportional to the box width. The default is 1, but the "old" style sets the default to 0.125.}
  \item{\dots}{other arguments.}
}
\details{}
\value{An invisible vector, actually identical to the at argument, with the coordinates ("x" if horizontal is false, "y" otherwise) of box centers, useful for adding to the plot.}
\author{IAZI}
\examples{}
