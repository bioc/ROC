\name{plot-methods}
\docType{methods}
\title{plot method for ROC curves}
\description{plot method for ROC curves}
\alias{plot,rocc-method}
\alias{lines,rocc-method}
\alias{points,rocc-method}
\section{Methods}{\describe{
\item{x = rocc}{ plots an ROC curve object, with
additional parameters available:
 {
 \describe{
  \item{show.thresh (logical):}{should marker threshold values be plotted?}
  \item{jit (logical):}{should plotted points be jittered?}
  \item{add (logical):}{increment to current plot?}
  \item{line (logical):}{plot points or lines?}
  \item{threshCex (numeric):}{if showing threshold values, set
character expansion in \code{text} call to this value}
  \item{threshYsh (numeric):}{if showing threshold values, add
this quantity to y coordinate of curve to
plot the threshold value (should be negative for printing below point)}
  \item{threshDig (numeric):}{if showing threshold values, use
this as the digits parameter to \code{round} to display the
threshold}
  \item{\dots}{extra parameters passed to base 
\code{plot}, \code{lines} or \code{points} as needed}
}}}}}
\examples{
set.seed(123)
R1 <- rocdemo.sca( rbinom(40,1,.3), rnorm(40), dxrule.sca,
   caseLabel="new case", markerLabel="demo Marker" )
plot(R1, line=TRUE, show.thresh=TRUE, lwd=2, threshDig=2)
R2 <- rocdemo.sca( rbinom(40,1,.3), rnorm(40), dxrule.sca,
   caseLabel="new case", markerLabel="demo Marker" )
plot(R2, line=TRUE, add=TRUE, col="green", lwd=2 )
R3 <- rocdemo.sca( rbinom(40,1,.4), rnorm(40), dxrule.sca,
   caseLabel="new case", markerLabel="demo Marker" )
points(R3, col="red", pch=19)
}
\keyword{methods}

