\name{plot-methods}
\docType{methods}
\title{plot method for ROC curves}
\description{plot method for ROC curves}
\alias{plot-methods}
\section{Methods}{\describe{
\item{x = rocc, y = missing}{ plots an ROC curve object, with
additional parameters available:
 {
 \describe{
  \item{show.thresh (logical):}{should marker threshold values be plotted?}
  \item{jit (logical):}{should plotted points be jittered?}
  \item{add (logical):}{increment to current plot?}
  \item{line (logical):}{plot points or lines?}
}}}}}
\examples{
set.seed(123)
R1 <- rocdemo.sca( rbinom(40,1,.3), rnorm(40), dxrule.sca,
   caseLabel="new case", markerLabel="demo Marker" )
plot(R1, line=TRUE, show.thresh=TRUE)
}
\keyword{methods}

