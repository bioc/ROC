\name{rocc-class}
\docType{class}
\alias{rocc-class}
\alias{.initClasses}
\title{Class rocc, ROC curve representation }
\description{object representing ROC curve, typically created using rocdemo.sca }
\section{Creating Objects}{
\code{  new('rocc',}\cr
\code{    sens        = ...., # Object of class numeric}\cr
\code{    spec        = ...., # Object of class numeric}\cr
\code{    rule        = ...., # Object of class function}\cr
\code{    cuts        = ...., # Object of class numeric}\cr
\code{    markerLabel = ...., # Object of class character}\cr
\code{    caseLabel   = ...., # Object of class character}\cr
\code{  )}}
\section{Slots}{
  \describe{
    \item{\code{sens}:}{Object of class "numeric" sensitivity values }
    \item{\code{spec}:}{Object of class "numeric" specificity values }
    \item{\code{rule}:}{Object of class "function" rule to classify objects }
    \item{\code{cuts}:}{Object of class "numeric" thresholds defining curve }
    \item{\code{markerLabel}:}{Object of class "character" name of measured marker }
    \item{\code{caseLabel}:}{Object of class "character" name of condition }
  }
}

\section{Methods}{
  \describe{
    \item{plot}{(rocc, missing): a plotting function with some additional parameters }
  }
}

\examples{
set.seed(123)
R1 <- rocdemo.sca( rbinom(40,1,.3), rnorm(40), dxrule.sca,
   caseLabel="new case", markerLabel="demo Marker" )
plot( R1, show.thresh=TRUE )
}

\keyword{methods}

