\name{rocc-class}
\docType{class}
\alias{rocc-class}
\title{Class rocc, class for representing ROC curves }
\description{  very simple formal class for ROC curves}
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
    \item{\code{sens}:}{Object of class "numeric" vector of sensitivities
at marker levels defined in slot cuts }
    \item{\code{spec}:}{Object of class "numeric" vector of specificities }
    \item{\code{rule}:}{Object of class "function" rule used to 
decide class of instance }
    \item{\code{cuts}:}{Object of class "numeric" marker value thresholds used to
evaluate sensitivity and specificity }
    \item{\code{markerLabel}:}{Object of class "character" tag naming the marker used }
    \item{\code{caseLabel}:}{Object of class "character" tag naming the diagnosis }
  }
}

\section{Methods}{
No methods defined with class "rocc" in the signature.
}
\keyword{methods}

