\name{trapezint}
\alias{trapezint}
\title{simple trapezoidal integration rule for AUC}
\description{simple trapezoidal integration rule for AUC}
\usage{
trapezint( x, y, a, b )
}
\arguments{
 \item{x}{abscissae}
 \item{y}{ordinates}
 \item{a}{lower boundary}
 \item{b}{upper boundary}
}
\details{ simple evaluation of trapezoidal area
beneath piecewise linear function joining values of
(x,y).}
\example{
x <- sort(runif(10))
y <- sin(x)
trapezint(x,y,0,1)
}
\keyword{models}

