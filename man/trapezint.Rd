\name{trapezint}
\alias{trapezint}
\title{trapezoidal rule for AUC  }
\description{trapezoidal rule for AUC
}
\usage{
trapezint(x, y, a, b)
}
\arguments{
  \item{x}{ x - abscissae}
  \item{y}{ y - ordinates}
  \item{a}{ a - lower limit of integration}
  \item{b}{ b - upper limit of integration}
}
\details{
uses approx
}
\value{
estimated AUC
}
\references{  }
\author{  }
\note{  }
\seealso{  }
\examples{
x <- sort(runif(30))
y <- sin(x)
print(trapezint(x,y,0,1))
}
\keyword{ models }

