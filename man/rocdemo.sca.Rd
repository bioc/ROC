\name{rocdemo.sca}
\alias{rocdemo.sca}
\alias{dxrule.sca}
\title{ function to build objects of class 'rocc'  }
\description{
rocdemo.sca -- demonstrate 'rocc' class construction
using a scalar marker and simple functional rule
}
\usage{
rocdemo.sca(truth, data, rule, 
       seqlen=min(length(truth),20), cutpts=seq(min(data), max(data), length = seqlen), 
       markerLabel="unnamed marker", caseLabel="unnamed diagnosis")
}
\arguments{
  \item{truth}{ true classification of objects}
  \item{data}{ quantitative markers used to classify}
  \item{rule}{ rule: a function with arguments}
  \item{seqlen}{ number of thresholds}
  \item{cutpts}{ values of thresholds}
  \item{markerLabel}{ textual label describing marker}
  \item{caseLabel}{ textual label describing classification}
}
\details{

dxrule.sca is function (x, thresh) ifelse(x > thresh, 1, 0)

}
\value{
an object of S4 class rocc
}
\references{  }
\author{Vince Carey (stvjc@channing.harvard.edu)  }
\note{  }


\seealso{  }

\examples{
set.seed(123)
R1 <- rocdemo.sca( rbinom(40,1,.3), rnorm(40), dxrule.sca,
   caseLabel="new case", markerLabel="demo Marker" )
plot(R1, line=TRUE, show.thresh=TRUE)
}
\keyword{ models }

