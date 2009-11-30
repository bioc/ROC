\name{rocdemo.sca}
\alias{rocdemo.sca}
\alias{dxrule.sca}
\title{ function to build objects of class 'rocc'  }
\description{
rocdemo.sca -- demonstrate 'rocc' class construction
using a scalar marker and simple functional rule
}
\usage{
rocdemo.sca(truth, data, rule=NULL, 
       cutpts=NA,
       markerLabel="unnamed marker", caseLabel="unnamed diagnosis")
}
\arguments{
  \item{truth}{ true classification of objects. Must take values 0 or 1.}
  \item{data}{ quantitative markers used to classify}
  \item{rule}{ rule: a function with arguments (x, thresh) returning 0
    or 1. If no rule is provided or the standard rule \code{dxrule.sca} is
passed,
    a faster C-based implementation is used to compute sensitivity and specificity.}
  \item{cutpts}{ values of thresholds}
  \item{markerLabel}{ textual label describing marker}
  \item{caseLabel}{ textual label describing classification}
}
\details{

  \code{dxrule.sca} is \code{function (x, thresh) ifelse(x > thresh, 1, 0)}

  The default value of argument cutpts is a point less than min(data), points
  separating the unique values of data and a point greater than max(data).

}
\value{
an object of S4 class \code{rocc}
}
\references{  }
\author{Vince Carey (stvjc@channing.harvard.edu)  }
\note{  }


\seealso{AUC}

\examples{
set.seed(123)
R1 <- rocdemo.sca( rbinom(40,1,.3), rnorm(40), caseLabel="new case", markerLabel="demo Marker" )
plot(R1, line=TRUE, show.thresh=TRUE)

truth <- c(0, 1, 0, 1, 1, 0, 1, 1)
data <- c(2, 3, 4, 4, 5, 6, 7, 8)
R2 <- rocdemo.sca(truth, data, dxrule.sca)
plot(R2, line=TRUE, show.thresh=TRUE)
R3 <- rocdemo.sca(truth, data, function(x, thresh) 1 - dxrule.sca(x, thresh))
if (AUC(R2) + AUC(R3) != 1) stop('Sum of AUCs should be 1.')
#
# more involved
#
set.seed(1234)
x = runif(1000)
w = runif(1000)
z = rbinom(1000, 1, plogis(-2.7+6.2*x + .3*w))
m1 = glm(z~x, fam=binomial)
demorule.glm.clo = function(model) function(w,thresh)
 ifelse(predict(model, newdata=list(x=w), type="response")>thresh, 1, 0)
demorule.glm = demorule.glm.clo(m1)
R4 = rocdemo.sca(z, x, demorule.glm )
plot(R4)
  
}
\keyword{ models }

