\name{AUC}
\alias{AUC}
\alias{AUCi}
\alias{pAUC}
\alias{pAUCi}
\alias{ROC}
\title{ functionals of ROC curve }
\description{various functionals of ROC curves
}
\usage{
AUC(rocobj)
pAUC(rocobj,t0)
}
\arguments{
  \item{rocobj}{ element of class rocc}
  \item{t0}{ FPR point at which TPR is evaluated 
          or limit in (0,1) to integrate to}
}
\details{
AUC employs trapezoidal rule.  AUCi uses integrate().
}
\value{
}
\references{  }
\author{  }
\note{  }


\seealso{  }

\examples{
set.seed(123)
R1 <- rocdemo.sca( rbinom(40,1,.3), rnorm(40), dxrule.sca,
   caseLabel="new case", markerLabel="demo Marker" )
print(AUC(R1))
print(pAUC(R1,.3))
print(pAUCi(R1,.3))
print(ROC(R1,.3))
}
\keyword{ models }

