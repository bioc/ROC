\name{AUC}
\alias{AUC}
\alias{AUCi}
\alias{pAUC}
\alias{pAUCi}
\alias{ROC}
\title{ functionals of ROC curve }
\description{various functionals of ROC (Receiver Operating
  Characteristic) curves
}
\usage{
AUC(rocobj)
AUCi(rocobj)
pAUC(rocobj,t0)
pAUCi(rocobj,t0)
}
\arguments{
  \item{rocobj}{ element of class rocc}
  \item{t0}{ FPR point at which TPR is evaluated 
          or limit in (0,1) to integrate to}
}
\details{
AUC, pAUC, AUCi and pAUCi compute the Area Under the Curve.

AUC and pAUC employ the trapezoidal rule.  AUCi and pAUCi use
integrate().

AUC and AUCi compute the area under the curve from 0 to 1 on the x-axis
(i.e., the 1 - specificity axis).

pAUC and pAUCi compute the are under the curve from 0 to argument t0 on
the x-axis (i.e., the 1 - specificity axis).

Elements of class rocc can be created by rocdemo.sca() or
other constructors you might make using the code of rocdemo.sca()
as a template.
}
\value{
}
\references{
  Rosner, B., 2000, \emph{Fundamentals of Biostatistics, 5th Ed.},
  pp. 63--65

  Duda, R. O., Hart, P. E., Stork, D. G., 2001 \emph{Pattern
    Classification, 2nd Ed.}, p. 49
}
\author{Vince Carey (stvjc@channing.harvard.edu)  }
\note{  }


\seealso{rocdemo.sca}

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

