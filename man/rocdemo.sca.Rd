\name{rocdemo.sca}
\alias{rocdemo.sca}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ compute ROC curve }
\description{
given a vector of known diagnoses, a vector of marker values and
a prediction rule, evaluate the ROC curve for the rule
}
\usage{
rocdemo.sca(truth, data, rule, seqlen=10, 
       cutpts=seq(min(data), max(data), length = seqlen),
       markerLabel="unnamed marker", caseLabel="unnamed diagnosis")
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{truth}{ binary classification of outcomes corresponding
to the data points }
  \item{data}{ data on the marker }
  \item{rule}{ classification rule that applies to data to
yield predictions }
  \item{seqlen}{ number of (t, ROC(t)) points sought }
  \item{cutpts}{ marker threshold values at which (t, ROC(t))
are desired }
  \item{markerLabel}{ character atom naming the marker used}
 \item{caseLabel}{ character atom naming the diagnosis of interest}
}
\details{
primitive implementation
}
\value{
an object of class \code{rocc}
}
\author{ VJ Carey }

\examples{
library(methods)
set.seed(1234)
# note that dxrule.sca is in the package
TR <- rbinom(300,1,.5)
# marker Y is uninformative
Y <- runif(300)
# by default a range-dependent set of thresholds is applied
R1 <- rocdemo.sca( TR, Y, dxrule.sca )

# in this case marker is informative
TR2 <- rbinom(300,1,plogis(4*Y))
R2 <- rocdemo.sca( TR2, Y, dxrule.sca )

# Pepe et al type I biomarker
MI <- c(rnorm(100,0,.5),rnorm(100,3,.5))
DxI <- c(rep(0,100), rep(1,100))
R3 <- rocdemo.sca( DxI, MI,  dxrule.sca, markerLabel="Pepe+ Type I" )

# Pepe et al type II biomarker
MII <- c(rnorm(100,0,.5),rnorm(70,0,.5),rnorm(30,3,.5))
DxII <- c(rep(0,100), rep(1,100))
R4 <- rocdemo.sca( DxII, MII, dxrule.sca, markerLabel="Pepe+ Type II" )

par(mfrow=c(2,2))
plot(R1)
plot(R2)
plot(R3)
plot(R4)

}

