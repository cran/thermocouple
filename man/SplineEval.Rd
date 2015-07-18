\name{ SplineEval}
\alias{ SplineEval}
\title{Spline algorithm used in The Observed Properties of Liquid Helium at the
 Saturated Vapor Pressure}
\usage{ SplineEval(x, knotsK, coeffsC)}
\description{\code{ SplineEval} Spline algorithm used in The Observed Properties
 of Liquid Helium at the Saturated Vapor Pressure}
\arguments{
  \item{x}{ Temperature vector}
  \item{knotsK}{ knots, internal and external, vector}
  \item{coeffsC}{ coefficients vector}
}
\value{
  \item{S}{Spline result}
}
\source{
Donnelly, Donnelly and Hills [J. Low Temp. Phys. 44, 471 (1981)]
}
\references{
Donnelly, Donnelly and Hills [J. Low Temp. Phys. 44, 471 (1981)]
}
\author{Jose Gama}
\keyword{programming}
