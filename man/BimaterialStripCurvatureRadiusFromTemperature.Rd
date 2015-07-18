\name{BimaterialStripCurvatureRadiusFromTemperature}
\alias{BimaterialStripCurvatureRadiusFromTemperature}
\alias{BimaterialStripTemperatureFromCurvatureRadius}
\title{curvature radius of a bimetallic strip uniformly heated from T0 to T}
\usage{BimaterialStripCurvatureRadiusFromTemperature(T0, R0, T, m, n, alpha1, alpha2, thickn)}
\description{\code{BimaterialStripCurvatureRadiusFromTemperature} curvature radius 
of a bimetallic strip uniformly heated from T0 to T in the absence of external forces}
\arguments{
  \item{T0}{Initial temperature}
  \item{R0}{1/R0 = Initial curvature of the strip at temperature T0}
  \item{T}{Measured temperature}
  \item{m}{t1/t2, with t1 and t2 their respective thicknesses}
  \item{n}{E1/E2, with E1 and E2 their respective Young’s moduli}
  \item{alpha1}{Coefficient of expansion of element 1}
  \item{alpha2}{Coefficient of expansion of element 2}
  \item{thickn}{t1 + t2 thickness of the strip}
}
\value{
  \item{R}{voltage (V)}
}
\source{ 
John G. Webster, 1999
The Measurement, Instrumentation and Sensors Handbook
CRC Press LLC
}
\references{
John G. Webster, 1999
The Measurement, Instrumentation and Sensors Handbook
CRC Press LLC
}
\author{Jose Gama}
\keyword{programming}
