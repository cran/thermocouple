\name{ThermistorAlphaApproximatedFromBeta}
\alias{ThermistorAlphaApproximatedFromBeta}
\title{Thermistor Alpha Approximated From Beta}
\usage{ThermistorAlphaApproximatedFromBeta(T, betaTH)}
\description{\code{ThermistorAlphaApproximatedFromBeta} Thermistor alpha approximated
from beta}
\arguments{
  \item{T}{temperature}
  \item{betaTH}{Beta parameter of the thermistor (calculated or from the data sheet)}
}
\value{
  \item{a}{ parameter of the thermistor}
}
\source{
Daycounter, Inc. Engineering Services, 2014
Steinhart-Hart Thermistor Calculator
\url{http://www.daycounter.com/Calculators/Steinhart-Hart-Thermistor-Calculator.phtml}
}
\references{
Daycounter, Inc. Engineering Services, 2014
Steinhart-Hart Thermistor Calculator
\url{http://www.daycounter.com/Calculators/Steinhart-Hart-Thermistor-Calculator.phtml}
}
\author{Jose Gama}
\examples{
data(ThermistorAlphaApproximatedFromBeta)
str(ThermistorAlphaApproximatedFromBeta)
}
\keyword{programming}
