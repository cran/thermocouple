\name{ThermistorTemperatureDeviation}
\alias{ThermistorTemperatureDeviation}
\title{Thermistor temperature Deviation}
\usage{ThermistorTemperatureDeviation(deltaBetaTH, deltaR25, alpha)}
\description{\code{ThermistorTemperatureDeviation} Thermistor temperature Deviation}
\arguments{
  \item{deltaBetaTH}{delta(beta)}
  \item{deltaR25}{delta(R25)}
  \item{alpha}{Thermistor alpha coefficient}
}
\value{
  \item{T}{temperature (C)}
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
data(ThermistorTemperatureDeviation)
str(ThermistorTemperatureDeviation)
}
\keyword{programming}
