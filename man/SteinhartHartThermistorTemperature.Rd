\name{ SteinhartHartThermistorTemperature}
\alias{ SteinhartHartThermistorTemperature}
\title{Steinhart-Hart Equation for Thermistor Temperature}
\usage{ SteinhartHartThermistorTemperature(R, R0, A, B, C=0, D)}
\description{\code{ SteinhartHartThermistorTemperature} Estimates the thermistor temperature 
using the Steinhart-Hart equation 
}
\arguments{
  \item{R}{measured resistance for temperature T}
  \item{R0}{resistance at temperature To (25°C, expressed in ohms)}
  \item{A}{Steinhart-Hart Coefficient A1 (K^0)}
  \item{B}{Steinhart-Hart Coefficient B1 (K^-1)}
  \item{C}{Steinhart-Hart Coefficient C1 (K^-2)}
  \item{D}{Steinhart-Hart Coefficient D1 (K^-3)}
}
\value{
  \item{R}{resistance}
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
\keyword{programming}
