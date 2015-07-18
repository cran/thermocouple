\name{ ThermistorResistanceSteinhartHart}
\alias{ ThermistorResistanceSteinhartHart}
\title{Steinhart-Hart Equation for Thermistor Resistance}
\usage{ ThermistorResistanceSteinhartHart(T, A, B, C)}
\description{\code{ ThermistorResistanceSteinhartHart} Estimates the thermistor resistance 
using the Steinhart-Hart equation 
}
\arguments{
  \item{T}{measured temperature for resistance R}
  \item{A}{Steinhart-Hart Coefficient A (K^0)}
  \item{B}{Steinhart-Hart Coefficient B (K^1)}
  \item{C}{Steinhart-Hart Coefficient C (K^2)}
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
