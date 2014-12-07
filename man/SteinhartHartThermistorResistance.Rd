\name{ SteinhartHartThermistorResistance}
\alias{ SteinhartHartThermistorResistance}
\title{Steinhart-Hart Equation for Thermistor Resistance}
\usage{ SteinhartHartThermistorResistance(T, T2, T3, R0, A1, B1, C1=0, D1)}
\description{\code{ SteinhartHartThermistorResistance} Estimates the thermistor resistance 
using the Steinhart-Hart equation 
}
\arguments{
  \item{T}{measured temperature for resistance R}
  \item{T2}{measured temperature for resistance R2}
  \item{T3}{measured temperature for resistance R3}
  \item{R0}{resistance at temperature To (25°C, expressed in ohms)}
  \item{A1}{Steinhart-Hart Coefficient A1 (K^0)}
  \item{B1}{Steinhart-Hart Coefficient B1 (K^1)}
  \item{C1}{Steinhart-Hart Coefficient C1 (K^2)}
  \item{D1}{Steinhart-Hart Coefficient D1 (K^3)}
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
