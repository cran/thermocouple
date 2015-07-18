\name{ ThermistorTemperatureSteinhartHart}
\alias{ ThermistorTemperatureSteinhartHart}
\title{Steinhart-Hart Equation for Thermistor Temperature}
\usage{ ThermistorTemperatureSteinhartHart(R, R0, A, B, C=0, D)}
\description{\code{ ThermistorTemperatureSteinhartHart} Estimates the thermistor temperature 
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
\note{
Equation \code{ThermistorCalibrationEquation} should be used instead of the
Steinhart and Hart equation because the performance of this equation
is affected by:
1. the thermistor's R(25 C) value
2. the unit of measurement
3. R0
5. the thermistors being connected in series or parallel

Source:
John G. Webster and Halit Eren, 2014, Measurement, Instrumentation, 
and Sensors Handbook, Second Edition, CRC Press

Bennett, A. S., 1971, The calibration of thermistors over the range 0-30 C
Deep Sea Research, 19, 157-163.
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
