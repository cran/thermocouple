\name{ ThermistorResistanceSteinhartHartUsing3T}
\alias{ ThermistorResistanceSteinhartHartUsing3T}
\title{Steinhart-Hart equation for thermistor resistance using 3 temperature points}
\usage{ ThermistorResistanceSteinhartHartUsing3T(T, T2, T3, R0, A1, B1, C1=0, D1)}
\description{\code{ ThermistorResistanceSteinhartHartUsing3T} Steinhart-Hart equation for 
thermistor resistance using 3 temperature points}
\arguments{
  \item{T}{measured temperature for resistance R}
  \item{T2}{2nd measured temperature for resistance R}
  \item{T3}{3rd measured temperature for resistance R}
  \item{R0}{ measured resistance}
  \item{A1}{Steinhart-Hart Coefficient A (K^0)}
  \item{B1}{Steinhart-Hart Coefficient B (K^1)}
  \item{C1}{Steinhart-Hart Coefficient C (K^2)}
  \item{D1}{Steinhart-Hart Coefficient D (K^3)}
}
\value{
  \item{R}{resistance}
}
\source{
Daycounter, Inc. Engineering Services
Steinhart-Hart Thermistor Calculator
\url{http://www.daycounter.com/Calculators/Steinhart-Hart-Thermistor-Calculator.phtml}
}
\author{Jose Gama}
\keyword{programming}
