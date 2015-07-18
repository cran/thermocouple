\name{ThermistorResistanceDeviation}
\alias{ThermistorResistanceDeviation}
\title{Thermistor Resistance Deviation}
\usage{ThermistorResistanceDeviation(deltaBetaTH, deltaR25)}
\description{\code{ThermistorResistanceDeviation} Thermistor Resistance Deviation}
\arguments{
  \item{deltaBetaTH}{delta(beta)}
  \item{deltaR25}{delta(R25)}
}
\value{
  \item{R}{Resistance (ohm)}
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
data(ThermistorResistanceDeviation)
str(ThermistorResistanceDeviation)
}
\keyword{programming}
