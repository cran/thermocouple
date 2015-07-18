\name{ThermocoupleEquationTemperatureToVoltage}
\alias{ThermocoupleEquationTemperatureToVoltage}
\title{Thermocouple cold junction voltages}
\usage{ThermocoupleEquationTemperatureToVoltage(vT, thermocoupleType='k')}
\description{\code{ThermocoupleEquationTemperatureToVoltage} Thermocouple cold junction voltages}
\arguments{
  \item{vT}{vector with temperatures}
  \item{thermocoupleType}{Thermocouple type}
}
\value{
  \item{V}{voltage (V)}
}
\source{
Mosaic Industries, Inc., 2014
Computing cold junction voltages
\url{http://www.mosaic-industries.com/embedded-systems/microcontroller-projects/temperature-measurement/thermocouple/calibration-table#computing-cold-junction-voltages}
}
\references{
Mosaic Industries, Inc., 2014
Computing cold junction voltages
\url{http://www.mosaic-industries.com/embedded-systems/microcontroller-projects/temperature-measurement/thermocouple/calibration-table#computing-cold-junction-voltages}
}
\author{Jose Gama}
\examples{
data(ThermocoupleEquationTemperatureToVoltage)
str(ThermocoupleEquationTemperatureToVoltage)
}
\keyword{programming}
