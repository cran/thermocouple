\name{ThermocoupleEquationTypeKrationalPolynomial}
\alias{ThermocoupleEquationTypeKrationalPolynomial}
\alias{ThermocoupleEquationTypeJrationalPolynomial}
\alias{ThermocoupleEquationTypeErationalPolynomial}
\alias{ThermocoupleEquationTypeNrationalPolynomial}
\alias{ThermocoupleEquationTypeRrationalPolynomial}
\alias{ThermocoupleEquationTypeSrationalPolynomial}
\alias{ThermocoupleEquationTypeTrationalPolynomial}
\alias{ThermocoupleEquationTypeBrationalPolynomial}
\title{Thermocouple polynomial function approximation}
\usage{ThermocoupleEquationTypeKrationalPolynomial(vV, thermocoupleType='k')}
\description{\code{ThermocoupleEquationTypeKrationalPolynomial} Thermocouple polynomial function approximation}
\arguments{
  \item{vV}{vector with voltages}
  \item{thermocoupleType}{Thermocouple type}
}
\value{
  \item{T}{temperature (C)}
}
\source{
Mosaic Industries, Inc., 2014
Rational polynomial function approximation for Type K thermocouples
\url{http://www.mosaic-industries.com/embedded-systems/microcontroller-projects/temperature-measurement/thermocouple/calibration-table#computing-cold-junction-voltages}
}
\references{
Mosaic Industries, Inc., 2014
Rational polynomial function approximation for Type K thermocouples
\url{http://www.mosaic-industries.com/embedded-systems/microcontroller-projects/temperature-measurement/thermocouple/calibration-table#computing-cold-junction-voltages}
}
\author{Jose Gama}
\examples{
data(ThermocoupleEquationTypeKrationalPolynomial)
str(ThermocoupleEquationTypeKrationalPolynomial)
}
\keyword{programming}
