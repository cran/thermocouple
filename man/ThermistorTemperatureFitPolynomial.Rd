\name{ThermistorTemperatureFitPolynomial}
\alias{ThermistorTemperatureFitPolynomial}
\title{RTD temperature Fit Polynomial}
\usage{ThermistorTemperatureFitPolynomial(R, R0, A, B, C, D)}
\description{\code{ThermistorTemperatureFitPolynomial} RTD temperature Fit Polynomial}
\arguments{
  \item{R}{resistance measured}
  \item{R0}{resistance at 0C}
  \item{A}{Coefficient A}
  \item{B}{Coefficient B}
  \item{C}{Coefficient C}
  \item{D}{Coefficient D}
}
\value{
  \item{T}{temperature (C)}
}
\source{
Mosaic Industries, Inc., 2014
ntc-thermistors
\url{http://www.mosaic-industries.com/embedded-systems/microcontroller-projects/temperature-measurement/ntc-thermistors/resistance-equation}
}
\references{
Mosaic Industries, Inc., 2014
ntc-thermistors
\url{http://www.mosaic-industries.com/embedded-systems/microcontroller-projects/temperature-measurement/ntc-thermistors/resistance-equation}
}
\author{Jose Gama}
\examples{
data(ThermistorTemperatureFitPolynomial)
str(ThermistorTemperatureFitPolynomial)
}
\keyword{programming}
