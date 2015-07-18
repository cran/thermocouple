\name{ThermistorTemperature}
\alias{ThermistorTemperature}
\title{RTD temperature Fit}
\usage{ThermistorTemperature(R, R0, betaTH, T0)}
\description{\code{ThermistorTemperature} RTD temperature Fit}
\arguments{
  \item{R0}{resistance at 0C}
  \item{R}{resistance measured}
  \item{betaTH}{beta parameter of the thermistor (calculated or from the data sheet)}
  \item{T0}{temperature at resistance R0}
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
data(ThermistorTemperature)
str(ThermistorTemperature)
}
\keyword{programming}
