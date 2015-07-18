\name{RTDtemperatureFit}
\alias{RTDtemperatureFit}
\title{RTD temperature Fit}
\usage{RTDtemperatureFit(R, R0, fitRTD='linear', alpha=0.00385)}
\description{\code{RTDtemperatureFit} RTD temperature Fit}
\arguments{
  \item{R}{resistance at temperature T}
  \item{R0}{resistance at 0C}
  \item{fitRTD}{type of fitting method (linear, quadratic, cubic, polynomial)}
  \item{alpha}{(optional) resistance's temperature coefficient}
}
\value{
  \item{T}{temperature (C)}
}
\source{
Mosaic Industries, Inc., 2014
Relating resistance to temperature
\url{http://www.mosaic-industries.com/embedded-systems/microcontroller-projects/temperature-measurement/platinum-rtd-sensors/resistance-calibration-table}
}
\references{
Mosaic Industries, Inc., 2014
Relating resistance to temperature
\url{http://www.mosaic-industries.com/embedded-systems/microcontroller-projects/temperature-measurement/platinum-rtd-sensors/resistance-calibration-table}
}
\author{Jose Gama}
\examples{
data(RTDtemperatureFit)
str(RTDtemperatureFit)
}
\keyword{programming}
