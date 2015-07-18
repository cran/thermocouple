\name{RTDplatinumTemperature}
\alias{RTDplatinumTemperature}
\title{Callendar-Van Dusen equation for platinum RTD temperature from resistance}
\usage{RTDplatinumTemperature(R0, R, alpha, beta, delta)}
\description{\code{ RTDplatinumTemperature} calculates the Callendar-Van Dusen
 equation for platinum RTD temperature from resistance}
\arguments{
  \item{R0}{resistance at 0C}
  \item{R}{  Measured resistance}
  \item{alpha}{specific constant A}
  \item{beta}{specific constant B}
  \item{delta}{specific constant C}
}
\value{
  \item{T}{RTD temperature}
}
\source{
John G. Webster, 1999
The Measurement, Instrumentation and Sensors Handbook
CRC Press LLC
}
\references{
John G. Webster, 1999
The Measurement, Instrumentation and Sensors Handbook
CRC Press LLC
}
\author{Jose Gama}
\keyword{programming}
