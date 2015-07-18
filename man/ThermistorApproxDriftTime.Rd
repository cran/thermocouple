\name{ ThermistorApproxDriftTime}
\alias{ ThermistorApproxDriftTime}
\title{Approximation of Drift Time of NTC Thermistors}
\usage{ ThermistorApproxDriftTime(Ri, Rt, a, b)}
\description{\code{ ThermistorApproxDriftTime} Estimates the Drift Time of
 NTC Thermistors}
\arguments{
  \item{Ri}{ initial resistance}
  \item{Rt}{resistance at time T}
  \item{a}{ intercept at T=1}
  \item{b}{ slope (\%deltaR per decade of time T)}
}
\value{
  \item{T}{ aging time}
}
\source{
Quality Thermistor, Inc. 2108
\url{http://www.cornerstonesensors.com/About.asp?PageCode=Stability&Print=Page}
}
\references{
Quality Thermistor, Inc. 2108
\url{http://www.cornerstonesensors.com/About.asp?PageCode=Stability&Print=Page}
}
\author{Jose Gama}
\keyword{programming}
