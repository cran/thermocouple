\name{ ThermistorApproxDriftResistance}
\alias{ ThermistorApproxDriftResistance}
\title{Approximation of Drift Resistance of NTC Thermistors}
\usage{ ThermistorApproxDriftResistance(Ri, T, a, b)}
\description{\code{ ThermistorApproxDriftResistance} Estimates the Drift Resistance of
 NTC Thermistors}
\arguments{
  \item{Ri}{ initial resistance}
  \item{T}{ aging time}
  \item{a}{ intercept at T=1}
  \item{b}{ slope (\%deltaR per decade of time T)}
}
\value{
  \item{Rt}{resistance at time T}
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
