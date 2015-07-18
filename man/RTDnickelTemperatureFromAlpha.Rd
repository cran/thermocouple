\name{RTDnickelTemperatureFromAlpha}
\alias{RTDnickelTemperatureFromAlpha}
\title{Simplified Equation for Nickel RTD temperature}
\usage{RTDnickelTemperatureFromAlpha(R0, Rt, alpha=NA)}
\description{\code{ RTDnickelTemperatureFromAlpha} calculates simplified equation for Nickel RTD temperature}
\arguments{
  \item{R0}{resistance at 0C}
  \item{Rt}{ resistance at temperature T}
  \item{alpha}{optional resistance's temperature coefficient}
}
\value{
  \item{T}{RTD temperature}
}
\source{
Capgo Inc., 2014
Introduction to RTDs
\url{http://www.capgo.com/Resources/Temperature/RTDs/RTD.html}
}
\references{
Capgo Inc., 2014
Introduction to RTDs
\url{http://www.capgo.com/Resources/Temperature/RTDs/RTD.html}
}
\author{Jose Gama}
\keyword{programming}
