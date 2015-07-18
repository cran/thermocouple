\name{RTDnickelResistanceFromAlpha}
\alias{RTDnickelResistanceFromAlpha}
\title{Simplified Equation for Nickel RTD Resistance}
\usage{RTDnickelResistanceFromAlpha(R0, T, alpha=NA)}
\description{\code{ RTDnickelResistanceFromAlpha} calculates simplified equation for Nickel RTD resistance}
\arguments{
  \item{R0}{resistance at 0C}
  \item{T}{ temperature in C}
  \item{alpha}{optional resistance's temperature coefficient}
}
\value{
  \item{R}{RTD resistance}
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
