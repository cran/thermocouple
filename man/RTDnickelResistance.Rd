\name{RTDnickelResistance}
\alias{RTDnickelResistance}
\title{Simplified Equation for Nickel-Iron RTD Resistance}
\usage{RTDnickelResistance(R0, T, A=NA, B=NA, D=NA, F=NA)}
\description{\code{ RTDnickelResistance} calculates simplified equation for Nickel-Iron RTD resistance}
\arguments{
  \item{R0}{resistance at 0C}
  \item{T}{ temperature in C}
  \item{A}{specific constant A (optional)}
  \item{B}{specific constant B (optional)}
  \item{D}{specific constant D (optional)}
  \item{F}{specific constant F (optional)}
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
