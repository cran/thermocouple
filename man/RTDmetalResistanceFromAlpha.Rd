\name{RTDmetalResistanceFromAlpha}
\alias{RTDmetalResistanceFromAlpha}
\title{Simplified Equation for Meta RTD Resistance}
\usage{RTDmetalResistanceFromAlpha(R0, T, alpha=NA, metal='nickel')}
\description{\code{ RTDmetalResistanceFromAlpha} calculates simplified equation for Meta RTD resistance}
\arguments{
  \item{R0}{resistance at 0C}
  \item{T}{ temperature in C}
  \item{alpha}{optional resistance's temperature coefficient}
  \item{metal}{optional metal to get alpha}
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
