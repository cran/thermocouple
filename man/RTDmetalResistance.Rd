\name{RTDmetalResistance}
\alias{RTDmetalResistance}
\title{Metal RTD resistance}
\usage{RTDmetalResistance(R0, T, A, B, C, metal=NA)}
\description{\code{ RTDmetalResistance} calculates Metal RTD resistance}
\arguments{
  \item{R0}{resistance at 0C}
  \item{T}{ temperature in C}
  \item{A}{specific constant A}
  \item{B}{specific constant B}
  \item{C}{specific constant C}
  \item{metal}{ optional, if chosen then A, B and C are the ones for this metal}
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
