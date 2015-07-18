\name{RTDplatinumResistance}
\alias{RTDplatinumResistance}
\title{Metal RTD resistance}
\usage{RTDplatinumResistance(R0, T, A=NA, B=NA, C=NA, stdRTD='DIN43760')}
\description{\code{ RTDplatinumResistance} calculates Metal RTD resistance}
\arguments{
  \item{R0}{resistance at 0C}
  \item{T}{ temperature in C}
  \item{A}{specific constant A}
  \item{B}{specific constant B}
  \item{C}{specific constant C}
  \item{stdRTD}{ standard, optional alternative to get A, B and C}
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
