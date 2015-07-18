\name{RTDplatinumResistanceFromAlpha}
\alias{RTDplatinumResistanceFromAlpha}
\title{Simplified Equation for Platinum RTD Resistance}
\usage{RTDplatinumResistanceFromAlpha(R0, T, alpha=NA, stdRTD='DIN43760')}
\description{\code{ RTDplatinumResistanceFromAlpha} calculates simplified equation for Platinum RTD resistance}
\arguments{
  \item{R0}{resistance at 0C}
  \item{T}{temperature in C}
  \item{alpha}{optional resistance's temperature coefficient}
  \item{stdRTD}{standard, optional alternative way to get alpha}
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
