\name{ThermocoupleWithReference}
\alias{ThermocoupleWithReference}
\title{Thermocouple with Reference}
\usage{ThermocoupleWithReference(Sa, Sb, T0, T1, T2)}
\description{\code{ThermocoupleWithReference} Thermocouple with Reference
}
\arguments{
  \item{Sa}{Seebeck coefficient for material a}
  \item{Sb}{Seebeck coefficient for material b}
  \item{T0}{temperatures at T0 end}
  \item{T1}{temperatures at T1 end}
  \item{T2}{temperatures at T3 end}
}
\value{
  \item{V}{voltage (V)}
}
\source{
Kerlin, T.W., 1999
Practical Thermocouple Thermometry
International Society of Automation (ISA)
}
\references{
Kerlin, T.W., 1999
Practical Thermocouple Thermometry
International Society of Automation (ISA)
}
\author{Jose Gama}
\examples{
data(ThermocoupleWithReference)
str(ThermocoupleWithReference)
}
\keyword{datasets}
