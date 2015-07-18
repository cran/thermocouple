\name{ThermocoupleVoltageContributionTwoHomogeneousWires}
\alias{ThermocoupleVoltageContributionTwoHomogeneousWires}
\title{Voltage Contribution of Two Homogeneous Wires}
\usage{ThermocoupleVoltageContributionTwoHomogeneousWires(Sab, T0, T1, T2)}
\description{\code{ThermocoupleVoltageContributionTwoHomogeneousWires} Voltage Contribution 
of Two Homogeneous Wires
}
\arguments{
  \item{Sab}{Seebeck coefficient between material a and b}
  \item{T0}{temperatures at T0 end}
  \item{T1}{temperatures at T1 end}
  \item{T2}{temperatures at T2 end}
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
data(ThermocoupleVoltageContributionTwoHomogeneousWires)
str(ThermocoupleVoltageContributionTwoHomogeneousWires)
}
\keyword{datasets}
