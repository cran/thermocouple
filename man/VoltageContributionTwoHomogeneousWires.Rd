\name{VoltageContributionTwoHomogeneousWires}
\alias{VoltageContributionTwoHomogeneousWires}
\title{Voltage Contribution of Two Homogeneous Wires}
\usage{VoltageContributionTwoHomogeneousWires(Sab, T0, T1, T2)}
\description{\code{VoltageContributionTwoHomogeneousWires} Voltage Contribution 
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
data(VoltageContributionTwoHomogeneousWires)
str(VoltageContributionTwoHomogeneousWires)
}
\keyword{datasets}
