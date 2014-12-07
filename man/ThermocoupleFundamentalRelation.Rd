\name{ThermocoupleFundamentalRelation}
\alias{ThermocoupleFundamentalRelation}
\title{Thermocouple Fundamental Relation}
\usage{ThermocoupleFundamentalRelation(S, T0, T1)}
\description{\code{ThermocoupleFundamentalRelation} Thermocouple Fundamental Relation
}
\arguments{
  \item{S}{Seebeck coefficient (uV/C) or Sab Seebeck coefficient between material a and b}
  \item{T0}{temperatures at T0 end}
  \item{T1}{temperatures at T1 end}
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
data(ThermocoupleFundamentalRelation)
str(ThermocoupleFundamentalRelation)
}
\keyword{datasets}
