\name{ThermocoupleFundamentalRelation2}
\alias{ThermocoupleFundamentalRelation2}
\title{Thermocouple Fundamental Relation}
\usage{ThermocoupleFundamentalRelation2(Sa, Sb, T0, T1)}
\description{\code{ThermocoupleFundamentalRelation2} Thermocouple Fundamental Relation}
\arguments{
  \item{Sa}{Seebeck coefficient for material a}
  \item{Sb}{Seebeck coefficient for material b}
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
data(ThermocoupleFundamentalRelation2)
str(ThermocoupleFundamentalRelation2)
}
\keyword{datasets}
