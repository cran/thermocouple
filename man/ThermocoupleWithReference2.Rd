\name{ThermocoupleWithReference2}
\alias{ThermocoupleWithReference2}
\title{Thermocouple with Reference}
\usage{ThermocoupleWithReference2(Sab, T1, T2)}
\description{\code{ThermocoupleWithReference2} Thermocouple with Reference
}
\arguments{
  \item{Sab}{Seebeck coefficient between material a and b}
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
data(ThermocoupleWithReference2)
str(ThermocoupleWithReference2)
}
\keyword{datasets}
