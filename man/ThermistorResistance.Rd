\name{ ThermistorResistance}
\alias{ ThermistorResistance}
\title{Estimate thermistor resistance from temperature}
\usage{ ThermistorResistance(Tx, R0, betaTH, T0)}
\description{\code{ ThermistorResistance} Estimates thermistor resistance from temperature
}
\arguments{
  \item{Tx}{measured temperature}
  \item{R0}{R0 resistance at temperature To (25C, expressed in Kelvin)}
  \item{betaTH}{Beta parameter of the thermistor (calculated or from the data sheet)}
  \item{T0}{resistance temperature}
}
\value{
  \item{R}{resistance in ohms}
}
\source{
Chris Palmer, 2007
Measuring temperature the easy way
\url{http://hydraraptor.blogspot.co.uk/2007/10/measuring-temperature-easy-way.html}
}
\references{
Chris Palmer, 2007
Measuring temperature the easy way
\url{http://hydraraptor.blogspot.co.uk/2007/10/measuring-temperature-easy-way.html}
}
\author{Jose Gama}
\keyword{programming}
