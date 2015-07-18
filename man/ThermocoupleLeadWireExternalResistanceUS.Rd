\name{ ThermocoupleLeadWireExternalResistanceUS}
\alias{ ThermocoupleLeadWireExternalResistanceUS}
\title{Calculate the external resistance to an instrument}
\usage{ ThermocoupleLeadWireExternalResistanceUS(thermocoupleType, thermocoupleLength,
 thermocoupleGauge, leadWireType, leadWireLength, leadWireGauge)}
\description{\code{ ThermocoupleLeadWireExternalResistanceUS} 
Calculates the external resistance to an instrument}
\arguments{
  \item{thermocoupleType}{Type of thermocouple wire}
  \item{thermocoupleLength}{Length of thermocouple wire (feet)}
  \item{thermocoupleGauge}{Gauge of thermocouple wire (AWG)}
  \item{leadWireType}{Type of lead wire}
  \item{leadWireLength}{Length of lead wire (feet)}
  \item{leadWireGauge}{Gauge of lead wire (AWG)}
}
\value{
  \item{R}{resistance (ohms)}
}
\source{
Conax(TM) Buffalo, 2014
thermocouple wire size and resistance table
\url{www.conaxbuffalo.com}
}
\references{
Conax(TM) Buffalo, 2014
thermocouple wire size and resistance table
\url{www.conaxbuffalo.com}
}
\examples{
# What is external resistance to my instrument if I use a 20 gauge Chromel/Alumel 
# thermocouple 3 feet long and 14 gauge Chromel/Alumel lead wire 20 feet in length?
# Answer: 4.7002 ohms
ThermocoupleLeadWireExternalResistanceUS('k',3,20,'k',20,14)
}
\author{Jose Gama}
\keyword{programming}
