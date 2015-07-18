\name{ ThermistorConvertTemperatureCtoADCreading}
\alias{ ThermistorConvertTemperatureCtoADCreading}
\title{Convert temperature in Celcius into ADC reading, with two resistors}
\usage{ ThermistorConvertTemperatureCtoADCreading(T, R0, T0, R1, R2, 
betaTH, vadc = 5.0, vcc = 5.0, ADCbits=10)}
\description{\code{ ThermistorConvertTemperatureCtoADCreading} Converts temperature 
in Celcius into ADC reading, with two resistors}
\arguments{
  \item{T}{Temperature in Celsius}
  \item{R0}{ resistance measurement 1}
  \item{T0}{ resistance temperature 1}
  \item{R1}{ resistor value 1}
  \item{R2}{ resistor value 2}
  \item{betaTH}{beta coefficient}
  \item{vadc}{ ADC reference}
  \item{vcc}{ supply voltage to potential divider}
  \item{ADCbits}{ ADC bit resolution}
}
\value{
  \item{adc}{ ADC value}
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
