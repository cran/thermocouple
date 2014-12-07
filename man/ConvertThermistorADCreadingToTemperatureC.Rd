\name{ ConvertThermistorADCreadingToTemperatureC}
\alias{ ConvertThermistorADCreadingToTemperatureC}
\title{Convert ADC reading into a temperature in Celcius by using two resistors}
\usage{ ConvertThermistorADCreadingToTemperatureC(adc, R0, T0, betaTH,
 R1, R2, vadc = 5.0, vcc = 5.0, ADCbits=10)}
\description{\code{ ConvertThermistorADCreadingToTemperatureC} Converts ADC reading into a 
temperature in Celcius by using two resistors values}
\arguments{
  \item{adc}{ ADC reading}
  \item{R0}{ resistance measurement 1}
  \item{T0}{ resistance temperature 1}
  \item{betaTH}{beta coefficient}
  \item{R1}{ resistor value 1}
  \item{R2}{ resistor value 2}
  \item{vadc}{ ADC reference}
  \item{vcc}{ supply voltage to potential divider}
  \item{ADCbits}{ ADC bit resolution}
}
\value{
  \item{C}{Temperature in Celsius}
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
