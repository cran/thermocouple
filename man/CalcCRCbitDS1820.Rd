\name{ CalcCRCbitDS1820}
\alias{ CalcCRCbitDS1820}
\title{Calculate 8-bit CRC for DS1820}
\usage{ CalcCRCbitDS1820(shiftReg, dataBit)}
\description{\code{ CalcCRCbitDS1820} Calculates 8-bit CRC for DS1820}
\arguments{
  \item{shiftReg}{ shift register}
  \item{dataBit}{ data bit}
}
\value{
  \item{b}{beta coefficient}
}
\source{
Peter H. Anderson, 1998
DS1820 Digital Thermometer - Calculating an 8-bit CRC Value
\url{http://www.phanderson.com/PIC/16C84/crc.html}
}
\references{
Peter H. Anderson, 1998
DS1820 Digital Thermometer - Calculating an 8-bit CRC Value
\url{http://www.phanderson.com/PIC/16C84/crc.html}
}
\author{Jose Gama}
\keyword{programming}
