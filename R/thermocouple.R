
EquationThermocouplesTypeB <- function(vT)
{
# vT vector with temperatures
sapply(vT,function(x){
if (x<630.615) w<-1:7 else w<-8:16
n <- 0:(length(w)-1)
round(sum(thermocouple::coefficientsThermocoupleTypeB[w,1]*x^n), 3)
})
}

EquationThermocouplesTypeE <- function(vT)
{
# vT vector with temperatures
sapply(vT,function(x){
if (x<0) w<-1:13 else w<-14:24
n <- 0:(length(w)-1)
round(sum(thermocouple::coefficientsThermocoupleTypeE[w,1]*x^n), 3)
})
}

EquationThermocouplesTypeJ <- function(vT)
{
# vT vector with temperatures
sapply(vT,function(x){
if (x<760) w<-1:8 else w<-9:14
n <- 0:(length(w)-1)
round(sum(thermocouple::coefficientsThermocoupleTypeJ[w,1]*x^n), 3)
})
}

EquationThermocouplesTypeK <- function(vT)
{
# vT vector with temperatures
a0 <-  0.118597600000E+00
a1 <- -0.118343200000E-03
a2 <-  0.126968600000E+03
sapply(vT,function(x){
if (x<0) w<-1:10 else  w<-11:20
n <- 0:(length(w)-1)
if (x<0) return(round(sum(thermocouple::coefficientsThermocoupleTypeK[w,1]*x^n), 3)) else 
return(round(sum(thermocouple::coefficientsThermocoupleTypeK[w,1]*x^n  + a0*exp(a1*(x - a2)^2) ), 3))
})
}

EquationThermocouplesTypeN <- function(vT)
{
# vT vector with temperatures
sapply(vT,function(x){
if (x<0) w<-1:8 else  w<-9:19
n <- 0:(length(w)-1)
round(sum(thermocouple::coefficientsThermocoupleTypeN[w,1]*x^n), 3)
})
}

EquationThermocouplesTypeR <- function(vT)
{
# vT vector with temperatures
sapply(vT,function(x){
if (x<1064.180) w<-1:10 else {
if (x<1768.100) w<-11:16 else w<-17:21
}
n <- 0:(length(w)-1)
round(sum(thermocouple::coefficientsThermocoupleTypeR[w,1]*x^n), 3)
})
}

EquationThermocouplesTypeS <- function(vT)
{
# vT vector with temperatures
sapply(vT,function(x){
if (x<1064.180) w<-1:9 else {
if (x<1768.100) w<-10:14 else w<-15:19
}
n <- 0:(length(w)-1)
round(sum(thermocouple::coefficientsThermocoupleTypeS[w,1]*x^n), 3)
})
}

EquationThermocouplesTypeT <- function(vT)
{
# vT vector with temperatures
sapply(vT,function(x){
if (x<0) w<-1:16 else w<-17:24
n <- 0:(length(w)-1)
round(sum(thermocouple::coefficientsThermocoupleTypeT[w,1]*x^n), 3)
})
}

InverseEquationThermocouplesTypeB <- function(vV)
{
# vV vector with voltages
sapply(vV,function(x){
n <- 0:(dim(thermocouple::inverseCoefficientsThermocoupleTypeB)[1]-1)
k <- 1
if (x>=2.431) k <- 2
round(sum(thermocouple::inverseCoefficientsThermocoupleTypeB[,k]*x^n), 3)
})
}

InverseEquationThermocouplesTypeE <- function(vV)
{
# vV vector with voltages
sapply(vV,function(x){
n <- 0:(dim(thermocouple::inverseCoefficientsThermocoupleTypeE)[1]-1)
k <- 1
if (x>=0) k <- 2
round(sum(thermocouple::inverseCoefficientsThermocoupleTypeE[,k]*x^n), 3)
})
}

InverseEquationThermocouplesTypeJ <- function(vV)
{
# vV vector with voltages
sapply(vV,function(x){
n <- 0:(dim(thermocouple::inverseCoefficientsThermocoupleTypeJ)[1]-1)
k <- 1
if (x>=42.919) k <- 3 else {
if (x>=0) k <- 2 
}
round(sum(thermocouple::inverseCoefficientsThermocoupleTypeJ[,k]*x^n), 3)
})
}

InverseEquationThermocouplesTypeK <- function(vV)
{
# vV vector with voltages
sapply(vV,function(x){
n <- 0:(dim(thermocouple::inverseCoefficientsThermocoupleTypeK)[1]-1)
k <- 1
if (x>=20.644) k <- 3 else {
if (x>=0) k <- 2 
}
round(sum(thermocouple::inverseCoefficientsThermocoupleTypeK[,k]*x^n), 3)
})
}

InverseEquationThermocouplesTypeN <- function(vV)
{
# vV vector with voltages
sapply(vV,function(x){
n <- 0:(dim(thermocouple::inverseCoefficientsThermocoupleTypeN)[1]-1)
k <- 1
if (x>=20.613) k <- 3 else {
if (x>=0) k <- 2 
}
round(sum(thermocouple::inverseCoefficientsThermocoupleTypeN[,k]*x^n), 3)
})
}

InverseEquationThermocouplesTypeR <- function(vV)
{
# vV vector with voltages
sapply(vV,function(x){
n <- 0:(dim(thermocouple::inverseCoefficientsThermocoupleTypeR)[1]-1)
k <- 1
if (x>=19.739) k <- 4 else {
if (x>=11.361) k <- 3 else if (x>=1.923) k <- 2 
}
round(sum(thermocouple::inverseCoefficientsThermocoupleTypeR[,k]*x^n), 3)
})
}

InverseEquationThermocouplesTypeS <- function(vV)
{
# vV vector with voltages
sapply(vV,function(x){
n <- 0:(dim(thermocouple::inverseCoefficientsThermocoupleTypeS)[1]-1)
k <- 1
if (x>=17.536) k <- 4 else {
if (x>=10.332) k <- 3 else if (x>=1.874) k <- 2 
}
round(sum(thermocouple::inverseCoefficientsThermocoupleTypeS[,k]*x^n), 3)
})
}

InverseEquationThermocouplesTypeT <- function(vV)
{
# vV vector with voltages
sapply(vV,function(x){
n <- 0:(dim(thermocouple::inverseCoefficientsThermocoupleTypeT)[1]-1)
k <- 1
if (x>=0) k <- 2 
round(sum(thermocouple::inverseCoefficientsThermocoupleTypeT[,k]*x^n), 3)
})
}

ThermistorResistance <- function(Tx, R0, betaTH, T0) R0*exp(betaTH*(1/Tx)-(1/T0))
# Estimate thermistor resistance from temperature
# Tx variable temperature
# Ro resistance at temperature To (25C, expressed in Kelvin)
# Beta parameter of the thermistor (calculated or from the data sheet)
# http://hydraraptor.blogspot.co.uk/2007/10/measuring-temperature-easy-way.html

ConvertThermistorADCreadingToTemperatureC <- function(adc, R0, T0, betaTH, R1, R2, vadc = 5.0, vcc = 5.0, ADCbits=10)
{
# Convert ADC reading into a temperature in Celcius by using two resistors
# vadc ADC reference
# vcc supply voltage to potential divider
# http://hydraraptor.blogspot.co.uk/2007/10/measuring-temperature-easy-way.html
T0 = T0 + 273.15 # temperature at stated resistance, e.g. 25C
vs = R1 * vcc / (R1 + R2) # effective bias voltage
rs = R1 * R2 / (R1 + R2)# effective bias impedance
k = R0 * exp(-betaTH / T0)  # constant part of calculation
v = adc * vadc / 2^ADCbits   # convert the 10 bit ADC value to a voltage
r = rs * v / (vs - v)     # resistance of thermistor
(betaTH / log(r / k)) - 273.15 # temperature
}

ConvertThermistorTemperatureCToADCreading <- function(T, R0, T0, R1, R2, betaTH, vadc = 5.0, vcc = 5.0, ADCbits=10)
{
# Convert a temperature into a ADC value by using two resistors
# vadc ADC reference
# http://hydraraptor.blogspot.co.uk/2007/10/measuring-temperature-easy-way.html
T0 = T0 + 273.15 # temperature at stated resistance, e.g. 25C
vs = R1 * vcc / (R1 + R2) # effective bias voltage
rs = R1 * R2 / (R1 + R2)# effective bias impedance
r = R0 * exp(betaTH * (1 / (T + 273.15) - 1 / T0)) # resistance of the thermistor
v = vs * r / (rs + r)     # the voltage at the potential divider
round(v / vadc * 2^ADCbits)  # the ADC reading
}

CalculateThermistorBeta <- function(R0, T0, R1, T1) b <- log(R1/R0) / ((1/(T1+273.15))-(1/(T0+273.15)))
# Estimate thermistor beta coefficient from two known resistance/temperature values
# RepRap wiki
# Measuring Thermistor Beta
# http://reprap.org/wiki/MeasuringThermistorBeta

SteinhartHartThermistorTemperature <- function(R, R0, A, B, C=0, D) 1/ (A + B*log(R/R0) + C*log(R/R0)^2 + D*log(R/R0)^3)
# Steinhart-Hart equation for thermistor temperature
# Steinhart-Hart Coefficient A (K^0)
# Steinhart-Hart Coefficient B (K^-1)
# Steinhart-Hart Coefficient C (K^-2)
# Steinhart-Hart Coefficient D (K^-3)
# Ro resistance at temperature To (25°C, expressed in ohms)
# R resistance at temperature T
# Daycounter, Inc. Engineering Services
# Steinhart-Hart Thermistor Calculator
# http://www.daycounter.com/Calculators/Steinhart-Hart-Thermistor-Calculator.phtml

SteinhartHartThermistorResistance <- function(T, T2, T3, R0, A1, B1, C1=0, D1) R0*exp(A1+B1/T+C1/T2+D1/T3)
# Steinhart-Hart equation for thermistor resistance
# Steinhart-Hart Coefficient A1 (K^0)
# Steinhart-Hart Coefficient B1 (K^1)
# Steinhart-Hart Coefficient C1 (K^2)
# Steinhart-Hart Coefficient D1 (K^3)
# Ro resistance at temperature To (25°C, expressed in ohms)
# T measured temperature for resistance R
# Daycounter, Inc. Engineering Services
# Steinhart-Hart Thermistor Calculator
# http://www.daycounter.com/Calculators/Steinhart-Hart-Thermistor-Calculator.phtml

CalcCRCbitDS1820 <- function(shiftReg, dataBit)
{
#  Calculate 8-bit CRC for DS1820
# Peter H. Anderson, 98
# DS1820 Digital Thermometer - Calculating an 8-bit CRC Value
# http://www.phanderson.com/PIC/16C84/crc.html
fb <- bitwXor(bitwAnd(shiftReg, 0x01), dataBit) #exclusive or least sig bit of current shift reg with the data bit
shiftReg = shiftReg %/% 2 # shift one place to the right
        if (fb==1)
        {
           shiftReg <- bitwXor(shiftReg, 0x10001100 ) # CRC ^ binary 1000 1100 
           }
shiftReg 
}

VolumeResistivityFromRho <- function(Rho, Thck, L, W) Rho * Thck / (L * W)
# Rho material resistivity in ohm/cm
# Thck thickness of the conductor (chip) (cm)
# L length of the conductor (chip) (cm)
# W width of the conductor (chip) (cm)
# Equation #1
# NTC Thermistor theory
# BetaTHERM sensors
# www.betatherm.com

VolumeResistivityFromR25 <- function(R25, Thck, L, W) L * W / Thck * R25
# R25 measured resistance 25C (ohms)
# Thck thickness of the conductor (chip) (cm)
# L length of the conductor (chip) (cm)
# W width of the conductor (chip) (cm)
# Equation #1
# NTC Thermistor theory
# BetaTHERM sensors
# www.betatherm.com

AWGTOmm <- function(n) 0.127 * 92 ^((36-n)/39)
# convert American wire gauge (SWG) to mm
# n gauge number
# http://www.rapidtables.com/calc/wire/awg-to-mm.htm

ThermocoupleFundamentalRelation<-function(S, T0, T1) S * (T1 - T0)
# T0, T1 temperatures at both ends
# S Seebeck coefficient (uV/C) or Sab Seebeck coefficient between material a and b
# V voltage difference
# pp. 13 eq. 2.1

ThermocoupleFundamentalRelation2<-function(Sa, Sb, T0, T1) (Sa - Sb) * (T1 - T0)
# T0, T1 temperatures at both ends
# Sa Seebeck coefficient of material a
# Sb Seebeck coefficient of material b
# V voltage difference
# pp. 13 eq. 2.4

VoltageContributionTwoHomogeneousWires<-function(Sab, T0, T1, T2) Sab * (T2 - T0) + Sab * (T1 - T2)
# Voltage Contribution of Two Homogeneous Wires
# T0, T1 temperatures at both ends
# T2 temperature at a point !=T0, T1
# Sab Seebeck coefficient between material a and b
# V voltage difference
# pp. 15 eq. 2.9

ThermocoupleWithReference<-function(Sa, Sb, T0, T1, T2) Sa * (T2 - T0) + Sb * (T1 - T2) + Sa * (T0 - T1)
# Thermocouple with Reference
# T0, T2 temperatures at both ends
# T1 temperature at a reference point
# Sa Seebeck coefficient of material a
# Sb Seebeck coefficient of material b
# V voltage difference
# pp. 17 eq. 2.13

ThermocoupleWithReference2<-function(Sab, T1, T2) Sab * (T2 - T1)
# Thermocouple with Reference
# T0, T2 temperatures at both ends
# T1 temperature at a reference point
# Sa Seebeck coefficient of material a
# Sb Seebeck coefficient of material b
# V voltage difference
# pp. 17 eq. 2.15

ThermocoupleStemLossErrorEstimate <- function(L, h, k, r0, ri){
# E = error (percent of difference between tip temperature and back-end temperature)
# L = sensor insertion depth (cm)
# h = surface heat transfer coefficient (watts.cm2 C)
# k = thermal conductivity of sheath material (watts.cm C)
# r0 = sheath outer radius
# ri = sheath inner radius
# pp. 47 eq. 3.8
alpha <- sqrt(2*r0 * h/(k*(r0^2 - ri^2)))
F <- k * alpha/h
2 * F / ((1 + F)*exp(alpha * L)-(1 - F)*exp(-alpha * L))
}


