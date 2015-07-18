\name{ThermocoupleStemLossErrorEstimate}
\alias{ThermocoupleStemLossErrorEstimate}
\title{Stem Loss Error Estimate for Thermocouple}
\usage{ThermocoupleStemLossErrorEstimate(L, h, k, r0, ri)}
\description{\code{ThermocoupleStemLossErrorEstimate} Stem Loss Error Estimate for Thermocouple
}
\arguments{
  \item{L}{sensor insertion depth (cm)}
  \item{h}{surface heat transfer coefficient (watts.cm2 C)}
  \item{k}{thermal conductivity of sheath material (watts.cm C)}
  \item{r0}{sheath outer radius}
  \item{ri}{sheath inner radius}
}
\value{
  \item{E}{error (percent of difference between tip temperature and back-end temperature)}
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
data(ThermocoupleStemLossErrorEstimate)
str(ThermocoupleStemLossErrorEstimate)
}
\keyword{programming}
