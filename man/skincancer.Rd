% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{skincancer}
\alias{skincancer}
\title{Skin cancer in women}
\format{
A data frame with 16 rows and 4 variables:
\describe{
  \item{cases}{a numeric vector giving the nonmelanoma skin cancer counts.}
  \item{city}{a factor giving the city to which correspond the skin cancer counts: "St.Paul" and "Ft.Worth".}
  \item{age}{a factor giving the age range to which correspond the skin cancer counts: "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84" and "85+".}
  \item{population}{a numeric vector giving the population of women.}
}
}
\usage{
data(skincancer)
}
\description{
The data describe the incidence of nonmelanoma
skin cancer for women stratified by age in Minneapolis
(St. Paul) and Dallas (Fort Worth). See Kleinbaum et al. (2013, page 751).
}
\examples{
barplot(1000*cases/population ~ city + age, beside=TRUE,
        col=c("yellow","blue"), data=skincancer)
legend(1.5, 8.5, legend=c("St.Paul","Ft.Worth"), title="City",
       fill=c("yellow","blue"), bty="n", cex=0.9)

}
\references{
Kleinbaum, D. and Kupper, L. and Nizam, A. and Rosenberg, E.S. (2013) \emph{Applied Regression Analysis and
other Multivariable Methods, Fifth Edition}, Cengage Learning, Boston.
}
\keyword{datasets}
