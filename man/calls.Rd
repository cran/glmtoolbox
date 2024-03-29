% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{calls}
\alias{calls}
\title{Calls to a technical support help line}
\format{
A data frame with 16 rows and 2 variables:
\describe{
  \item{week}{a numeric vector indicating number of weeks that have elapsed since the product’s release.}
  \item{calls}{a numeric vector indicating the number of technical support calls.}
}
}
\source{
\url{https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.3/statug/statug_mcmc_examples12.htm}
}
\usage{
data(calls)
}
\description{
Data on technical support calls after a product release. Using
this information, new products could be allocated technical support
resources.
}
\examples{
data(calls)
dev.new()
with(calls,plot(week,calls,xlab="The number of weeks since the release of the product",
     pch=16,col="blue",ylab="Technical support calls"))
}
\keyword{datasets}
