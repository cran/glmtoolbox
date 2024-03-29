% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{anova.gnm}
\alias{anova.gnm}
\title{Comparison of nested models in Generalized Nonlinear Models.}
\usage{
\method{anova}{gnm}(object, ..., verbose = TRUE)
}
\arguments{
\item{object}{an object of the class \emph{gnm}.}

\item{...}{another objects of the class \emph{gnm}.}

\item{verbose}{an (optional) logical indicating if should the report of results be printed. As default, \code{verbose}
is set to TRUE.}
}
\value{
A matrix with the following three columns:
\tabular{ll}{
\code{Chi} \tab The value of the statistic of the test,\cr
\tab \cr
\code{Df}\tab The number of degrees of freedom,\cr
\tab \cr
\code{Pr(>Chi)} \tab The \emph{p}-value of the \code{test}-type test computed using the Chi-square distribution.\cr
}
}
\description{
Allows to use the likelihood-ratio test to compare nested models in generalized nonlinear models.
}
\examples{
###### Example: The effects of fertilizers on coastal Bermuda grass
data(Grass)
fit1 <- gnm(Yield ~ b0 + b1/(Nitrogen + a1) + b2/(Phosphorus + a2) + b3/(Potassium + a3),
            family=gaussian(inverse), start=c(b0=0.1,b1=13,b2=1,b3=1,a1=45,a2=15,a3=30), data=Grass)

fit2 <- update(fit1,Yield ~ I(b0 + b2/(Phosphorus + a2) + b3/(Potassium + a3)),
               start=c(b0=0.1,b2=1,b3=1,a2=15,a3=30))

anova(fit2,fit1)

}
