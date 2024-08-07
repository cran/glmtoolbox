% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{localInfluence.gnm}
\alias{localInfluence.gnm}
\title{Local Influence for Generalized Nonlinear Models}
\usage{
\method{localInfluence}{gnm}(
  object,
  type = c("total", "local"),
  perturbation = c("case-weight", "response"),
  coefs,
  plot.it = FALSE,
  identify,
  ...
)
}
\arguments{
\item{object}{an object of class \emph{gnm}.}

\item{type}{an (optional) character string indicating the type of approach to study the
local influence. The options are: the absolute value of the elements of the eigenvector which corresponds to the maximum absolute eigenvalue ("local"); and the absolute value of the elements of the main diagonal ("total"). As default, \code{type} is set to "total".}

\item{perturbation}{an (optional) character string indicating the perturbation scheme
to apply. The options are: case weight perturbation of observations ("case-weight") and perturbation of response ("response"). As default, \code{perturbation} is set to "case-weight".}

\item{coefs}{an (optional) character string which (partially) match with the names of
some of the parameters in the 'linear' predictor.}

\item{plot.it}{an (optional) logical indicating if the plot of the measures of local
influence is required or just the data matrix in which that plot is based. As default,
\code{plot.it} is set to \code{FALSE}.}

\item{identify}{an (optional) integer indicating the number of observations to identify
on the plot of the measures of local influence. This is only appropriate if
\code{plot.it=TRUE}.}

\item{...}{further arguments passed to or from other methods. If \code{plot.it=TRUE}
then \code{...} may be used to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.}
}
\value{
A matrix as many rows as observations in the sample and one column with the values of the measures of local influence.
}
\description{
Computes some measures and, optionally, display	graphs of them to perform
influence analysis based on the approaches described by Cook (1986).
}
\examples{
###### Example 1: The effects of fertilizers on coastal Bermuda grass
data(Grass)
fit1 <- gnm(Yield ~ b0 + b1/(Nitrogen + a1) + b2/(Phosphorus + a2) + b3/(Potassium + a3),
            family=gaussian(inverse), start=c(b0=0.1,b1=13,b2=1,b3=1,a1=45,a2=15,a3=30), data=Grass)

localInfluence(fit1, type="local", perturbation="case-weight", plot.it=TRUE, col="red",
               lty=1, lwd=1, col.lab="blue", col.axis="blue", col.main="black", family="mono")

###### Example 2: Assay of an Insecticide with a Synergist
data(Melanopus)
fit2 <- gnm(Killed/Exposed ~ b0 + b1*log(Insecticide-a1) + b2*Synergist/(a2 + Synergist),
            family=binomial(logit), weights=Exposed, start=c(b0=-3,b1=1.2,a1=1.7,b2=1.7,a2=2),
		   data=Melanopus)

### Local Influence just for the parameter "b1"
localInfluence(fit2, type="local", perturbation="case-weight", plot.it=TRUE, coefs="b1", col="red",
               lty=1, lwd=1, col.lab="blue", col.axis="blue", col.main="black", family="mono")

###### Example 3: Developmental rate of Drosophila melanogaster
data(Drosophila)
fit3 <- gnm(Duration ~ b0 + b1*Temp + b2/(Temp-a), family=Gamma(log),
            start=c(b0=3,b1=-0.25,b2=-210,a=55), weights=Size, data=Drosophila)

localInfluence(fit3, type="total", perturbation="case-weight", plot.it=TRUE, col="red",
               lty=1, lwd=1, col.lab="blue", col.axis="blue", col.main="black", family="mono")

###### Example 4: Radioimmunological Assay of Cortisol
data(Cortisol)
fit4 <- gnm(Y ~ b0 + (b1-b0)/(1 + exp(b2+ b3*lDose))^b4, family=Gamma(identity),
            start=c(b0=130,b1=2800,b2=3,b3=3,b4=0.5), data=Cortisol)

localInfluence(fit4, type="total", perturbation="case-weight", plot.it=TRUE, col="red",
               lty=1, lwd=1, col.lab="blue", col.axis="blue", col.main="black", family="mono")

}
\references{
Cook D. (1986) Assessment of Local Influence. \emph{Journal of the Royal Statistical Society: Series B (Methodological)} 48, 133-155.

Thomas W., Cook D. (1989) Assessing Influence on Regression Coefficients in Generalized Linear Models. \emph{Biometrika} 76, 741-749.
}
