% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overglm3.R
\name{cooks.distance.zeroinflation}
\alias{cooks.distance.zeroinflation}
\title{Cook's Distance for Regression Models to deal with Zero-Excess in Count Data}
\usage{
\method{cooks.distance}{zeroinflation}(
  model,
  submodel = c("counts", "zeros", "full"),
  plot.it = FALSE,
  coefs,
  identify,
  ...
)
}
\arguments{
\item{model}{an object of class \emph{zeroinflation}.}

\item{submodel}{an (optional) character string which allows to specify the model: "counts", "zeros" or "full". By default,
\code{submodel} is set to "counts".}

\item{plot.it}{an (optional) logical indicating if the plot is required or just the data matrix in which that
plot is based. As default, \code{plot.it} is set to \code{FALSE}.}

\item{coefs}{an (optional) character string which (partially) match with the names of some model parameters.}

\item{identify}{an (optional) integer indicating the number of individuals to identify on the plot of the Cook's
distance. This is only appropriate if \code{plot.it=TRUE}.}

\item{...}{further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used
to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main},
\code{sub}, \code{xlab}, \code{ylab}.}
}
\value{
A matrix as many rows as individuals in the sample and one column with the values of the Cook's distance.
}
\description{
Produces an approximation, better known as the \emph{one-step approximation}, of the Cook's distance,
which is aimed to measure the effect on the estimates of the parameters in the linear predictor of deleting each
observation in turn. This function also can produce an index plot of the Cook's distance for all parameters in
the linear predictor or for some subset of them (via the argument \code{coefs}).
}
\details{
The Cook's distance consists of the \emph{distance} between two estimates of the parameters in the linear
predictor using a metric based on the (estimate of the) variance-covariance matrix. The first one set of estimates
is computed from a dataset including all individuals, and the second one is computed from a dataset in which the
\emph{i}-th individual is excluded. To avoid computational burden, the second set of estimates is replaced by its
\emph{one-step approximation}. See the \link{dfbeta.zeroinflation} documentation.
}
\examples{

####### Example 1: Self diagnozed ear infections in swimmers
data(swimmers)
fit <- zeroinf(infections ~ frequency + location, family="nb1(log)", data=swimmers)

### Cook's distance for all parameters in the "counts" model
cooks.distance(fit, submodel="counts", plot.it=TRUE, col="red", lty=1, lwd=1,
         col.lab="blue", col.axis="blue", col.main="black", family="mono", cex=0.8)

### Cook's distance for all parameters in the "zeros" model
cooks.distance(fit, submodel="zeros", plot.it=TRUE, col="red", lty=1, lwd=1,
         col.lab="blue", col.axis="blue", col.main="black", family="mono", cex=0.8)

}
