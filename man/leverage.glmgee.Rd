% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geeglm.R
\name{leverage.glmgee}
\alias{leverage.glmgee}
\title{Leverage for Generalized Estimating Equations}
\usage{
\method{leverage}{glmgee}(
  object,
  level = c("clusters", "observations"),
  plot.it = FALSE,
  identify,
  ...
)
}
\arguments{
\item{object}{an object of class \emph{glmgee}.}

\item{level}{an (optional) character string indicating the level for which the leverage measures are required. The options are: cluster-level ("clusters") and observation-level ("observations"). By default, \code{level} is set to be "clusters".}

\item{plot.it}{an (optional) logical indicating if the plot of the measures of leverage are required or just the data matrix in which that plot is based. By default, \code{plot.it} is set to be FALSE.}

\item{identify}{an (optional) integer indicating the number of (\code{level=``clusters''}) or observations (\code{level=``observations''}) to identify on the plot of the leverage measures. This is only appropriate if \code{plot.it} is specified to be \code{TRUE}.}

\item{...}{further arguments passed to or from other methods. If \code{plot.it} is specified to be \code{TRUE} then \code{...} may be used to include graphical parameters to customize the plot. For example,  \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.}
}
\value{
A vector with the values of the leverage measures with so many rows as clusters (\code{level=``clusters''}) or observations (\code{level=``observations''}) in the sample.
}
\description{
Computes and, optionally, displays a graph of the leverage measures at the cluster- and observation-level.
}
\examples{

###### Example 1: Tests of Auditory Perception in Children with OME
OME <- MASS::OME
mod <- cbind(Correct, Trials-Correct) ~ Loud + Age + OME
fit1 <- glmgee(mod, family = binomial(cloglog), id = ID, corstr = "Exchangeable", data = OME)
leverage(fit1,level="clusters",plot.it=TRUE)

###### Example 2: Guidelines for Urinary Incontinence Discussion and Evaluation
mod <- bothered ~ gender + age + dayacc + severe + toilet
fit2 <- glmgee(mod, family=binomial(logit), id=practice, corstr="Exchangeable", data=GUIDE)
leverage(fit2,level="clusters",plot.it=TRUE)
leverage(fit2,level="observations",plot.it=TRUE)

}
\references{
Preisser, J.S. and Qaqish, B.F. (1996). Deletion diagnostics for generalised estimating equations. \emph{Biometrika}, 83, 551-562.

Hammill, B.G. and Preisser, J.S. (2006). A SAS/IML software program for GEE and regression diagnostics. \emph{Computational Statistics & Data Analysis}, 51, 1197-1212.
}
