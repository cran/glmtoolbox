% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geeglm.R
\name{CIC}
\alias{CIC}
\title{Correlation Information Criterion for Generalized Estimating Equations}
\usage{
CIC(..., verbose = TRUE, digits = max(3, getOption("digits") - 2))
}
\arguments{
\item{...}{one or several objects of the class \emph{glmgee}.}

\item{verbose}{an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.}

\item{digits}{an (optional) integer indicating the number of digits to print. As default, \code{digits} is set to \code{max(3, getOption("digits") - 2)}.}
}
\value{
A \code{data.frame} with the values of the CIC for each \emph{glmgee} object in the input.
}
\description{
Computes the Correlation Information Criterion (CIC) for one or more objects of the class glmgee.
}
\examples{
###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
data(spruces)
mod1 <- size ~ poly(days,4) + treat
fit1 <- glmgee(mod1, id=tree, family=Gamma(log), data=spruces)
fit2 <- update(fit1, corstr="AR-M-dependent")
fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
fit4 <- update(fit1, corstr="Exchangeable")
CIC(fit1, fit2, fit3, fit4)

###### Example 2: Treatment for severe postnatal depression
data(depression)
mod2 <- depressd ~ visit + group
fit1 <- glmgee(mod2, id=subj, family=binomial(logit), data=depression)
fit2 <- update(fit1, corstr="AR-M-dependent")
fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
fit4 <- update(fit1, corstr="Exchangeable")
CIC(fit1, fit2, fit3, fit4)

###### Example 3: Treatment for severe postnatal depression (2)
mod3 <- dep ~ visit*group
fit1 <- glmgee(mod3, id=subj, family=gaussian(identity), data=depression)
fit2 <- update(fit1, corstr="AR-M-dependent")
fit3 <- update(fit1, corstr="Exchangeable")
CIC(fit1, fit2, fit3)

}
\references{
Hin L.-Y., Wang Y.-G. (2009) Working-Correlation-Structure Identification in Generalized Estimating Equations. \emph{Statistics in Medicine}, 28:642-658.

Hin L.-Y., Carey V.J., Wang Y.-G. (2007) Criteria for Working–Correlation–Structure Selection in GEE:
Assessment via Simulation. \emph{The American Statistician} 61:360–364.

\href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
\emph{The R Journal} 15:105-133.}
}
\seealso{
\link{QIC}, \link{GHYC}, \link{RJC}, \link{AGPC}, \link{SGPC}
}
