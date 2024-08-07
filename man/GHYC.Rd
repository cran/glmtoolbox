% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geeglm.R
\name{GHYC}
\alias{GHYC}
\title{Gosho-Hamada-Yoshimura's Criterion for Generalized Estimating Equations}
\usage{
GHYC(..., verbose = TRUE)
}
\arguments{
\item{...}{one or several objects of the class \emph{glmgee}.}

\item{verbose}{an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.}
}
\value{
A \code{data.frame} with the values of the GHYC for each \emph{glmgee} object in the input.
}
\description{
Computes the Gosho-Hamada-Yoshimura's criterion (GHYC) for one or more objects of the class glmgee.
}
\examples{
###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
mod1 <- size ~ poly(days,4) + treat
fit1 <- glmgee(mod1, id=tree, family=Gamma("log"), data=spruces)
fit2 <- update(fit1, corstr="AR-1")
fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
fit4 <- update(fit1, corstr="Exchangeable")
GHYC(fit1, fit2, fit3, fit4)

###### Example 2: Treatment for severe postnatal depression
mod2 <- depressd ~ visit + group
fit1 <- glmgee(mod2, id=subj, family=binomial("logit"), data=depression)
fit2 <- update(fit1, corstr="AR-1")
fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
fit4 <- update(fit1, corstr="Exchangeable")
GHYC(fit1, fit2, fit3, fit4)

###### Example 3: Treatment for severe postnatal depression (2)
mod3 <- dep ~ visit*group
fit1 <- glmgee(mod3, id=subj, family=gaussian("identity"), data=depression)
fit2 <- update(fit1, corstr="AR-1")
fit3 <- update(fit1, corstr="Exchangeable")
GHYC(fit1, fit2, fit3)

}
\references{
Gosho, M. and Hamada, C. and Yoshimura, I. (2011) Criterion for the Selection
of a Working Correlation Structure in the Generalized Estimating Equation Approach for
Longitudinal Balanced Data. \emph{Communications in Statistics — Theory and Methods} 40,
3839-3856.

Gosho, M. (2014) Criteria to Select a Working Correlation Structure in SAS.
\emph{Journal of Statistical Software, Code Snippets} 57, 1548-7660.
}
\seealso{
\link{QIC}, \link{CIC}, \link{RJC}, \link{AGPC}, \link{SGPC}
}
