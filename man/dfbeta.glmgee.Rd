% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geeglm.R
\name{dfbeta.glmgee}
\alias{dfbeta.glmgee}
\title{Dfbeta for Generalized Estimating Equations}
\usage{
\method{dfbeta}{glmgee}(
  model,
  level = c("clusters", "observations"),
  method = c("Preisser-Qaqish", "full"),
  coefs,
  identify,
  ...
)
}
\arguments{
\item{model}{an object of class \emph{glmgee}.}

\item{level}{an (optional) character string indicating the level for which the Dfbeta statistic is required. The options are: cluster-level ("clusters") and observation-level ("observations"). As default, \code{level} is set to "clusters".}

\item{method}{an (optional) character string indicating the method of calculation for the \emph{one-step approximation}. The options are: the \emph{one-step approximation} described by Preisser and Qaqish (1996) in which the working-correlation matrix is assumed to be known ("Preisser-Qaqish"); and the "authentic" \emph{one-step approximation} ("full"). As default, \code{method} is set to "Preisser-Qaqish".}

\item{coefs}{an (optional) character string which (partially) match with the names of some parameters in the linear predictor.}

\item{identify}{an (optional) integer indicating the number of clusters/observations to identify on the plot of the Dfbeta statistic. This is only appropriate if \code{coefs} is specified.}

\item{...}{further arguments passed to or from other methods. If \code{coefs} is specified then \code{...} may be used to include graphical parameters to customize the plot. For example,  \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.}
}
\value{
A matrix with so many rows as clusters/observations in the sample and so many
columns as parameters in the linear predictor. For clusters, the \eqn{i}-th row of that matrix corresponds to the
difference between the estimates of the parameters in the linear predictor using all clustersand the \emph{one-step approximation} of those estimates when the \emph{i}-th cluster is excluded from the dataset.
}
\description{
Produces an approximation, better known as the \emph{one-step approximation},
of the effect on the parameter estimates of deleting each cluster/observation in turn. This function also can produce
an index plot of the Dfbeta Statistic for some parameters via the argument \code{coefs}.
}
\details{
The \emph{one-step approximation} (with the \code{method} "full") of the estimates of the parameters in the linear
predictor of a GEE when the \emph{i}-th cluster is excluded from the dataset is given by the
vector obtained as the result of the first iteration of the fitting algorithm of that GEE
when it is performed using:  (1) a dataset in which the \emph{i}-th cluster is excluded; and
(2) a starting value which is the solution to the same GEE but based on the dataset inluding all clusters.
}
\examples{
###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
data(spruces)
mod1 <- size ~ poly(days,4) + treat
fit1 <- glmgee(mod1, id=tree, family=Gamma(log), corstr="AR-M-dependent", data=spruces)
dfbs1 <- dfbeta(fit1, method="full", coefs="treat", col="red", lty=1, lwd=1, col.lab="blue",
         col.axis="blue", col.main="black", family="mono", cex=0.8, main="treat")

### Calculation by hand of dfbeta for the tree labeled by "N1T01"
onestep1 <- glmgee(mod1, id=tree, family=Gamma(log), corstr="AR-M-dependent",
            data=spruces, start=coef(fit1), subset=c(tree!="N1T01"), maxit=1)

coef(fit1)-coef(onestep1)
dfbs1[rownames(dfbs1)=="N1T01",]

###### Example 2: Treatment for severe postnatal depression
data(depression)
mod2 <- depressd ~ visit + group
fit2 <- glmgee(mod2, id=subj, family=binomial(logit), corstr="AR-M-dependent",
               data=depression)

dfbs2 <- dfbeta(fit2, method="full", coefs="group" ,col="red", lty=1, lwd=1, col.lab="blue",
         col.axis="blue", col.main="black", family="mono", cex=0.8, main="group")

### Calculation by hand of dfbeta for the woman labeled by "18"
onestep2 <- glmgee(mod2, id=subj, family=binomial(logit), corstr="AR-M-dependent",
            data=depression, start=coef(fit2), subset=c(subj!=18), maxit=1)

coef(fit2)-coef(onestep2)
dfbs2[rownames(dfbs2)==18,]

###### Example 3: Treatment for severe postnatal depression (2)
mod3 <- dep ~ visit*group
fit3 <- glmgee(mod3, id=subj, family=gaussian(identity), corstr="AR-M-dependent",
               data=depression)

dfbs3 <- dfbeta(fit3, method="full", coefs="visit:group" ,col="red", lty=1, lwd=1, col.lab="blue",
         col.axis="blue", col.main="black", family="mono", cex=0.8, main="visit:group")

### Calculation by hand of dfbeta for the woman labeled by "18"
onestep3 <- glmgee(mod3, id=subj, family=gaussian(identity), corstr="AR-M-dependent",
            data=depression, start=coef(fit3), subset=c(subj!=18), maxit=1)

coef(fit3)-coef(onestep3)
dfbs3[rownames(dfbs3)==18,]

}
\references{
Pregibon D. (1981). Logistic regression diagnostics. \emph{The Annals of Statistics} 9, 705-724.

Preisser J.S., Qaqish B.F. (1996) Deletion diagnostics for generalised estimating equations.
\emph{Biometrika} 83:551–562.

Hammill B.G., Preisser J.S. (2006) A SAS/IML software program for GEE and regression diagnostics.
\emph{Computational Statistics & Data Analysis} 51:1197-1212.

\href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
\emph{The R Journal} 15:105-133.}
}
