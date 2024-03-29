% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glms.R
\name{estequa.glm}
\alias{estequa.glm}
\title{Estimating Equations in Generalized Linear Models}
\usage{
\method{estequa}{glm}(object, ...)
}
\arguments{
\item{object}{an object of the class glm which is obtained from the fit of a generalized linear model.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
A vector with the value of the estimating equations evaluated at the parameter estimates and the observed data.
}
\description{
Extracts estimating equations evaluated at the parameter estimates and the observed data for a generalized linear model fitted to the data.
}
\examples{
## Example 1
Auto <- ISLR::Auto
mod <- mpg ~ cylinders + displacement + acceleration + origin + horsepower*weight
fit1 <- glm(mod, family=inverse.gaussian("log"), data=Auto)
estequa(fit1)

## Example 2
burn1000 <- aplore3::burn1000
burn1000 <- within(burn1000, death <- factor(death, levels=c("Dead","Alive")))
mod2 <- death ~ age + gender + race + tbsa + inh_inj + flame + age*inh_inj + tbsa*inh_inj
fit2 <- glm(mod2, family=binomial("logit"), data=burn1000)
estequa(fit2)

## Example 3
data(skincancer)
fit3 <- glm(cases ~ offset(log(population)) + city + age, family=poisson("log"), data=skincancer)
estequa(fit3)
}
