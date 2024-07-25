#'
#' @title Alternatives to the Poisson and Binomial Regression Models under the presence of Overdispersion.
#' @description Allows to fit regression models based on the negative binomial, beta-binomial, and random-clumped binomial.
#' distributions, which are alternatives to the Poisson and binomial regression models under the presence of overdispersion.
#' @param formula a \code{formula} expression of the form \code{response ~ x1 + x2 + ...}, which is a symbolic description
#'        of the linear predictor of the model to be fitted to the data.
#' @param family A character string that allows you to specify the
#'        distribution describing the response variable. In addition,
#'        it allows you to specify the link function to be used in the
#'        model for \eqn{\mu}. The following distributions are
#'        supported: negative binomial I ("nb1"), negative binomial II
#'        ("nb2"), negative binomial ("nbf"), zero-truncated negative
#'        binomial I ("ztnb1"), zero-truncated negative binomial II
#'        ("ztnb2"), zero-truncated negative binomial ("ztnbf"),
#'        zero-truncated poisson ("ztpoi"), beta-binomial ("bb") and
#'        random-clumped binomial ("rcb"). Link functions available for
#'        these models are the same as those available for Poisson and
#'        binomial models via \link{glm}. See \link{family} documentation.
#' @param offset this can be used to specify an \emph{a priori} known component to be included in the linear predictor during fitting. This should be \code{NULL} or a numeric vector of length equal to the number of cases.
#' @param weights an (optional) vector of positive "prior weights" to be used in the fitting process. The length of
#'        \code{weights} should be the same as the number of observations.
#' @param data an (optional) \code{data frame} in which to look for variables involved in the \code{formula} expression,
#'        as well as for variables specified in the arguments \code{weights} and \code{subset}.
#' @param subset an (optional) vector specifying a subset of individuals to be used in the fitting process.
#' @param start an (optional) vector of starting values for the parameters in the linear predictor.
#' @param reltol an (optional) positive value which represents the \emph{relative convergence tolerance} for the BFGS method in \link{optim}.
#'        As default, \code{reltol} is set to 1e-13.
#' @param na.action a function which indicates what should happen when the data contain NAs. By default \code{na.action}
#'        is set to \code{na.omit()}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return an object of class \emph{overglm} in which the main results of the model fitted to the data are stored, i.e., a
#' list with components including
#' \tabular{ll}{
#' \code{coefficients} \tab a vector containing the parameter estimates,\cr
#' \tab \cr
#' \code{fitted.values}\tab a vector containing the estimates of \eqn{\mu_1,\ldots,\mu_n},\cr
#' \tab \cr
#' \code{start}        \tab a vector containing the starting values used,\cr
#' \tab \cr
#' \code{prior.weights}\tab a vector containing the case weights used,\cr
#' \tab \cr
#' \code{offset}       \tab a vector containing the offset used, \cr
#' \tab \cr
#' \code{terms}        \tab an object containing the terms objects,\cr
#' \tab \cr
#' \code{loglik}       \tab the value of the log-likelihood function avaliated at the parameter estimates,\cr
#' \tab \cr
#' \code{estfun}       \tab a vector containing the estimating functions evaluated at the parameter estimates\cr
#'                     \tab and the observed data,\cr
#' \tab \cr
#' \code{formula}      \tab the formula,\cr
#' \tab \cr
#' \code{levels}       \tab the levels of the categorical regressors,\cr
#' \tab \cr
#' \code{contrasts}    \tab an object containing the contrasts corresponding to levels,\cr
#' \tab \cr
#' \code{converged}    \tab a logical indicating successful convergence,\cr
#' \tab \cr
#' \code{model}        \tab the full model frame,\cr
#' \tab \cr
#' \code{y}            \tab the response count vector,\cr
#' \tab \cr
#' \code{family}       \tab an object containing the \link{family} object used,\cr
#' \tab \cr
#' \code{linear.predictors} \tab a vector containing the estimates of \eqn{g(\mu_1),\ldots,g(\mu_n)},\cr
#' \tab \cr
#' \code{R}            \tab a matrix with the Cholesky decomposition of the inverse of the variance-covariance\cr
#'                     \tab matrix of all parameters in the model,\cr
#' \tab \cr
#' \code{call}         \tab the original function call.\cr
#' }
#'
#' @details
#' The negative binomial distribution can be obtained as mixture of the Poisson and Gamma distributions. If
#' \eqn{Y | \lambda} ~ Poisson\eqn{(\lambda)}, where E\eqn{(Y | \lambda)=} Var\eqn{(Y | \lambda)=\lambda}, and
#' \eqn{\lambda} ~ Gamma\eqn{(\theta,\nu)}, in which E\eqn{(\lambda)=\theta} and Var\eqn{(\lambda)=\nu\theta^2}, then
#' \eqn{Y} is distributed according to the negative binomial distribution. As follows, some special cases are described:
#'
#' (1) If \eqn{\theta=\mu} and \eqn{\nu=\phi} then \eqn{Y} ~ Negative Binomial I,
#' E\eqn{(Y)=\mu} and Var\eqn{(Y)=\mu(1 + \phi\mu)}.
#'
#' (2) If \eqn{\theta=\mu} and \eqn{\nu=\phi/\mu} then \eqn{Y} ~ Negative Binomial II,
#' E\eqn{(Y)=\mu} and Var\eqn{(Y)=\mu(1 +\phi)}.
#'
#' (3) If \eqn{\theta=\mu} and \eqn{\nu=\phi\mu^\tau} then \eqn{Y} ~ Negative Binomial,
#' E\eqn{(Y)=\mu} and Var\eqn{(Y)=\mu(1 +\phi\mu^{\tau+1})}.
#'
#' Therefore, the regression models based on the negative binomial and
#' zero-truncated negative binomial distributions are alternatives,
#' under overdispersion, to those based on the Poisson and
#' zero-truncated Poisson distributions, respectively.
#'
#' The beta-binomial distribution can be obtained as a mixture of the binomial and beta distributions. If
#' \eqn{mY | \pi} ~ Binomial\eqn{(m,\pi)}, where E\eqn{(Y | \pi)=\pi} and Var\eqn{(Y | \pi)=m^{-1}\pi(1-\pi)},
#' and \eqn{\pi} ~ Beta\eqn{(\mu,\phi)}, in which E\eqn{(\pi)=\mu} and Var\eqn{(\pi)=(\phi+1)^{-1}\mu(1-\mu)},
#' with \eqn{\phi>0}, then \eqn{mY} ~ Beta-Binomial\eqn{(m,\mu,\phi)}, so that E\eqn{(Y)=\mu} and
#' Var\eqn{(Y)=m^{-1}\mu(1-\mu)[1 + (\phi+1)^{-1}(m-1)]}. Therefore, the regression model based on the
#' beta-binomial distribution is an alternative, under overdispersion, to the binomial regression model.
#'
#' The random-clumped binomial distribution can be obtained as a mixture of the binomial and Bernoulli distributions. If
#' \eqn{mY | \pi} ~ Binomial\eqn{(m,\pi)}, where E\eqn{(Y | \pi)=\pi} and Var\eqn{(Y | \pi)=m^{-1}\pi(1-\pi)},
#' whereas \eqn{\pi=(1-\phi)\mu + \phi} with probability \eqn{\mu}, and \eqn{\pi=(1-\phi)\mu} with probability \eqn{1-\mu},
#' in which E\eqn{(\pi)=\mu} and Var\eqn{(\pi)=\phi^{2}\mu(1-\mu)}, with \eqn{\phi \in (0,1)}, then \eqn{mY} ~ Random-clumped
#' Binomial\eqn{(m,\mu,\phi)}, so that E\eqn{(Y)=\mu} and Var\eqn{(Y)=m^{-1}\mu(1-\mu)[1 + \phi^{2}(m-1)]}. Therefore,
#' the regression model based on the random-clumped binomial distribution is an alternative, under
#' overdispersion, to the binomial regression model.
#'
#' In all cases, even where the response variable is described by a
#' zero-truncated distribution, the fitted model describes the way in
#' which \eqn{\mu} is dependent on some covariates. Parameter estimation
#' is performed using the maximum likelihood method. The model
#' parameters are estimated by maximizing the log-likelihood function
#' through the BFGS method available in the routine \link{optim}. The
#' accuracy and speed of the BFGS method are increased because the call
#' to the routine \link{optim} is performed using analytical instead
#' of the numerical  derivatives. The variance-covariance matrix
#' estimate is obtained as being minus the inverse of the (analytical)
#' hessian matrix evaluated at the parameter estimates and the observed
#' data.
#'
#' A set of standard extractor functions for fitted model objects is available for objects of class  \emph{zeroinflation},
#' including methods to the generic functions such as \code{print}, \code{summary},	\code{model.matrix}, \code{estequa},
#' \code{coef}, \code{vcov}, \code{logLik}, \code{fitted}, \code{confint}, \code{AIC}, \code{BIC} and \code{predict}.
#' In addition, the model fitted to the	data may be assessed using functions such as \link{anova.overglm},
#' \link{residuals.overglm}, \link{dfbeta.overglm}, \link{cooks.distance.overglm}, \link{localInfluence.overglm},
#' \link{gvif.overglm} and \link{envelope.overglm}. The variable selection may be accomplished using the routine
#' \link{stepCriterion.overglm}.
#'
#' @export overglm
#' @seealso \link{zeroalt}, \link{zeroinf}
#' @examples
#' ### Example 1: Ability of retinyl acetate to prevent mammary cancer in rats
#' data(mammary)
#' fit1 <- overglm(tumors ~ group, family="nb1(identity)", data=mammary)
#' summary(fit1)
#'
#' ### Example 2: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit2 <- overglm(infections ~ frequency + location, family="nb1(log)", data=swimmers)
#' summary(fit2)
#'
#' ### Example 3: Urinary tract infections in HIV-infected men
#' data(uti)
#' fit3 <- overglm(episodes ~ cd4 + offset(log(time)), family="nb1(log)", data = uti)
#' summary(fit3)
#'
#' ### Example 4: Article production by graduate students in biochemistry PhD programs
#' bioChemists <- pscl::bioChemists
#' fit4 <- overglm(art ~ fem + kid5 + ment, family="nb1(log)", data = bioChemists)
#' summary(fit4)
#'
#' ### Example 5: Agents to stimulate cellular differentiation
#' data(cellular)
#' fit5 <- overglm(cbind(cells,200-cells) ~ tnf + ifn, family="bb(logit)", data=cellular)
#' summary(fit5)
#'
#' ### Example 6: Teratogenic effects of phenytoin and trichloropropene oxide
#' data(ossification)
#' model6 <- cbind(fetuses,litter-fetuses) ~ pht + tcpo
#' fit6 <- overglm(model6, family="rcb(cloglog)", data=ossification)
#' summary(fit6)
#'
#' ### Example 7: Germination of orobanche seeds
#' data(orobanche)
#' model7 <- cbind(germinated,seeds-germinated) ~ specie + extract
#' fit7 <- overglm(model7, family="rcb(cloglog)", data=orobanche)
#' summary(fit7)
#'
#' @references Crowder M. (1978) Beta-binomial anova for proportions, \emph{Journal of the Royal Statistical
#' Society Series C (Applied Statistics)} 27, 34-37.
#' @references Lawless J.F. (1987) Negative binomial and mixed poisson regression, \emph{The Canadian Journal
#' of Statistics} 15, 209-225.
#' @references Morel J.G., Neerchal N.K. (1997) Clustered binary logistic regression in teratology data
#' using a finite mixture distribution, \emph{Statistics in Medicine} 16, 2843-2853.
#' @references Morel J.G., Nagaraj N.K. (2012) \emph{Overdispersion Models in SAS}. SAS Institute Inc.,
#' Cary, North Carolina, USA.
#'

overglm <- function(formula, offset, family="nb1(log)", weights, data, subset, na.action=na.omit(), reltol=1e-13, start=NULL, ...){
  if(missingArg(data)) data <- environment(formula)
  mmf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "offset", "weights"), names(mmf), 0)
  mmf <- mmf[c(1,m)]
  mmf$drop.unused.levels <- TRUE
  mmf[[1]] <- as.name("model.frame")
  mmf <- eval(mmf, parent.frame())
  mt <- attr(mmf, "terms")
  y <- as.matrix(model.response(mmf, "any"))

  temp <- strsplit(gsub(" |link|=|'|'","",tolower(family)),"[()]")[[1]]
  family <- temp[1]
  if(!(family %in% c("bb","rcb","nb1","nb2","nbf","ztpoi","ztnb1","ztnb2","ztnbf")))
    stop("Only 'bb', 'rcb', 'nb1', 'nb2', 'nbf', 'ztpoi', 'ztnb1', 'ztnb2' and 'ztnbf' families are supported!!",call.=FALSE)
  if(is.na(temp[2])){
    if(family %in% c("bb","rcb")) link <- "logit" else link <- "log"
  }else link <- temp[2]
  zero.trunc <- FALSE
  if(family %in% c("bb","rcb")) familyf <- binomial(link)
  else{
    familyf <- poisson(link)
    if(family %in% c("ztpoi","ztnb1","ztnb2","ztnbf")){
      zero.trunc <- TRUE
      family <- substr(family,3,5)
    }
    else zero.trunc <- FALSE
    if(zero.trunc & any(y==0)) stop("Under zero-truncated models only positive counts are allowed!!",call.=FALSE)
  }
  if(family %in% c("bb","rcb")){
    m <- as.matrix(y[,1] + y[,2])
    y <- as.matrix(y[,1])
  }
  weights <- as.vector(model.weights(mmf))
  offset <- as.vector(model.offset(mmf))
  X <- model.matrix(mt, mmf)
  p <- ncol(X)
  n <- nrow(X)
  if(is.null(offset)) offs <- matrix(0,n,1) else offs <- as.matrix(offset)
  if(is.null(weights)) weights <- matrix(1,n,1) else weights <- as.matrix(weights)

  if(family=="rcb"){
    ep <- 1
    objective <- function(theta){
      mu <- familyf$linkinv(X%*%theta[1:p] + offs)
      phi <- exp(theta[p + 1])/(1 + exp(theta[p + 1]))
      l <- log(mu*dbinom(y,m,(1 - phi)*mu + phi) + (1 - mu)*dbinom(y,m,(1 - phi)*mu))
      return(sum(weights*l))
    }
    score <- function(theta){
      eta <- X%*%theta[1:p] + offs
      phi <- exp(theta[p + 1])/(1 + exp(theta[p + 1]))
      mu <- familyf$linkinv(eta)
      dmudeta <- familyf$mu.eta(eta)
      a1 <- mu*dbinom(y,m,(1 - phi)*mu + phi); a2 <- (1 - mu)*dbinom(y,m,(1 - phi)*mu);
      a3 <- y/((1 - phi)*mu + phi); a4 <- (m - y)/(1-(1 - phi)*mu - phi);
      a5 <- y/((1 - phi)*mu); a6 <- (m - y)/(1 - (1 - phi)*mu)
      a <- (a1*((a3 - a4)*(1 - phi) + 1/mu) + a2*((a5 - a6)*(1 - phi) - 1/(1 - mu)))/(a1 + a2)
      b <- (a1*(a3 - a4)*(1 - mu) + a2*(a6 - a5)*mu)*phi*(1 - phi)/(a1 + a2)
      return(c(crossprod(X,weights*a*dmudeta),sum(weights*b)))
    }
    theta0 <- function(start=NULL){
      if(is.null(start)) betas <- suppressWarnings(glm.fit(y=cbind(y,m - y),x=X,offset=offs,weights=weights,family=familyf)$coefficients)
      else betas <- start
      mus <- familyf$linkinv(X%*%betas + offs)
      l0 <- mean(abs((y - m*mus)^2/(m*mus*(1 - mus)) - 1)/ifelse(m==1,1,m - 1))
      return(c(betas,sqrt(abs(log(l0/(1-l0))))))
    }
    hess <- function(theta){
      eta <- X%*%theta[1:p] + offs
      phi <- exp(theta[p + 1])/(1 + exp(theta[p + 1]))
      mu <- familyf$linkinv(eta)
      dmudeta <- familyf$mu.eta(eta)
      dmu2deta2 <- grad(familyf$mu.eta,eta)
      hessiana <- matrix(0,ncol(X) + 1,ncol(X) + 1)
      a1 <- mu*dbinom(y,m,(1 - phi)*mu + phi); a2 <- (1 - mu)*dbinom(y,m,(1 - phi)*mu)
      a3 <- y/((1 - phi)*mu + phi);a4 <- (m - y)/(1 - (1 - phi)*mu - phi)
      a5 <- y/((1 - phi)*mu);a6 <- (m - y)/(1 - (1 - phi)*mu)
      a <- (a1*((a3 - a4)*(1-phi) + 1/mu) + a2*((a5 - a6)*(1-phi) - 1/(1 - mu)))/(a1 + a2)
      b <- (a1*(a3 - a4)*(1-mu) + a2*(a6 - a5)*mu)*phi*(1 - phi)/(a1 + a2)
      aa <- (a1*((a3 - a4)*(1 - phi) + 1/mu)^2 + a2*((a5 - a6)*(1 - phi) - 1/(1 - mu))^2 -
               a1*((1 - phi)^2*(y/((1 - phi)*mu + phi)^2 + (m - y)/(1 - (1 - phi)*mu - phi)^2) + 1/mu^2) -
               a2*((1 - phi)^2*(y/((1 - phi)*mu)^2 + (m - y)/(1 - (1 - phi)*mu)^2) + 1/(1 - mu)^2))/(a1 + a2) - a^2
      ab <- (-a1*y/((1 - phi)*mu + phi)^2 + a2*(m - y)/(1 - (1 - phi)*mu)^2 +
               a1*((a3 - a4)*(1 - phi) + 1/mu)*(a3 - a4)*(1 - mu) +
               a2*((a5 - a6)*(1 - phi) - 1/(1 - mu))*(a6 - a5)*mu)*phi*(1 - phi)/(a1 + a2) - a*b
      bb <- (a1*((a3 - a4)*(1 - mu))^2*phi*(1 - phi) + a2*((a6 - a5)*mu)^2*phi*(1 - phi) +
               a1*((a3 - a4)*(1 - mu)*(1 - 2*phi) - phi*(1 - phi)*(1 - mu)^2*(y/((1 - phi)*mu + phi)^2 +
                                                                                (m - y)/(1 - (1 - phi)*mu - phi)^2)) + a2*((a6 - a5)*mu*(1 - 2*phi) - phi*(1 - phi)*mu^2*(y/((1 - phi)*mu)^2 +
                                                                                                                                                                            (m - y)/(1 - (1 - phi)*mu)^2)))*phi*(1 - phi)/(a1 + a2) - b^2
      hessiana[1:p,1:p] <- crossprod(X,matrix(weights*(aa*dmudeta^2 + a*dmu2deta2),n,p)*X)
      hessiana[p + 1,1:ncol(X)] <- hessiana[1:p,p + 1] <- crossprod(X,weights*ab*dmudeta)
      hessiana[p + 1,p+1] <- sum(weights*bb)
      return(hessiana)
    }
  }
  if(family=="bb"){
    ep <- 1
    objective <- function(theta){
      eta <- X%*%theta[1:p] + offs
      phi <- exp(theta[p + 1])
      mu <- familyf$linkinv(eta)
      l <- (Lgamma(m + 1) - Lgamma(y + 1) - Lgamma(m - y + 1) + Lgamma(1/phi) + Lgamma(y + mu/phi) +
              Lgamma(m - y + (1 - mu)/phi) - Lgamma(m + 1/phi) - Lgamma(mu/phi) - Lgamma((1 - mu)/phi))
      return(sum(weights*l))
    }
    score <- function(theta){
      eta <- X%*%theta[1:p] + offs
      phi <- exp(theta[p + 1])
      mu <- familyf$linkinv(eta)
      dmudeta <- familyf$mu.eta(eta)
      dmu2deta2 <- grad(familyf$mu.eta,eta)
      c1 <- Digamma(y + mu/phi) - Digamma(mu/phi)
      c2 <- Digamma((1 - mu)/phi) - Digamma(m - y + (1 - mu)/phi)
      c3 <- - c1*mu + c2*(1 - mu) - Digamma(1/phi) + Digamma(m + 1/phi)
      return(c(crossprod(X,weights*familyf$mu.eta(eta)*(c1 + c2)/phi),sum(weights*c3/phi)))
    }
    theta0 <- function(start=NULL){
      if(is.null(start)) betas <- suppressWarnings(glm.fit(y=cbind(y,m - y),x=X,offset=offset,weights=weights,family=familyf)$coefficients)
      else betas <- start
      mus <- familyf$linkinv(X%*%betas + offs)
      l0 <- mean(abs((y - m*mus)^2/(m*mus*(1 - mus)) - 1)/ifelse(m==1,1,m - 1))
      return(c(betas,log(l0/(1-l0))))
    }
    hess <- function(theta){
      eta <- X%*%theta[1:p] + offs
      phi <- exp(theta[p + 1])
      mu <- familyf$linkinv(eta)
      dmudeta <- familyf$mu.eta(eta)
      dmu2deta2 <- grad(familyf$mu.eta,eta)
      c1 <- Digamma(y + mu/phi) - Digamma(mu/phi)
      c2 <- Digamma((1 - mu)/phi) - Digamma(m - y + (1 - mu)/phi)
      c3 <- -c1*mu + c2*(1 - mu) - Digamma(1/phi) + Digamma(m + 1/phi)
      c4 <-  psigamma(y + mu/phi,1) - psigamma(mu/phi,1)
      c5 <- -psigamma((1 - mu)/phi,1) + psigamma(m - y + (1 - mu)/phi,1)
      hessiana <- matrix(0,p + 1,p + 1)
      hessiana[1:p,1:p] <- crossprod(X,matrix(weights*((c4 + c5)*dmudeta^2/phi^2 + (c1 + c2)*dmu2deta2/phi),n,p)*X)
      hessiana[1:p,p+1] <- hessiana[p + 1,1:p] <- crossprod(X,weights*dmudeta*((1 - mu)*c5 - mu*c4 - phi*(c1 + c2))/phi^2)
      hessiana[p+1,p+1] <- sum(weights*((mu^2*c4 + (1 - mu)^2*c5 + psigamma(1/phi,1) - psigamma(m + 1/phi,1))/phi^2 - c3/phi))
      return(hessiana)
    }
  }

  if(family=="poi"){
    ep <- 0
    objective <- function(theta){
      mu <- familyf$linkinv(X%*%theta[1:p] + offs)
      if(zero.trunc) k0 <- - log(1 - exp(-mu)) else k0 <- 0
      l <- - mu + y*log(mu) - Lgamma(y + 1) + k0
      return(sum(weights*l))
    }
    score <- function(theta){
      eta <- X%*%theta[1:p] + offs
      mu <- familyf$linkinv(eta)
      dmudeta <- familyf$mu.eta(eta)
      if(zero.trunc) k <- - exp(-mu)/(1 - exp(-mu)) else k <- 0
      s <- (y/mu - 1 + k)*dmudeta
      return(crossprod(X,weights*s))
    }
    theta0 <- function(start=NULL){
      if(!is.null(start)) betas <- start
      else betas <- suppressWarnings(glm.fit(y=y,x=X,offset=offs,weights=weights,family=familyf)$coefficients)
      return(betas)
    }
    hess <- function(theta){
      eta <- X%*%theta[1:p] + offs
      mu <- familyf$linkinv(eta)
      dmudeta <- familyf$mu.eta(eta)
      dmu2deta2 <- grad(familyf$mu.eta,eta)
      k1 <- k2 <- 0
      if(zero.trunc){
        k1 <- - exp(-mu)/(1 - exp(-mu)); k2 <- exp(-mu)/(1 - exp(-mu))^2
      }
      ss <- (-y/mu^2 + k2)*dmudeta^2 + (y/mu - 1 + k1)*dmu2deta2
      hessiana <- crossprod(X,matrix(weights*ss,n,p)*X)
      return(hessiana)
    }
  }

  if(family %in% c("nb1","nb2","nbf")){
    ep <- 2
    if(family=="nb1"){
      ep <- 1; tau <- 0
    }
    if(family=="nb2"){
      ep <- 1; tau <- -1
    }
    objective <- function(theta){
      mu <- familyf$linkinv(X%*%theta[1:p] + offs)
      phi <- exp(theta[p + 1])
      if(family=="nbf") tau <- theta[p + ep]
      u <- phi*mu^tau; v <- u*mu
      if(zero.trunc) k1 <- - log(1 - (v + 1)^(-1/u)) else k1 <- 0
      l <- Lgamma(y + 1/u) - Lgamma(y + 1) - Lgamma(1/u) + y*log(v) - (y + 1/u)*log(v + 1) + k1
      return(sum(weights*l))
    }
    score <- function(theta){
      eta <- X%*%theta[1:p] + offs
      phi <- exp(theta[p + 1])
      if(family=="nbf") tau <- theta[p + ep]
      mu <- familyf$linkinv(eta)
      u <- phi*mu^tau; v <- u*mu; k1 <- k2 <- 0
      dmudeta <- familyf$mu.eta(eta)
      E1 <- - (Digamma(y + 1/u) - log(v + 1) - Digamma(1/u))/u
      E2 <-   (y - mu)/(v + 1)
      if(zero.trunc){
        k1 <- (log(v + 1)*tau/v - (tau + 1)/(v + 1))/((v + 1)^(1/u) - 1)
        k2 <- (log(v + 1)/u - mu/(v + 1))/((v + 1)^(1/u) - 1)
      }
      s1 <- (tau*E1 + (tau + 1)*E2 + k1*mu)*dmudeta/mu
      s2 <- E1 + E2 + k2; s3 <- s2*log(mu)
      if(family=="nbf") return(c(crossprod(X,weights*s1),sum(weights*s2),sum(weights*s3)))
      else return(c(crossprod(X,weights*s1),sum(weights*s2)))
    }
    theta0 <- function(start=NULL){
      if(!is.null(start)) betas <- start
      else betas <- suppressWarnings(glm.fit(y=y,x=X,offset=offs,weights=weights,family=familyf)$coefficients)
      mus <- familyf$linkinv(X%*%betas + offs)
      if(family=="nbf"){
        fik <- lm((y - mus)^2 ~ -1 + I(mus^2),offset=mus)
        betas <- c(betas,log(abs(coef(fik))),0)
      }
      if(family=="nb1"){
        fik <- lm((y - mus)^2 ~ -1 + I(mus^2),offset=mus)
        betas <- c(betas,log(abs(coef(fik))))
      }
      if(family=="nb2"){
        fik <- lm((y - mus)^2 ~ -1 + mus)
        betas <- c(betas,log(abs(coef(fik)-1)))
      }
      return(betas)
    }
    hess <- function(theta){
      eta <- X%*%theta[1:p] + offs
      phi <- exp(theta[p + 1])
      if(family=="nbf") tau <- theta[p + ep]
      mu <- familyf$linkinv(eta)
      u <- phi*mu^tau; v <- u*mu
      dmudeta <- familyf$mu.eta(eta)
      dmu2deta2 <- grad(familyf$mu.eta,eta)
      E1 <- - (Digamma(y + 1/u) - log(v + 1) - Digamma(1/u))/u
      E2 <- (y - mu)/(v + 1)
      hessiana <- matrix(0,ncol(X) + ep,ncol(X) + ep)
      psis <- psigamma(y + 1/u,1) - psigamma(1/u,1)
      cc1 <- - E1 + psis/u^2 + mu/(v + 1)
      cc2 <- - (y - mu)*v/(v + 1)^2
      k1 <- k11 <- k12 <- k13 <- k22 <- k23 <- k24 <- 0
      if(zero.trunc){
        f <- (v + 1)^(1/u)
        f1 <- log(v + 1)*tau/v - (tau + 1)/(v + 1)
        f2 <- log(v + 1)/u - mu/(v + 1)
        f22 <- mu/(v + 1) - log(v + 1)/u + v*mu/(v + 1)^2
        f11 <- (tau + 1)^2*u/(v + 1)^2 - log(v + 1)*tau*(tau + 1)/(mu*v) + tau*(tau + 1)/(mu*(v + 1))
        f12 <- (tau + 1)*mu^(tau + 1)*phi/(v + 1)^2 - log(v + 1)*tau/v + tau/(v + 1)
        f13 <- ((tau + 1)*v*log(mu) - v - 1)/(v + 1)^2 + v*log(v + 1)*(1 - tau*log(mu))/v^2 + phi*tau*(tau + 1)*log(mu)/(v*(v + 1))
        k11 <- ((f - 1)*f11 + f*f1^2)/(f - 1)^2; k12 <- ((f - 1)*f12 + f*f2*f1)/(f - 1)^2; k13 <- ((f - 1)*f13 + f*f2*f1*log(mu))/(f - 1)^2
        k22 <- ((f - 1)*f22 + f*f2^2)/(f - 1)^2; k23 <- ((f - 1)*f22 + f*f2^2)/(f - 1)^2; k1 <-  f1/(f - 1); k24 <- f2/(f - 1)
      }
      aa <- - (tau/mu)*(tau*E1/mu - tau*psis/(u^2*mu) - (tau + 1)/(v + 1)) + k11 -
        ((tau + 1)/mu)*(u*(mu + (y - mu)*(tau + 1)) + 1)/(v + 1)^2 - E2*(tau + 1)/mu^2 - tau*E1/mu^2
      hessiana[1:ncol(X),1:ncol(X)] <- crossprod(X,matrix(weights*(aa*dmudeta^2 + (tau*E1 + (tau + 1)*E2 + k1*mu)*dmu2deta2/mu),n,p)*X)
      hessiana[ncol(X) + 1,1:ncol(X)] <- hessiana[1:ncol(X),ncol(X) + 1] <- crossprod(X,weights*(tau*cc1 + (tau + 1)*cc2 + mu*k12)*dmudeta/mu)
      hessiana[ncol(X) + 1,ncol(X) + 1] <- sum(weights*(cc1 + cc2 + k22))
      if(family=="nbf"){
        hessiana[ncol(X) + 2,1:ncol(X)] <- hessiana[1:ncol(X),ncol(X) + 2] <- crossprod(X,weights*(log(mu)*(tau*cc1 + (tau + 1)*cc2) + E1 + E2 + mu*k13)*dmudeta/mu)
        hessiana[ncol(X) + 2,1:ncol(X)] <- hessiana[1:ncol(X),ncol(X) + 2] <- crossprod(X,weights*(log(mu)*(tau*cc1 + (tau + 1)*cc2 + mu*k12) + E1 + E2 + k24)*dmudeta/mu)
        hessiana[ncol(X) + 2,ncol(X) + 1] <- hessiana[ncol(X) + 1,ncol(X) + 2] <- sum(weights*(cc1 + cc2 + k23)*log(mu))
        hessiana[ncol(X) + 2,ncol(X) + 2] <- sum(weights*(cc1 + cc2 + k23)*log(mu)^2)
      }
      return(hessiana)
    }
  }
  thetanew <- theta0(start)
  salida <- optim(thetanew,objective,score,method="BFGS",control=list(reltol=reltol,fnscale=-1))
  if(salida$convergence != 0) warning("Convergence not achieved!!",call.=FALSE)
  theta_hat <- matrix(salida$par,p + ep,1)
  if(ep==0) rownames(theta_hat) <- colnames(X)
  if(ep==1) rownames(theta_hat) <- c(colnames(X),"log(phi)")
  if(ep==2) rownames(theta_hat) <- c(colnames(X),"log(phi)","tau")
  if(family=="rcb") rownames(theta_hat) <- c(colnames(X),"logit(phi)")
  eta <- X%*%theta_hat[1:p] + offs
  mu <- familyf$linkinv(eta)
  estfun <- matrix(score(theta_hat),p + ep,1)
  rownames(estfun) <- rownames(theta_hat)
  familyf$family <- family
  if(family %in% c("bb","rcb")){
    yres <- cbind(y,m - y); colnames(yres) <- c("successes","failures")
  }else{
    yres <- y; colnames(yres) <- "response"
  }
  thetanew <- matrix(thetanew,length(thetanew),1)
  rownames(thetanew) <- rownames(theta_hat)
  R <- try(chol(-hess(theta_hat)),silent=TRUE)
  if(!is.matrix(R)){
    warning("Estimate of variance-covariance matrix is not positive definite",call.=FALSE)
    R <- solve(-hess(theta_hat)); attr(R,"pd") <- FALSE
  }else{
    attr(R,"pd") <- TRUE
  }
  #print(all.equal(grad(objective,thetanew),score(thetanew)))
  #print(all.equal(hessian(objective,salida$par),hess(salida$par)))
  #print(hessian(objective,salida$par))
  #print(hess(salida$par))
  out_ <- list(coefficients=theta_hat,fitted.values=mu,linear.predictors=eta,y=yres,prior.weights=weights,
               formula=formula,call=match.call(),estfun=estfun,logLik=objective(theta_hat),zero.trunc=zero.trunc,
               R=R,converged=ifelse(salida$convergence==0,TRUE,FALSE),model=mmf,
               data=data,terms=mt,score=score,hess=hess,family=familyf,offset=offs,start=thetanew,
               df.residual=n-length(theta_hat),levels=.getXlevels(attr(mmf,"terms"),mmf),
               contrasts=attr(X,"contrasts"),parms=c(p,ep))
  class(out_) <- "overglm"
  return(out_)
}

#' @title Zero-Altered Regression Models to deal with Zero-Excess in Count Data
#' @description Allows to fit a zero-altered (Poisson or negative binomial) regression model to deal with zero-excess in count data.
#' @param formula a \code{Formula} expression of the form \code{response ~ x1 + x2 + ...| z1 + z2 + ...},
#'        which is a symbolic description of the linear predictors of the models to be fitted to
#'        \eqn{\mu} and \eqn{\pi}, respectively.	See \link[Formula]{Formula} documentation. If a formula
#'        of the form \code{response ~ x1 + x2 + ...} is supplied, the same regressors are
#'        employed in both components. This is equivalent to \code{response ~ x1 + x2 + ...| x1 + x2 + ...}.
#' @param family an (optional) character string that allows you to specify the distribution
#'        to describe the response variable, as well as the link function to be used in
#'        the model for \eqn{\mu}. The following distributions are supported:
#'        (zero-altered) negative binomial I ("nb1"), (zero-altered) negative binomial II
#'        ("nb2"), (zero-altered) negative binomial ("nbf"), and (zero-altered) poisson
#'        ("poi"). Link functions are the same as those available in Poisson models via
#'        \link{glm}. See \link{family} documentation. As default, \code{family} is set to
#'        be Poisson with log link.
#' @param zero.link an (optional) character string which allows to specify the link function to be used in the model for \eqn{\pi}.
#' 		  Link functions available are the same than those available in binomial models via \link{glm}. See \link{family} documentation.
#' 		  As default, \code{zero.link} is set to "logit".
#' @param offset this can be used to specify an \emph{a priori} known component to be included in the linear predictor during fitting. This should be \code{NULL} or a numeric vector of length equal to the number of cases.
#' @param weights an (optional) vector of positive "prior weights" to be used in the fitting process. The length of
#'        \code{weights} should be the same as the number of observations. As default, \code{weights} is set to a vector of 1s.
#' @param data an (optional) \code{data frame} in which to look for variables involved in the \code{formula} expression,
#'        as well as for variables specified in the arguments \code{weights} and \code{subset}.
#' @param subset an (optional) vector specifying a subset of observations to be used in the fitting process.
#' @param start an (optional) list with two components named "counts" and "zeros", which allows to specify the starting values to be used in the
#'        iterative process to obtain the estimates of the parameters in the linear predictors of the models for \eqn{\mu}
#'        and \eqn{\pi}, respectively.
#' @param reltol an (optional) positive value which represents the \emph{relative convergence tolerance} for the BFGS method in \link{optim}.
#'        As default, \code{reltol} is set to 1e-13.
#' @param na.action a function which indicates what should happen when the data contain NAs. By default \code{na.action} is set to \code{na.omit()}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return
#' An object of class  \emph{zeroinflation} in which the main results of the model fitted to the data are stored, i.e., a list with components including
#' \tabular{ll}{
#' \code{coefficients} \tab a list with elements "counts" and "zeros" containing the parameter estimates\cr
#'                     \tab from the respective models,\cr
#' \tab \cr
#' \code{fitted.values}\tab a list with elements "counts" and "zeros" containing the estimates of \eqn{\mu_1,\ldots,\mu_n}\cr
#'                     \tab and \eqn{\pi_1,\ldots,\pi_n}, respectively,\cr
#' \tab \cr
#' \code{start}        \tab a vector containing the starting values for all parameters in the model,\cr
#' \tab \cr
#' \code{prior.weights}\tab a vector containing the case weights used,\cr
#' \tab \cr
#' \code{offset}       \tab a list with elements "counts" and "zeros" containing the offset vectors, if any, \cr
#'                     \tab from the respective models,\cr
#' \tab \cr
#' \code{terms}        \tab a list with elements "counts", "zeros" and "full" containing the terms objects for \cr
#'                     \tab the respective models,\cr
#' \tab \cr
#' \code{loglik}       \tab the value of the log-likelihood function avaliated at the parameter estimates and\cr
#'                     \tab the observed data,\cr
#' \tab \cr
#' \code{estfun}       \tab a list with elements "counts" and "zeros" containing the estimating functions \cr
#'                     \tab evaluated at the parameter estimates and the observed data for the respective models,\cr
#' \tab \cr
#' \code{formula}      \tab the formula,\cr
#' \tab \cr
#' \code{levels}       \tab the levels of the categorical regressors,\cr
#' \tab \cr
#' \code{contrasts}    \tab a list with elements "counts" and "zeros" containing the contrasts corresponding\cr
#'                     \tab to levels from the respective models,\cr
#' \tab \cr
#' \code{converged}    \tab a logical indicating successful convergence,\cr
#' \tab \cr
#' \code{model}        \tab the full model frame,\cr
#' \tab \cr
#' \code{y}            \tab the response count vector,\cr
#' \tab \cr
#' \code{family}       \tab a list with elements "counts" and "zeros" containing the \link{family} objects used\cr
#'                     \tab  in the respective models,\cr
#' \tab \cr
#' \code{linear.predictors} \tab  a list with elements "counts" and "zeros" containing the estimates of \cr
#'                          \tab  \eqn{g(\mu_1),\ldots,g(\mu_n)} and \eqn{h(\pi_1),\ldots,h(\pi_n)}, respectively,\cr
#' \tab \cr
#' \code{R}            \tab a matrix with the Cholesky decomposition of the inverse of the variance-covariance\cr
#'                     \tab matrix of all parameters in the model,\cr
#' \tab \cr
#' \code{call}         \tab the original function call.\cr
#' }
#' @details
#' The zero-altered count distributions, also called \emph{hurdle models}, may be obtained as the mixture between
#' a zero-truncated count distribution and the Bernoulli distribution. Indeed, if \eqn{Y} is a count random variable
#' such that \eqn{Y|\nu=1} is 0 with probability 1
#' and \eqn{Y|\nu=0} ~ ZTP\eqn{(\mu)}, where \eqn{\nu} ~ Bernoulli\eqn{(\pi)}, then
#' \eqn{Y} is distributed according to the Zero-Altered Poisson distribution, denoted here as
#' ZAP\eqn{(\mu,\pi)}.
#'
#' Similarly, if \eqn{Y} is a count random variable such that \eqn{Y|\nu=1} is 0 with probability 1
#' and \eqn{Y|\nu=0} ~ ZTNB\eqn{(\mu,\phi,\tau)}, where \eqn{\nu} ~ Bernoulli\eqn{(\pi)}, then
#' \eqn{Y} is distributed according to the Zero-Altered Negative Binomial distribution, denoted here as
#' ZANB\eqn{(\mu,\phi,\tau,\pi)}. The Zero-Altered Negative Binomial I \eqn{(\mu,\phi,\pi)} and
#' Zero-Altered Negative Binomial II \eqn{(\mu,\phi,\pi)} distributions are special cases of ZANB when
#' \eqn{\tau=0} and \eqn{\tau=-1}, respectively.
#'
#' The "counts" model may be expressed as \eqn{g(\mu_i)=x_i^{\top}\beta} for \eqn{i=1,\ldots,n}, where
#' \eqn{g(\cdot)} is the link function specified at the argument \code{family}. Similarly, the "zeros" model may
#' be expressed as \eqn{h(\pi_i)=z_i^{\top}\gamma} for \eqn{i=1,\ldots,n}, where \eqn{h(\cdot)} is the
#' link function specified at the argument \code{zero.link}. Parameter estimation is
#' performed using the maximum likelihood method. The parameter vector \eqn{\gamma} is
#' estimated by applying the routine \link{glm.fit}, where a binary-response model
#' (\eqn{1} or "success" if \code{response}=0 and \eqn{0} or "fail" if \code{response}>0)
#' is fitted. Then, the rest of the model parameters are estimated by maximizing the
#' log-likelihood function based on the zero-truncated count distribution through the
#' BFGS method available in the routine \link{optim}. The accuracy and speed of the BFGS
#' method are increased because the call to the routine \link{optim} is performed using
#' the analytical instead of the numerical derivatives. The variance-covariance matrix
#' estimate is obtained as being minus the inverse of the (analytical) hessian matrix
#' evaluated at the parameter estimates and the observed data.

#' A set of standard extractor functions for fitted model objects is available for objects
#' of class  \emph{zeroinflation}, including methods to the generic functions such as
#' \link{print}, \link{summary}, \link{model.matrix}, \link{estequa},
#' \link{coef}, \link{vcov}, \link{logLik}, \link{fitted}, \link{confint}, \link{AIC}, \link{BIC} and
#' \link{predict}. In addition, the model fitted to the	data may be assessed using functions such as
#' \link{anova.zeroinflation}, \link{residuals.zeroinflation}, \link{dfbeta.zeroinflation},
#' \link{cooks.distance.zeroinflation} and \link{envelope.zeroinflation}.
#'
#' @examples
#' ####### Example 1: Roots Produced by the Columnar Apple Cultivar Trajan
#' data(Trajan)
#' fit1 <- zeroalt(roots ~ photoperiod, family="nbf(log)", zero.link="logit", data=Trajan)
#' summary(fit1)
#'
#' ####### Example 2: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit2 <- zeroalt(infections ~ frequency | location, family="nb1(log)", data=swimmers)
#' summary(fit2)
#'
#' ####### Example 3: Article production by graduate students in biochemistry PhD programs
#' bioChemists <- pscl::bioChemists
#' fit3 <- zeroalt(art ~ fem + kid5 + ment, family="nb1(log)", data = bioChemists)
#' summary(fit3)
#'
#' @seealso \link{overglm}, \link{zeroinf}
#' @export zeroalt
#' @references Cameron A.C., Trivedi P.K. (1998) \emph{Regression Analysis of Count Data}. New York:
#'             Cambridge University Press.
#' @references Mullahy J. (1986) Specification and Testing of Some Modified Count Data Models. \emph{Journal of
#'             Econometrics} 33, 341–365.
zeroalt <- function(formula, data, offset, subset, na.action=na.omit(), weights, family="poi(log)",
                    zero.link=c("logit", "probit", "cloglog", "cauchit", "log"), reltol=1e-13,
                    start=list(counts=NULL,zeros=NULL),...){
  if(missing(data)) data <- environment(formula)
  zero.link <- match.arg(zero.link)
  mmf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "offset", "subset", "na.action", "weights"), names(mmf), 0)
  mmf <- mmf[c(1,m)]
  mmf$drop.unused.levels <- TRUE
  mmf[[1]] <- as.name("model.frame")
  mmf$formula <- Formula(formula)
  mmf <- eval(mmf, parent.frame())

  y <- as.matrix(model.part(Formula(formula), data = mmf, lhs=1)); zeros <- y==0
  if(any(y != floor(y)) | any(y < 0)) stop("There are negative or non-integer values in the response variable!!",call.=FALSE)
  if(all(y > 0)) stop("Zero values in the response variable are required!!",call.=FALSE)
  mx <- model.part(Formula(formula), data = mmf, rhs = 1, terms = TRUE)
  X <- model.matrix(mx, data = mmf); p <- ncol(X);nro <- nrow(X)
  if(!is.null(start$counts) & length(start$counts) != p) stop("Incorrect size of the starting values vector for the counts model!!",call.=FALSE)
  offsx <- model.offset(mx)
  if(is.null(offsx)) offsx <- matrix(0,nro,1)
  if(suppressWarnings(formula(Formula(formula),lhs=0,rhs=2)=="~0")) mz <- mx
  else mz <- model.part(Formula(formula), data = mmf, rhs = 2, terms = TRUE)
  Z <- model.matrix(mz, data = mmf); q <- ncol(Z)
  if(!is.null(start$zeros) & length(start$zeros) != q) stop("Incorrect size of the starting values vector for the zeros model!!",call.=FALSE)
  offsz <- model.offset(mz)
  if(is.null(offsz)) offsz <- matrix(0,nro,1)
  weights <- model.weights(mmf)
  if(is.null(weights)) weights <- matrix(1,nro,1)
  if(any(weights <= 0)) stop("Only positive weights are allowed!!",call.=FALSE)
  temp <- strsplit(gsub(" |link|=|'|'","",tolower(family)),"[()]")[[1]]
  family <- temp[1]
  if(is.na(temp[2])) link="log" else link <- temp[2]
  familyx <- poisson(link); familyz <- binomial(zero.link)
  X2 <- as.matrix(X[!zeros,]);offsx2 <- offsx[!zeros];y2 <- y[!zeros];weights2 <- weights[!zeros]

  if(family=="poi"){
    ep <- 0
    objective <- function(theta){
      mu <- familyx$linkinv(X%*%theta[1:p] + offsx)
      if(is.null(known)) pi <- familyz$linkinv(Z%*%theta[-c(1:p)] + offsz)
      else pi <- familyz$linkinv(Z%*%known + offsz)
      l <- ifelse(zeros,log(pi),log(1 - pi) - mu + y*log(mu) - lgamma(y + 1) - log(1 - exp(-mu)))
      return(sum(weights*l))
    }
    score <- function(theta){
      etax <- X2%*%theta[1:p] + offsx2; mu <- familyx$linkinv(etax);
      dmudetax <- familyx$mu.eta(etax);s1 <- y2/mu - 1 - exp(-mu)/(1 - exp(-mu))
      if(is.null(known)){
        etaz <- Z%*%theta[-c(1:p)] + offsz
        pi <- familyz$linkinv(etaz);dpidetaz <- familyz$mu.eta(etaz)
        s2 <- ifelse(zeros,1/pi,-1/(1 - pi))
        return(c(crossprod(X2,weights2*s1*dmudetax),crossprod(Z,weights*s2*dpidetaz)))
      }else return(crossprod(X2,weights2*s1*dmudetax))
    }
    theta0 <- function(start){
      if(!is.null(start$counts)) betas <- start$counts
      else betas <- suppressWarnings(glm.fit(y=y[!zeros],x=X[!zeros,],offset=offsx[!zeros],weights=weights[!zeros],family=familyx)$coefficients)
      mus <- familyx$linkinv(X%*%betas + offsx)
      gammas <- suppressWarnings(glm.fit(y=ifelse(y==0,1,0),x=Z,offset=offsz,weights=weights,family=familyz,start=start$zeros))
      out <- matrix(c(betas,gammas$coefficients),length(betas) + length(gammas$coefficients),1)
      attr(out,"converged") <- gammas$converged
      return(out)
    }
    hess <- function(theta){
      etax <- X2%*%theta[1:p] + offsx2; etaz <- Z%*%theta[-c(1:p)] + offsz
      mu <- familyx$linkinv(etax); pi <- familyz$linkinv(etaz)
      dmudetax <- familyx$mu.eta(etax); dpidetaz <- familyz$mu.eta(etaz)
      dmu2detax2 <- grad(familyx$mu.eta,etax); dpi2detaz2 <- grad(familyz$mu.eta,etaz)
      aa <- - y2/mu^2 + exp(-mu)/(1 - exp(-mu))^2; a <- y2/mu - 1 - exp(-mu)/(1 - exp(-mu))
      bb <- ifelse(zeros,-1/pi^2,-1/(1 - pi)^2); b <- ifelse(zeros,1/pi,-1/(1 - pi))
      hessiana <- matrix(0,p + q,p + q)
      hessiana[1:p,1:p] <- crossprod(X2,matrix(weights2*(aa*dmudetax^2 + a*dmu2detax2),nrow(X2),ncol(X2))*X2)
      hessiana[-c(1:p),-c(1:p)] <- crossprod(Z,matrix(weights*(bb*dpidetaz^2 + b*dpi2detaz2),nrow(Z),q)*Z)
      return(hessiana)
    }
  }
  if(family %in% c("nb1","nb2","nbf")){
    ep <- 2;
    if(family=="nb1"){
      ep <- 1; tau <- 0
    }
    if(family=="nb2"){
      ep <- 1; tau <- -1
    }
    objective <- function(theta){
      mu <- familyx$linkinv(X%*%theta[1:p] + offsx)
      phi <- exp(theta[p + 1])
      if(family=="nbf") tau <- theta[p + ep]
      if(is.null(known)) pi <- familyz$linkinv(Z%*%theta[-c(1:(p + ep))] + offsz)
      else pi <- familyz$linkinv(Z%*%known + offsz)
      u <- phi*mu^tau; v <- u*mu; l <- ifelse(zeros,log(pi),log(1 - pi) + Lgamma(y + 1/u) -
                                                Lgamma(1/u) - log(1 - (v + 1)^(-1/u)) - Lgamma(y + 1) + y*log(v) - (y + 1/u)*log(v + 1))
      return(sum(weights*l))
    }
    score <- function(theta){
      etax <- X2%*%theta[1:p] + offsx2
      phi <- exp(theta[p + 1])
      if(family=="nbf") tau <- theta[p + ep]
      mu <- familyx$linkinv(etax)
      if(is.null(known)){
        etaz <- Z%*%theta[-c(1:(p + ep))] + offsz
        pi <- familyz$linkinv(etaz)
        dpidetaz <- familyz$mu.eta(etaz)
      }
      else pi <- familyz$linkinv(Z%*%known + offsz)
      dmudetax <- familyx$mu.eta(etax)
      u <- phi*mu^tau; v <- u*mu; k0 <- 1/((v + 1)^(1/u) - 1)
      E1 <- -(Digamma(y2 + 1/u) - log(v + 1) - Digamma(1/u))/u
      E2 <- (y2 - mu)/(v + 1); k1 <- k0*(log(v + 1)*tau/v - (tau + 1)/(v + 1))
      k2 <- k0*(log(v + 1)/u - mu/(v + 1))
      out_ <- c(crossprod(X2,weights2*(tau*E1 + (tau + 1)*E2 + k1*mu)*dmudetax/mu),sum(weights2*(E1 + E2 + k2)))
      if(family=="nbf") out_ <- c(out_,sum(weights2*(E1 + E2 + k2)*log(mu)))
      if(is.null(known)){
        s2 <- ifelse(zeros,1/pi,-1/(1 - pi))
        return(c(out_,crossprod(Z,weights*s2*dpidetaz)))
      }else return(out_)
    }
    theta0 <- function(start){
      if(!is.null(start$counts)) betas <- start$counts
      else betas <- suppressWarnings(glm.fit(y=y[!zeros],x=X[!zeros,],offset=offsx[!zeros],weights=weights[!zeros],family=familyx)$coefficients)
      mus <- familyx$linkinv(X%*%betas + offsx)
      if(family=="nbf"){
        fik <- lm((y - mus)^2 ~ -1 + I(mus^2),offset=mus)
        betas <- c(betas,log(abs(coef(fik))),0)
      }
      if(family=="nb1"){
        fik <- lm((y - mus)^2 ~ -1 + I(mus^2),offset=mus)
        betas <- c(betas,log(abs(coef(fik))))
      }
      if(family=="nb2"){
        fik <- lm((y - mus)^2 ~ -1 + mus)
        betas <- c(betas,log(abs(coef(fik)-1)))
      }
      gammas <- suppressWarnings(glm.fit(y=ifelse(y==0,1,0),x=Z,offset=offsz,weights=weights,family=familyz,start=start$zeros))
      out <- matrix(c(betas,gammas$coefficients),length(betas) + length(gammas$coefficients),1)
      attr(out,"converged") <- gammas$converged
      return(out)
    }
    hess <- function(theta){
      etax <- X2%*%theta[1:p] + offsx2;phi <- exp(theta[p + 1])
      if(family=="nbf") tau <- theta[p + ep]
      mu <- familyx$linkinv(etax); etaz <- Z%*%theta[-c(1:(p + ep))] + offsz
      pi <- familyz$linkinv(etaz); dmudetax <- familyx$mu.eta(etax); dpidetaz <- familyz$mu.eta(etaz)
      u <- phi*mu^tau; v <- u*mu
      dmudetax <- familyx$mu.eta(etax); dpidetaz <- familyz$mu.eta(etaz)
      dmu2detax2 <- grad(familyx$mu.eta,etax); dpi2detaz2 <- grad(familyz$mu.eta,etaz)
      hessiana <- matrix(0,p + ep + q,p + ep + q)
      E1 <- -(digamma(y2 + 1/u) - log(v + 1) - digamma(1/u))/u; E2 <- (y2 - mu)/(v + 1)
      psi1 <- psigamma(y2 + 1/u,1); psi2 <- psigamma(1/u,1)
      cc1 <- - E1 + (psi1 - psi2)/u^2 + mu/(v + 1)
      cc2 <- - (y2 - mu)*v/(v + 1)^2; f <- (v + 1)^(1/u)
      f1 <- log(v + 1)*tau/v - (tau + 1)/(v + 1); f2 <- log(v + 1)/u - mu/(v + 1)
      f22 <- mu/(v + 1) - log(v + 1)/u + mu*v/(v + 1)^2
      f11 <- (tau + 1)^2*u/(v + 1)^2 - log(v + 1)*tau*(tau + 1)/(mu*v) + tau*(tau + 1)/(mu*(v + 1))
      f12 <- (tau + 1)*v/(v + 1)^2 - log(v + 1)*tau/v + tau/(v + 1)
      f13 <- ((tau + 1)*v*log(mu) - v - 1)/(v + 1)^2 + v*log(v + 1)*(1 - tau*log(mu))/v^2 + phi*tau*(tau + 1)*log(mu)/(v*(v + 1))
      k11 <- ((f-1)*f11 + f*f1^2)/(f-1)^2;k12 <- ((f-1)*f12 + f*f2*f1)/(f-1)^2;k13 <- ((f-1)*f13 + f*f2*f1*log(mu))/(f-1)^2
      k22 <- ((f-1)*f22 + f*f2^2)/(f-1)^2;k23 <- ((f-1)*f22 + f*f2^2)/(f-1)^2;k1 <- f1/(f-1)
      aa <- -(tau/mu)*(tau*E1/mu - tau*(psi1 - psi2)/(mu*u^2) - (tau + 1)/(v + 1)) + k11 -
        ((tau + 1)/mu)*(u*(mu + (y2 - mu)*(tau + 1)) + 1)/(v + 1)^2 - E2*(tau + 1)/mu^2 - tau*E1/mu^2
      hessiana[1:p,1:p] <- crossprod(X2,matrix(weights2*(aa*dmudetax^2 + (tau*E1 + (tau + 1)*E2 + k1*mu)*dmu2detax2/mu),nrow(X2),ncol(X2))*X2)
      hessiana[p + 1,1:p] <- hessiana[1:p,p + 1] <- crossprod(X2,weights2*(tau*cc1 + (tau + 1)*cc2 + mu*k12)*dmudetax/mu)
      hessiana[p + 1,p + 1] <- sum(weights2*(cc1 + cc2 + k22))
      if(family=="nbf"){
        hessiana[p + ep,1:p] <- hessiana[1:p,p + ep] <- crossprod(X2,weights2*(log(mu)*(tau*cc1 + (tau + 1)*cc2 + mu*k12) + E1 + E2 + f2/(f-1))*dmudetax/mu)
        hessiana[p + ep,p + 1] <- hessiana[p+1,p+2] <- sum(weights2*(cc1 + cc2 + k23)*log(mu))
        hessiana[p + ep,p + ep] <- sum(weights2*(cc1 + cc2 + k23)*log(mu)^2)
      }
      bb <- ifelse(zeros,-1/pi^2,-1/(1 - pi)^2); b <- ifelse(zeros,1/pi,-1/(1 - pi))
      hessiana[-c(1:(p + ep)),-c(1:(p + ep))] <- crossprod(Z,matrix(weights*(bb*dpidetaz^2 + b*dpi2detaz2),nrow(Z),q)*Z)
      return(hessiana)
    }
  }
  thetanew <- theta0(start)
  known <- thetanew[-c(1:(p + ep))]
  salida <- optim(thetanew[c(1:(p + ep))],objective,score,method="BFGS",control=list(reltol=reltol,fnscale=-1))
  salida$par <- c(salida$par,known); known <- NULL
  if(salida$convergence != 0 | attr(thetanew,"converged")!=TRUE) stop("Convergence not achieved!!",call.=FALSE)
  theta_hat <- list(counts=matrix(salida$par[1:(p + ep)],nrow=p + ep,1),zeros=matrix(salida$par[-c(1:(p + ep))],nrow=q,1))
  nomb <- c(colnames(X),"log(phi)","tau"); rownames(theta_hat$counts) <- nomb[1:length(theta_hat$counts)]
  rownames(theta_hat$zeros) <- colnames(Z)
  etax <- X%*%theta_hat$counts[1:p] + offsx
  etaz <- Z%*%theta_hat$zeros + offsz
  mu <- familyx$linkinv(etax);pi <- familyz$linkinv(etaz)
  estfun <- score(salida$par)
  estfun <- list(counts=matrix(estfun[1:(p + ep)],nrow=p + ep,1),zeros=matrix(estfun[-c(1:(p + ep))],nrow=q,1))
  familyx$family <- family
  rownames(estfun$counts) <- rownames(theta_hat$counts); rownames(estfun$zeros) <- rownames(theta_hat$zeros)
  R <- try(chol(-hess(salida$par)),silent=TRUE)
  if(!is.matrix(R)){
    warning("Estimate of variance-covariance matrix is not positive definite",call.=FALSE)
    R <- solve(-hess(salida$par)); attr(R,"pd") <- FALSE
  }else{
    attr(R,"pd") <- TRUE
  }
  #print(all.equal(grad(objective,thetanew),score(thetanew)))
  #print(all.equal(hessian(objective,salida$par),hess(salida$par)))
  out_ <- list(coefficients=theta_hat,fitted.values=list(counts=mu,zeros=pi),linear.predictors=list(counts=etax,zeros=etaz),
               prior.weights=weights,y=y,formula=Formula(formula),call=match.call(),estfun=estfun,logLik=objective(salida$par),
               parms=c(p,ep,q),R=R,converged=ifelse(salida$convergence==0 & attr(thetanew,"converged")==TRUE,
                                                    TRUE,FALSE),model=mmf,
               data=data,terms=list(counts=terms(mx),zeros=terms(mz),full=terms(mmf)),score=score,hess=hess,type="Zero-alteration",
               family=list(counts=familyx,zeros=familyz),offset=list(counts=offsx,zeros=offsz),
               start=thetanew,levels=.getXlevels(attr(mmf,"terms"),mmf),
               contrasts = list(counts=attr(X,"contrasts"),zeros=attr(Z,"contrasts")))
  class(out_) <- "zeroinflation"
  return(out_)
}

#' @title Zero-Inflated Regression Models to deal with Zero-Excess in Count Data
#' @description Allows to fit a zero-inflated (Poisson or negative binomial) regression model to deal with zero-excess in count data.
#' @param formula a \code{Formula} expression of the form \code{response ~ x1 + x2 + ... | z1 + z2 + ...}, which is a symbolic description
#'        of the linear predictors of the models to be fitted to \eqn{\mu} and \eqn{\pi}, respectively.	See \link[Formula]{Formula} documentation.  If a
#'        formula of the form \code{response ~ x1 + x2 + ...} is supplied, then the same regressors are employed in both components. This is equivalent to
#'        \code{response ~ x1 + x2 + ...| x1 + x2 + ...}.
#' @param family an (optional) character string that allows you to specify the distribution
#'        to describe the response variable, as well as the link function to be used in
#'        the model for \eqn{\mu}. The following distributions are supported:
#'        (zero-inflated) negative binomial I ("nb1"), (zero-inflated) negative binomial II
#'        ("nb2"), (zero-inflated) negative binomial ("nbf"), and (zero-inflated) poisson
#'        ("poi"). Link functions are the same as those available in Poisson models via
#'        \link{glm}. See \link{family} documentation. As default, \code{family} is set to
#'        be Poisson with log link.
#' @param offset this can be used to specify an \emph{a priori} known component to be included in the linear predictor during fitting. This should be \code{NULL} or a numeric vector of length equal to the number of cases.
#' @param zero.link an (optional) character string which allows to specify the link function to be used in the model for \eqn{\pi}.
#' 		  Link functions available are the same than those available in binomial models via \link{glm}. See \link{family} documentation.
#' 		  As default, \code{zero.link} is set to "logit".
#' @param weights an (optional) vector of positive "prior weights" to be used in the fitting process. The length of
#'        \code{weights} should be the same as the number of observations. As default, \code{weights} is set to a vector of 1s.
#' @param data an (optional) \code{data frame} in which to look for variables involved in the \code{formula} expression,
#'        as well as for variables specified in the arguments \code{weights} and \code{subset}.
#' @param subset an (optional) vector specifying a subset of observations to be used in the fitting process.
#' @param start an (optional) list with two components named "counts" and "zeros", which allows to specify the starting values to be used in the
#'        iterative process to obtain the estimates of the parameters in the linear predictors to the models for \eqn{\mu}
#'        and \eqn{\pi}, respectively.
#' @param reltol an (optional) positive value which represents the \emph{relative convergence tolerance} for the BFGS method in \link{optim}.
#'        As default, \code{reltol} is set to 1e-13.
#' @param na.action a function which indicates what should happen when the data contain NAs. By default \code{na.action} is set to \code{na.omit()}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return
#' An object of class  \emph{zeroinflation} in which the main results of the model fitted to the data are stored, i.e., a
#' list with components including
#' \tabular{ll}{
#' \code{coefficients} \tab a list with elements "counts" and "zeros" containing the parameter estimates\cr
#'                     \tab from the respective models,\cr
#' \tab \cr
#' \code{fitted.values}\tab a list with elements "counts" and "zeros" containing the estimates of \eqn{\mu_1,\ldots,\mu_n}\cr
#'                     \tab and \eqn{\pi_1,\ldots,\pi_n}, respectively,\cr
#' \tab \cr
#' \code{start}        \tab a vector containing the starting values for all parameters in the model,\cr
#' \tab \cr
#' \code{prior.weights}\tab a vector containing the case weights used,\cr
#' \tab \cr
#' \code{offset}       \tab a list with elements "counts" and "zeros" containing the offset vectors, if any, \cr
#'                     \tab from the respective models,\cr
#' \tab \cr
#' \code{terms}        \tab a list with elements "counts", "zeros" and "full" containing the terms objects for \cr
#'                     \tab the respective models,\cr
#' \tab \cr
#' \code{loglik}       \tab the value of the log-likelihood function avaliated at the parameter estimates and\cr
#'                     \tab the observed data,\cr
#' \tab \cr
#' \code{estfun}       \tab a list with elements "counts" and "zeros" containing the estimating functions \cr
#'                     \tab evaluated at the parameter estimates and the observed data for the respective models,\cr
#' \tab \cr
#' \code{formula}      \tab the formula,\cr
#' \tab \cr
#' \code{levels}       \tab the levels of the categorical regressors,\cr
#' \tab \cr
#' \code{contrasts}    \tab a list with elements "counts" and "zeros" containing the contrasts corresponding\cr
#'                     \tab to levels from the respective models,\cr
#' \tab \cr
#' \code{converged}    \tab a logical indicating successful convergence,\cr
#' \tab \cr
#' \code{model}        \tab the full model frame,\cr
#' \tab \cr
#' \code{y}            \tab the response count vector,\cr
#' \tab \cr
#' \code{family}       \tab a list with elements "counts" and "zeros" containing the \link{family} objects used\cr
#'                     \tab  in the respective models,\cr
#' \tab \cr
#' \code{linear.predictors} \tab  a list with elements "counts" and "zeros" containing the estimates of \cr
#'                          \tab  \eqn{g(\mu_1),\ldots,g(\mu_n)} and \eqn{h(\pi_1),\ldots,h(\pi_n)}, respectively,\cr
#' \tab \cr
#' \code{R}            \tab a matrix with the Cholesky decomposition of the inverse of the variance-covariance\cr
#'                     \tab matrix of all parameters in the model,\cr
#' \tab \cr
#' \code{call}         \tab the original function call.\cr
#' }
#' @details
#' The zero-inflated count distributions may be obtained as the mixture between a count
#' distribution and the Bernoulli distribution. Indeed, if \eqn{Y} is a count random
#' variable such that \eqn{Y|\nu=1} is 0 with probability 1
#' and \eqn{Y|\nu=0} ~ Poisson\eqn{(\mu)}, where \eqn{\nu} ~ Bernoulli\eqn{(\pi)}, then
#' \eqn{Y} is distributed according to the Zero-Inflated Poisson distribution, denoted here as
#' ZIP\eqn{(\mu,\pi)}.
#'
#' Similarly, if \eqn{Y} is a count random variable such that \eqn{Y|\nu=1} is 0 with probability 1
#' and \eqn{Y|\nu=0} ~ NB\eqn{(\mu,\phi,\tau)}, where \eqn{\nu} ~ Bernoulli\eqn{(\pi)}, then
#' \eqn{Y} is distributed according to the Zero-Inflated Negative Binomial distribution, denoted here as
#' ZINB\eqn{(\mu,\phi,\tau,\pi)}. The Zero-Inflated Negative Binomial I \eqn{(\mu,\phi,\pi)} and
#' Zero-Inflated Negative Binomial II \eqn{(\mu,\phi,\pi)} distributions are special cases of ZINB when
#' \eqn{\tau=0} and \eqn{\tau=-1}, respectively.
#'
#' The "counts" model may be expressed as \eqn{g(\mu_i)=x_i^{\top}\beta} for \eqn{i=1,\ldots,n}, where
#' \eqn{g(\cdot)} is the link function specified at the argument \code{family}. Similarly, the "zeros" model may
#' be expressed as \eqn{h(\pi_i)=z_i^{\top}\gamma} for \eqn{i=1,\ldots,n}, where \eqn{h(\cdot)} is the
#' link function specified at the argument \code{zero.link}. Parameter estimation is
#' performed using the maximum likelihood method. The model parameters are estimated by
#' maximizing the log-likelihood function through the BFGS method available in the routine
#' \link{optim}. Analytical derivatives are used instead of numerical derivatives to
#' increase BFGS method accuracy and speed. The variance-covariance matrix estimate is
#' obtained as being minus the inverse of the (analytical) hessian matrix evaluated at the
#' parameter estimates and the observed data.
#'
#' A set of standard extractor functions for fitted model objects is available for objects
#' of class \emph{zeroinflation}, including methods for generic functions such as
#' \link{print}, \link{summary}, \link{model.matrix}, \link{estequa},
#' \link{coef}, \link{vcov}, \link{logLik}, \link{fitted}, \link{confint}, \link{AIC}, \link{BIC} and
#' \link{predict}. In addition, the model fitted to the	data may be assessed using functions such as
#' \link{anova.zeroinflation}, \link{residuals.zeroinflation}, \link{dfbeta.zeroinflation},
#' \link{cooks.distance.zeroinflation} and \link{envelope.zeroinflation}.
#'
#' @examples
#' ####### Example 1: Roots Produced by the Columnar Apple Cultivar Trajan
#' data(Trajan)
#' fit1 <- zeroinf(roots ~ photoperiod, family="nbf(log)", zero.link="logit", data=Trajan)
#' summary(fit1)
#'
#' ####### Example 2: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit2 <- zeroinf(infections ~ frequency | location, family="nb1(log)", data=swimmers)
#' summary(fit2)
#'
#' ####### Example 3: Article production by graduate students in biochemistry PhD programs
#' bioChemists <- pscl::bioChemists
#' fit3 <- zeroinf(art ~ fem + kid5 + ment | ment, family="nb1(log)", data = bioChemists)
#' summary(fit3)
#'
#' @seealso \link{overglm}, \link{zeroalt}
#' @export zeroinf
#' @references Cameron A.C., Trivedi P.K. 1998. \emph{Regression Analysis of Count Data}. New York:
#'             Cambridge University Press.
#' @references Lambert D. 1992. Zero-Inflated Poisson Regression, with an Application to Defects in
#'             Manufacturing. \emph{Technometrics} 34, 1-14.
#' @references Garay A.M., Hashimoto E.M., Ortega E.M.M., Lachos V. (2011) On estimation and
#'             influence diagnostics for zero-inflated negative binomial regression models. \emph{Computational
#' 			   Statistics & Data Analysis} 55, 1304-1318.
#'
zeroinf <- function(formula, data, offset, subset, na.action=na.omit(), weights, family="poi(log)",
                    zero.link=c("logit", "probit", "cloglog", "cauchit", "log"), reltol=1e-13,
                    start=list(counts=NULL,zeros=NULL),...){
  if(missing(data)) data <- environment(formula)
  zero.link <- match.arg(zero.link)
  mmf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "offset", "subset", "na.action", "weights"), names(mmf), 0)
  mmf <- mmf[c(1,m)]
  mmf$drop.unused.levels <- TRUE
  mmf[[1]] <- as.name("model.frame")
  mmf$formula <- Formula(formula)
  mmf <- eval(mmf, parent.frame())

  y <- as.matrix(model.part(Formula(formula), data = mmf, lhs=1)); zeros <- y==0
  if(any(y != floor(y)) | any(y < 0)) stop("There are negative or non-integer values in the response variable!!",call.=FALSE)
  if(all(y > 0)) stop("Zero values in the response variable are required!!",call.=FALSE)
  mx <- model.part(Formula(formula), data = mmf, rhs = 1, terms = TRUE)
  X <- model.matrix(mx, data = mmf); p <- ncol(X)
  if(!is.null(start$counts) & length(start$counts) != p) stop("Incorrect size of the starting values vector for the counts model!!",call.=FALSE)
  offsx <- model.offset(mx)
  if(is.null(offsx)) offsx <- matrix(0,nrow(X),1)
  if(suppressWarnings(formula(Formula(formula),lhs=0,rhs=2)=="~0")) mz <- mx
  else mz <- model.part(Formula(formula), data = mmf, rhs = 2, terms = TRUE)
  Z <- model.matrix(mz, data = mmf); q <- ncol(Z)
  if(!is.null(start$zeros) & length(start$zeros) != q) stop("Incorrect size of the starting values vector for the zeros model!!",call.=FALSE)
  offsz <- model.offset(mz)
  if(is.null(offsz)) offsz <- matrix(0,nrow(X),1)
  weights <- model.weights(mmf)
  if(is.null(weights)) weights <- matrix(1,nrow(X),1)
  if(any(weights <= 0)) stop("Only positive weights are allowed!!",call.=FALSE)
  temp <- strsplit(gsub(" |link|=|'|'","",tolower(family)),"[()]")[[1]]
  family <- temp[1]
  if(is.na(temp[2])) link="log" else link <- temp[2]
  familyx <- poisson(link); familyz <- binomial(zero.link)

  if(family=="poi"){
    ep <- 0
    objective <- function(theta){
      mu <- familyx$linkinv(X%*%theta[1:p] + offsx)
      pi <- familyz$linkinv(Z%*%theta[-c(1:p)] + offsz)
      l <- ifelse(zeros,log(pi + (1 - pi)*exp(-mu)),log(1 - pi) - mu + y*log(mu) - lgamma(y + 1))
      return(sum(weights*l))
    }
    score <- function(theta){
      etax <- X%*%theta[1:p] + offsx; etaz <- Z%*%theta[-c(1:p)] + offsz
      mu <- familyx$linkinv(etax); pi <- familyz$linkinv(etaz)
      dmudetax <- familyx$mu.eta(etax); dpidetaz <- familyz$mu.eta(etaz)
      s0 <- pi + (1 - pi)*exp(-mu)
      s1 <- ifelse(zeros,-(1 - pi)*exp(-mu)/s0,y/mu - 1)
      s2 <- ifelse(zeros,(1 - exp(-mu))/s0,-1/(1 - pi))
      return(c(crossprod(X,weights*s1*dmudetax),crossprod(Z,weights*s2*dpidetaz)))
    }
    theta0 <- function(start){
      if(!is.null(start$counts)) betas <- start$counts
      else betas <- suppressWarnings(glm.fit(y=y[!zeros],x=X[!zeros,],offset=offsx[!zeros],weights=weights[!zeros],family=familyx)$coefficients)
      if(!is.null(start$zeros)) gammas <- start$zeros
      else gammas <- suppressWarnings(glm.fit(y=ifelse(y==0,1,0),x=Z,offset=offsz,weights=weights,family=familyz)$coefficients)
      return(matrix(c(betas,gammas),length(betas)+length(gammas),1))
    }
    hess <- function(theta){
      etax <- X%*%theta[1:p] + offsx; etaz <- Z%*%theta[-c(1:p)] + offsz
      mu <- familyx$linkinv(etax); pi <- familyz$linkinv(etaz)
      dmudetax <- familyx$mu.eta(etax); dpidetaz <- familyz$mu.eta(etaz)
      dmu2detax2 <- grad(familyx$mu.eta,etax); dpi2detaz2 <- grad(familyz$mu.eta,etaz)
      s0 <- pi + (1 - pi)*exp(-mu)
      s11 <- ifelse(zeros,(pi*(1 - pi)*exp(-mu)/s0^2)*dmudetax^2 - ((1 - pi)*exp(-mu)/s0)*dmu2detax2,
                    - (y/mu^2)*dmudetax^2 + (y/mu - 1)*dmu2detax2)
      s12 <- ifelse(zeros,exp(-mu)/s0^2,0)*dmudetax*dpidetaz
      s22 <- ifelse(zeros,-((1 - exp(-mu))/s0)^2*dpidetaz^2 + ((1 - exp(-mu))/s0)*dpi2detaz2,
                    - (1 - pi)^(-2)*dpidetaz^2 - (1 - pi)^(-1)*dpi2detaz2)
      hessiana <- matrix(0,p + q,p + q)
      hessiana[1:p,1:p] <- crossprod(X,matrix(weights*s11,nrow(X),p)*X)
      hessiana[1:p,-c(1:p)] <- crossprod(X,matrix(weights*s12,nrow(Z),q)*Z); hessiana[-c(1:p),1:p] <- t(hessiana[1:p,-c(1:p)])
      hessiana[-c(1:p),-c(1:p)] <- crossprod(Z,matrix(weights*s22,nrow(Z),q)*Z)
      return(hessiana)
    }
  }
  if(family %in% c("nb1","nb2","nbf")){
    ep <- 2;
    if(family=="nb1"){
      ep <- 1; tau <- 0
    }
    if(family=="nb2"){
      ep <- 1; tau <- -1
    }
    objective <- function(theta){
      mu <- familyx$linkinv(X%*%theta[1:p] + offsx)
      phi <- exp(theta[p + 1])
      if(family=="nbf") tau <- theta[p + ep]
      pi <- familyz$linkinv(Z%*%theta[-c(1:(p + ep))] + offsz)
      u <- phi*mu^tau; v <- u*mu
      l <- ifelse(zeros,log(pi + (1 - pi)*(v + 1)^(-1/u)),Lgamma(y + 1/u) - Lgamma(1/u) - Lgamma(y + 1) +
                    y*log(v) - (y + 1/u)*log(v + 1) + log(1 - pi))
      return(sum(weights*l))
    }
    score <- function(theta){
      etax <- X%*%theta[1:p] + offsx
      phi <- exp(theta[p + 1])
      if(family=="nbf") tau <- theta[p + ep]
      mu <- familyx$linkinv(etax)
      etaz <- Z%*%theta[-c(1:(p + ep))] + offsz
      pi <- familyz$linkinv(etaz)
      dmudetax <- familyx$mu.eta(etax); dpidetaz <- familyz$mu.eta(etaz)
      u <- phi*mu^tau; v <- u*mu
      E1 <- ifelse(zeros,0,-(Digamma(y + 1/u) - Digamma(1/u) - log(v + 1))/u)
      E2 <- ifelse(zeros,0,(y - mu)/(v + 1))
      k0 <- ifelse(zeros,(v + 1)^(-1/u)*(1 - pi)/(pi + (1 - pi)*(v + 1)^(-1/u)),0)
      s1 <- ifelse(zeros,mu*k0*(log(v + 1)*tau/v - (tau + 1)/(v + 1)),tau*E1 + (tau + 1)*E2)
      s2 <- ifelse(zeros,k0*(log(v + 1)/u - mu/(v + 1)),E1 + E2)
      s3 <- ifelse(zeros,(1 - (v + 1)^(-1/u))/(pi + (1 - pi)*(v + 1)^(-1/u)),-1/(1 - pi))
      out_ <- c(crossprod(X,weights*s1*dmudetax/mu),sum(weights*s2))
      if(family=="nbf") out_ <- c(out_,sum(weights*s2*log(mu)))
      return(c(out_,crossprod(Z,weights*s3*dpidetaz)))
    }
    theta0 <- function(start){
      if(!is.null(start$counts)) betas <- start$counts
      else betas <- suppressWarnings(glm.fit(y=y[!zeros],x=X[!zeros,],offset=offsx[!zeros],weights=weights[!zeros],family=familyx)$coefficients)
      mus <- familyx$linkinv(X%*%betas + offsx)
      if(family=="nbf"){
        fik <- lm((y - mus)^2 ~ -1 + I(mus^2),offset=mus)
        betas <- c(betas,log(abs(coef(fik))),0)
      }
      if(family=="nb1"){
        fik <- lm((y - mus)^2 ~ -1 + I(mus^2),offset=mus)
        betas <- c(betas,log(abs(coef(fik))))
      }
      if(family=="nb2"){
        fik <- lm((y - mus)^2 ~ -1 + mus)
        betas <- c(betas,log(abs(coef(fik)-1)))
      }
      if(!is.null(start$zeros)) gammas <- start$zeros
      else gammas <- suppressWarnings(glm.fit(y=ifelse(y==0,1,0),x=Z,offset=offsz,weights=weights,family=familyz)$coefficients)
      return(matrix(c(betas,gammas),length(betas)+length(gammas),1))
    }
    hess <- function(theta){
      etax <- X%*%theta[1:p] + offsx
      phi <- exp(theta[p + 1])
      if(family=="nbf") tau <- theta[p + ep]
      mu <- familyx$linkinv(etax)
      etaz <- Z%*%theta[-c(1:(p + ep))] + offsz
      pi <- familyz$linkinv(etaz)
      dmudetax <- familyx$mu.eta(etax); dpidetaz <- familyz$mu.eta(etaz)
      u <- phi*mu^tau; v <- u*mu
      dmudetax <- familyx$mu.eta(etax); dpidetaz <- familyz$mu.eta(etaz)
      dmu2detax2 <- grad(familyx$mu.eta,etax); dpi2detaz2 <- grad(familyz$mu.eta,etaz)
      hessiana <- matrix(0,p + ep + q,p + ep + q)
      E1 <- ifelse(zeros,0,-(digamma(y + 1/u) - log(v + 1) - digamma(1/u))/u);E2 <- ifelse(zeros,0,(y - mu)/(v + 1))
      psis <- ifelse(zeros,0,psigamma(y + 1/u,1) - psigamma(1/u,1))
      cc1 <- ifelse(zeros,0,-E1 + psis/u^2 + mu/(v + 1));cc2 <- ifelse(zeros,0,-(y - mu)*v/(v + 1)^2)
      f <- (v + 1)^(1/u);m <- ifelse(zeros,pi*f + (1 - pi),0); m2 <- ifelse(zeros,m^2/(1 - pi),0)
      f1 <-  ifelse(zeros,log(v + 1)*tau/v - (tau + 1)/(v + 1),0)
      f2 <-  ifelse(zeros,log(v + 1)/u - mu/(v + 1),0)
      f22 <- ifelse(zeros,mu/(v + 1) - log(v + 1)/u + v*mu/(v + 1)^2,0)
      f11 <- ifelse(zeros,(tau + 1)^2*u/(v + 1)^2 - log(v + 1)*tau*(tau + 1)/(v*mu) + tau*(tau + 1)/(mu*(v + 1)),0)
      f12 <- ifelse(zeros,(tau + 1)*v/(v + 1)^2 - log(v + 1)*tau/v + tau/(v + 1),0)
      f13 <- ifelse(zeros,((tau + 1)*v*log(mu) - v - 1)/(v + 1)^2 + v*log(v + 1)*(1 - tau*log(mu))/v^2 +
                      phi*tau*(tau + 1)*log(mu)/(v*(v + 1)),0)
      aa <- ifelse(zeros,(m*f11 + pi*f*f1^2)/m2,-(tau/mu)*(tau*E1/mu - tau*psis/(u^2*mu) - (tau+1)/(v + 1)) -
                     ((tau + 1)/mu)*(u*(mu + (y - mu)*(tau + 1)) + 1)/(v + 1)^2 - E2*(tau + 1)/mu^2 - tau*E1/mu^2)
      s1 <- ifelse(zeros,mu*(1 - pi)*f1/m,tau*E1 + (tau + 1)*E2)
      s2 <- ifelse(zeros,mu*(m*f12 + pi*f*f2*f1)/m2,tau*cc1 + (tau + 1)*cc2)
      s3 <- s2*log(mu)
      k0 <- ifelse(zeros,(v + 1)^(-1/u)*(1 - pi)/(pi + (1 - pi)*(v + 1)^(-1/u)),0)
      s3 <-	s3 + ifelse(zeros,mu*k0*(log(v + 1)/v - 1/(v + 1)),E1 + E2)
      s4 <- ifelse(zeros,(m*f22 + pi*f*f2^2)/m2,cc1 + cc2)
      hessiana[1:p,1:p] <- crossprod(X,matrix(weights*(aa*dmudetax^2 + s1*dmu2detax2/mu),nrow(X),ncol(X))*X)
      hessiana[p + 1,1:p] <- hessiana[1:p,p + 1] <- crossprod(X,weights*s2*dmudetax/mu)
      hessiana[p + 1,p + 1] <- sum(weights*s4)
      if(family=="nbf"){
        hessiana[p + ep,1:p] <- hessiana[1:p,p + ep] <- crossprod(X,weights*s3*dmudetax/mu)
        hessiana[p + ep,p + 1] <- hessiana[p + 1,p + ep] <- sum(weights*s4*log(mu))
        hessiana[p + ep,p + ep] <- sum(weights*s4*log(mu)^2)
      }
      z0 <- ifelse(zeros,-dpidetaz/(f*(pi + (1 - pi)*(1/f))^2),0)
      hessiana[1:p,-c(1:(p + ep))] <- crossprod(X,matrix(weights*f1*dmudetax*z0,nrow(Z),ncol(Z))*Z)
      hessiana[-c(1:(p + ep)),1:p] <- t(hessiana[1:p,-c(1:(p + ep))])
      hessiana[p + 1,-c(1:(p + ep))] <- t(weights*f2*z0)%*%Z;hessiana[-c(1:(p + ep)),p + 1] <- t(hessiana[p + 1,-c(1:(p + ep))])
      if(family=="nbf"){
        hessiana[p + ep,-c(1:(p + ep))] <- crossprod(weights*f2*log(mu)*z0,Z);hessiana[-c(1:(p + ep)),p + ep] <- t(hessiana[p + ep,-c(1:(p + ep))])
      }
      z1 <- ifelse(zeros,(1 - (v + 1)^(-1/u))/(pi + (1 - pi)*(v + 1)^(-1/u)),-1/(1 - pi))
      hessiana[-c(1:(p + ep)),-c(1:(p + ep))] <- crossprod(Z,matrix(weights*(dpi2detaz2*z1 - dpidetaz^2*z1^2),nrow(Z),ncol(Z))*Z)
      return(hessiana)
    }
  }
  thetanew <- theta0(start)
  salida <- optim(thetanew,objective,score,method="BFGS",control=list(reltol=reltol,fnscale=-1))
  if(salida$convergence != 0) warning("Convergence not achieved!!",call.=FALSE)
  theta_hat <- list(counts=matrix(salida$par[1:(p + ep)],nrow=p + ep,1),zeros=matrix(salida$par[-c(1:(p + ep))],nrow=q,1))
  nomb <- c(colnames(X),"log(phi)","tau"); rownames(theta_hat$counts) <- nomb[1:length(theta_hat$counts)]
  rownames(theta_hat$zeros) <- colnames(Z)
  etax <- X%*%theta_hat$counts[1:p] + offsx
  etaz <- Z%*%theta_hat$zeros + offsz
  mu <- familyx$linkinv(etax);pi <- familyz$linkinv(etaz)
  estfun <- score(salida$par)
  estfun <- list(counts=matrix(estfun[1:(p + ep)],nrow=p + ep,1),zeros=matrix(estfun[-c(1:(p + ep))],nrow=q,1))
  familyx$family <- family
  rownames(estfun$counts) <- rownames(theta_hat$counts); rownames(estfun$zeros) <- rownames(theta_hat$zeros)
  theta_new <- matrix(thetanew,length(thetanew),1)
  rownames(thetanew) <- rownames(theta_hat)
  R <- try(chol(-hess(salida$par)),silent=TRUE)
  if(!is.matrix(R)){
    warning("Estimate of variance-covariance matrix is not positive definite",call.=FALSE)
    R <- solve(-hess(salida$par)); attr(R,"pd") <- FALSE
  }else{
    attr(R,"pd") <- TRUE
  }
  #print(all.equal(grad(objective,thetanew),score(thetanew)))
  #print(all.equal(hessian(objective,salida$par),hess(salida$par)))
  #print(hessian(objective,salida$par))
  #print(hess(salida$par))
  out_ <- list(coefficients=theta_hat,fitted.values=list(counts=mu,zeros=pi),linear.predictors=list(counts=etax,zeros=etaz),
               prior.weights=weights,y=y,formula=Formula(formula),call=match.call(),estfun=estfun,logLik=objective(salida$par),
               parms=c(p,ep,q),R=R,converged=ifelse(salida$convergence==0,TRUE,FALSE),model=mmf,
               data=data,terms=list(counts=terms(mx),zeros=terms(mz),full=terms(mmf)),score=score,hess=hess,type="Zero-inflation",
               family=list(counts=familyx,zeros=familyz),offset=list(counts=offsx,zeros=offsz),start=thetanew,
               levels=.getXlevels(attr(mmf,"terms"),mmf),contrasts = list(counts=attr(X,"contrasts"),zeros=attr(Z,"contrasts")))
  class(out_) <- "zeroinflation"
  return(out_)
}

#' @method model.matrix zeroinflation
#' @export

model.matrix.zeroinflation <- function(object, submodel=c("counts","zeros"), ...) {
  submodel <- match.arg(submodel)
  out_ <- model.matrix(object$terms[[submodel]], object$model, contrasts=object$contrasts[[submodel]])
  return(out_)
}

#' @method model.matrix overglm
#' @export

model.matrix.overglm <- function(object, ...) {
  out_ <- model.matrix(object$terms, object$model, contrasts=object$contrasts)
  return(out_)
}

#' @method coef zeroinflation
#' @export

coef.zeroinflation <- function(object, submodel=c("counts","zeros"), ...) {
  submodel <- match.arg(submodel)
  if(submodel=="counts"){
    out_ <- as.matrix(object$coefficients[["counts"]][1:object$parms[1]])
    rownames(out_) <- rownames(object$coefficients[["counts"]])[1:object$parms[1]]
  }
  else out_ <- object$coefficients[[submodel]]
  colnames(out_) <- "Estimates"
  return(out_)
}

#' @method coef overglm
#' @export

coef.overglm <- function(object, ...) {
  out_ <- as.matrix(object$coefficients[1:object$parms[1]])
  rownames(out_) <- rownames(object$coefficients)[1:object$parms[1]]
  colnames(out_) <- "Estimates"
  return(out_)
}

#' @method vcov zeroinflation
#' @export

vcov.zeroinflation <- function(object, submodel=c("counts","zeros"), ...) {
  submodel <- match.arg(submodel)
  if(attr(object$R,"pd")) varcovar <- chol2inv(object$R) else varcovar <- object$R
  if(submodel=="counts") out_ <- varcovar[1:object$parms[1],1:object$parms[1]]
  else out_ <- varcovar[-c(1:sum(object$parms[1:2])),-c(1:sum(object$parms[1:2]))]
  rownames(out_) <- colnames(out_) <- rownames(object$coefficients[[submodel]][1:ncol(out_)])
  return(out_)
}
#' @method vcov overglm
#' @export

vcov.overglm <- function(object, ...) {
  if(attr(object$R,"pd")) out_ <- chol2inv(object$R)[1:object$parms[1],1:object$parms[1]]
  else out_ <- object$R
  rownames(out_) <- colnames(out_) <- rownames(coef(object))
  return(out_)
}

#' @method logLik zeroinflation
#' @export

logLik.zeroinflation <- function(object, ...){
  out_ <- object$logLik
  attr(out_,"df") <- sum(object$parms)
  attr(out_,"nobs") <- length(object$prior.weights)
  class(out_) <- "logLik"
  return(out_)
}

#' @method logLik overglm
#' @export

logLik.overglm <- function(object, ...){
  out_ <- object$logLik
  attr(out_,"df") <- length(object$coefficients)
  attr(out_,"nobs") <- length(object$prior.weights)
  class(out_) <- "logLik"
  return(out_)
}

#' @method fitted zeroinflation
#' @export

fitted.zeroinflation <- function(object, submodel=c("counts","zeros"), ...){
  submodel <- match.arg(submodel)
  out_ <- object$fitted.values[[submodel]]
  colnames(out_) <- "Fitted values"
  return(out_)
}

#' @method fitted overglm
#' @export

fitted.overglm <- function(object, ...){
  out_ <- object$fitted.values
  colnames(out_) <- "Fitted values"
  return(out_)
}

#' @method predict overglm
#' @export

predict.overglm <- function(object,newdata,se.fit=FALSE,type=c("link","response"),na.action=na.omit(), ...){
  type <- match.arg(type)
  if(missingArg(newdata)){
    predicts <- object$linear.predictors
    X <- model.matrix(object)
  }
  else{
    newdata <- data.frame(newdata)
    mf <- model.frame(delete.response(object$terms),newdata,na.action=na.action,xlev=object$levels)
    X <- model.matrix(delete.response(object$terms),mf,contrasts=object$contrasts)
    betas <- object$coefficients[1:object$parms[1]]
    predicts <- X%*%betas
    offs <- model.offset(mf)
    if(!is.null(offs)) predicts <- predicts + offs
  }
  family <- object$family
  if(type=="response") predicts <- family$linkinv(predicts)
  if(se.fit){
    se <- sqrt(apply((X%*%vcov(object))*X,1,sum))
    if(type=="response") se <- se*abs(family$mu.eta(family$linkfun(predicts)))
    predicts <- cbind(predicts,se)
    colnames(predicts) <- c("fit","se.fit")
  }else colnames(predicts) <- c("fit")
  rownames(predicts) <- rep(" ",nrow(predicts))
  return(predicts)
}

#' @method predict zeroinflation
#' @export

predict.zeroinflation <- function(object,newdata,submodel=c("counts","zeros"),se.fit=FALSE,
                                  type=c("link","response"),na.action=na.omit(), ...){
  type <- match.arg(type)
  submodel <- match.arg(submodel)
  if(missingArg(newdata)){
    predicts <- object$linear.predictors[[submodel]]
    X <- model.matrix(object,submodel=submodel)
  }
  else{
    newdata <- data.frame(newdata)
    mf <- model.frame(delete.response(object$terms[[submodel]]),newdata,na.action=na.action,xlev=object$levels)
    X <- model.matrix(delete.response(object$terms[[submodel]]),mf,contrasts=object$contrasts[[submodel]])
    betas <- object$coefficients[[submodel]]
    predicts <- X%*%betas
    offs <- model.offset(mf)
    if(!is.null(offs)) predicts <- predicts + offs
  }
  family <- object$family[[submodel]]
  if(type=="response") predicts <- family$linkinv(predicts)
  if(se.fit){
    se <- sqrt(apply((X%*%vcov(object,submodel=submodel))*X,1,sum))
    if(type=="response") se <- se*abs(family$mu.eta(family$linkfun(predicts)))
    predicts <- cbind(predicts,se)
    colnames(predicts) <- c("fit","se.fit")
  }else colnames(predicts) <- c("fit")
  rownames(predicts) <- rep(" ",nrow(predicts))
  return(predicts)
}

#' @title Estimating Equations for alternatives to the Poisson and Binomial Regression Models under the presence of Overdispersion.
#' @description Computes the estimating equations evaluated at the parameter estimates and the observed data for
#' regression models based on the negative binomial, beta-binomial, and random-clumped binomial
#' distributions, which are alternatives to the Poisson and binomial regression models under the presence of overdispersion.
#' @param object an object of the class \emph{overglm}.
#' @param ... further arguments passed to or from other methods.
#' @return A vector with the values of the estimating equations evaluated at the parameter estimates and the observed data.
#' @method estequa overglm
#' @export
#' @examples
#' ### Example 1: Ability of retinyl acetate to prevent mammary cancer in rats
#' data(mammary)
#' fit1 <- overglm(tumors ~ group, family="nb1(identity)", data=mammary)
#' estequa(fit1)
#'
#' ### Example 2: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit2 <- overglm(infections ~ frequency + location, family="nb1(log)", data=swimmers)
#' estequa(fit2)
#'
#' ### Example 3: Urinary tract infections in HIV-infected men
#' data(uti)
#' fit3 <- overglm(episodes ~ cd4 + offset(log(time)), family="nb1(log)", data = uti)
#' estequa(fit3)
#'
#' ### Example 4: Article production by graduate students in biochemistry PhD programs
#' bioChemists <- pscl::bioChemists
#' fit4 <- overglm(art ~ fem + kid5 + ment, family="nb1(log)", data = bioChemists)
#' estequa(fit4)
#'
#' ### Example 5: Agents to stimulate cellular differentiation
#' data(cellular)
#' fit5 <- overglm(cbind(cells,200-cells) ~ tnf + ifn, family="bb(logit)", data=cellular)
#' estequa(fit5)
#'
#' ### Example 6: Teratogenic effects of phenytoin and trichloropropene oxide
#' data(ossification)
#' model6 <- cbind(fetuses,litter-fetuses) ~ pht + tcpo
#' fit6 <- overglm(model6, family="rcb(cloglog)", data=ossification)
#' estequa(fit6)
#'
#' ### Example 7: Germination of orobanche seeds
#' data(orobanche)
#' model7 <- cbind(germinated,seeds-germinated) ~ specie + extract
#' fit7 <- overglm(model7, family="rcb(cloglog)", data=orobanche)
#' estequa(fit7)
#'

estequa.overglm <- function(object, ...){
  out_ <- object$estfun
  colnames(out_) <- " "
  return(out_)
}

#' @title Estimating Equations in Regression Models to deal with Zero-Excess in Count Data
#' @description Computes the estimating equations evaluated at the parameter estimates and the observed data for regression models to deal with zero-excess in count data.
#' @param object an object of the class \emph{zeroinflation}.
#' @param submodel an (optional) character string which allows to specify the model: "counts" or "zeros". By default,
#' \code{submodel} is set to "counts".
#' @param ... further arguments passed to or from other methods.
#' @return A vector with the values of the estimating equations evaluated at the parameter estimates and the observed data.
#' @examples
#' ####### Example 1: Roots Produced by the Columnar Apple Cultivar Trajan
#' data(Trajan)
#' fit1 <- zeroalt(roots ~ photoperiod, family="nbf(log)", zero.link="logit", data=Trajan)
#' estequa(fit1)
#'
#' fit1a <- zeroinf(roots ~ photoperiod, family="nbf(log)", zero.link="logit", data=Trajan)
#' estequa(fit1a)
#'
#' ####### Example 2: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit2 <- zeroalt(infections ~ frequency | location, family="nb1(log)", data=swimmers)
#' estequa(fit2)
#'
#' fit2a <- zeroinf(infections ~ frequency | location, family="nb1(log)", data=swimmers)
#' estequa(fit2a)
#'
#' ####### Example 3: Article production by graduate students in biochemistry PhD programs
#' bioChemists <- pscl::bioChemists
#' fit3 <- zeroalt(art ~ fem + kid5 + ment, family="nb1(log)", data = bioChemists)
#' estequa(fit3)
#'
#' fit3a <- zeroinf(art ~ fem + kid5 + ment | ment, family="nb1(log)", data = bioChemists)
#' estequa(fit3a)
#' @method estequa zeroinflation
#' @export
#'
estequa.zeroinflation <- function(object, submodel=c("counts","zeros"), ...){
  submodel <- match.arg(submodel); submodel <- match.arg(submodel)
  out_ <- object$estfun[[submodel]]
  colnames(out_) <- " "
  return(out_)
}


#' @method summary overglm
#' @export

summary.overglm <- function(object,digits=max(3, getOption("digits") - 2),signif.legend=FALSE,...){
  cat("\nSample size:",nrow(object$y),"\n")
  family <- switch(object$family$family,
                   "nb1"="Negative Binomial type I","nb2"="Negative Binomial type II",
                   "nbf"="Negative Binomial","poi"="Poisson",
                   "rcb"="Random-clumped Binomial","bb"="Beta-Binomial")
  cat("     Family:",paste0(ifelse(object$zero.trunc,"zero-truncated ",""),family),"with",object$family$link,"link")
  cat("\n*************************************************************\n")
  varcovar <- sqrt(diag(chol2inv(object$R)))
  TAB	<- cbind(Estimate <- object$coefficients,
               StdErr <- varcovar,
               tval <- Estimate/StdErr,
               p.value <- 2*pnorm(-abs(tval)))
  colnames(TAB) <- c("Estimate", "Std.Error", "z-value", "Pr(>|z|)")
  rownames(TAB) <- rownames(object$coefficients)
  if(object$parms[2] > 0){
    if(object$family$family=="rcb"){
      TAB[object$parms[1] + 1,1] <- exp(TAB[object$parms[1] + 1,1])/(1 + exp(TAB[object$parms[1] + 1,1]))
      TAB[object$parms[1] + 1,2] <- TAB[object$parms[1] + 1,2]*TAB[object$parms[1] + 1,1]/exp(0.5*TAB[object$parms[1] + 1,1])
    }else{
      TAB[object$parms[1] + 1,1] <- exp(TAB[object$parms[1] + 1,1])
      TAB[object$parms[1] + 1,2] <- TAB[object$parms[1] + 1,2]*TAB[object$parms[1] + 1,1]
    }
    TAB[object$parms[1] + 1,3:4] <- c(NA,NA)
    TAB <- rbind(TAB[c(1:object$parms[1]),],rep(NA,4),TAB[-c(1:object$parms[1]),])
    rownames(TAB)[object$parms[1] + 2] <- "phi"
  }
  printCoefmat(TAB, P.values=TRUE, signif.stars=FALSE, has.Pvalue=TRUE, digits=digits, dig.tst=digits, tst.ind=c(1,2,3), na.print = " ")
  cat("*************************************************************\n")
  cat("                 -2*log-likelihood: ",round(-2*object$logLik,digits=3),"\n")
  cat("                               AIC: ",round(-2*object$logLik + 2*sum(object$parms),digits=3),"\n")
  cat("                               BIC: ",round(-2*object$logLik + log(nrow(object$y))*sum(object$parms),digits=3),"\n")
  return(invisible(list(coefficients=round(TAB,digits=digits))))
}

#' @method summary zeroinflation
#' @export

summary.zeroinflation <- function(object,digits=max(3, getOption("digits") - 2),signif.legend=FALSE,...){
  cat("\nSample size:",nrow(object$y),"\n")
  family <- switch(object$family$counts$family,
                   "nb1"="Negative Binomial type I","nb2"="Negative Binomial type II",
                   "nbf"="Negative Binomial","poi"="Poisson")
  cat(ifelse(object$type=="Zero-alteration","     Family: zero-altered","     Family: zero-inflated"),family)
  cat("\n*************************************************************\n")
  if(attr(object$R,"pd")) varcovar <- sqrt(diag(chol2inv(object$R))) else varcovar <- sqrt(diag(object$R))
  cat(paste0("Count model coefficients (",family," with ",object$family$counts$link," link):\n"))
  rownamu <- rownames(object$coefficients$counts)
  rownapi <- rownames(object$coefficients$zeros)
  delta <- max(nchar(rownamu)) - max(nchar(rownapi))
  falta <- paste(replicate(max(abs(delta)-1,0)," "),collapse="")
  if(delta > 0) rownapi[1] <- paste(rownapi[1],falta,collapse="")
  if(delta < 0) rownamu[1] <- paste(rownamu[1],falta,collapse="")
  TAB	<- cbind(Estimate <- object$coefficients$counts,
               StdErr <- varcovar[1:sum(object$parms[1:2])],
               tval <- Estimate/StdErr,
               p.value <- 2*pnorm(-abs(tval)))
  colnames(TAB) <- c("Estimate", "Std.Error", "z-value", "Pr(>|z|)")
  rownames(TAB) <- rownamu
  if(object$parms[2] > 0){
    TAB[object$parms[1] + 1,1] <- exp(TAB[object$parms[1] + 1,1])
    TAB[object$parms[1] + 1,2] <- TAB[object$parms[1] + 1,2]*TAB[object$parms[1] + 1,1]
    TAB[object$parms[1] + 1,3:4] <- c(NA,NA)
    TAB <- rbind(TAB[c(1:object$parms[1]),],rep(NA,4),TAB[-c(1:object$parms[1]),])
    rownames(TAB)[object$parms[1] + 2] <- "phi"
  }
  printCoefmat(TAB, P.values=TRUE, signif.stars=FALSE, has.Pvalue=TRUE, digits=digits, dig.tst=digits, tst.ind=c(1,2,3), na.print = " ")
  out_ <- list(coefficients_count=round(TAB,digits=digits))
  cat("\n")
  cat(paste0(object$type," model coefficients (Bernoulli with ",object$family$zeros$link," link):"),"\n")
  TAB	<- cbind(Estimate <- object$coefficients$zeros,
               StdErr <- varcovar[-c(1:sum(object$parms[1:2]))],
               tval <- Estimate/StdErr,
               p.value <- 2*pnorm(-abs(tval)))
  colnames(TAB) <- c("Estimate", "Std.Error", "z-value", "Pr(>|z|)")
  rownames(TAB) <- rownapi
  printCoefmat(TAB, P.values=TRUE, signif.stars=FALSE, has.Pvalue=TRUE, digits=digits, dig.tst=digits, tst.ind=c(1,2,3))
  out_$coefficients_zero=round(TAB,digits=digits)
  cat("*************************************************************\n")
  cat("                 -2*log-likelihood: ",round(-2*object$logLik,digits=3),"\n")
  cat("                               AIC: ",round(-2*object$logLik + 2*sum(object$parms),digits=3),"\n")
  cat("                               BIC: ",round(-2*object$logLik + log(nrow(object$y))*sum(object$parms),digits=3),"\n")
  return(invisible(out_))
}

#' @method print zeroinflation
#' @export

print.zeroinflation <- function(x,...){
  cat("\nSample size:",nrow(x$y),"\n")
  family <- switch(x$family$counts$family,
                   "nb1"="Negative Binomial type I","nb2"="Negative Binomial type II",
                   "nbf"="Negative Binomial","poi"="Poisson")
  cat(ifelse(x$type=="Zero-alteration","     Family: zero-altered","     Family: zero-inflated"),family)
  cat("\n*************************************************************\n")
  cat(paste(rep(" ",nchar(x$type)-6),collapse=""),"Sample size:",nrow(x$y),"\n")
  cat(paste(rep(" ",nchar(x$type)-6),collapse=""),"Count model:",family,"with",x$family$counts$link,"link\n")
  cat(x$type,"model: Bernoulli with",x$family$zeros$link,"link\n")
  cat("*************************************************************\n")
  cat("                 -2*log-likelihood: ",round(-2*x$logLik,digits=3),"\n")
  cat("                               AIC: ",round(-2*x$logLik + 2*sum(x$parms),digits=3),"\n")
  cat("                               BIC: ",round(-2*x$logLik + log(nrow(x$y))*sum(x$parms),digits=3),"\n")
}

#' @method print overglm
#' @export

print.overglm <- function(x,...){
  cat("\n Sample size:",nrow(x$y),"\n")
  family <- switch(x$family$family,
                   "nb1"="Negative Binomial type I","nb2"="Negative Binomial type II",
                   "nbf"="Negative Binomial","poi"="Poisson",
                   "rcb"="Random-clumped Binomial","bb"="Beta-Binomial")
  cat("     Family:",paste0(ifelse(x$zero.trunc,"zero-truncated ",""),family),"with",x$family$link,"link")
  cat("\n*************************************************************\n")
  cat("                 -2*log-likelihood: ",round(-2*x$logLik,digits=3),"\n")
  cat("                               AIC: ",round(-2*x$logLik + 2*sum(x$parms),digits=3),"\n")
  cat("                               BIC: ",round(-2*x$logLik + log(nrow(x$y))*sum(x$parms),digits=3),"\n")
}

#' @title Residuals in Regression Models to deal with Zero-Excess in Count Data
#' @description Computes various types of residuals to assess the individual quality of model fit in regression models
#' to deal with zero-excess in count data.
#' @param object an object of class \emph{zeroinflation}.
#' @param type an (optional) character string which allows to specify the required type of residuals. The available options are: (1)
#' the difference between the observed response and the fitted mean ("response"); (2) the standardized difference between
#' the observed response and the fitted mean ("standardized"); (3) the randomized quantile residual ("quantile"). By
#' default, \code{type} is set to "quantile".
#' @param plot.it an (optional) logical switch indicating if the plot of residuals versus the fitted values is required. As default, \code{plot.it} is set to \code{FALSE}.
#' @param identify an (optional) positive integer value indicating the number of individuals to identify on the plot of residuals versus the fitted values. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A vector with the observed residuals type \code{type}.
#' @examples
#' ####### Example 1: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit1 <- zeroalt(infections ~ frequency | location, family="nb1(log)", data=swimmers)
#' residuals(fit1, type="quantile", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'           col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ####### Example 2: Article production by graduate students in biochemistry PhD programs
#' bioChemists <- pscl::bioChemists
#' fit2 <- zeroinf(art ~ fem + kid5 + ment | ment, family="nb1(log)", data = bioChemists)
#' residuals(fit2, type="quantile", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'           col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' @method residuals zeroinflation
#' @export
#' @references Dunn P.K., Smyth G.K. (1996) Randomized Quantile Residuals. \emph{Journal of Computational and Graphical Statistics}, 5, 236-244.
#'
residuals.zeroinflation <- function(object,type=c("quantile","standardized","response"),plot.it=FALSE,identify,...){
  type <- match.arg(type)
  mu <- object$fitted.values$counts
  pi <- object$fitted.values$zeros
  y <- object$y
  n <- length(mu)
  if(object$family$counts$family %in% c("nb1","nb2","nbf")){
    phi <- exp(object$coefficients$counts[object$parms[1]+1])
    tau <- switch(object$family$counts$family,nb1=0,nb2=-1,nbf=object$coefficients$counts[object$parms[1]+2])
    if(object$type=="Zero-inflation") delta <- 1 - pi
    else delta <- (1 - pi)/(1 - dnbinom(0,mu=mu,size=1/(phi*mu^tau)))
    res <- y - delta*mu
    if(type=="quantile"){
      u <- runif(n)
      res <- (1 - delta) + delta*pnbinom(y - 1,mu=mu,size=1/(phi*mu^tau)) + delta*dnbinom(y,mu=mu,size=1/(phi*mu^tau))*u
      res <- ifelse(y==0,(1 - delta)*u + delta*dnbinom(0,mu=mu,size=1/(phi*mu^tau))*u,res)
      res <- ifelse(res < 1e-16,1e-16,res); res <- ifelse(res > 1 - (1e-16),1 - (1e-16),res); res <- qnorm(res)
    }
    if(type=="standardized") res <- res/sqrt(delta*mu*(1 + phi*mu^(tau + 1)) + mu^2*delta*(1 - delta))
  }
  if(object$family$counts$family=="poi"){
    if(object$type=="Zero-inflation") delta <- 1 - pi
    else delta <- (1 - pi)/(1 - dpois(0,lambda=mu))
    res <- y - delta*mu
    if(type=="quantile"){
      u <- runif(n)
      res <- (1 - delta) + delta*ppois(y - 1,lambda=mu) + delta*dpois(y,lambda=mu)*u
      res <- ifelse(y==0,(1 - delta)*u + delta*dpois(0,lambda=mu)*u,res)
      res <- ifelse(res < 1e-16,1e-16,res); res <- ifelse(res > 1 - (1e-16),1 - (1e-16),res); res <- qnorm(res)
    }
    if(type=="standardized") res <- res/sqrt(delta*mu + mu^2*delta*(1 - delta))
  }
  if(plot.it){
    nano <- list(...)
    nano$x <- delta*mu
    nano$y <- res
    if(is.null(nano$ylim)) nano$ylim <- c(min(-3.5,min(res)),max(+3.5,max(res)))
    if(is.null(nano$xlab)) nano$xlab <- "Estimated mean"
    if(is.null(nano$ylab)) nano$ylab <- paste(type," - type residuals",sep="")
    if(is.null(nano$pch))  nano$pch  <- 20
    if(is.null(nano$labels)) labels <- 1:n
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    do.call("plot",nano)
    abline(h=-3,lty=3)
    abline(h=+3,lty=3)
    if(!missingArg(identify)) identify(delta*mu,res,n=max(1,floor(abs(identify))),labels=labels)
  }
  res <- as.matrix(res)
  colnames(res) <- type
  return(res)
}

#' @title Residuals for alternatives to the Poisson and Binomial Regression Models under the presence of Overdispersion.
#' @description Computes various types of residuals to assess the individual quality of model fit for
#' regression models based on the negative binomial, beta-binomial, and random-clumped binomial
#' distributions, which are alternatives to the Poisson and binomial regression models under the presence of overdispersion.
#' @param object an object of class \emph{overglm}.
#' @param type an (optional) character string which allows to specify the required type of residuals. The available options are: (1)
#' the difference between the observed response and the fitted mean ("response"); (2) the standardized difference between
#' the observed response and the fitted mean ("standardized"); and (3) the randomized quantile residual ("quantile"). By
#' default, \code{type} is set to "quantile".
#' @param plot.it an (optional) logical switch indicating if the plot of residuals versus the fitted values is required. As default, \code{plot.it} is set to \code{FALSE}.
#' @param identify an (optional) positive integer value indicating the number of individuals to identify on the plot of residuals versus the fitted values. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A vector with the observed \code{type}-type residuals.
#' @examples
#' ###### Example 1: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit1 <- overglm(infections ~ frequency + location, family="nb1(log)", data=swimmers)
#' residuals(fit1, type="quantile", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'           col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 2: Article production by graduate students in biochemistry PhD programs
#' bioChemists <- pscl::bioChemists
#' fit2 <- overglm(art ~ fem + kid5 + ment, family="nb1(log)", data = bioChemists)
#' residuals(fit2, type="quantile", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'           col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 3: Agents to stimulate cellular differentiation
#' data(cellular)
#' fit3 <- overglm(cbind(cells,200-cells) ~ tnf + ifn, family="bb(logit)", data=cellular)
#' residuals(fit3, type="quantile", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'           col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' @method residuals overglm
#' @export
#' @references Dunn P.K., Smyth G.K. (1996) Randomized Quantile Residuals. \emph{Journal of Computational and Graphical Statistics}, 5, 236-244.

residuals.overglm <- function(object,type=c("quantile","standardized","response"), plot.it=FALSE, identify, ...){
  type <- match.arg(type)

  if(object$family$family=="bb"){
    m <- apply(object$y,1,sum);delta <- m
    mu <- object$fitted.values
    phi <- exp(object$coefficients[object$parms[1] + 1])
    y <- as.matrix(object$y[,1])
    res <- y - m*mu; n <- length(y)
    if(type=="quantile"){
      alpha <- mu/phi; lambda <- (1 - mu)/phi; u <- runif(n)
      res <- matrix(NA,n,1)
      for(i in 1:n){
        ys <- 0:max(0,y[i] - 1)
        res[i] <- sum(choose(m[i],ys)*exp(Lgamma(alpha[i] + ys) + Lgamma(m[i] - ys + lambda[i]) - Lgamma(m[i] + 1/phi))) +
          choose(m[i],y[i])*exp(Lgamma(alpha[i] + y[i]) + Lgamma(m[i] - y[i] + lambda[i]) - Lgamma(m[i] + 1/phi))*u[i]
      }
      res <- res/beta(alpha,lambda)
      res <- ifelse(res < 1e-16,1e-16,res); res <- ifelse(res > 1 - (1e-16),1 - (1e-16),res); res <- qnorm(res)
    }
    if(type=="standardized") res <- (y - m*mu)/sqrt(m*mu*(1 - mu)*(1 + phi*(m - 1)/(phi + 1)))
  }

  if(object$family$family=="rcb"){
    m <- apply(object$y,1,sum);delta <- m
    mu <- object$fitted.values
    phi <- exp(object$coefficients[object$parms[1] + 1])/(1 + exp(object$coefficients[object$parms[1] + 1]))
    y <- as.matrix(object$y[,1])
    res <- y - m*mu; n <- length(y)
    if(type=="quantile"){
      res <- mu*pbinom(y - 1,m,(1 - phi)*mu + phi) + (1 - mu)*pbinom(y - 1,m,(1 - phi)*mu) +
        (mu*dbinom(y,m,(1 - phi)*mu + phi) + (1 - mu)*dbinom(y,m,(1 - phi)*mu))*runif(n)
      res <- ifelse(res < 1e-16,1e-16,res); res <- ifelse(res > 1 - (1e-16),1 - (1e-16),res); res <- qnorm(res)
    }
    if(type=="standardized") res <- (y - m*mu)/sqrt(m*mu*(1 - mu)*(1 + (m - 1)*phi^2))
  }

  if(object$family$family %in% c("nb1","nb2","nbf")){
    mu <- object$fitted.values
    phi <- exp(object$coefficients[object$parms[1] + 1])
    tau <- switch(object$family$family, nb1=0, nb2=-1, nbf=object$coefficients$counts[object$parms[1] + 2])
    y <- object$y
    n <- length(mu)
    if(object$zero.trunc) delta <- 1/(1 - dnbinom(0,mu=mu,size=1/(phi*mu^tau))) else delta <- 1
    res <- y - delta*mu
    if(type=="quantile"){
      u <- runif(n)
      res <- (1 - delta) + delta*pnbinom(y - 1,mu=mu,size=1/(phi*mu^tau)) + delta*dnbinom(y,mu=mu,size=1/(phi*mu^tau))*u
      res <- ifelse(y==0,(1 - delta)*u + delta*dnbinom(0,mu=mu,size=1/(phi*mu^tau))*u,res)
      res <- ifelse(res < 1e-16,1e-16,res); res <- ifelse(res > 1 - (1e-16),1 - (1e-16),res); res <- qnorm(res)
    }
    if(type=="standardized") res <- res/sqrt(delta*mu*(1 + phi*mu^(tau + 1)) + mu^2*delta*(1 - delta))
  }

  if(object$family$family=="poi"){
    mu <- object$fitted.values
    y <- object$y
    n <- length(mu)
    if(object$zero.trunc) delta <- 1/(1 - dpois(0,lambda=mu)) else delta <- 1
    res <- y - delta*mu
    if(type=="quantile"){
      u <- runif(n)
      res <- (1 - delta) + delta*ppois(y - 1,lambda=mu) + delta*dpois(y,lambda=mu)*u
      res <- ifelse(y==0,(1 - delta)*u + delta*dpois(0,lambda=mu)*u,res)
      res <- ifelse(res < 1e-16,1e-16,res); res <- ifelse(res > 1 - (1e-16),1 - (1e-16),res); res <- qnorm(res)
    }
    if(type=="standardized") res <- res/sqrt(delta*mu + mu^2*delta*(1 - delta))
  }

  if(plot.it){
    nano <- list(...)
    nano$x <- delta*mu
    nano$y <- res
    if(is.null(nano$ylim)) nano$ylim <- c(min(-3.5,min(res)),max(+3.5,max(res)))
    if(is.null(nano$xlab)) nano$xlab <- "Estimated mean"
    if(is.null(nano$ylab)) nano$ylab <- paste(type," - type residuals",sep="")
    if(is.null(nano$pch))  nano$pch  <- 20
    if(is.null(nano$labels)) labels <- 1:n
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    do.call("plot",nano)
    abline(h=-3,lty=3)
    abline(h=+3,lty=3)
    if(!missingArg(identify)) identify(delta*mu,res,n=max(1,floor(abs(identify))),labels=labels)
  }
  res <- as.matrix(res)
  colnames(res) <- type
  return(res)
}

#' @title Comparison of nested models for alternatives to the Poisson and Binomial Regression Models under the presence of Overdispersion.
#' @description Allows to compare nested models for regression models based on the negative binomial, beta-binomial, and random-clumped binomial
#' distributions, which are alternatives to the Poisson and binomial regression models under the presence of overdispersion.
#' The comparisons are performed by using the Wald, score, gradient or likelihood ratio tests.
#' @param object an object of the class \emph{overglm}.
#' @param ... another objects of the class \emph{overglm}.
#' @param test an (optional) character string which allows to specify the required test. The available options are: Wald ("wald"),
#' Rao's score ("score"), likelihood ratio ("lr") and Terrell's gradient ("gradient") tests. As default, \code{test} is
#' set to "wald".
#' @param verbose an (optional) logical indicating if should the report of results be printed. As default, \code{verbose}
#' is set to TRUE.
#' @return A matrix with the following three columns:
#' \tabular{ll}{
#' \code{Chi} \tab The value of the statistic of the test,\cr
#' \tab \cr
#' \code{Df}\tab The number of degrees of freedom,\cr
#' \tab \cr
#' \code{Pr(>Chi)} \tab The \emph{p}-value of the \code{test}-type test computed using the Chi-square distribution.\cr
#' }
#' @method anova overglm
#' @export
#' @references Buse A. (1982) The Likelihood Ratio, Wald, and Lagrange Multiplier Tests: An Expository Note.
#'                             \emph{The American Statistician} 36, 153-157.
#' @references Terrell G.R. (2002) The gradient statistic. \emph{Computing Science and Statistics} 34, 206–215.
#' @examples
#' ## Example 1: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit1 <- overglm(infections ~ frequency + location + age + gender, family="nb1(log)", data=swimmers)
#' anova(fit1, test="wald")
#' anova(fit1, test="score")
#' anova(fit1, test="lr")
#' anova(fit1, test="gradient")
#'
#' ## Example 2: Agents to stimulate cellular differentiation
#' data(cellular)
#' fit2 <- overglm(cbind(cells,200-cells) ~ tnf*ifn, family="bb(logit)", data=cellular)
#' anova(fit2, test="wald")
#' anova(fit2, test="score")
#' anova(fit2, test="lr")
#' anova(fit2, test="gradient")
#'
anova.overglm <- function(object,...,test=c("wald","lr","score","gradient"),verbose=TRUE){
  test <- match.arg(test)
  x <- list(object,...)
  if(any(unlist(lapply(x,function(xx) class(xx)[1])!="overglm")))
    stop("Only overglm-type objects are supported!!",call.=FALSE)
  if(length(x)==1){
    terminos <- attr(object$terms,"term.labels")
    x[[1]] <- update(object,paste(". ~ . -",paste(terminos,collapse="-")))
    for(i in 1:length(terminos)) x[[i+1]] <- update(x[[i]],paste(". ~ . + ",terminos[i]))
  }else{
    current <- list(x[[1]]$y,x[[1]]$family,x[[1]]$prior.weights,x[[1]]$offset,x[[1]]$zero.trunc)
    for(i in 2:length(x)){
      target <- list(x[[i]]$y,x[[i]]$family,x[[i]]$prior.weights,x[[i]]$offset,x[[i]]$zero.trunc)
      if(!isTRUE(all.equal(target,current))) stop("These models are not nested!!!",call.=FALSE)
    }
  }
  hast <- length(x)
  out_ <- matrix(0,hast-1,3)

  for(i in 2:hast){
    vars0 <- rownames(coef(x[[i-1]]))
    vars1 <- rownames(coef(x[[i]]))
    nest <- vars0 %in% vars1
    ids <- is.na(match(vars1,vars0))
    if(test=="wald") sc <- crossprod(coef(x[[i]])[ids],chol2inv(chol(vcov(x[[i]])[ids,ids])))%*%coef(x[[i]])[ids]
    if(test=="lr") sc <- 2*(logLik(x[[i]])-logLik(x[[i-1]]))
    if(test %in% c("score","gradient")){
      envir <- environment(x[[i]]$score)
      environment(x[[i]]$hess) <- envir
      envir$weights <- x[[i]]$prior.weights
      envir$zero.trunc <- x[[i]]$zero.trunc
      envir$offs <- x[[i]]$offset
      if(ncol(x[[i]]$y)==2){
        envir$m <- x[[i]]$y[,1] + x[[i]]$y[,2]
        envir$y <- x[[i]]$y[,1]
      }else envir$y <- x[[i]]$y
      envir$X <- model.matrix(x[[i]])
      envir$p <- ncol(envir$X)
      envir$n <- nrow(envir$X)
      if(x[[i]]$family$family %in% c("bb","rcb")) familyf <- binomial(x[[i]]$family$link)
      else familyf <- poisson(x[[i]]$family$link)
      envir$familyf <- familyf
      envir$parms <- x[[i]]$parms
      theta0 <- x[[i]]$coefficients
      theta0[ids] <- rep(0,sum(ids))
      theta0[!ids] <- x[[i-1]]$coefficients
      u0 <- x[[i]]$score(theta0)[ids]
      if(test=="score"){
        v0 <- try(chol(-x[[i]]$hess(theta0)),silent=TRUE)
        if(is.matrix(v0)) v0 <- chol2inv(v0) else v0 <- solve(-x[[i]]$hess(theta0))
        sc <- abs(crossprod(u0,v0[ids,ids])%*%u0)
      }else sc <- abs(crossprod(u0,coef(x[[i]])[ids]))
    }
    df <- sum(ids)
    out_[i-1,] <- cbind(sc,df,1-pchisq(sc,df))
  }
  colnames(out_) <- c(" Chi  ", " Df", "  Pr(>Chi)")
  rownames(out_) <- paste(1:(hast-1),"vs",2:hast)
  if(verbose){
    test <- switch(test,
                   "lr"="Likelihood-ratio test",
                   "wald"="Wald test",
                   "score"="Rao's score test",
                   "gradient"="Gradient test")
    cat("\n ",test,"\n\n")
    for(i in 1:hast) cat(paste("Model", i,": ",x[[i]]$formula[2],x[[i]]$formula[1],x[[i]]$formula[3:length(x[[i]]$formula)],collapse=""),"\n")
    cat("\n")
    printCoefmat(out_, P.values=TRUE, has.Pvalue=TRUE, digits=5, signif.legend=TRUE, cs.ind=2)
  }
  return(invisible(out_))
}

#' @title Comparison of nested models for Regression Models to deal with Zero-Excess in Count Data
#' @description Allows to compare nested models for regression models used to deal with zero-excess in count data.
#' The comparisons are performed by using the Wald, score, gradient or likelihood ratio tests.
#' @param object an object of the class \emph{zeroinflation}.
#' @param submodel an (optional) character string which allows to specify the model: "counts" or "zeros". By default,
#' \code{submodel} is set to "counts".
#' @param ... another objects of the class \emph{zeroinflation}.
#' @param test an (optional) character string which allows to specify the required test. The available options are: Wald ("wald"),
#' Rao's score ("score"), likelihood ratio ("lr") and Terrell's gradient ("gradient") tests. As default, \code{test} is
#' set to "wald".
#' @param verbose an (optional) logical indicating if should the report of results be printed. As default, \code{verbose}
#' is set to TRUE.
#' @return A matrix with the following three columns:
#' \describe{
#' \item{\code{Chi}}{ The value of the statistic of the test,}
#' \item{\code{Df}}{ The number of degrees of freedom,}
#' \item{\code{Pr(>Chi)}}{ The \emph{p}-value of the test \emph{test} computed using the Chi-square distribution.}
#' }
#' @method anova zeroinflation
#' @export
#' @references Buse A. (1982) The Likelihood Ratio, Wald, and Lagrange Multiplier Tests: An Expository Note.
#'                             \emph{The American Statistician} 36, 153-157.
#' @references Terrell G.R. (2002) The gradient statistic. \emph{Computing Science and Statistics} 34, 206–215.
#' @examples
#' ####### Example 1: Article production by graduate students in biochemistry PhD programs
#' bioChemists <- pscl::bioChemists
#' fit1 <- zeroinf(art ~ fem + kid5 + ment | ment, family="nb1(log)", data = bioChemists)
#' anova(fit1,test="wald")
#' anova(fit1,test="lr")
#' anova(fit1,test="score")
#' anova(fit1,test="gradient")
#'
#' fit2 <- zeroalt(art ~ fem + kid5 + ment, family="nb1(log)", data = bioChemists)
#' anova(fit2,submodel="zeros",test="wald")
#' anova(fit2,submodel="zeros",test="lr")
#' anova(fit2,submodel="zeros",test="score")
#' anova(fit2,submodel="zeros",test="gradient")
#'
anova.zeroinflation <- function(object,...,test=c("wald","lr","score","gradient"),verbose=TRUE,submodel=c("counts","zeros")){
  test <- match.arg(test)
  submodel <- match.arg(submodel)
  x <- list(object,...)
  if(any(unlist(lapply(x,function(xx) class(xx)[1])!="zeroinflation")))
    stop("Only zeroinflation-type objects are supported!!",call.=FALSE)
  if(length(x)==1){
    if(submodel=="counts"){
      terminos <- attr(object$terms[["counts"]],"term.labels"); terminos2 <- attr(object$terms[["zeros"]],"term.labels")
      mnul <- paste("~",attr(object$terms[["counts"]],"intercept"),"|")
      x[[1]] <- eval(parse(text=paste("update(object,formula =",object$formula[[2]],mnul,paste(terminos2,collapse=" + "),")")))
      for(i in 1:length(terminos)) x[[i+1]] <- eval(parse(text=paste("update(x[[i]], . ~ . + ",terminos[i],")")))
    }else{
      terminos <- attr(object$terms[["zeros"]],"term.labels"); terminos2 <- attr(object$terms[["counts"]],"term.labels")
      mnul <- paste("|",attr(object$terms[["zeros"]],"intercept"),")")
      x[[1]] <- eval(parse(text=paste("update(object,formula =",object$formula[[2]],"~",paste(terminos2,collapse=" + "),mnul)))
      for(i in 1:length(terminos)) x[[i+1]] <- eval(parse(text=paste("update(x[[i]], . ~ . |. +",terminos[i],")")))
    }
  }else{
    current <- list(x[[1]]$y,x[[1]]$family,x[[1]]$prior.weights,x[[1]]$offset,x[[1]]$type)
    for(i in 2:length(x)){
      target <- list(x[[i]]$y,x[[i]]$family,x[[i]]$prior.weights,x[[i]]$offset,x[[i]]$type)
      if(!isTRUE(all.equal(target,current))) stop("These models are not nested!!!",call.=FALSE)
    }
  }
  hast <- length(x)
  out_ <- matrix(0,hast-1,3)
  for(i in 2:hast){
    vars0 <- c(paste0("c",rownames(x[[i-1]]$coefficients$counts)),paste0("z",rownames(x[[i-1]]$coefficients$zeros)))
    vars1 <- c(paste0("c",rownames(x[[i]]$coefficients$counts)),paste0("z",rownames(x[[i]]$coefficients$zeros)))
    nest <- vars0 %in% vars1
    ids <- is.na(match(vars1,vars0))
    if(test=="wald"){
      b <- c(x[[i]]$coefficients$counts,x[[i]]$coefficients$zeros)[ids]
      vc <- chol2inv(x[[i]]$R)[ids,ids]
      sc <- crossprod(b,chol2inv(chol(vc)))%*%b
    }
    if(test=="lr") sc <- 2*(logLik(x[[i]])-logLik(x[[i-1]]))
    if(test %in% c("score","gradient")){
      envir <- environment(x[[i]]$score)
      environment(x[[i]]$hess) <- envir
      envir$weights <- x[[i]]$prior.weights
      n <- length(x[[i]]$prior.weights)
      envir$y <- x[[i]]$y
      envir$X <- model.matrix(x[[i]],submodel="counts"); envir$Z <- model.matrix(x[[i]],submodel="zeros")
      envir$p <- x[[i]]$parms[1]; envir$ep <- x[[i]]$parms[2]; envir$q <- x[[i]]$parms[3]; envir$n <- nrow(envir$X)
      envir$offsx <- x[[i]]$offset$counts;envir$offsz <- x[[i]]$offset$zeros
      envir$familyx <- x[[i]]$family$counts;envir$familyz <- x[[i]]$family$zeros;zeros <- envir$y==0
      envir$X2 <- envir$X[!zeros,];envir$offsx2 <- envir$offsx[!zeros];envir$y2 <- envir$y[!zeros]
      envir$weights2 <- envir$weights[!zeros];envir$zeros <- zeros
      b <- c(x[[i]]$coefficients$counts,x[[i]]$coefficients$zeros)
      theta0 <- b
      theta0[ids] <- rep(0,sum(ids))
      theta0[!ids] <- c(x[[i-1]]$coefficients$counts,x[[i-1]]$coefficients$zeros)
      u0 <- x[[i]]$score(theta0)[ids]
      if(test=="score"){
        v0 <- try(chol(-x[[i]]$hess(theta0)),silent=TRUE)
        if(is.matrix(v0)) v0 <- chol2inv(v0) else v0 <- solve(-x[[i]]$hess(theta0))
        sc <- abs(crossprod(u0,v0[ids,ids])%*%u0)
      }else sc <- abs(crossprod(u0,b[ids]))
    }
    df <- sum(ids)
    out_[i-1,] <- cbind(sc,df,1-pchisq(sc,df))
  }
  colnames(out_) <- c(" Chi  ", " Df", "  Pr(>Chi)")
  rownames(out_) <- paste(1:(hast-1),"vs",2:hast," ")
  if(verbose){
    test <- switch(test,
                   "lr"="Likelihood-ratio test",
                   "wald"="Wald test",
                   "score"="Rao's score test",
                   "gradient"="Gradient test")
    cat("\n ",test,"\n\n")
    deltac <- nchar(paste(attr(x[[hast]]$terms[["counts"]],"term.labels"),collapse=" + "))
    deltaz <- nchar(paste(attr(x[[hast]]$terms[["zeros"]],"term.labels"),collapse=" + "))
    for(i in 1:hast){
      c <- paste(attr(x[[i]]$terms[["counts"]],"term.labels"),collapse=" + ")
      z <- paste(attr(x[[i]]$terms[["zeros"]],"term.labels"),collapse=" + ")
      fc <- paste0(c,paste(replicate(deltac-nchar(c)," "),collapse=""))
      fz <- paste0(z,paste(replicate(deltaz-nchar(z)," "),collapse=""))
      cat(paste("Model",i,":",x[[i]]$formula[2],x[[i]]$formula[1],fc,"|",fz,collapse=""),"\n")
    }
    cat("\n")
    printCoefmat(out_, P.values=TRUE, has.Pvalue=TRUE, digits=5, signif.legend=TRUE, cs.ind=2)
  }
  return(invisible(out_))
}

#' @title Dfbeta statistic for alternatives to the Poisson and Binomial Regression Models under the presence of Overdispersion.
#' @description Produces an approximation, better known as the \emph{one-step approximation}, of the effect on the
#' parameter estimates of deleting each individual in turn. This function also can produce an index plot of the
#' Dfbeta statistic for some parameter chosen via the argument \code{coefs}.
#' @param model an object of class \emph{overglm}.
#' @param coefs	an (optional) character string which (partially) match with the names of some model parameters.
#' @param identify an (optional) integer indicating the number of individuals to identify on the plot of the Dfbeta statistic.
#' This is only appropriate if \code{coefs} is specified.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used
#' to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main},
#' \code{sub}, \code{xlab}, \code{ylab}.
#' @details The \emph{one-step approximation} of the estimates of the parameters when the \emph{i}-th individual
#' is excluded from the dataset consists of the vector obtained as result of the first iteration of the Newthon-Raphson
#' algorithm when it is performed using: (1) a dataset in which the \emph{i}-th individual is excluded; and (2)
#' a starting value which is the estimate of the same model but based on the dataset inluding all individuals.
#' @return A matrix with so many rows as individuals in the sample and so many columns as parameters in the linear
#' predictor. The \eqn{i}-th row of that matrix corresponds to the difference between the estimates of the parameters
#' in the linear predictor using all individuals and the \emph{one-step approximation} of those estimates when the
#' \emph{i}-th individual is excluded from the dataset.
#' @references Pregibon D. (1981). Logistic regression diagnostics. \emph{The Annals of Statistics}, 9, 705-724.
#' @method dfbeta overglm
#' @export
#' @examples
#' ###### Example 1: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit1 <- overglm(infections ~ frequency + location, family="nb1(log)", data=swimmers)
#' dfbeta(fit1, coefs="frequency", col="red", lty=1, lwd=1, col.lab="blue",
#'        col.axis="blue", col.main="black", family="mono", cex=0.8, main="frequency")
#'
#' ###### Example 2: Article production by graduate students in biochemistry PhD programs
#' bioChemists <- pscl::bioChemists
#' fit2 <- overglm(art ~ fem + kid5 + ment, family="nb1(log)", data = bioChemists)
#' dfbeta(fit2, coefs="fem", col="red", lty=1, lwd=1, col.lab="blue",
#'        col.axis="blue", col.main="black", family="mono", cex=0.8, main="fem")
#'
#' ###### Example 3: Agents to stimulate cellular differentiation
#' data(cellular)
#' fit3 <- overglm(cbind(cells,200-cells) ~ tnf + ifn, family="bb(logit)", data=cellular)
#' dfbeta(fit3, coefs="tnf", col="red", lty=1, lwd=1, col.lab="blue",
#'        col.axis="blue", col.main="black", family="mono", cex=0.8, main="tnf")
#' @export
#'
dfbeta.overglm <- function(model, coefs, identify, ...){
  envir <- environment(model$score)
  environment(model$hess) <- envir
  weights <- envir$weights <- model$prior.weights
  envir$zero.trunc <- model$zero.trunc
  envir$offs <- model$offset
  if(ncol(model$y)==2){
    envir$m <- model$y[,1] + model$y[,2]
    envir$y <- model$y[,1]
  }else envir$y <- model$y
  envir$X <- model.matrix(model)
  envir$p <- ncol(envir$X)
  n <- envir$n <- nrow(envir$X)
  if(model$family$family %in% c("bb","rcb")) familyf <- binomial(model$family$link)
  else familyf <- poisson(model$family$link)
  envir$familyf <- familyf
  envir$parms <- model$parms
  dfbetas <- matrix(0,n,envir$p+1)
  temp <- data.frame(y=model$y,X=envir$X,offs=envir$offs,weights=weights,ids=1:n)
  d <- ncol(temp)
  colnames(temp) <- c(paste("var",1:(d-1),sep=""),"ids")
  orden <- eval(parse(text=paste("with(temp,order(",paste(colnames(temp)[-d],collapse=","),"))",sep="")))
  temp2 <- temp[orden,]
  envir$weights[temp2$ids[1]] <- 0
  dfbetas[1,] <- chol2inv(chol(-model$hess(model$coefficients)))%*%model$score(model$coefficients)
  for(i in 2:n){
    if(all(temp2[i,-d]==temp2[i-1,-d])) dfbetas[i,] <- dfbetas[i-1,]
    else{
      envir$weights <- weights
      envir$weights[temp2$ids[i]] <- 0
      dfbetas[i,] <- -chol2inv(chol(-model$hess(model$coefficients)))%*%model$score(model$coefficients)
    }
  }
  dfbetas <- dfbetas[order(temp2$ids),]
  colnames(dfbetas) <- rownames(model$coefficients)

  if(!missingArg(coefs)){
    ids <- grep(coefs,colnames(dfbetas),ignore.case=TRUE)
    if(length(ids) > 0){
      nano <- list(...)
      if(is.null(nano$labels)) labels <- 1:nrow(dfbetas)
      else{
        labels <- nano$labels
        nano$labels <- NULL
      }
      nano$x <- 1:nrow(dfbetas)
      if(is.null(nano$xlab)) nano$xlab <- "Observation (i)"
      if(is.null(nano$type)) nano$type <- "h"
      if(is.null(nano$ylab)) nano$ylab <- expression(hat(beta)-hat(beta)[("- i")])
      oldpar <- par(no.readonly=TRUE)
      on.exit(par(oldpar))
      par(mfrow=c(1,length(ids)))
      for(i in 1:length(ids)){
        nano$y <- dfbetas[,ids[i]]
        nano$main <- colnames(dfbetas)[ids[i]]
        do.call("plot",nano)
        if(any(nano$y > 0)) abline(h=3*mean(nano$y[nano$y > 0]),lty=3)
        if(any(nano$y < 0)) abline(h=3*mean(nano$y[nano$y < 0]),lty=3)
        if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=labels)

      }
    }else stop(paste("There are no variables with the name",coefs,collapse=""),call.=FALSE)
  }
  return(dfbetas)
}

#' @title Dfbeta statistic for Regression Models to deal with Zero-Excess in Count Data
#' @description Produces an approximation, better known as the \emph{one-step approximation}, of the effect on the
#' parameter estimates of deleting each individual in turn. This function also can produce an index plot
#' of the Dfbeta statistic for some parameter chosen via the argument \code{coefs}.
#' @param model an object of class \emph{zeroinflation}.
#' @param submodel an (optional) character string which allows to specify the model: "counts" or "zeros". By default,
#' \code{submodel} is set to "counts".
#' @param coefs	an (optional) character string which (partially) match with the names of some model parameters.
#' @param identify an (optional) integer indicating the number of individuals to identify on the plot of the Dfbeta statistic. This
#' is only appropriate if \code{coefs} is specified.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used
#' to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main},
#' \code{sub}, \code{xlab}, \code{ylab}.
#' @details The \emph{one-step approximation} of the estimates of the parameters when the \emph{i}-th individual
#' is excluded from the dataset consists of the vector obtained as result of the first iteration of the Newthon-Raphson
#' algorithm when it is performed using: (1) a dataset in which the \emph{i}-th individual is excluded; and (2)
#' a starting value which is the estimate of the same model but based on the dataset inluding all individuals.
#' @return A matrix with so many rows as individuals in the sample and so many columns as parameters in the linear
#' predictor. The \eqn{i}-th row of that matrix corresponds to the difference between the estimates of the parameters
#' in the linear predictor using all individuals and the \emph{one-step approximation} of those estimates when the
#' \emph{i}-th individual is excluded from the dataset.
#' @references Pregibon D. (1981). Logistic regression diagnostics. \emph{The Annals of Statistics}, 9, 705-724.
#' @method dfbeta zeroinflation
#' @export
#' @examples
#' ####### Example 1: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit <- zeroinf(infections ~ frequency + location, family="nb1(log)", data=swimmers)
#'
#' dfbeta(fit, submodel="counts", coefs="frequency", col="red", lty=1, lwd=1,
#'        col.lab="blue", col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' dfbeta(fit, submodel="zeros", coefs="location", col="red", lty=1, lwd=1,
#'        col.lab="blue", col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
dfbeta.zeroinflation <- function(model,submodel=c("counts","zeros"),coefs,identify,...){
  submodel <- match.arg(submodel)
  envir <- environment(model$score)
  environment(model$hess) <- envir
  envir$weights <- model$prior.weights
  n <- length(model$prior.weights)
  envir$y <- model$y
  envir$X <- model.matrix(model,submodel="counts"); envir$Z <- model.matrix(model,submodel="zeros")
  envir$p <- model$parms[1]; envir$ep <- model$parms[2]; envir$q <- model$parms[3]
  envir$offsx <- model$offset$counts;envir$offsz <- model$offset$zeros
  envir$familyx <- model$family$counts;envir$familyz <- model$family$zeros;zeros <- envir$y==0
  envir$X2 <- envir$X[!zeros,];envir$offsx2 <- envir$offsx[!zeros];envir$y2 <- envir$y[!zeros]
  envir$weights2 <- envir$weights[!zeros];envir$zeros <- zeros

  dfbetas <- matrix(0,n,sum(model$parms))
  temp <- data.frame(y=envir$y,X=envir$X,Z=envir$Z,offsx=envir$offsx,offsz=envir$offsz,weights=envir$weights,ids=1:n)
  theta_hat <- c(model$coefficients$counts,model$coefficients$zeros)
  p2 <- sum(model$parms[1:2])
  d <- ncol(temp)
  colnames(temp) <- c(paste("var",1:(d-1),sep=""),"ids")
  orden <- eval(parse(text=paste("with(temp,order(",paste(colnames(temp)[-d],collapse=","),"))",sep="")))
  temp2 <- temp[orden,]
  envir$weights[temp2$ids[1]] <- 0
  envir$weights2 <- envir$weights[!zeros]
  hessiana <- model$hess(theta_hat);scoref <- model$score(theta_hat)
  if(model$type=="Zero-inflation") dfbetas[1,] <- chol2inv(chol(-hessiana))%*%scoref
  else{
    dfbetas[1,c(1:p2)] <- chol2inv(chol(-hessiana[c(1:p2),c(1:p2)]))%*%scoref[c(1:p2)]
    dfbetas[1,-c(1:p2)] <- chol2inv(chol(-hessiana[-c(1:p2),-c(1:p2)]))%*%scoref[-c(1:p2)]
  }
  for(i in 2:n){
    if(all(temp2[i,-d]==temp2[i-1,-d])) dfbetas[i,] <- dfbetas[i-1,]
    else{
      envir$weights <- model$prior.weights
      envir$weights[temp2$ids[i]] <- 0
      envir$weights2 <- envir$weights[!zeros]
      hessiana <- model$hess(theta_hat);scoref <- model$score(theta_hat)
      if(model$type=="Zero-inflation") dfbetas[i,] <- chol2inv(chol(-hessiana))%*%scoref
      else{
        dfbetas[i,c(1:p2)] <- chol2inv(chol(-hessiana[c(1:p2),c(1:p2)]))%*%scoref[c(1:p2)]
        dfbetas[i,-c(1:p2)] <- chol2inv(chol(-hessiana[-c(1:p2),-c(1:p2)]))%*%scoref[-c(1:p2)]
      }
    }
  }
  dfbetas <- -dfbetas[order(temp2$ids),]
  out_ <- list(counts=dfbetas[,c(1:p2)],zeros=dfbetas[,-c(1:p2)])
  colnames(out_$counts) <- rownames(model$coefficients$counts)
  colnames(out_$zeros) <- rownames(model$coefficients$zeros)
  if(!missingArg(coefs)){
    ids <- grep(coefs,colnames(out_[[submodel]]),ignore.case=TRUE)
    if(length(ids) > 0){
      nano <- list(...)
      if(is.null(nano$labels)) labels <- 1:n
      else{
        labels <- nano$labels
        nano$labels <- NULL
      }
      nano$x <- 1:nrow(dfbetas)
      if(is.null(nano$xlab)) nano$xlab <- "Observation (i)"
      if(is.null(nano$type)) nano$type <- "h"
      if(is.null(nano$ylab)) nano$ylab <- expression(hat(beta)-hat(beta)[("- i")])
      oldpar <- par(no.readonly=TRUE)
      on.exit(par(oldpar))
      par(mfrow=c(1,length(ids)))
      for(i in 1:length(ids)){
        nano$y <- out_[[submodel]][,ids[i]]
        nano$main <- colnames(out_[[submodel]])[ids[i]]
        do.call("plot",nano)
        if(any(nano$y > 0)) abline(h=3*mean(nano$y[nano$y > 0]),lty=3)
        if(any(nano$y < 0)) abline(h=3*mean(nano$y[nano$y < 0]),lty=3)
        if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=labels)
      }
    }else stop(paste("There are no variables with the name",coefs,collapse=""),call.=FALSE)
  }
  return(out_)
}

#' @title Cook's Distance for alternatives to the Poisson and Binomial Regression Models under the presence of Overdispersion
#' @description Produces an approximation, better known as the \emph{one-step approximation}, of the Cook's distance,
#' which is aimed to measure the effect on the estimates of the parameters in the linear predictor of deleting each
#' observation in turn. This function also can produce an index plot of the Cook's distance for all parameters in
#' the linear predictor or for some subset of them (via the argument \code{coefs}).
#' @param model an object of class \emph{overglm}.
#' @param plot.it an (optional) logical indicating if the plot is required or just the data matrix in which that
#' plot is based. As default, \code{plot.it} is set to \code{FALSE}.
#' @param coefs	an (optional) character string which (partially) match with the names of some model parameters.
#' @param identify an (optional) integer indicating the number of individuals to identify on the plot of the Cook's
#' distance. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used
#' to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main},
#' \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix as many rows as individuals in the sample and one column with the values of the Cook's distance.
#' @details The Cook's distance consists of the \emph{distance} between two estimates of the parameters in the linear
#' predictor using a metric based on the (estimate of the) variance-covariance matrix. The first one set of estimates
#' is computed from a dataset including all individuals, and the second one is computed from a dataset in which the
#' \emph{i}-th individual is excluded. To avoid computational burden, the second set of estimates is replaced by its
#' \emph{one-step approximation}. See the \link{dfbeta.overglm} documentation.
#' @method cooks.distance overglm
#' @export
#' @examples
#' ###### Example 1: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit1 <- overglm(infections ~ frequency + location, family="nb1(log)", data=swimmers)
#'
#' ### Cook's distance for all parameters in the linear predictor
#' cooks.distance(fit1, plot.it=TRUE, col="red", lty=1, lwd=1, col.lab="blue",
#'                col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ### Cook's distance just for the parameter associated with 'frequency'
#' cooks.distance(fit1, plot.it=TRUE, coef="frequency", col="red", lty=1, lwd=1,
#'    col.lab="blue", col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 2: Article production by graduate students in biochemistry PhD programs
#' bioChemists <- pscl::bioChemists
#' fit2 <- overglm(art ~ fem + kid5 + ment, family="nb1(log)", data = bioChemists)
#'
#' ### Cook's distance for all parameters in the linear predictor
#' cooks.distance(fit2, plot.it=TRUE, col="red", lty=1, lwd=1, col.lab="blue",
#'                col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ### Cook's distance just for the parameter associated with 'fem'
#' cooks.distance(fit2, plot.it=TRUE, coef="fem", col="red", lty=1, lwd=1,
#'    col.lab="blue", col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 3: Agents to stimulate cellular differentiation
#' data(cellular)
#' fit3 <- overglm(cbind(cells,200-cells) ~ tnf + ifn, family="bb(logit)", data=cellular)
#'
#' ### Cook's distance for all parameters in the linear predictor
#' cooks.distance(fit3, plot.it=TRUE, col="red", lty=1, lwd=1, col.lab="blue",
#'                col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ### Cook's distance just for the parameter associated with 'tnf'
#' cooks.distance(fit3, plot.it=TRUE, coef="tnf", col="red", lty=1, lwd=1,
#'   col.lab="blue", col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
cooks.distance.overglm <- function(model, plot.it=FALSE, coefs, identify,...){
  dfbetas <- dfbeta(model)
  p <- model$parms[1]
  met <- vcov(model)
  dfbetas <- dfbetas[,1:p]
  subst <- NULL
  if(!missingArg(coefs)){
    ids <- grepl(coefs,colnames(dfbetas),ignore.case=TRUE)
    if(sum(ids) > 0){
      subst <- colnames(dfbetas)[ids]
      dfbetas <- as.matrix(dfbetas[,ids])
      met <- as.matrix(met[ids,ids])
    }
  }
  met2 <- try(chol(met),silent=TRUE)
  if(is.matrix(met2)) met2 <- chol2inv(met2) else met2 <- solve(met)
  CD <- as.matrix(apply((dfbetas%*%met2)*dfbetas,1,mean))
  colnames(CD) <- "Cook's distance"
  if(plot.it){
    nano <- list(...)
    if(is.null(nano$labels)) labels <- 1:nrow(dfbetas)
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    nano$x <- 1:nrow(dfbetas)
    nano$y <- CD
    if(is.null(nano$xlab)) nano$xlab <- "Observation (i)"
    if(is.null(nano$type)) nano$type <- "h"
    if(is.null(nano$ylab)) nano$ylab <- expression(frac(1,p)(hat(beta)-hat(beta)[{(-~~i)}])^{T}~(Var(hat(beta)))^{-1}~(hat(beta)-hat(beta)[{(-~~i)}]))
    do.call("plot",nano)
    abline(h=3*mean(CD),lty=3)
    if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=labels)
  }
  if(!is.null(subst)){
    message("The coefficients included in the Cook's distance are:\n")
    message(subst)
  }
  return(CD)
}

#' @title Cook's Distance for Regression Models to deal with Zero-Excess in Count Data
#' @description Produces an approximation, better known as the \emph{one-step approximation}, of the Cook's distance,
#' which is aimed to measure the effect on the estimates of the parameters in the linear predictor of deleting each
#' observation in turn. This function also can produce an index plot of the Cook's distance for all parameters in
#' the linear predictor or for some subset of them (via the argument \code{coefs}).
#' @param model an object of class \emph{zeroinflation}.
#' @param submodel an (optional) character string which allows to specify the model: "counts", "zeros" or "full". By default,
#' \code{submodel} is set to "counts".
#' @param plot.it an (optional) logical indicating if the plot is required or just the data matrix in which that
#' plot is based. As default, \code{plot.it} is set to \code{FALSE}.
#' @param coefs	an (optional) character string which (partially) match with the names of some model parameters.
#' @param identify an (optional) integer indicating the number of individuals to identify on the plot of the Cook's
#' distance. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used
#' to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main},
#' \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix as many rows as individuals in the sample and one column with the values of the Cook's distance.
#' @details The Cook's distance consists of the \emph{distance} between two estimates of the parameters in the linear
#' predictor using a metric based on the (estimate of the) variance-covariance matrix. The first one set of estimates
#' is computed from a dataset including all individuals, and the second one is computed from a dataset in which the
#' \emph{i}-th individual is excluded. To avoid computational burden, the second set of estimates is replaced by its
#' \emph{one-step approximation}. See the \link{dfbeta.zeroinflation} documentation.
#' @method cooks.distance zeroinflation
#' @export
#' @examples
#'
#' ####### Example 1: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit <- zeroinf(infections ~ frequency + location, family="nb1(log)", data=swimmers)
#'
#' ### Cook's distance for all parameters in the "counts" model
#' cooks.distance(fit, submodel="counts", plot.it=TRUE, col="red", lty=1, lwd=1,
#'          col.lab="blue", col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ### Cook's distance for all parameters in the "zeros" model
#' cooks.distance(fit, submodel="zeros", plot.it=TRUE, col="red", lty=1, lwd=1,
#'          col.lab="blue", col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
cooks.distance.zeroinflation <- function(model, submodel=c("counts","zeros","full"), plot.it=FALSE, coefs, identify,...){
  submodel <- match.arg(submodel)
  dfbetas <- dfbeta(model)
  if(submodel=="full"){
    dfbetas <- cbind(dfbetas$counts[,1:model$parms[1]],dfbetas$zeros)
    met <- chol2inv(model$R)[-c(model$parms[1]+1,sum(model$parms[1:2])),-c(model$parms[1]+1,sum(model$parms[1:2]))]
  }
  if(submodel=="counts"){
    dfbetas <- dfbetas$counts[,1:model$parms[1]]
    met <- vcov(model,submodel="counts")[1:model$parms[1],1:model$parms[1]]
  }
  if(submodel=="zeros"){
    dfbetas <- dfbetas$zeros
    met <- vcov(model,submodel="zeros")
  }
  subst <- NULL
  if(!missingArg(coefs)){
    ids <- grepl(coefs,colnames(dfbetas),ignore.case=TRUE)
    if(sum(ids) > 0){
      subst <- colnames(dfbetas)[ids]
      dfbetas <- as.matrix(dfbetas[,ids])
      met <- as.matrix(met[ids,ids])
    }
  }
  CD <- as.matrix(apply((dfbetas%*%chol2inv(chol(met)))*dfbetas,1,mean))
  colnames(CD) <- "Cook's distance"
  if(plot.it){
    nano <- list(...)
    if(is.null(nano$labels)) labels <- 1:nrow(dfbetas)
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    nano$x <- 1:nrow(dfbetas)
    nano$y <- CD
    if(is.null(nano$xlab)) nano$xlab <- "Index (i)"
    if(is.null(nano$type)) nano$type <- "h"
    if(is.null(nano$ylab)) nano$ylab <- expression(frac(1,p)(hat(beta)-hat(beta)[{(-~~i)}])^{T}~(Var(hat(beta)))^{-1}~(hat(beta)-hat(beta)[{(-~~i)}]))
    do.call("plot",nano)
    abline(h=3*mean(CD),lty=3)
    if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=labels)
  }
  if(!is.null(subst)){
    message("The coefficients included in the Cook's distance are:\n")
    message(subst)
  }
  return(CD)
}

#' @title Normal QQ-plot with Simulated Envelope of Residuals for Regression Models to deal with Zero-Excess in Count Data
#' @description Produces a normal QQ-plot with simulated envelope of residuals for regression models used to deal with
#' zero-excess in count data.
#' @param object an object of the class \emph{zeroinflation}.
#' @param rep an (optional) positive integer which allows to specify the number of replicates which should be used to build the simulated envelope. As default, \code{rep} is set to 25.
#' @param conf an (optional) value in the interval \eqn{(0,1)} indicating the confidence level which should be used to build the pointwise confidence intervals, which conform the simulated envelope. As default, \code{conf} is set to 0.95.
#' @param type an (optional) character string which allows to specify the required type of residuals. The available options are: (1) the difference between the observed response
#' and the fitted mean ("response"); (2) the standardized difference between the observed response and the fitted mean ("standardized"); (3) the randomized quantile
#' residual ("quantile"). As default, \code{type} is set to "quantile".
#' @param plot.it an (optional) logical switch indicating if the normal QQ-plot with simulated envelope of residuals is required or just the data matrix in which it is based. As default, \code{plot.it} is set to TRUE.
#' @param identify an (optional) positive integer value indicating the number of individuals to identify on the QQ-plot with simulated envelope of residuals. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix with the following four columns:
#' \tabular{ll}{
#' \code{Lower limit} \tab the quantile (1 - \code{conf})/2 of the random sample of size \code{rep} of the \eqn{i}-th order\cr
#'                    \tab  statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n},\cr
#' \tab \cr
#' \code{Median} \tab the quantile 0.5 of the random sample of size \code{rep} of the \eqn{i}-th order\cr
#'               \tab  statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n},\cr
#' \tab \cr
#' \code{Upper limit} \tab the quantile (1 + \code{conf})/2 of the random sample of size \code{rep} of the \eqn{i}-th order\cr
#'                    \tab  statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n},\cr
#' \tab \cr
#' \code{Residuals}\tab the observed \code{type}-type residuals.\cr
#' }
#' @details The simulated envelope is builded by simulating \code{rep} independent realizations of the response variable for each
#' individual, which is accomplished taking into account the following: (1) the model assumption about the distribution of
#' the response variable; (2) the estimates of the parameters in the linear predictor; and (3) the estimate of the
#' dispersion parameter. The interest model is re-fitted \code{rep} times, as each time the vector of observed responses
#' is replaced by one of the simulated samples. The \code{type}-type residuals are computed and then sorted for each
#' replicate, so that for each \eqn{i=1,2,...,n}, where \eqn{n} is the number of individuals in the sample, there is a random
#' sample of size \code{rep} of the \eqn{i}-th order statistic of the  \code{type}-type residuals. Therefore, the simulated
#' envelope is composed of the quantiles (1 - \code{conf})/2 and (1 + \code{conf})/2 of the random sample of size \code{rep} of
#' the \eqn{i}-th order statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n}.
#' @references Atkinson A.C. (1985) \emph{Plots, Transformations and Regression}. Oxford University Press, Oxford.
#' @references Dunn P.K., Smyth G.K. (1996) Randomized Quantile Residuals. \emph{Journal of Computational and Graphical Statistics} 5, 236-244.
#' @seealso \link{envelope.lm}, \link{envelope.glm}, \link{envelope.overglm}
#' @method envelope zeroinflation
#' @export
#' @examples
#' ####### Example 1: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit <- zeroinf(infections ~ frequency | location, family="nb1(log)", data=swimmers)
#' envelope(fit, rep=30, conf=0.95, type="quantile", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
envelope.zeroinflation <- function(object, rep=20, conf=0.95, type=c("quantile","response","standardized"), plot.it=TRUE, identify, ...){
  defaultW <- getOption("warn")
  options(warn = -1)
  type <- match.arg(type)
  phi <- exp(object$coefficients$counts[object$parms[1] + 1])
  tau <- switch(object$family$counts$family,nb1=0,nb2=-1,nbf=object$coefficientscounts[object$parms[1] + 2])
  mu <- object$fitted.values$counts
  pi <- object$fitted.values$zeros
  n <- length(mu)
  p0 <- dnbinom(0,mu=mu,size=1/(phi*mu^tau))
  rep <- max(1,floor(abs(rep)))
  e <- matrix(0,n,rep)
  bar <- txtProgressBar(min=0, max=rep, initial=0, width=min(50,rep), char="+", style=3)
  X1 <- model.matrix(object,submodel="counts")
  X2 <- model.matrix(object,submodel="zeros")
  familia <- paste0(object$family$counts$family,"(",object$family$counts$link,")")
  i <- 1
  while(i <= rep){
    if(object$type=="Zero-inflation"){
      resp <- ifelse(runif(n) <= pi,0,rnbinom(n,mu=mu,size=1/(phi*mu^tau)))
      fits <- try(zeroinf(resp ~ -1 + X1 + offset(object$offset$counts)|-1 + X2 + offset(object$offset$zeros),start=list(counts=coef(object),zeros=coef(object,submodel="zeros")),family=familia,zero.link=object$family$zeros$link,weights=object$prior.weights),silent=TRUE)
    }
    else{
      resp <- ifelse(runif(n) <= pi,0,qnbinom(p0 + (1-p0)*runif(n),mu=mu,size=1/(phi*mu^tau)))
      fits <- try(zeroalt(resp ~ -1 + X1 + offset(object$offset$counts)|-1 + X2 + offset(object$offset$zeros),start=list(counts=coef(object),zeros=coef(object,submodel="zeros")),family=familia,zero.link=object$family$zeros$link,weights=object$prior.weights),silent=TRUE)
    }
    if(is.list(fits)){
      if(fits$converged==TRUE){
        rs <- residuals(fits,type=type,plot.it=FALSE)
        e[,i] <- sort(rs)
        setTxtProgressBar(bar,i)
        i <- i + 1
      }
    }
  }
  close(bar)
  alpha <- 1 - max(min(abs(conf),1),0)
  es <- t(apply(e,1,function(x) return(quantile(x,probs=c(alpha/2,0.5,1-alpha/2)))))
  rd <- residuals(object,type=type,plot.it=FALSE)
  out_ <- as.matrix(cbind(es,sort(rd)))
  colnames(out_) <- c("Lower limit","Median","Upper limit","Residuals")
  if(plot.it){
    nano <- list(...)
    nano$y <- rd
    nano$type <- "p"
    if(is.null(nano$ylim)) nano$ylim <- 1.1*range(out_)
    if(is.null(nano$pch)) nano$pch <- 20
    if(is.null(nano$col)) nano$col <- "black"
    if(is.null(nano$xlab)) nano$xlab <- "Expected quantiles"
    if(is.null(nano$ylab)) nano$ylab <- "Observed quantiles"
    if(is.null(nano$main)) nano$main <- paste0("Normal QQ plot with simulated envelope\n of ",type,"-type residuals")
    if(is.null(nano$labels)) labels <- 1:length(rd)
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    outm <- do.call("qqnorm",nano)
    lines(sort(outm$x),es[,2],xlab="",ylab="",main="", type="l",lty=3)
    lines(sort(outm$x),es[,1],xlab="",ylab="",main="", type="l",lty=1)
    lines(sort(outm$x),es[,3],xlab="",ylab="",main="", type="l",lty=1)
    if(!missingArg(identify)) identify(outm$x,outm$y,n=max(1,floor(abs(identify))),labels=labels)
  }
  options(warn = defaultW)
  return(invisible(out_))
}

#' @title Normal QQ-plot with Simulated Envelope of Residuals for alternatives to the Poisson and Binomial Regression Models under the presence of Overdispersion
#' @description Produces a normal QQ-plot with simulated envelope of residuals for regression models based on the negative binomial, beta-binomial, and random-clumped binomial
#' distributions, which are alternatives to the Poisson and binomial regression models under the presence of overdispersion.
#' @param object an object of class \emph{overglm}.
#' @param rep an (optional) positive integer which allows to specify the number of replicates which should be used to build the simulated envelope. As default, \code{rep} is set to 25.
#' @param conf an (optional) value in the interval \eqn{(0,1)} indicating the confidence level which should be used to build the pointwise confidence intervals, which conform the simulated envelope. As default, \code{conf} is set to 0.95.
#' @param type an (optional) character string which allows to specify the required type of residuals. The available options are: (1) the difference between the observed response
#' and the fitted mean ("response"); (2) the standardized difference between the observed response and the fitted mean ("standardized"); and (3) the randomized quantile
#' residual ("quantile"). As default, \code{type} is set to "quantile".
#' @param plot.it an (optional) logical switch indicating if the normal QQ-plot with simulated envelope of residuals is required or just the data matrix in which it is based. As default, \code{plot.it} is set to TRUE.
#' @param identify an (optional) positive integer value indicating the number of individuals to identify on the QQ-plot with simulated envelope of residuals. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix with the following four columns:
#' \tabular{ll}{
#' \code{Lower limit} \tab the quantile (1 - \code{conf})/2 of the random sample of size \code{rep} of the \eqn{i}-th order\cr
#'                    \tab  statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n},\cr
#' \tab \cr
#' \code{Median} \tab the quantile 0.5 of the random sample of size \code{rep} of the \eqn{i}-th order\cr
#'               \tab  statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n},\cr
#' \tab \cr
#' \code{Upper limit} \tab the quantile (1 + \code{conf})/2 of the random sample of size \code{rep} of the \eqn{i}-th order\cr
#'                    \tab  statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n},\cr
#' \tab \cr
#' \code{Residuals}\tab the observed \code{type}-type residuals,\cr
#' }
#' @details The simulated envelope is builded by simulating \code{rep} independent realizations of the response variable for each
#' individual, which is accomplished taking into account the following: (1) the model assumption about the distribution of
#' the response variable; (2) the estimates of the parameters in the linear predictor; and (3) the estimate of the
#' dispersion parameter. The interest model is re-fitted \code{rep} times, as each time the vector of observed responses
#' is replaced by one of the simulated samples. The \code{type}-type residuals are computed and then sorted for each
#' replicate, so that for each \eqn{i=1,2,...,n}, where \eqn{n} is the number of individuals in the sample, there is a random
#' sample of size \code{rep} of the \eqn{i}-th order statistic of the  \code{type}-type residuals. Therefore, the simulated
#' envelope is composed of the quantiles (1 - \code{conf})/2 and (1 + \code{conf})/2 of the random sample of size \code{rep} of
#' the \eqn{i}-th order statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n}.
#' @references Atkinson A.C. (1985) \emph{Plots, Transformations and Regression}. Oxford University Press, Oxford.
#' @references Dunn P.K., Smyth G.K. (1996) Randomized Quantile Residuals. \emph{Journal of Computational and Graphical Statistics} 5, 236-244.
#' @seealso \link{envelope.lm}, \link{envelope.glm}, \link{envelope.zeroinflation}
#' @method envelope overglm
#' @export
#' @examples
#' ###### Example 1: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit1 <- overglm(infections ~ frequency + location, family="nb1(log)", data=swimmers)
#' envelope(fit1, rep=30, conf=0.95, type="quantile", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8, plot.it=TRUE)
#'
#' ###### Example 2: Article production by graduate students in biochemistry PhD programs
#' bioChemists <- pscl::bioChemists
#' fit2 <- overglm(art ~ fem + kid5 + ment, family="nb1(log)", data = bioChemists)
#' envelope(fit2, rep=30, conf=0.95, type="quantile", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8, plot.it=TRUE)
#'
#' ###### Example 3: Agents to stimulate cellular differentiation
#' data(cellular)
#' fit3 <- overglm(cbind(cells,200-cells) ~ tnf + ifn, family="bb(logit)", data=cellular)
#' envelope(fit3, rep=30, conf=0.95, type="quantile", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8, plot.it=TRUE)
#'
envelope.overglm <- function(object, rep=25, conf=0.95, type=c("quantile","response","standardized"), plot.it=TRUE, identify, ...){
  defaultW <- getOption("warn")
  options(warn = -1)
  type <- match.arg(type)
  mu <- object$fitted.values
  n <- length(mu)
  p <- object$parms[1]
  if(object$parms[2] > 0) phi <- exp(object$coefficients[object$parms[1] + 1])
  if(object$family$family %in% c("nb1","nb2","nbf")){
    tau <- switch(object$family$family,nb1=0,nb2=-1,nbf=object$coefficients[object$parms[1] + 2])
    if(object$zero.trunc) p0 <- dnbinom(0,mu=mu,size=1/(phi*mu^tau)) else p0 <- 0
  }
  if(object$family$family == "poi") p0 <- ifelse(object$zero.trunc,exp(-mu),0)
  rep <- max(1,floor(abs(rep)))
  e <- matrix(0,n,rep)
  bar <- txtProgressBar(min=0, max=rep, initial=0, width=min(50,rep), char="+", style=3)
  i <- 1
  X <- model.matrix(object)
  familia <- paste0(object$family$family,"(",object$family$link,")")
  while(i <= rep){
    if(object$family$family %in% c("nb1","nb2","nbf")){
      resp. <- qnbinom(p0 + (1 - p0)*runif(n),mu=mu,size=1/(phi*mu^tau))
      fits <- try(overglm(resp. ~ -1 + X + offset(object$offset),start=coef(object),weights=object$prior.weights,zero.trunc=object$zero.trunc,family=familia),silent=TRUE)
    }
    if(object$family$family == "poi"){
      resp. <- qpois(p0 + (1 - p0)*runif(n),lambda=mu)
      fits <- try(overglm(resp. ~ -1 + X + offset(object$offset),start=coef(object),weights=object$prior.weights,zero.trunc=object$zero.trunc,family=familia),silent=TRUE)
    }
    if(object$family$family == "bb"){
      size. <- apply(object$y,1,sum); prob <- rbeta(n,shape1=mu/phi,shape2=(1 - mu)/phi)
      resp. <- rbinom(n,size=size.,prob=prob)
      fits <- try(overglm(cbind(resp.,size.-resp.) ~ -1 + X + offset(object$offset),start=coef(object),weights=object$prior.weights,zero.trunc=object$zero.trunc,family=familia),silent=TRUE)
    }
    if(object$family$family == "rcb"){
      phi <- exp(object$coefficients[p + 1])/(1 + exp(object$coefficients[p + 1]))
      size. <- apply(object$y,1,sum)
      resp. <- ifelse(runif(n) <= mu,rbinom(n,size=size.,prob=(1 - phi)*mu + phi),rbinom(n,size=size.,prob=(1 - phi)*mu))
      fits <- try(overglm(cbind(resp.,size.-resp.) ~ -1 + X + offset(object$offset),start=coef(object),weights=object$prior.weights,zero.trunc=object$zero.trunc,family=familia),silent=TRUE)
    }
    if(is.list(fits)){
      if(fits$converged==TRUE){
        rs <- residuals(fits,type=type,plot.it=FALSE)
        e[,i] <- sort(rs)
        setTxtProgressBar(bar,i)
        i <- i + 1
      }
    }
  }
  close(bar)
  alpha <- 1 - max(min(abs(conf),1),0)
  es <- t(apply(e,1,function(x) return(quantile(x,probs=c(alpha/2,0.5,1-alpha/2)))))
  rd <- residuals(object,type=type,plot.it=FALSE)
  out_ <- as.matrix(cbind(es,sort(rd)))
  colnames(out_) <- c("Lower limit","Median","Upper limit","Residuals")
  if(plot.it){
    nano <- list(...)
    nano$y <- rd
    nano$type <- "p"
    if(is.null(nano$ylim)) nano$ylim <- 1.1*range(out_)
    if(is.null(nano$pch)) nano$pch <- 20
    if(is.null(nano$col)) nano$col <- "black"
    if(is.null(nano$xlab)) nano$xlab <- "Expected quantiles"
    if(is.null(nano$ylab)) nano$ylab <- "Observed quantiles"
    if(is.null(nano$main)) nano$main <- paste0("Normal QQ plot with simulated envelope\n of ",type,"-type residuals")
    if(is.null(nano$labels)) labels <- 1:length(rd)
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    outm <- do.call("qqnorm",nano)
    lines(sort(outm$x),es[,2],xlab="",ylab="",main="", type="l",lty=3)
    lines(sort(outm$x),es[,1],xlab="",ylab="",main="", type="l",lty=1)
    lines(sort(outm$x),es[,3],xlab="",ylab="",main="", type="l",lty=1)
    if(!missingArg(identify)) identify(outm$x,outm$y,n=max(1,floor(abs(identify))),labels=labels)
  }
  options(warn = defaultW)
  return(invisible(out_))
}

#' @title Variable selection for alternatives to the Poisson and Binomial Regression Models under the presence of Overdispersion
#' @description Performs variable selection using hybrid versions of forward stepwise and backward stepwise by comparing
#' hierarchically builded candidate models using a criterion previously specified such as AIC, BIC or \eqn{p}-value of the
#' significance tests.
#' @param model an object of the class \emph{overglm}.
#' @param direction an (optional) character string which allows to specify the type of procedure which should be used. The available
#' options are: hybrid backward stepwise ("backward") and hybrid forward stepwise ("forward"). As default, \code{direction}
#' is set to "forward".
#' @param levels an (optional) two-dimensional vector of values in the interval \eqn{(0,1)} indicating the levels at which
#' the variables should in and out from the model. This is only appropiate if \code{criterion}="p-value". By default,
#' \code{levels} is set to \code{c(0.05,0.05)}.
#' @param test an (optional) character string which allows to specify the statistical test which should be used to compare nested
#' models. The available options are: Wald ("wald"), Rao's score ("score"), likelihood-ratio ("lr") and gradient
#' ("gradient") tests. As default, \code{test} is set to "wald".
#' @param criterion an (optional) character string which allows to specify the criterion which should be used to compare the
#' candidate models. The available options are: AIC ("aic"), BIC ("bic"), and \emph{p}-value of the \code{test}-type test
#' ("p-value"). As default, \code{criterion} is set to "bic".
#' @param ...	further arguments passed to or from other methods. For example, \code{k}, that is, the magnitude of the
#' penalty in the AIC, which by default is set to 2.
#' @param trace an (optional) logical switch indicating if should the stepwise reports be printed. By default,
#' \code{trace} is set to TRUE.
#' @param scope an (optional) list, containing components \code{lower} and \code{upper}, both formula-type objects,
#' indicating the range of models which should be examined in the stepwise search. As default, \code{lower} is a model
#' with no predictors and \code{upper} is the linear predictor of the model in \code{model}.
#' @param force.in an (optional) formula-type object indicating the effects that should be in all models
#' @param force.out an (optional) formula-type object indicating the effects that should be in no models
#' @return A list which contains the following objects:
#' \tabular{ll}{
#' \code{initial} \tab a character string indicating the linear predictor of the "initial model",\cr
#' \tab \cr
#' \code{direction}\tab a character string indicating the type of procedure which was used,\cr
#' \tab \cr
#' \code{criterion}\tab a character string indicating the criterion used to compare the candidate models,\cr
#' \tab \cr
#' \code{final}\tab a character string indicating the linear predictor of the "final model",\cr
#' \tab \cr
#' \code{final.fit} \tab an object of class \code{overglm} with the results of the fit to the data of the "final model",\cr
#' }
#' @seealso \link{stepCriterion.lm}, \link{stepCriterion.glm}, \link{stepCriterion.glmgee}
#' @examples
#' ###### Example 1: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit1 <- overglm(infections ~ age + gender + frequency + location, family="nb1(log)", data=swimmers)
#'
#' stepCriterion(fit1, criterion="p-value", direction="forward", test="lr")
#'
#' stepCriterion(fit1, criterion="bic", direction="backward", test="score", force.in=~location)
#'
#' ###### Example 2: Article production by graduate students in biochemistry PhD programs
#' bioChemists <- pscl::bioChemists
#' fit2 <- overglm(art ~ fem + mar + kid5 + phd + ment, family="nb1(log)", data = bioChemists)
#'
#' stepCriterion(fit2, criterion="p-value", direction="forward", test="lr")
#'
#' stepCriterion(fit2, criterion="bic", direction="backward", test="score", force.in=~fem)
#'
#' ###### Example 3: Agents to stimulate cellular differentiation
#' data(cellular)
#' fit3 <- overglm(cbind(cells,200-cells) ~ tnf + ifn + tnf*ifn, family="bb(logit)", data=cellular)
#'
#' stepCriterion(fit3, criterion="p-value", direction="backward", test="lr")
#'
#' stepCriterion(fit3, criterion="bic", direction="forward", test="score")
#'
#' @method stepCriterion overglm
#' @export
#' @references James G., Witten D., Hastie T., Tibshirani R. (2013, page 210) An Introduction to Statistical Learning
#' with Applications in R. Springer, New York.
#'
stepCriterion.overglm <- function(model, criterion=c("bic","aic","p-value"), test=c("wald","score","lr","gradient"), direction=c("forward","backward"), levels=c(0.05,0.05), trace=TRUE, scope, force.in, force.out, ...){
  xxx <- list(...)
  if(is.null(xxx$k)) k <- 2 else k <- xxx$k
  if(!missingArg(criterion)) criterion <- tolower(criterion)
  criterion <- match.arg(criterion)
  direction <- match.arg(direction)
  test <- match.arg(test)
  if(test=="wald") test2 <- "Wald test"
  if(test=="score") test2 <- "Rao's score test"
  if(test=="lr") test2 <- "likelihood-ratio test"
  if(test=="gradient") test2 <- "Gradient test"

  criters <- c("aic","bic","adjr2","p-value")
  criters2 <- c("AIC","BIC","adj.R-squared","P(Chisq>)(*)")
  sentido <- c(1,1,-1,1)
  if(missingArg(scope)){
    upper <- formula(eval(model$call$formula))
    lower <- formula(eval(model$call$formula))
    lower <- formula(paste(deparse(lower[[2]]),"~",attr(terms(lower,data=eval(model$call$data)),"intercept")))
  }else{
    if(is.null(scope$lower)){
      lower <- formula(eval(model$call$formula))
      lower <- formula(paste(deparse(lower[[2]]),"~",attr(terms(lower,data=eval(model$call$data)),"intercept")))
    }else lower <- scope$lower
    if(is.null(scope$upper)) upper <- formula(eval(model$call$formula)) else upper <- scope$upper
  }
  if(is.null(model$call$data)) datas <- get_all_vars(upper,environment(eval(model$call$formula)))
  else datas <- get_all_vars(upper,eval(model$call$data))
  if(!is.null(model$call$subset)) datas <- datas[eval(model$call$subset,datas),]
  datas <- na.omit(datas)
  if(length(eval(model$call$formula)[[length(eval(model$call$formula))]])==1)
    if(eval(model$call$formula)[[length(eval(model$call$formula))]]==".")
      upper <- as.formula(paste0(eval(model$call$formula)[[length(eval(model$call$formula))]],"~",paste0(attr(terms(eval(model$call$formula),data=datas),"term.labels"),collapse="+")))

  if(!missingArg(force.in)){
    force.in <- attr(terms(as.formula(force.in)),"term.labels")
    lower <- as.formula(paste0("~",paste0(c(deparse(lower[[length(lower)]]),force.in[!(force.in %in% attr(terms(lower),"term.labels"))]),collapse="+")))
    upper <- as.formula(paste0("~",paste0(c(deparse(upper[[length(upper)]]),force.in[!(force.in %in% attr(terms(upper),"term.labels"))]),collapse="+")))
  }else force.in <- ""
  if(!missingArg(force.out)){
    force.out <- attr(terms(as.formula(force.out)),"term.labels")
    upper <- as.formula(paste0("~",paste0(deparse(upper[[length(upper)]])[!(deparse(upper[[length(upper)]]) %in% force.out)],collapse="+")))
  }else force.out <- ""

U <- unlist(lapply(strsplit(attr(terms(upper,data=datas),"term.labels"),":"),function(x) paste(sort(x),collapse =":")))
  fs <- attr(terms(upper,data=datas),"factors")
  long <- max(nchar(U)) + 2
  nonename <- paste("<none>",paste(replicate(max(long-6,0)," "),collapse=""),collapse="")
  cambio <- ""
  paso <- 1
  tol <- TRUE
  familia <- switch(model$family$family,"nb1"="Negative Binomial I",
                    "nb2"="Negative Binomial II","nbf"="Negative Binomial Family",
                    "rcb"="Random-clumped Binomial","bb"="Beta-Binomial","poi"="poisson")
  if(model$zero.trunc) familia <- paste("Zero-truncated",familia)
  if(trace){
    cat("\n       Family: ",familia,"\n")
    cat("Link function: ",model$family$link,"\n")
    cat("    Criterion: ",criters2[criters==criterion])
  }
  if(direction=="forward"){
    oldformula <- lower
    if(trace){
      cat("\nInitial model:\n")
      cat(paste("~",as.character(oldformula)[length(oldformula)],sep=" "),"\n\n")
      cat("\nStep",0,":\n")
    }
    out_ <- list(initial=paste("~",as.character(oldformula)[length(oldformula)],sep=" "),direction=direction,criterion=criters2[criters==criterion])
    while(tol){
      oldformula <-  update(oldformula,paste(as.character(eval(model$call$formula))[2],"~ ."))
      fit.x <- update(model,formula=oldformula,start=NULL)
      S <- unlist(lapply(strsplit(attr(terms(oldformula),"term.labels"),":"),function(x) paste(sort(x),collapse =":")))
      entran <- seq(1:length(U))[is.na(match(U,S))]
      salen <- seq(1:length(U))[!is.na(match(U,S))]
      mas <- TRUE

      fsalen <- matrix(NA,length(salen),5)
      if(length(salen) > 0){
        nombres <- matrix("",length(salen),1)
        for(i in 1:length(salen)){
          salida <- apply(as.matrix(fs[,salen[i]]*fs[,-c(entran,salen[i])]),2,sum)
          if(all(salida < sum(fs[,salen[i]])) & U[salen[i]]!=cambio & !(U[salen[i]] %in% force.in)){
            newformula <- update(oldformula, paste("~ . -",U[salen[i]]))
            fit.0 <- update(model,formula=newformula,start=NULL)
            fsalen[i,1] <- fit.0$df.residual - fit.x$df.residual
            fsalen[i,5] <- anova(fit.0,fit.x,test=test,verbose=FALSE)[1,1]
            fsalen[i,5] <- sqrt(9*fsalen[i,1]/2)*((fsalen[i,5]/fsalen[i,1])^(1/3) - 1 + 2/(9*fsalen[i,1]))
            fsalen[i,2] <- AIC(fit.0,k=k)
            fsalen[i,3] <- BIC(fit.0)
            fsalen[i,4] <- 0
            nombres[i] <- U[salen[i]]
          }
        }
        rownames(fsalen) <- paste("-",nombres)
        if(criterion=="p-value" & any(!is.na(fsalen))){
          colnames(fsalen) <- c("df",criters2)
          if(nrow(fsalen) > 1){
            fsalen <- fsalen[order(fsalen[,5]),]
            fsalen <- na.omit(fsalen)
            attr(fsalen,"na.action")	<- attr(fsalen,"class") <- NULL
          }
          fsalen[,5] <- 1-pchisq((sqrt(2/(9*fsalen[,1]))*fsalen[,5] + 1 - 2/(9*fsalen[,1]))^3*fsalen[,1],fsalen[,1])
          if(fsalen[1,5] > levels[2]){
            fsalen <- rbind(fsalen,c(NA,AIC(fit.x,k=k),BIC(fit.x),NA,NA))
            rownames(fsalen)[nrow(fsalen)] <- nonename
            fsalen <- fsalen[,-4]
            if(trace){
              printCoefmat(fsalen,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=2:(ncol(fsalen)-2),
                           signif.stars=FALSE,tst.ind=ncol(fsalen)-1,dig.tst=4,digits=5)
              cat("\nStep",paso,":",rownames(fsalen)[1],"\n\n")
            }
            mas <- FALSE
            oldformula <- update(oldformula, paste("~ .",rownames(fsalen)[1]))
            paso <- paso + 1
            cambio <- substring(rownames(fsalen)[1],3)
          }
        }
      }

      if(length(entran) > 0 & mas){
        fentran <- matrix(NA,length(entran),5)
        nombres <- matrix("",length(entran),1)
        for(i in 1:length(entran)){
          salida <- apply(as.matrix(fs[,-c(salen,entran[i])]),2,function(x) sum(fs[,entran[i]]*x)!=sum(x))
          if(all(salida) & U[entran[i]]!=cambio & !(U[entran[i]] %in% force.out)){
            newformula <- update(oldformula, paste("~ . +",U[entran[i]]))
            fit.0 <- update(model,formula=newformula,adjr2=FALSE,start=NULL)
            fentran[i,1] <- fit.x$df.residual - fit.0$df.residual
            fentran[i,5] <- anova(fit.x,fit.0,test=test,verbose=FALSE)[1,1]
            fentran[i,5] <- sqrt(9*fentran[i,1]/2)*((fentran[i,5]/fentran[i,1])^(1/3) - 1 + 2/(9*fentran[i,1]))
            fentran[i,2] <- AIC(fit.0,k=k)
            fentran[i,3] <- BIC(fit.0)
            fentran[i,4] <- 0
            nombres[i] <- U[entran[i]]
          }
        }
        rownames(fentran) <- paste("+",nombres)
        if(criterion=="p-value"){
          colnames(fentran) <- c("df",criters2)
          if(nrow(fentran) > 1){
            fentran <- fentran[order(-fentran[,5]),]
            fentran <- na.omit(fentran)
            attr(fentran,"na.action")	<- attr(fentran,"class") <- NULL
          }
          fentran[,5] <- 1-pchisq((sqrt(2/(9*fentran[,1]))*fentran[,5] + 1 - 2/(9*fentran[,1]))^3*fentran[,1],fentran[,1])
          fentran <- rbind(fentran,c(NA,AIC(fit.x,k=k),BIC(fit.x),NA,NA))
          rownames(fentran)[nrow(fentran)] <- nonename
          fentran <- fentran[,-4]
          if(trace) printCoefmat(fentran,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=2:(ncol(fentran)-2),
                                 signif.stars=FALSE,tst.ind=ncol(fentran)-1,dig.tst=4,digits=5)
          if(fentran[1,ncol(fentran)] < levels[1]){
            if(trace) cat("\nStep",paso,":",rownames(fentran)[1],"\n\n")
            paso <- paso + 1
            cambio <- substring(rownames(fentran)[1],3)
            oldformula <- update(oldformula, paste("~ .",rownames(fentran)[1]))
          }else tol <- FALSE
        }
      }
      if(length(entran) > 0 & criterion!="p-value"){
        if(any(!is.na(fsalen))) fentran <- rbind(fentran,fsalen)
        fentran[,5] <- 1-pchisq((sqrt(2/(9*fentran[,1]))*fentran[,5] + 1 - 2/(9*fentran[,1]))^3*fentran[,1],fentran[,1])
        fentran <- rbind(fentran,c(0,AIC(fit.x,k=k),BIC(fit.x),0,0))
        rownames(fentran)[nrow(fentran)] <- nonename
        ids <- criters == criterion
        colnames(fentran) <- c("df",criters2)
        fentran <- na.omit(fentran)
        attr(fentran,"na.action")	<- attr(fentran,"class") <- NULL
        fentran[nrow(fentran),c(1,5)] <- NA
        fentran <- fentran[order(sentido[ids]*fentran[,c(FALSE,ids)]),]
        if(rownames(fentran)[1]!=nonename){
          fentran <- fentran[,-4]
          if(trace){
            printCoefmat(fentran,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=2:(ncol(fentran)-2),
                         signif.stars=FALSE,tst.ind=ncol(fentran)-1,dig.tst=4,digits=5)
            cat("\nStep",paso,":",rownames(fentran)[1],"\n\n")
          }
          paso <- paso + 1
          cambio <- substring(rownames(fentran)[1],3)
          oldformula <- update(oldformula, paste("~ .",rownames(fentran)[1]))
        }else{
          tol <- FALSE
          fentran <- fentran[,-4]
          if(trace) printCoefmat(fentran,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=2:(ncol(fentran)-2),
                                 signif.stars=FALSE,tst.ind=ncol(fentran)-1,dig.tst=4,digits=5)
        }
      }
      if(length(entran) == 0 & mas) tol <- FALSE
    }
  }
  if(direction=="backward"){
    oldformula <- upper
    if(trace){
      cat("\nInitial model:\n")
      cat(paste("~",as.character(oldformula)[length(oldformula)],sep=" "),"\n\n")
      cat("\nStep",0,":\n")
    }
    out_ <- list(initial=paste("~",as.character(oldformula)[length(oldformula)],sep=" "),direction=direction,criterion=criters2[criters==criterion])
    while(tol){
      oldformula <-  update(oldformula,paste(as.character(eval(model$call$formula))[2],"~ ."))
      fit.x <- update(model,formula=oldformula,start=NULL)
      S <- unlist(lapply(strsplit(attr(terms(oldformula),"term.labels"),":"),function(x) paste(sort(x),collapse =":")))
      entran <- seq(1:length(U))[is.na(match(U,S))]
      salen <- seq(1:length(U))[!is.na(match(U,S))]
      menos <- TRUE

      fentran <- matrix(NA,length(entran),5)
      if(length(entran) > 0){
        nombres <- matrix("",length(entran),1)
        for(i in 1:length(entran)){
          salida <- apply(as.matrix(fs[,-c(salen,entran[i])]),2,function(x) sum(fs[,entran[i]]*x)!=sum(x))
          if(all(salida) & U[entran[i]]!=cambio & !(U[entran[i]] %in% force.out)){
            newformula <- update(oldformula, paste("~ . +",U[entran[i]]))
            fit.0 <- update(model,formula=newformula,adjr2=FALSE,start=NULL)
            fentran[i,1] <- fit.x$df.residual - fit.0$df.residual
            fentran[i,5] <- anova(fit.x,fit.0,test=test,verbose=FALSE)[1,1]
            fentran[i,5] <- sqrt(9*fentran[i,1]/2)*((fentran[i,5]/fentran[i,1])^(1/3) - 1 + 2/(9*fentran[i,1]))
            fentran[i,2] <- AIC(fit.0,k=k)
            fentran[i,3] <- BIC(fit.0)
            fentran[i,4] <- 0
            nombres[i] <- U[entran[i]]
          }
        }
        rownames(fentran) <- paste("+",nombres)
        if(criterion=="p-value" & any(!is.na(fentran))){
          colnames(fentran) <- c("df",criters2)
          if(nrow(fentran) > 1){
            fentran <- fentran[order(-fentran[,5]),]
            fentran <- na.omit(fentran)
            attr(fentran,"na.action")	<- attr(fentran,"class") <- NULL
          }
          fentran[,5] <- 1-pchisq((sqrt(2/(9*fentran[,1]))*fentran[,5] + 1 - 2/(9*fentran[,1]))^3*fentran[,1],fentran[,1])
          if(fentran[1,5] < levels[1]){
            fentran <- rbind(fentran,c(NA,AIC(fit.x,k=k),BIC(fit.x),NA,NA))
            rownames(fentran)[nrow(fentran)] <- nonename
            fentran <- fentran[,-4]
            if(trace){
              printCoefmat(fentran,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=2:(ncol(fentran)-2),
                           signif.stars=FALSE,tst.ind=ncol(fentran)-1,dig.tst=4,digits=5)
              cat("\nStep",paso,":",rownames(fentran)[1],"\n\n")
            }
            menos <- FALSE
            oldformula <- update(oldformula, paste("~ .",rownames(fentran)[1]))
            paso <- paso + 1
            cambio <- substring(rownames(fentran)[1],3)
          }
        }
      }

      if(length(salen) > 0 & menos){
        fsalen <- matrix(NA,length(salen),5)
        nombres <- matrix("",length(salen),1)
        for(i in 1:length(salen)){
          salida <- apply(as.matrix(fs[,salen[i]]*fs[,-c(entran,salen[i])]),2,sum)
          if(all(salida < sum(fs[,salen[i]])) & !(U[salen[i]] %in% force.in)){
            newformula <- update(oldformula, paste("~ . -",U[salen[i]]))
            fit.0 <- update(model,formula=newformula,start=NULL)
            fsalen[i,1] <- fit.0$df.residual - fit.x$df.residual
            fsalen[i,5] <- anova(fit.0,fit.x,test=test,verbose=FALSE)[1,1]
            fsalen[i,5] <- sqrt(9*fsalen[i,1]/2)*((fsalen[i,5]/fsalen[i,1])^(1/3) - 1 + 2/(9*fsalen[i,1]))
            fsalen[i,2] <- AIC(fit.0,k=k)
            fsalen[i,3] <- BIC(fit.0)
            fsalen[i,4] <- 0
            nombres[i] <- U[salen[i]]
          }
        }
        rownames(fsalen) <- paste("-",nombres)
        if(criterion=="p-value"){
          colnames(fsalen) <- c("df",criters2)
          if(nrow(fsalen) > 1){
            fsalen <- fsalen[order(fsalen[,5]),]
            fsalen <- na.omit(fsalen)
            attr(fsalen,"na.action")	<- attr(fsalen,"class") <- NULL
          }
          fsalen[,5] <- 1-pchisq((sqrt(2/(9*fsalen[,1]))*fsalen[,5] + 1 - 2/(9*fsalen[,1]))^3*fsalen[,1],fsalen[,1])
          fsalen <- rbind(fsalen,c(NA,AIC(fit.x,k=k),BIC(fit.x),NA,NA))
          rownames(fsalen)[nrow(fsalen)] <- nonename
          fsalen <- fsalen[,-4]
          if(trace) printCoefmat(fsalen,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=2:(ncol(fsalen)-2),
                                 signif.stars=FALSE,tst.ind=ncol(fsalen)-1,dig.tst=4,digits=5)
          if(fsalen[1,ncol(fsalen)] > levels[2]){
            if(trace) cat("\nStep",paso,":",rownames(fsalen)[1],"\n\n")
            paso <- paso + 1
            cambio <- substring(rownames(fsalen)[1],3)
            oldformula <- update(oldformula, paste("~ .",rownames(fsalen)[1]))
          }else tol <- FALSE
        }
      }
      if(criterion!="p-value"){
        if(any(!is.na(fentran))) fsalen <- rbind(fsalen,fentran)
        fsalen[,5] <- 1-pchisq((sqrt(2/(9*fsalen[,1]))*fsalen[,5] + 1 - 2/(9*fsalen[,1]))^3*fsalen[,1],fsalen[,1])
        fsalen <- rbind(fsalen,c(0,AIC(fit.x,k=k),BIC(fit.x),0,0))
        rownames(fsalen)[nrow(fsalen)] <- nonename
        ids <- criters == criterion
        colnames(fsalen) <- c("df",criters2)
        fsalen <- na.omit(fsalen)
        attr(fsalen,"na.action")	<- attr(fsalen,"class") <- NULL
        fsalen[nrow(fsalen),c(1,5)] <- NA
        fsalen <- fsalen[order(sentido[ids]*fsalen[,c(FALSE,ids)]),]
        if(rownames(fsalen)[1]!=nonename){
          fsalen <- fsalen[,-4]
          if(trace){
            printCoefmat(fsalen,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=2:(ncol(fsalen)-2),
                         signif.stars=FALSE,tst.ind=ncol(fsalen)-1,dig.tst=4,digits=5)
            cat("\nStep",paso,":",rownames(fsalen)[1],"\n\n")
          }
          paso <- paso + 1
          cambio <- substring(rownames(fsalen)[1],3)
          oldformula <- update(oldformula, paste("~ .",rownames(fsalen)[1]))
        }else{
          tol <- FALSE
          fsalen <- fsalen[,-4]
          if(trace) printCoefmat(fsalen,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=2:(ncol(fsalen)-2),
                                 signif.stars=FALSE,tst.ind=ncol(fsalen)-1,dig.tst=4,digits=5)
        }
      }
      if(length(salen) == 0 & menos) tol <- FALSE
    }
  }
  if(trace){
    cat("\n\nFinal model:\n")
    cat(paste("~",as.character(oldformula)[length(oldformula)],sep=" "),"\n\n")
    cat("****************************************************************************")
    cat("\n(*) p-values of the",test2)
    if(criterion=="p-value"){
      cat("\n Effects are included when their p-values are lower than",levels[1])
      cat("\n Effects are dropped when their p-values are higher than",levels[2])
    }
    if(!is.null(xxx$k)) cat("The magnitude of the penalty in the AIC was set to ",xxx$k)
    cat("\n")
  }
  out_$final <- paste("~",as.character(oldformula)[length(oldformula)],sep=" ")
  if(!trace){
    final.fit <- fit.0 <- update(model,formula=oldformula,adjr2=FALSE,start=NULL)
    out_$final.fit <- final.fit
  }
  return(invisible(out_))
}

#' @title Local Influence for alternatives to the Poisson and Binomial Regression Models under the presence of Overdispersion
#' @description Computes local influence measures under the case-weight perturbation scheme for alternatives to the Poisson and
#' Binomial Regression Models under the presence of Overdispersion. Those local influence measures
#' may be chosen to correspond to all parameters in the linear predictor or (via \code{coefs}) for just some subset of them.
#' @param object an object of class \emph{overglm}.
#' @param type an (optional) character string which allows to specify the local influence approach:
#' the absolute value of the elements of the main diagonal of the normal curvature matrix ("total") or
#' the eigenvector which corresponds to the maximum absolute eigenvalue of the normal curvature matrix ("local").
#' As default, \code{type} is set to "total".
#' @param plot.it an (optional) logical indicating if the plot is required or just the data matrix in which that plot is based. As default, \code{plot.it} is set to \code{FALSE}.
#' @param coefs	an (optional) character string which (partially) match with the names of some model parameters.
#' @param identify an (optional) integer indicating the number of individuals to identify on the plot. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used
#' to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main},
#' \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix as many rows as individuals in the sample and one column with the values of the local influence measure.
#' @method localInfluence overglm
#' @export
#' @references Cook R.D. (1986) Assessment of Local Influence. \emph{Journal of the Royal Statistical Society: Series B (Methodological)} 48, 133-155.
#' @examples
#' ###### Example 1: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit1 <- overglm(infections ~ frequency + location, family="nb1(log)", data=swimmers)
#'
#' ### Local influence for all parameters in the linear predictor
#' localInfluence(fit1, type="local", plot.it=TRUE, col="red", lty=1, lwd=1, col.lab="blue",
#'                col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ### Local influence for the parameter associated with 'frequency'
#' localInfluence(fit1, type="local", plot.it=TRUE, col="red", lty=1, lwd=1, col.lab="blue",
#'                coef="frequency", col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 2: Article production by graduate students in biochemistry PhD programs
#' bioChemists <- pscl::bioChemists
#' fit2 <- overglm(art ~ fem + kid5 + ment, family="nb1(log)", data = bioChemists)
#'
#' ### Local influence for all parameters in the linear predictor
#' localInfluence(fit2, type="local", plot.it=TRUE, col="red", lty=1, lwd=1, col.lab="blue",
#'                col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ### Local influence for the parameter associated with 'fem'
#' localInfluence(fit2, type="local", plot.it=TRUE, col="red", lty=1, lwd=1, col.lab="blue",
#'                coef="fem", col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 3: Agents to stimulate cellular differentiation
#' data(cellular)
#' fit3 <- overglm(cbind(cells,200-cells) ~ tnf + ifn, family="bb(logit)", data=cellular)
#'
#' ### Local influence for all parameters in the linear predictor
#' localInfluence(fit3, type="local", plot.it=TRUE, col="red", lty=1, lwd=1, col.lab="blue",
#'                col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ### Local influence for the parameter associated with 'tnf'
#' localInfluence(fit3, type="local", plot.it=TRUE, col="red", lty=1, lwd=1, col.lab="blue",
#'                coef="tnf", col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
localInfluence.overglm <- function(object,type=c("total","local"),coefs,plot.it=FALSE,identify,...){
  type <- match.arg(type)
  subst <- NULL
  if(!missingArg(coefs)){
    ids <- grepl(coefs,rownames(coef(object)),ignore.case=TRUE)
    if(sum(ids) > 0) subst <- rownames(coef(object))[ids]
    ids <- c(ids,rep(FALSE,object$parms[2]))
  }else ids <- c(rep(TRUE,object$parms[1]),rep(FALSE,object$parms[2]))

  X <- model.matrix(object); n <- nrow(X); y <- object$y
  p <- object$parms[1]; ep <- object$parms[2]
  eta <- X%*%object$coefficients[1:p] + object$offset
  mu <- object$family$linkinv(eta)
  dmudeta <- object$family$mu.eta(eta)
  weights <- object$prior.weights
  Qpp2 <- chol2inv(object$R)
  if(ep > 0) Qpp2[!ids,!ids] <- Qpp2[!ids,!ids] - solve(-crossprod(object$R)[!ids,!ids])

  if(object$family$family %in% c("nb1","nb2","nbf")){
    if(object$family$family=="nb1") tau <- 0
    if(object$family$family=="nb2") tau <- -1
    phi <- exp(object$coefficients[p + 1])
    if(object$family$family=="nbf") tau <- object$coefficients[p + ep]
    u <- phi*mu^tau; v <- u*mu; k1 <- k2 <- 0
    E1 <- - (Digamma(y + 1/u) - log(v + 1) - Digamma(1/u))/u
    E2 <- (y - mu)/(v + 1)
    if(object$zero.trunc){
      k1 <- (log(v + 1)*tau/v - (tau + 1)/(v + 1))/((v + 1)^(1/u) - 1)
      k2 <- (log(v + 1)/u - mu/(v + 1))/((v + 1)^(1/u) - 1)
    }
    s1 <- (tau*E1 + (tau + 1)*E2 + k1*mu)*dmudeta/mu
    s2 <- E1 + E2 + k2; s3 <- s2*log(mu)
    if(object$family$family=="nbf") Delta <- cbind(X*matrix(weights*s1,n,p),weights*s2,weights*s3)
    else Delta <- cbind(X*matrix(weights*s1,n,p),weights*s2)
  }
  if(object$family$family=="poi"){
    if(object$zero.trunc) k <- - exp(-mu)/(1 - exp(-mu)) else k <- 0
    s <- (y/mu - 1 + k)*dmudeta
    Delta <- X*matrix(weights*s,n,p)
  }
  if(object$family$family=="bb"){
    m <- apply(object$y,1,sum); y <- object$y[,1]
    phi <- exp(object$coefficients[p + 1])
    c1 <- Digamma(y + mu/phi) - Digamma(mu/phi)
    c2 <- Digamma((1 - mu)/phi) - Digamma(m - y + (1 - mu)/phi)
    c3 <- - c1*mu + c2*(1 - mu) - Digamma(1/phi) + Digamma(m + 1/phi)
    Delta <- cbind(X*matrix(weights*dmudeta*(c1 + c2)/phi,n,p),weights*c3/phi)
  }
  if(object$family$family=="rcb"){
    m <- apply(object$y,1,sum); y <- object$y[,1]
    phi <- exp(object$coefficients[p + 1])/(1 + exp(object$coefficients[p + 1]))
    a1 <- mu*dbinom(y,m,(1 - phi)*mu + phi); a2 <- (1 - mu)*dbinom(y,m,(1 - phi)*mu)
    a3 <- y/((1 - phi)*mu + phi); a4 <- (m - y)/(1-(1 - phi)*mu - phi)
    a5 <- y/((1 - phi)*mu); a6 <- (m - y)/(1 - (1 - phi)*mu)
    a <- (a1*((a3 - a4)*(1 - phi) + 1/mu) + a2*((a5 - a6)*(1 - phi) - 1/(1 - mu)))/(a1 + a2)
    b <- (a1*(a3 - a4)*(1 - mu) + a2*(a6 - a5)*mu)*phi*(1 - phi)/(a1 + a2)
    Delta <- cbind(X*matrix(weights*a*dmudeta,n,p),weights*b)
  }
  li <- Delta%*%Qpp2
  if(type=="local"){
    tol <- 1
    bnew <- matrix(rnorm(nrow(li)),nrow(li),1)
    while(tol > 0.000001){
      bold <- bnew
      bnew <- li%*%crossprod(Delta,bold)
      bnew <- bnew/sqrt(sum(bnew^2))
      tol <- max(ifelse(abs(bold) > 1e-10,abs((bnew - bold)/bold),abs(bnew - bold)))
    }
    out_ <- bnew/sqrt(sum(bnew^2))
  }else out_ <- apply(li*Delta,1,sum)
  out_ <- matrix(out_,n,1)
  rownames(out_) <- 1:n
  colnames(out_) <- type
  if(plot.it){
    nano <- list(...)
    nano$x <- 1:n
    nano$y <- out_
    if(is.null(nano$xlab)) nano$xlab <- "Observation Index"
    if(is.null(nano$type)) nano$type <- "h"
    if(is.null(nano$ylab)) nano$ylab <- ifelse(type=="local",expression(d[max]),expression(diag[i]))
    if(is.null(nano$main)) nano$main <- ""
    if(is.null(nano$labels)) labels <- 1:n
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    do.call("plot",nano)
    if(any(out_ > 0)) abline(h=3*mean(out_[out_ > 0]),lty=3)
    if(any(out_ < 0)) abline(h=3*mean(out_[out_ < 0]),lty=3)
    if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=labels)
  }
  if(!is.null(subst)) message("The coefficients included in the measures of local influence are: ",paste(subst,sep=""),"\n")
  return(out_)
}

#' @title Generalized Variance Inflation Factor for alternatives to the Poisson and Binomial Regression Models under the presence of Overdispersion
#' @description Computes the generalized variance inflation factor (GVIF) for regression models based on the negative binomial, beta-binomial, and
#' random-clumped binomial distributions, which are alternatives to the Poisson and binomial regression models under the presence of overdispersion.
#' The GVIF is aimed to identify collinearity problems.
#' @param model an object of class \emph{overglm}.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param ... further arguments passed to or from other methods.
#' @details If the number of degrees of freedom is 1 then the GVIF reduces to the Variance
#' Inflation Factor (VIF).
#' @return A matrix with so many rows as effects in the model and the following columns:
#' \tabular{ll}{
#' \code{GVIF} \tab the values of GVIF,\cr
#' \tab \cr
#' \code{df}\tab the number of degrees of freedom,\cr
#' \tab \cr
#' \code{GVIF^(1/(2*df))}\tab the values of GVIF\eqn{^{1/2 df}},\cr
#' }
#' @method gvif overglm
#' @export
#' @examples
#' ###### Example 1: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit1 <- overglm(infections ~ frequency + location, family="nb1(log)", data=swimmers)
#' gvif(fit1)
#'
#' ###### Example 2: Article production by graduate students in biochemistry PhD programs
#' bioChemists <- pscl::bioChemists
#' fit2 <- overglm(art ~ fem + kid5 + ment, family="nb1(log)", data = bioChemists)
#' gvif(fit2)
#'
#' ###### Example 3: Agents to stimulate cellular differentiation
#' data(cellular)
#' fit3 <- overglm(cbind(cells,200-cells) ~ tnf + ifn, family="bb(logit)", data=cellular)
#' gvif(fit3)
#'
#' @references Fox J., Monette G. (1992) Generalized collinearity diagnostics, \emph{JASA} 87, 178–183.
#' @seealso \link{gvif.lm}, \link{gvif.glm}
#'
gvif.overglm <- function(model,verbose=TRUE,...){
  vars <- attr(model$terms,"term.labels")
  postos <- attr(model.matrix(model),"assign")[attr(model.matrix(model),"assign") > 0]
  vcovar <- vcov(model)[rownames(coef(model))!="(Intercept)",rownames(coef(model))!="(Intercept)"]
  nn <- max(postos)
  if(nn == 1) stop("At least two terms are required to compute GVIFs!!",call.=FALSE)
  results <- matrix(0,nn,3)
  cors <- cov2cor(vcovar); detx <- det(cors)
  for(i in 1:nn){
    rr2 <- as.matrix(cors[postos == i,postos == i])
    rr3 <- as.matrix(cors[postos != i,postos != i])
    results[i,1] <- round(det(rr2)*det(rr3)/detx,digits=4)
    results[i,2] <- ncol(rr2)
    results[i,3] <- round(results[i,1]^(1/(2*ncol(rr2))),digits=4)
  }
  rownames(results) <- vars
  colnames(results) <- c("GVIF", "df", "GVIF^(1/(2*df))")
  results <- results[order(-results[,3]),]
  if(verbose) print(results)
  return(results)
}

#' @method confint overglm
#' @export
confint.overglm <- function(object,parm,level=0.95,contrast,digits=max(3, getOption("digits") - 2),verbose=TRUE,...){
  name.s <- rownames(object$coefficients)
  if(missingArg(contrast)){
    bs <- coef(object)
    ee <- sqrt(diag(vcov(object)))
  }else{
    contrast <- as.matrix(contrast)
    if(ncol(contrast)!=length(name.s)) stop(paste("Number of columns of contrast matrix must to be",length(name.s)),call.=FALSE)
    bs <- contrast%*%coef(object)
    ee <- sqrt(diag(contrast%*%vcov(object)%*%t(contrast)))
    name.s <- apply(contrast,1,function(x) paste0(x[x!=0],"*",name.s[x!=0],collapse=" + "))
  }
  results <- matrix(0,length(ee),2)
  results[,1] <- bs - qnorm((1+level)/2)*ee
  results[,2] <- bs + qnorm((1+level)/2)*ee
  rownames(results) <- name.s
  colnames(results) <- c("Lower limit","Upper limit")
  if(verbose){
    cat("\n Approximate",round(100*level,digits=1),"percent confidence intervals based on the Wald test \n\n")
    print(round(results,digits=digits))
  }
  return((round(results,digits=digits)))
}

#' @method confint zeroinflation
#' @export
confint.zeroinflation <- function(object,parm,level=0.95,contrast,submodel=c("counts","zeros"),digits=max(3, getOption("digits") - 2),verbose=TRUE,...){
  coefs <- coef(object,submodel=submodel)
  name.s <- rownames(coefs)
  if(missingArg(contrast)){
    bs <- coefs
    ee <- sqrt(diag(vcov(object,submodel=submodel)))
  }else{
    contrast <- as.matrix(contrast)
    if(ncol(contrast)!=length(name.s)) stop(paste("Number of columns of contrast matrix must to be",length(name.s)),call.=FALSE)
    bs <- contrast%*%coefs
    ee <- sqrt(diag(contrast%*%vcov(object,submodel=submodel)%*%t(contrast)))
    name.s <- apply(contrast,1,function(x) paste0(x[x!=0],"*",name.s[x!=0],collapse=" + "))
  }
  results <- matrix(0,length(ee),2)
  results[,1] <- bs - qnorm((1+level)/2)*ee
  results[,2] <- bs + qnorm((1+level)/2)*ee
  rownames(results) <- name.s
  colnames(results) <- c("Lower limit","Upper limit")
  if(verbose){
    cat("\n Approximate",round(100*level,digits=1),"percent confidence intervals based on the Wald test \n\n")
    print(round(results,digits=digits))
  }
  return((round(results,digits=digits)))
}
