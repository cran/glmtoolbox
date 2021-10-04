#' @title Negative Binomial and Beta-Binomial Regression Models
#' @description Produces an object of the class overglm in which are stored the main results of the negative binomial or beta-binomial regression model fitted to the data.
#' @param formula a \code{formula} expression of the form \code{response ~ predictors}, which is a symbolic description of the linear predictor of the model to be fitted to the data.
#' @param family a character string which describe the distribution of the response variable and the link function. The following distributions are supported: negative binomial I ("nb1"), negative binomial II ("nb2"), negative binomial III ("nb3"), and beta-binomial ("bb"). Link functions available for negative binomial and beta-binomial regression models are the same than those available for Poisson and binomial regression models, respectively. See \link{family} documentation.
#' @param weights an (optional) vector of positive "prior weights" to be used in the fitting process. The length of \code{weights} should be the same as the number of observations.
#' @param data an (optional) \code{data frame} in which to look for variables involved in the \code{formula} expression, as well as the variable specified in the argument \code{weights}.
#' @param subset an (optional) vector specifying a subset of observations to be used in the fitting process.
#' @param start an (optional) vector of starting values for the parameters in the linear predictor.
#' @param ...	further arguments passed to or from other methods.
#' @return an object of the class overglm in which are stored the main results of the model fitted to the data. Some of those results can be easily accessed using functions as, for example, \code{print()}, \code{summary()}, \code{model.matrix()}, \code{estequa()}, \code{coef()}, \code{vcov()}, \code{logLik()}, \code{fitted()}, \code{confint()} and \code{predict()}. In addition, the model fitted to the data
#' can be assessed using functions as, for instance, \link{anova.overglm}, \link{residuals.overglm}, \link{dfbeta.overglm}, \link{cooks.distance.overglm} and \link{envelope.overglm}. The variable selection may be accomplished using \link{stepCriterion.overglm}.
#' @details
#' The negative binomial distributions can be obtained as mixture of the Poisson and Gamma distributions. Let \eqn{Y | \lambda} ~ Poisson\eqn{(\lambda)}, where
#' E\eqn{(Y | \lambda)=\lambda} and Var\eqn{(Y | \lambda)=\lambda}, and
#' \eqn{\lambda} ~ Gamma\eqn{(\beta,\alpha)}, where E\eqn{(\lambda)=\beta} and Var\eqn{(\lambda)=\alpha\beta^2}. Therefore,
#'
#' (1) If \eqn{\beta=\mu} and \eqn{\alpha=\phi} then \eqn{Y} ~ Binomial Negativa I\eqn{(\mu,\phi)},
#' E\eqn{(Y)=\mu} and Var\eqn{(Y)=\mu + \phi\mu^2}.
#'
#' (2) If \eqn{\beta=\mu} and \eqn{\alpha=\phi/\mu} then \eqn{Y} ~ Binomial Negativa II\eqn{(\mu,\phi)},
#' E\eqn{(Y)=\mu} and Var\eqn{(Y)=(\phi + 1)\mu}.
#'
#' (3) If \eqn{\beta=\mu} and \eqn{\alpha=\phi\mu} then \eqn{Y} ~ Binomial Negativa III\eqn{(\mu,\phi)},
#' E\eqn{(Y)=\mu} and Var\eqn{(Y)=\mu+\phi\mu^3}.
#'
#' So, the regression models based on the negative binomial distributions are alternatives to the Poisson regression model
#' under the presence of overdispersion.
#'
#' The beta-binomial distribution can be obtained as mixture of the binomial and Beta distributions. Let \eqn{mY | \pi} ~ Binomial\eqn{(m,\pi)}, where E\eqn{(Y | \pi)=\pi} and Var\eqn{(Y | \pi)=(1/m)\pi(1-\pi)},
#' and \eqn{\pi} ~ Beta\eqn{(\mu,\phi)}, where E\eqn{(\pi)=\mu} and Var\eqn{(\pi)=[\phi/(\phi+1)]\mu(1-\mu)}, then
#' \eqn{mY} ~ Beta Binomial\eqn{(m,\mu,\phi)}, E\eqn{(Y)=\mu} and
#' Var\eqn{(Y)=(1/m)\mu(1-\mu)[1 + (m-1)\phi/(\phi+1)]}.
#'
#' So, the regression model based on the beta-binomial distribution is an alternative to the binomial regression model
#' under the presence of overdispersion.
#'
#' @export overglm
#' @importFrom stats binomial fitted lm optim poisson dnbinom pnbinom runif logLik AIC BIC
#' @import numDeriv Rfast
#' @importFrom stats qqnorm quantile rbeta rbinom resid residuals rgamma rpois
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @examples
## Example 1
#' fit1 <- overglm(tumors ~ group, family="nb3(log)", data=mammary)
#' summary(fit1)
#'
## Example 2
#' fit2 <- overglm(infections ~ frequency + location, family="nb1(log)", data=swimmers)
#' summary(fit2)
#'
## Example 3
#' fit3 <- overglm(cbind(cells,200-cells) ~ tnf + ifn + tnf*ifn, family="bb(logit)", data=cellular)
#' summary(fit3)
#'
## Example 4
#' fit4 <- overglm(cbind(fetuses,litter-fetuses) ~ pht*tcpo, family="bb(logit)", data=ossification)
#' summary(fit4)
#'
#' @references Lawless J.F. (1987) Negative binomial and mixed poisson regression, \emph{The Canadian Journal
#' of Statistics} 15, 209-225.
#'
#' Crowder M. (1978) Beta-binomial anova for proportions, \emph{Journal of the Royal Statistical Society Series C (Applied Statistics)} 27, 34-37.

overglm <- function(formula,family,weights,data,subset,start=NULL,...){
  if (missingArg(data)) data <- environment(eval(formula))
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "weights", "data", "subset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- as.matrix(model.response(mf, "any"))
  temp <- strsplit(gsub(" |link|=|'|'","",tolower(family)),"[()]")[[1]]
  family <- temp[1]
  if(is.na(temp[2])){
    if(family=="bb") link <- "logit" else link="log"
  }else link <- temp[2]
  yres <- y
  colnames(yres) <- rep(" ",ncol(yres))
  if(family=="bb"){
    m <- as.matrix(y[,1]+y[,2])
    y <- as.matrix(y[,1])
  }
  weights <- as.vector(model.weights(mf))
  offset <- as.vector(model.offset(mf))
  X <- model.matrix(mt, mf)
  p <- ncol(X)
  n <- nrow(X)
  if(is.null(offset)) offs <- matrix(0,n,1) else offs <- as.matrix(offset)
  if(is.null(weights)) weights <- matrix(1,n,1) else weights <- as.matrix(weights)
  if(any(weights <= 0)) stop("Only positive weights are allowed!!",call.=FALSE)
  if(family=="bb") familyf <- binomial(link) else familyf <- poisson(link)
  if(family=="nb1"){
    escore <- function(theta){
      eta <- tcrossprod(X,t(theta[1:p])) + offs
      mu <- familyf$linkinv(eta)
      phi <- exp(theta[p+1])
      u1 <- -crossprod(X,weights*(y-mu)*familyf$mu.eta(eta)/(mu + phi*mu^2))
      u2 <- -sum(weights*(Digamma(1/phi)-Digamma(y+1/phi)+phi*(y-mu)/(phi*mu+1)+log(mu*phi+1))/phi)
      return(rbind(u1,u2))
    }
    objetive <- function(theta){
      mu <- familyf$linkinv(tcrossprod(X,t(theta[1:p])) + offs)
      phi <- exp(theta[p+1])
      -sum(weights*(Lgamma(y+1/phi)-Lgamma(1/phi)+y*log(mu*phi)-(y+1/phi)*log(mu*phi+1)))
    }
    theta0 <- function(fits){
      mus <- fitted(fits)
      fik <- lm((fit0$y - mus)^2 ~ -1 + I(mus^2) + offset(mus), weights=1/(mus + 2*mus^2))
      c(coef(fits),log(abs(coef(fik))))
    }
  }
  if(family=="nb2"){
    escore <- function(theta){
      eta <- tcrossprod(X,t(theta[1:p])) + offs
      mu <- familyf$linkinv(eta)
      phi <- exp(theta[p+1])
      const <- Digamma(y+mu/phi)-Digamma(mu/phi)-log(1+phi)
      u1 <- -crossprod(X,weights*familyf$mu.eta(eta)*const/phi)
      u2 <- -sum(weights*(-const*mu/phi+y-phi*(y+mu/phi)/(1+phi)))
      return(rbind(u1,u2))
    }
    objetive <- function(theta){
      mu <- familyf$linkinv(tcrossprod(X,t(theta[1:p])) + offs)
      phi <- exp(theta[p+1])
      result <- -sum(weights*(Lgamma(y+mu/phi)-Lgamma(mu/phi)+y*log(phi)-(y+mu/phi)*log(1+phi)))
    }
    theta0 <- function(fits){
      mus <- fitted(fits)
      fik <- lm((fit0$y - mus)^2 ~ -1 + mus, weights=1/(mus + 2*mus^2))
      c(coef(fits),log(abs(coef(fik)-1)))
    }
  }
  if(family=="nb3"){
    escore <- function(theta){
      eta <- tcrossprod(X,t(theta[1:p])) + offs
      mu <- familyf$linkinv(eta)
      phi <- exp(theta[p+1])
      const1 <- Digamma(1/(phi*mu))-Digamma(y+1/(phi*mu))+log(phi*mu^2+1)
      const2 <- phi*y*mu-(phi*y*mu+1)*phi*mu^2/(phi*mu^2+1)
      u1 <- -crossprod(X,weights*familyf$mu.eta(eta)*(const1+2*const2)/(phi*mu^2))
      u2 <- -sum(weights*((const1+const2)/(phi*mu)))
      return(rbind(u1,u2))
    }
    objetive <- function(theta){
      mu <- familyf$linkinv(tcrossprod(X,t(theta[1:p])) + offs)
      phi <- exp(theta[p+1])
      -sum(weights*(Lgamma(y+1/(phi*mu))-Lgamma(1/(phi*mu))+y*log(mu^2*phi)-(y+1/(phi*mu))*log(mu^2*phi+1)))
    }
    theta0 <- function(fits){
      mus <- fitted(fits)
      fik <- lm((fit0$y - mus)^2 ~ -1 + I(mus^3) + offset(mus), weights=1/(mus + 2*mus^2))
      c(coef(fits),log(abs(coef(fik))))
    }
  }
  if(family=="bb"){
    escore <- function(theta){
      eta <- tcrossprod(X,t(theta[1:p])) + offs
      mu <- familyf$linkinv(eta)
      phi <- exp(theta[p+1])
      const1 <- Digamma(y+mu/phi)-Digamma(mu/phi)
      const2 <- Digamma((1-mu)/phi)-Digamma(m-y+(1-mu)/phi)
      u1 <- -crossprod(X,as.matrix(weights*familyf$mu.eta(eta)*(const1+const2)/phi))
      const3 <- -const1*mu+const2*(1-mu)-Digamma(1/phi)+Digamma(m+1/phi)
      u2 <- -sum(weights*const3/phi)
      return(rbind(u1,u2))
    }
    objetive <- function(theta){
      mu <- familyf$linkinv(tcrossprod(X,t(theta[1:p])) + offs)
      phi <- exp(theta[p+1])
      -sum(weights*(Lgamma(1/phi)+Lgamma(y+mu/phi)+Lgamma(m-y+(1-mu)/phi)-Lgamma(m+1/phi)-Lgamma(mu/phi)-Lgamma((1-mu)/phi)))
    }
    theta0 <- function(fits){
      mus <- fitted(fits)
      l0 <- mean(abs((y - m*mus)^2/(m*mus*(1-mus)) - 1)/ifelse(m==1,1,m-1))
      c(coef(fits),log(l0/(1-l0)))
    }
  }
  if(is.null(start))
    fit0 <- glm.fit(y=yres,x=X,offset=offset,weights=weights,family=familyf)
  else
    fit0 <- glm.fit(y=yres,x=X,offset=offset,weights=weights,family=familyf,start=start)

  start <- theta0(fit0)

  salida <- optim(start,fn=objetive,gr=escore,method="BFGS",control=list(reltol=1e-15))
  theta_hat <- as.matrix(salida$par,nrow=length(theta_hat),1)
  eta <- tcrossprod(X,t(theta_hat[1:p])) + offs
  mu <- familyf$linkinv(eta)
  colnames(mu) <- ""
  rownames(theta_hat) <- c(colnames(X),"log(dispersion)")
  colnames(theta_hat) <- ""
  estfun <- -escore(theta_hat)
  if(salida$convergence != 0) stop("Convergence not achieved!!",call.=FALSE)
  rownames(estfun) <- c(colnames(X),"log(dispersion)")
  colnames(estfun) <- ""
  logLik <- switch(family,
                   "nb1"=-salida$value-sum(weights*lgamma(y+1)),
                   "nb2"=-salida$value-sum(weights*lgamma(y+1)),
                   "nb3"=-salida$value-sum(weights*lgamma(y+1)),
                   "bb"=-salida$value + sum(weights*(Lgamma(m+1)-Lgamma(y+1)-Lgamma(m-y+1))))
  out_ <- list(coefficients=theta_hat,fitted.values=mu,linear.predictors=eta,
               prior.weights=weights,y=yres,formula=formula,call=match.call(),offset=offs,model=mf,data=data,
               df.residual=n-p-1,logLik=logLik,converged=ifelse(salida$convergence==0,TRUE,FALSE),
               estfun=estfun,terms=mt,escore=escore,objetive=objetive)
  class(out_) <- "overglm"
  nano <- list(...)
  if(is.null(nano$si.mu.la.ti.on)){
    hess <- -chol(jacobian(escore,theta_hat))
    colnames(hess) <- rownames(theta_hat)
    rownames(hess) <- rownames(theta_hat)
    out_$R <- hess
  }
  familyf <- list(family=family,link=link)
  class(familyf) <- "family"
  out_$family <- familyf
  return(out_)
}

#' @method print overglm
#' @export
print.overglm <- function(x,...){
  cat("     Family: ",switch(x$family$family,"nb1"="Negative Binomial I",
                             "nb2"="Negative Binomial II",
                             "nb3"="Negative Binomial III",
                             "bb"="Beta-Binomial"))
  cat("\n       Link: ",x$family$link)
  p <- length(x$coefficients)
  cat("\n********************************************************")
  cat("\n -2*log-likelihood: ",round(-2*x$logLik,digits=3),"\n")
  cat("degrees of freedom: ",length(x$coefficients),"\n")
  cat("               AIC: ",round(-2*x$logLik + 2*(p),digits=3),"\n")
  cat("               BIC: ",round(-2*x$logLik + log(nrow(x$y))*(p),digits=3),"\n")
}

#' @method confint overglm
#' @export
confint.overglm <- function(object,parm,level=0.95,digits=4,verbose=TRUE,...){
  ee <- sqrt(diag(vcov(object)))
  results <- matrix(0,length(ee),2)
  results[,1] <- coef(object) - qnorm(1-level/2)*ee
  results[,2] <- coef(object) + qnorm(1-level/2)*ee
  rownames(results) <- rownames(object$coefficients)
  colnames(results) <- c("Lower limit","Upper limit")
  if(verbose){
    cat("\n Approximate",round(100*(1-level),digits=1),"percent confidence intervals based on the Wald test \n\n")
    print(round(results,digits=digits))
  }
  return(invisible(results))
}

#' @title Estimating Equations in Negative Binomial and Beta-Binomial Models
#' @description Extracts estimating equations evaluated at the estimates of the parameters for a negative binomial or beta-binomial model fitted to the data.
#' @param model an object of the class overglm which is obtained from the fit of a negative binomial or beta-binomial regression model.
#' @param ... further arguments passed to or from other methods.
#' @return A vector with the value of the estimating equations evaluated at the parameter estimates and the observed data.
#' @method estequa overglm
#' @export
#' @examples
#' ## Example 1
#' fit1 <- overglm(tumors ~ group, family="nb3(log)", data=mammary)
#' estequa(fit1)
#'
## Example 2
#' fit2 <- overglm(infections ~ frequency + location, family="nb1(log)", data=swimmers)
#' estequa(fit2)
#'
## Example 3
#' fit3 <- overglm(cbind(cells,200-cells) ~ tnf + ifn + tnf*ifn, family="bb(logit)", data=cellular)
#' estequa(fit3)
#'
## Example 4
#' fit4 <- overglm(cbind(fetuses,litter-fetuses) ~ pht*tcpo, family="bb(logit)", data=ossification)
#' estequa(fit4)

estequa.overglm <- function(model,...){
  salida <- model$estfun
  colnames(salida) <- " "
  return(salida)
}
#' @title Comparison of nested Negative Binomial and Beta-Binomial Regression Models
#' @description Allows to compare nested negative binomial and beta-binomial regression models using Wald, score, gradient and likelihood ratio tests.
#' @param object an object of the class overglm which is obtained from the fit of a negative binomial or beta-binomial model.
#' @param ... another objects of the class overglm which are obtained from the fit of negative binomial or beta-binomial models.
#' @param test an (optional) character string indicating the required test. The available options are: Wald ("wald"), Rao's score ("score"), likelihood ratio ("lr") and Terrell's gradient ("gradient") tests. By default, \code{test} is set to be "wald".
#' @param verbose an (optional) logical indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @return A matrix with three columns which contains the following:
#' \itemize{
#' \item \code{Chi:}{ The value of the statistic of the test.}
#' \item \code{Df:}{ The number of degrees of freedom.}
#' \item \code{Pr(>Chi):}{ The \emph{p}-value of the test computed using the Chi-square distribution.}
#' }
#' @method anova overglm
#' @export
#' @details The Wald, Rao's score and Terrell's gradient tests are performed using the observed Fisher information matrix.
#' @references Buse A. (1982) The Likelihood Ratio, Wald, and Lagrange Multiplier Tests: An Expository Note. \emph{The American Statistician} 36, 153 - 157.
#' @references Terrell G.R. (2002) The gradient statistic. \emph{Computing Science and Statistics} 34, 206 – 215.
#' @examples
#' ## Example 1
#' fit1 <- overglm(cbind(cells,200-cells) ~ tnf, family="bb(logit)", data=cellular)
#' fit2 <- update(fit1, . ~ . + ifn)
#' fit3 <- update(fit2, . ~ . + tnf:ifn)
#' anova(fit1, fit2, fit3, test="wald")
#' anova(fit1, fit2, fit3, test="score")
#' anova(fit1, fit2, fit3, test="lr")
#' anova(fit1, fit2, fit3, test="gradient")
#'
#' ## Example 2
#' fit1 <- overglm(infections ~ frequency, family="nb1(log)", data=swimmers)
#' fit2 <- update(fit1, . ~ . + location)
#' fit3 <- update(fit2, . ~ . + age)
#' fit4 <- update(fit3, . ~ . + gender)
#' anova(fit1, fit2, fit3, fit4, test="wald")
#' anova(fit1, fit2, fit3, fit4, test="score")
#' anova(fit1, fit2, fit3, fit4, test="lr")
#' anova(fit1, fit2, fit3, fit4, test="gradient")
anova.overglm <- function(object,...,test=c("wald","lr","score","gradient"),verbose=TRUE){
  test <- match.arg(test)
  x <- list(object,...)
  if(any(lapply(x,function(xx) class(xx)[1])!="overglm"))
    stop("Only glm-type objects are supported!!",call.=FALSE)
  if(length(x)==1){
    terminos <- attr(object$terms,"term.labels")
    x[[1]] <- update(object,paste(". ~ . -",paste(terminos,collapse="-")))
    for(i in 1:length(terminos)) x[[i+1]] <- update(x[[i]],paste(". ~ . + ",terminos[i]))
  }
  hast <- length(x)
  out_ <- matrix(0,hast-1,3)
  for(i in 2:hast){
    vars0 <- rownames(coef(x[[i-1]]))
    vars1 <- rownames(coef(x[[i]]))
    nest <- vars0 %in% vars1
    ids <- is.na(match(vars1,vars0))
    if(test=="wald") sc <- crossprod(coef(x[[i]])[ids],solve(vcov(x[[i]])[ids,ids]))%*%coef(x[[i]])[ids]
    if(test=="lr") sc <- 2*(logLik(x[[i]])-logLik(x[[i-1]]))
    if(test=="score" | test=="gradient"){
      envir <- environment(x[[i]]$escore)
      envir$weights <- x[[i]]$prior.weights
      n <- length(x[[i]]$prior.weights)
      if(is.null(x[[i]]$offset)) envir$offs <- matrix(0,n,1)
      else envir$offs <- as.matrix(x[[i]]$offset)
      if(ncol(x[[i]]$y)==2){
        envir$m <- x[[i]]$y[,1] + x[[i]]$y[,2]
        envir$y <- x[[i]]$y[,1]
      }else envir$y <- x[[i]]$y
      envir$X <- model.matrix(as.formula(x[[i]]$formula),x[[i]]$data)
      envir$p <- ncol(envir$X)
      if(x[[i]]$family$family=="bb") familyf <- binomial(x[[i]]$family$link)
      else familyf <- poisson(x[[i]]$family$link)
      envir$familyf <- familyf
      theta0 <- coef(x[[i]])
      theta0[ids] <- rep(0,sum(ids))
      theta0[!ids] <- coef(x[[i-1]])
      u0 <- x[[i]]$escore(theta0)[ids]
      if(test=="score"){
        v0 <- solve(jacobian(x[[i]]$escore,theta0))[ids,ids]
        sc <- abs(crossprod(u0,v0)%*%u0)
      }else sc <- abs(crossprod(u0,coef(x[[i]])[ids]))
    }
    df <- sum(ids)
    out_[i-1,] <- cbind(sc,df,1-pchisq(sc,df))
  }
  colnames(out_) <- c(" Chi  ", " Df", "  Pr(>Chi)")
  rownames(out_) <- paste(1:(hast-1),"vs",2:hast)
  if(verbose){
    test <- switch(test,"lr"="Likelihood-ratio test",
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

#' @title Dfbeta for Negative Binomial and Beta-binomial Models
#' @description Produces an approximation, better known as the \emph{one-step approximation}, of the effect on the parameter estimates of a negative binomial or beta-binomial model of deleting each individual in turn. This function also can produce a plot of those effects for a subset of the parameters in the linear predictor.
#' @param model an object of class overglm which is obtained from the fit of a negative binomial or beta-binomial model.
#' @param coefs	an (optional) character string which (partially) match the names of some parameters in the linear predictor.
#' @param identify an (optional) integer indicating the number of individuals to identify on the plot of dfbeta. This is only appropriate if the argument \code{coefs} is specified.
#' @param ... further arguments passed to or from other methods.   If \code{coefs} is specified then \code{...} may be used to include graphical parameters to customize the plot. For example,  \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix with so many rows as individuals in the sample and so many columns as parameters. The \eqn{i}-th row of that matrix corresponds to the difference between the estimates of the parameters using all individuals and the \emph{one-step approximation} of those estimates when the \emph{i}-th individual is excluded from the dataset.
#' @details The \emph{one-step approximation} of the estimates of the parameters in a negative binomial or beta-binomial model when the \emph{i}-th individual is excluded from the dataset consists of the vector obtained as result of the first iteration of the Newthon-Raphson algorithm when it is performed using: (1) a dataset in which the \emph{i}-th individual is excluded; and (2) a starting value which is the estimate of the same negative binomial or beta-binomial model but based on the dataset inluding all individuals.
#' @references Pregibon D. (1981). Logistic regression diagnostics. \emph{The Annals of Statistics}, 9, 705-724.
#' @method dfbeta overglm
#' @examples
#' fit <- glm(cbind(cells,200-cells) ~ tnf + ifn + tnf*ifn, family=binomial, data=cellular)
#' dfbs <- dfbeta(fit, coefs="tnf:ifn", col="red", lty=1, lwd=1, col.lab="blue",
#'                col.axis="blue", col.main="black", family="mono", cex=0.8, main="Dfbeta")
#' @export
dfbeta.overglm <- function(model, coefs, identify,...){
  envir <- environment(model$escore)
  weights <- model$prior.weights
  envir$weights <- weights
  n <- length(weights)
  if(is.null(model$offset)) envir$offs <- matrix(0,n,1) else envir$offs <- as.matrix(model$offset)
  if(ncol(model$y)==2){
    envir$m <- model$y[,1] + model$y[,2]
    envir$y <- model$y[,1]
  }else envir$y <- model$y
  envir$X <- model.matrix(model)
  envir$p <- ncol(envir$X)
  if(model$family$family=="bb") familyf <- binomial(model$family$link) else familyf <- poisson(model$family$link)
  envir$familyf <- familyf
  dfbetas <- matrix(0,n,envir$p+1)
  temp <- data.frame(y=model$y,X=envir$X,offs=envir$offs,weights=weights,ids=1:n)
  d <- ncol(temp)
  colnames(temp) <- c(paste("var",1:(d-1),sep=""),"ids")
  orden <- eval(parse(text=paste("with(temp,order(",paste(colnames(temp)[-d],collapse=","),"))",sep="")))
  temp2 <- temp[orden,]
  envir$weights[temp2$ids[1]] <- 0
  dfbetas[1,] <- -solve(jacobian(model$escore,model$coefficients))%*%model$escore(model$coefficients)
  for(i in 2:n){
    if(all(temp2[i,-d]==temp2[i-1,-d])) dfbetas[i,] <- dfbetas[i-1,]
    else{envir$weights <- weights
    envir$weights[temp2$ids[i]] <- 0
    dfbetas[i,] <- solve(jacobian(model$escore,model$coefficients))%*%model$escore(model$coefficients)
    }
  }
  dfbetas <- dfbetas[order(temp2$ids),]
  colnames(dfbetas) <- c(colnames(envir$X),"log(dispersion)")

  if(!missingArg(coefs)){
    ids <- grep(coefs,colnames(dfbetas),ignore.case=TRUE)
    if(length(ids) > 0){
      nano <- list(...)
      if(is.null(nano$labels)) labels <- 1:nrow(dfbetas) else{
        labels <- nano$labels
        nano$labels <-NULL
      }
      nano$x <- 1:nrow(dfbetas)
      if(is.null(nano$xlab)) nano$xlab <- "Cluster Index (i)"
      if(is.null(nano$type)) nano$type <- "h"
      if(is.null(nano$ylab)) nano$ylab <- expression(hat(beta)-hat(beta)[("- i")])
      oldpar <- par(no.readonly=TRUE)
      on.exit(par(oldpar))
      par(mfrow=c(1,length(ids)))
      for(i in 1:length(ids)){
        nano$y <- dfbetas[,ids[i]]
        nano$main <- colnames(dfbetas)[ids[i]]
        do.call("plot",nano)
        if(!missingArg(identify)){
          identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=model$ids)
        }
      }
    }
  }
  return(invisible(dfbetas))
}

#' @title Cook's Distance for Negative Binomial and Beta-Binomial Models
#' @description Produces an approximation, better known as the \emph{one-step approximation}, of the Cook's distance, which is aimed to measure the effect on the estimates of the parameters in the linear predictor of deleting each observation in turn. This function also can produce an index plot of the Cook's distance for all parameters in the linear predictor or for some subset of them.
#' @param model an object of class overglm obtained from the fit of a negative binomial or beta-binomial model.
#' @param plot.it an (optional) logical indicating if the plot is required or just the data matrix in which that plot is based. By default, \code{plot.it} is set to be TRUE.
#' @param coefs	an (optional) character string which (partially) match with the names of some model parameters.
#' @param identify an (optional) integer indicating the number of individuals to identify on the plot of the Cook's distance. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used to include graphical parameters to customize the plot. For example,  \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix as many rows as individuals in the sample and one column with the values of the Cook's distance.
#' @details The Cook's distance consists of the \emph{distance} between two estimates of the parameters in the linear predictor using a metric based on the (estimate of the) variance-covariance matrix. The first one set of estimates is computed from a dataset including all individuals, and the second one is computed from a dataset in which the \emph{i}-th individual is excluded. To avoid computational burden, the second set of estimates is replaced by its \emph{one-step approximation}. See the \link{dfbeta.overglm} documentation.
#' @method cooks.distance overglm
#' @examples
#' ## Cook's distance for all parameters in the linear predictor
#' fit <- overglm(cbind(cells,200-cells) ~ tnf + ifn + tnf*ifn, family="bb(logit)", data=cellular)
#' cooks.distance(fit, col="red", lty=1, lwd=1, col.lab="blue", main="Cook's distance",
#'                col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ## Cook's distance for the parameter associated to the interaction
#' cooks.distance(fit, coef="tnf:ifn", col="red", lty=1, lwd=1, col.lab="blue",
#'                main="Cook's distance", col.axis="blue", col.main="black",
#'                family="mono", cex=0.8)
#' @export
cooks.distance.overglm <- function(model, plot.it=TRUE, coefs, identify,...){
  dfbetas <- dfbeta(model)
  met <- vcov(model)
  met <- met[-ncol(dfbetas),-ncol(dfbetas)]
  dfbetas <- dfbetas[,-ncol(dfbetas)]
  subst <- NULL
  if(!missingArg(coefs)){
    ids <- grepl(coefs,colnames(dfbetas),ignore.case=TRUE)
    if(sum(ids) > 0){
      subst <- colnames(dfbetas)[ids]
      dfbetas <- as.matrix(dfbetas[,ids])
      met <- as.matrix(met[ids,ids])
    }
  }
  CD <- as.matrix(apply((dfbetas%*%solve(met))*dfbetas,1,sum))
  colnames(CD) <- "Cook's distance"
  if(plot.it){
    nano <- list(...)
    if(is.null(nano$labels)) labels <- 1:nrow(dfbetas) else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    nano$x <- 1:nrow(dfbetas)
    nano$y <- CD
    if(is.null(nano$xlab)) nano$xlab <- "Index (i)"
    if(is.null(nano$type)) nano$type <- "h"
    if(is.null(nano$ylab)) nano$ylab <- expression((hat(beta)-hat(beta)[{(-~~i)}])^{T}~(Var(hat(beta)))^{-1}~(hat(beta)-hat(beta)[{(-~~i)}]))
    do.call("plot",nano)
    if(!missingArg(identify)){
      identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=model$ids)
    }
  }
  if(!is.null(subst)){
    message("The coefficients included in the Cook's distance are:\n")
    message(subst)
  }
  return(invisible(CD))
}

#' @method summary overglm
#' @export
summary.overglm <- function(object,...){
  cat("\nSample size: ",nrow(object$y),"\n")
  cat("     Family: ",switch(object$family$family,"nb1"="Negative Binomial I",
                             "nb2"="Negative Binomial II",
                             "nb3"="Negative Binomial III",
                             "bb"="Beta-Binomial"))
  cat("\n       Link: ",object$family$link)
  cat("\n *************************************************************\n")
  p <- length(object$coefficients) - 1
  rownamu <- rownames(object$coefficients)[1:p]
  rownaphi <- "Dispersion"
  delta <- max(nchar(rownamu)) - nchar(rownaphi)
  falta <- paste(replicate(max(abs(delta)-1,0)," "),collapse="")
  if(delta > 0) rownaphi[1] <- paste(rownaphi[1],falta,collapse="")
  if(delta < 0) rownamu[1] <- paste(rownamu[1],falta,collapse="")
  TAB	<- cbind(Estimate <- object$coefficients[1:p],
               StdErr <- sqrt(diag(chol2inv(object$R)))[1:p],
               tval <- Estimate/StdErr,
               p.value <- 2*pnorm(-abs(tval)))
  colnames(TAB) <- c("Estimate", "Std.Error", "z-value", "Pr(>|z|)")
  rownames(TAB) <- rownamu
  printCoefmat(TAB, P.values=TRUE, signif.stars=FALSE, has.Pvalue=TRUE, digits=5, dig.tst=5, signif.legend=FALSE, tst.ind=c(1,2,3))
  hess <- crossprod(object$R)
  hess[,p+1] <- hess[,p+1]/exp(object$coefficients[p+1])
  hess[p+1,] <- hess[p+1,]/exp(object$coefficients[p+1])
  TAB		 <- cbind(Estimate <- exp(object$coefficients[p+1]),
                 StdErr <- sqrt(diag(solve(hess)))[p+1])
  colnames(TAB) <- c("Estimate", "Std.Error")
  rownames(TAB) <- rownaphi
  cat("\n")
  printCoefmat(TAB, digits=5, dig.tst=5, signif.legend=FALSE, tst.ind=c(1,2))
  cat(" *************************************************************\n")
  cat("                 -2*log-likelihood: ",round(-2*object$logLik,digits=3),"\n")
  cat("                               AIC: ",round(-2*object$logLik + 2*(p+1),digits=3),"\n")
  cat("                               BIC: ",round(-2*object$logLik + log(nrow(object$y))*(p+1),digits=3),"\n")
}

#' @method coef overglm
#' @export
coef.overglm <- function(object,...){
  out_ <- object$coefficients
  colnames(out_) <- ""
  return(out_)
}

#' @method vcov overglm
#' @export
vcov.overglm <- function(object,...){
out_ <- chol2inv(object$R)
rownames(out_) <- rownames(object$R)
colnames(out_) <- rownames(object$R)
return(out_)
}

#' @method model.matrix overglm
#' @export
model.matrix.overglm <-	function(object,...){
  if(is.null(object$call$data)) m <- get_all_vars(eval(object$call$formula))
  else m <- get_all_vars(eval(object$call$formula),eval(object$call$data))
  modelframe <- model.frame(object$call,m)
  X <- model.matrix(modelframe,m)
  return(X)
}

#' @method logLik overglm
#' @export
logLik.overglm <- function(object,...){
  out_ <- object$logLik
  attr(out_,"df") <- length(object$coefficients)
  class(out_) <- "logLik"
  return(out_)
}

#' @method fitted overglm
#' @export
fitted.overglm <- function(object,...) return(object$fitted.values)

#' @title Predictions for Negative Binomial and Beta-Binomial regression models
#' @description Produces predictions and optionally estimates standard errors of those predictions from a fitted negative binomial or beta-binomial regression model.
#' @param object an object of class overglm which is obtained from the fit of a negative binomial or beta-binomial regression model.
#' @param newdata	an (optional) \code{data frame} in which to look for variables with which to predict. If omitted, the fitted linear predictors are used.
#' @param type an (optional) character string giving the type of prediction required. The default, "link", is on the scale of the linear predictors, and the alternative, "response", is on the scale of the response variable.
#' @param se.fit	an (optional) logical switch indicating if standard errors are required. By default, \code{se.fit} is set to be FALSE.
#' @param ... further arguments passed to or from other methods.
#' @return A matrix with so many rows as \code{newdata} and one column with the predictions. If \code{se.fit=}TRUE then a second column with estimates standard errors is included.
#' @method predict overglm
#' @export
#' @examples
## Example 1
#' fit1 <- overglm(tumors ~ group, family="nb3(log)", data=mammary)
#' newdata <- data.frame(group=as.factor(c("control","retinoid")))
#' predict(fit1,newdata=newdata,type="response",se.fit=TRUE)
#'
## Example 2
#' fit2 <- overglm(cbind(cells,200-cells) ~ tnf + ifn + tnf*ifn, family="bb(logit)", data=cellular)
#' newdata <- data.frame(tnf=c(0,100),ifn=c(100,0))
#' predict(fit2,newdata=newdata,type="response",se.fit=TRUE)
#'
predict.overglm <- function(object, ...,newdata, se.fit=FALSE, type=c("link","response")){
  type <- match.arg(type)
  if(object$family$family=="bb") familyf <- binomial(object$family$link) else familyf <- poisson(object$family$link)
  type <- match.arg(type)
  if(missingArg(newdata)){
    predicts <- object$linear.predictors
    X <- model.matrix(object)
  }
  else{
    newdata <- data.frame(newdata)
    mf <- model.frame(delete.response(object$terms),newdata)
    X <- model.matrix(delete.response(object$terms),mf)
    predicts <- tcrossprod(X,t(object$coefficients[1:ncol(X)]))
    offs <- model.offset(mf)
    if(!is.null(offs)) predicts <- predicts + offs
  }
  if(type=="response") predicts <- familyf$linkinv(predicts)
  if(se.fit){
    se <- sqrt(apply((X%*%chol2inv(object$R)[1:ncol(X),1:ncol(X)])*X,1,sum))
    if(type=="response") se <- se*abs(familyf$mu.eta(familyf$linkfun(predicts)))
    predicts <- cbind(predicts,se)
    colnames(predicts) <- c("fit","se.fit")
  }else colnames(predicts) <- c("fit")
  rownames(predicts) <- rep(" ",nrow(predicts))
  return(predicts)
}

#' @title Residuals for Negative Binomial and Beta-Binomial Regression Models
#' @description Calculates residuals for a fitted negative binomial or beta-binomial model.
#' @param object an object of the class overglm obtained from the fit of a negative binomial or beta-binomial model.
#' @param type an (optional) character string giving the type of residuals which should be returned. The available options are: (1) the difference between the observed response and the fitted mean ("response"); (2) the standardized difference between the observed response and the fitted mean ("pearson"); (3) the randomized quantile residuals ("quantile"). By default, \code{type} is set to be "quantile".
#' @param plot.it an (optional) logical switch indicating if a plot of the residuals is required. By default, \code{plot} is set to be TRUE.
#' @param identify an (optional) integer value indicating the number of individuals to identify on the plot of residuals. This is only appropriate when \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods.
#' @return A vector with the observed residuals type \code{type}.
#' @examples
#' ## Example 1
#' fit1 <- overglm(cbind(fetuses,litter-fetuses) ~ pht + tcpo, family="bb(logit)", data=ossification)
#' residuals(fit1, type="quantile", col="red", pch=20, col.lab="blue",
#'           col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ## Example 2
#' fit2 <- overglm(infections ~ location + frequency, family="nb1(log)", data=swimmers)
#' residuals(fit2, type="quantile", col="red", pch=20, col.lab="blue",
#'           col.axis="blue", col.main="black", family="mono", cex=0.8)
#' @method residuals overglm
#' @export
#' @references Dunn P.K. and Smyth G.K. (1996) Randomized Quantile Residuals. \emph{Journal of Computational and Graphical Statistics}, 5, 236-244.
#'
residuals.overglm <- function(object, ...,type=c("quantile","standardized","response"), plot.it=TRUE, identify){
  type <- match.arg(type)
  mu <- object$fitted.values
  phi <- exp(object$coefficients[length(object$coefficients)])
  y <- object$y
  n <- length(mu)
  if(object$family$family=="bb"){
    m <- as.matrix(y[,1]+y[,2])
    y <- as.matrix(y[,1])
    res <- y/m - mu
    if(type=="quantile"){
      alpha <- mu/phi
      lambda <- (1-mu)/phi
      temp2 <- matrix(NA,length(alpha),1)
      temp1 <- choose(m,y)*beta(alpha+y,m-y+lambda)
      const <- beta(alpha,lambda)
      for(i in 1:length(alpha)){
        if(y[i]==0) temp2[i] <- 0
        if(y[i]==1) temp2[i] <- beta(alpha[i],m[i]+lambda[i])
        if(y[i]>=2){
          ys <- 1:(y[i]-1)
          temp2[i] <- beta(alpha[i],m[i]+lambda[i]) + sum(exp(cumsum(log(alpha[i]+ys-1)-log(ys) +
                                                                       log(m[i]+1-ys)-log(m[i]+lambda[i]-ys))-lgamma(m[i]+alpha[i]+lambda[i]) +
                                                                lgamma(m[i]+lambda[i])+lgamma(alpha[i])))
        }
      }
      res <- (temp2/const) + (temp1/const)*runif(n)
      res <- qnorm(ifelse(ifelse(res<1e-16,1e-16,res)>1-(1e-16),1-(1e-16),res))
    }
    if(type=="standardized") res <- (y - m*mu)/sqrt(m*mu*(1-mu)*(1 + (m-1)*phi/(phi+1)))
  }
  if(object$family$family=="nb3"){
    res <- y - mu
    if(type=="quantile"){
      res <- pnbinom(y-1,mu=mu,size=mu/phi) + dnbinom(y,mu=mu,size=1/(phi*mu))*runif(n)
      res <- qnorm(ifelse(ifelse(res<1e-16,1e-16,res)>1-(1e-16),1-(1e-16),res))
    }
    if(type=="standardized") res <- (y-mu)/sqrt((phi+1)*mu)
  }
  if(object$family$family=="nb2"){
    res <- y - mu
    if(type=="quantile"){
      res <- pnbinom(y-1,mu=mu,size=mu/phi) + dnbinom(y,mu=mu,size=mu/phi)*runif(n)
      res <- qnorm(ifelse(ifelse(res<1e-16,1e-16,res)>1-(1e-16),1-(1e-16),res))
    }
    if(type=="standardized") res <- (y-mu)/sqrt((phi+1)*mu)
  }
  if(object$family$family=="nb1"){
    res <- y - mu
    if(type=="quantile"){
      res <- pnbinom(y-1,mu=mu,size=1/phi) + dnbinom(y,mu=mu,size=1/phi)*runif(n)
      res <- qnorm(ifelse(ifelse(res<1e-16,1e-16,res)>1-(1e-16),1-(1e-16),res))
    }
    if(type=="standardized") res <- (y-mu)/sqrt(mu + phi*mu^2)
  }
  if(plot.it){
    nano <- list(...)
    nano$x <- object$fitted.values
    nano$y <- res
    if(is.null(nano$ylim)) nano$ylim <- c(min(-3.5,min(res)),max(+3.5,max(res)))
    if(is.null(nano$xlab)) nano$xlab <- "Fitted values"
    if(is.null(nano$ylab)) nano$ylab <- paste(type," - type residuals",sep="")
    if(is.null(nano$pch))  nano$pch  <- 20
    if(is.null(nano$labels))  labels <- 1:length(res)
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    do.call("plot",nano)
    abline(h=-3,lty=3)
    abline(h=+3,lty=3)
    if(!missingArg(identify)) identify(object$fitted.values,res,n=max(1,floor(abs(identify))),labels=labels)
  }
  res <- as.matrix(res)
  colnames(res) <- "Residuals"
  return(invisible(res))
}

#' @method AIC overglm
#' @export
AIC.overglm <- function(object,...,k=2,verbose=TRUE){
  x <- list(object,...)
  if(!all(unlist(lapply(x,function(a) return(class(a)[1]=="overglm" | class(a)[1]=="glm")))))
    stop("Only glm- and overglm-type objects are supported!!",call.=FALSE)
  results <- matrix(NA,length(x),3)
  results2 <- matrix(NA,length(x),4)
  rows <-  matrix(NA,length(x),1)
  call. <- match.call()
  for(i in 1:length(x)){
    results[i,1] <- -2*sum(logLik(x[[i]]))
    results[i,2] <- attr(logLik(x[[i]]),"df")
    results[i,3] <- -2*sum(logLik(x[[i]])) + k*attr(logLik(x[[i]]),"df")
    results2[i,1] <- as.character(call.[i+1])
    results2[i,2] <- switch(x[[i]]$family$family,"nb1"="Negative Binomial I",
                            "nb2"="Negative Binomial II",
                            "nb3"="Negative Binomial III",
                            "bb"="Beta-Binomial",
                            "poisson"="Poisson",
                            "binomial"="Binomial",
                            "Gamma"="Gamma",
                            "gaussian"="Gaussian",
                            "inverse.gaussian"="Inverse Gaussian")
    results2[i,3] <- x[[i]]$family$link
    results2[i,4] <- paste(c(ifelse(is.null(attr(x[[i]]$terms,"offset")),attr(x[[i]]$terms,"intercept"),
                                    paste(c(attr(x[[i]]$terms,"intercept"),as.character(attr(x[[i]]$terms,"variables"))[[attr(x[[i]]$terms,"offset")+1]]),collapse=" + ")),
                             attr(x[[i]]$terms,"term.labels")),collapse=" + ")
  }
  if(nrow(results) > 1){
    if(verbose){
      cat("\n")
      if(all(results2[,2]==results2[1,2]))
        cat("\n  Family: ",results2[1,2],"\n")
      if(all(results2[,3]==results2[1,3]))
        cat("     Link: ",results2[1,3],"\n")
      if(all(results2[,4]==results2[1,4]))
        cat("Predictor: ",results2[1,4],"\n")
      cat("\n")
      ids <- c(TRUE,!all(results2[,2]==results2[1,2]),!all(results2[,3]==results2[1,3]),!all(results2[,4]==results2[1,4]))
      temp <- as.matrix(results2[,ids])
      out_ <- data.frame(temp,results)
      colnames(out_) <- c(c("Object","Family","Link","Predictor")[ids],"-2*log-likelihood","df","AIC ")
      print(out_,row.names=FALSE)
      return(invisible(out_))
    }
  }else return(invisible(round(results[,3],digits=3)))
}

#' @method BIC overglm
#' @export
BIC.overglm <- function(object,...,verbose=TRUE){
  x <- list(object,...)
  if(!all(unlist(lapply(x,function(a) return(class(a)[1]=="overglm" | class(a)[1]=="glm")))))
    stop("Only glm- and overglm-type objects are supported!!",call.=FALSE)
  results <- matrix(NA,length(x),3)
  results2 <- matrix(NA,length(x),4)
  rows <-  matrix(NA,length(x),1)
  call. <- match.call()
  for(i in 1:length(x)){
    results[i,1] <- -2*sum(logLik(x[[i]]))
    results[i,2] <- attr(logLik(x[[i]]),"df")
    results[i,3] <- -2*sum(logLik(x[[i]])) + log(nrow(model.matrix(x[[i]])))*attr(logLik(x[[i]]),"df")
    results2[i,1] <- as.character(call.[i+1])
    results2[i,2] <- switch(x[[i]]$family$family,"nb1"="Negative Binomial I",
                            "nb2"="Negative Binomial II",
                            "nb3"="Negative Binomial III",
                            "bb"="Beta-Binomial",
                            "poisson"="Poisson",
                            "binomial"="Binomial",
                            "Gamma"="Gamma",
                            "gaussian"="Gaussian",
                            "inverse.gaussian"="Inverse Gaussian")
    results2[i,3] <- x[[i]]$family$link
    results2[i,4] <- paste(c(ifelse(is.null(attr(x[[i]]$terms,"offset")),attr(x[[i]]$terms,"intercept"),
                                    paste(c(attr(x[[i]]$terms,"intercept"),as.character(attr(x[[i]]$terms,"variables"))[[attr(x[[i]]$terms,"offset")+1]]),collapse=" + ")),
                             attr(x[[i]]$terms,"term.labels")),collapse=" + ")
  }
  if(nrow(results) > 1){
    if(verbose){
      cat("\n")
      if(all(results2[,2]==results2[1,2]))
        cat("\n  Family: ",results2[1,2],"\n")
      if(all(results2[,3]==results2[1,3]))
        cat("     Link: ",results2[1,3],"\n")
      if(all(results2[,4]==results2[1,4]))
        cat("Predictor: ",results2[1,4],"\n")
      cat("\n")
      ids <- c(TRUE,!all(results2[,2]==results2[1,2]),!all(results2[,3]==results2[1,3]),!all(results2[,4]==results2[1,4]))
      temp <- as.matrix(results2[,ids])
      out_ <- data.frame(temp,results)
      colnames(out_) <- c(c("Object","Family","Link","Predictor")[ids],"-2*log-likelihood","df","BIC ")
      print(out_,row.names=FALSE)
      return(invisible(out_))
    }
  }else return(invisible(round(results[,3],digits=3)))
}

#' @title Normal QQ-plot with simulated envelope of model residuals
#' @description Produces a normal QQ-plot with simulated envelope of residuals obtained from the fit of a negative binomial or beta-binomial regression model.
#' @param object an object of the class overglm which is obtained from the fit of a negative binomial or beta-binomial model.
#' @param rep an (optional) positive integer indicating the number of replicates which should be used to build the simulated envelope. By default, \code{rep} is set to be 100.
#' @param conf an (optional) value in the interval (0,1) indicating the confidence level which should be used to build the pointwise confidence intervals, which form the envelope. By default, \code{conf} is set to be 0.95.
#' @param type a character string indicating the type of residuals which should be used. The available options are: (1) the difference between the observed response
#' and the fitted mean ("response"); (2) the standardized difference between the
#' observed response and the fitted mean ("standardized"); (3) the randomized quantile
#' residuals ("quantile"). By default, \code{type} is set to be "quantile".
#' @param plot.it an (optional) logical switch indicating if the normal QQ-plot with simulated envelope of residuals is required or just the data matrix in which it is based. By default, \code{plot.it} is set to be TRUE.
#' @param identify an (optional) positive integer value indicating the number of individuals to identify on the QQ-plot with simulated envelope of residuals. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix with \eqn{n} rows and four columns: the first three (Lower limit, Median, and Upper limit) describe the simulated envelope, that is, each row corresponds to the quantiles (1-\code{conf})/2, 0.5 and (1+\code{conf})/2 of the random sample of size \code{rep} of the \eqn{i}-th order statistic of the residuals type \code{type} for \eqn{i=1,2,...,n}; and the last one column (Residuals) contains the observed type \code{type} residuals.
#' @details The simulated envelope is builded by simulating rep independent realizations of the response variable for each individual, which is accomplished taking into account the following: (1) the model assumption about the distribution of the response variable; (2) the estimates of the parameters in the linear predictor; and (3) the estimate of the dispersion parameter. The interest model is re-fitted \code{rep} times, as each time the vector of observed responses is replaced by one of the simulated samples. The residuals type \code{type} are computed and then ordered for each replicate, so that for each \eqn{i=1,2,...,n}, where n is the number of individuals in the sample, there is a random sample of size \code{rep} of the \eqn{i}-th order statistic of the residuals type \code{type}. Therefore, the simulated envelope is composed of the quantiles (1-\code{conf})/2 and (1+\code{conf})/2 of the random sample of size \code{rep} of the \eqn{i}-th order statistic of the residuals type \code{type} for \eqn{i=1,2,...,n}.
#' @references Atkinson A.C. (1985) \emph{Plots, Transformations and Regression}. Oxford University Press, Oxford.
#' @references Dunn P.K. and Smyth G.K. (1996) Randomized Quantile Residuals. \emph{Journal of Computational and Graphical Statistics} 5, 236-244.
#' @seealso \link{envelope.lm}, \link{envelope.glm}
#' @method envelope overglm
#' @export
#' @examples
#' ## Example 1
#' fit1 <- overglm(infections ~ frequency + location, family="nb1(log)", data=swimmers)
#' envelope(fit1, rep=100, conf=0.95, type="quantile", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ## Example 2
#' fit2 <- overglm(cbind(fetuses,litter-fetuses) ~ tcpo + pht, family="bb(logit)", data=ossification)
#' envelope(fit2, rep=100, conf=0.95, type="quantile", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
envelope.overglm <- function(object, rep=100, conf=0.95, type=c("quantile","response","standardized"), plot.it=TRUE, identify, ...){
  type <- match.arg(type)
  p <- length(coef(object))
  X <- model.matrix(object)
  mu <- fitted(object)
  n <- length(mu)
  rep <- max(1,floor(abs(rep)))
  e <- matrix(0,n,rep)
  bar <- txtProgressBar(min=0, max=rep, initial=0, width=min(50,rep), char="+", style=3)
  i <- 1
  while(i <= rep){
    if(object$family$family=="bb"){
      phi <- exp(coef(object)[p])
      size <- apply(object$y,1,sum)
      prob <- rbeta(n,shape1=mu*((phi+1)/phi-1),shape2=(1-mu)*((phi+1)/phi-1))
      resp <- rbinom(n,size=size,prob=prob)
      resp <- cbind(resp,size-resp)
    }else{
      lambda <- switch(object$family$family,
                       nb1 = rgamma(n,scale=mu*exp(coef(object)[p]),shape=1/exp(coef(object)[p])),
                       nb2 = rgamma(n,scale=exp(coef(object)[p]),shape=mu/exp(coef(object)[p])),
                       nb3 = rgamma(n,scale=mu^2*exp(coef(object)[p]),shape=1/(mu*exp(coef(object)[p]))))
      resp <- rpois(n,lambda=lambda)
    }
    fits <-  try(overglm(resp ~ 0 + X + offset(object$offset),weights=object$weights,family=object$family$family,start=c(coef(object)[-p]),si.mu.la.ti.on=TRUE),silent=TRUE)
    rs <- try(residuals(fits,type=type,plot.it=FALSE),silent=TRUE)
    if(is.list(fits)){
      if(fits$converged==TRUE){
        e[,i] <- sort(rs)
        setTxtProgressBar(bar,i)
        i <- i + 1
      }
    }
  }
  close(bar)
  alpha <- 1 - max(min(abs(conf),1),0)
  e <- as.matrix(e[,1:(i-1)])
  es <- apply(e,1,function(x) return(quantile(x,probs=c(alpha/2,0.5,1-alpha/2))))
  rd <- residuals(object,type=type,plot.it=FALSE)
  if(plot.it){
    rango <- 1.1*range(rd)
    oldpar <- par(no.readonly=TRUE)
    on.exit(par(oldpar))
    par(pty="s")
    qqnorm(es[2,],axes=FALSE,xlab="",ylab="",main="", type="l",ylim=rango,lty=3)
    par(new=TRUE)
    qqnorm(es[1,],axes=FALSE,xlab="",ylab="",main="", type="l",ylim=rango,lty=1)
    par(new=TRUE)
    qqnorm(es[3,],axes=FALSE,xlab="",ylab="", main="", type="l",ylim=rango,lty=1)
    par(new=TRUE)
    nano <- list(...)
    nano$y <- rd
    nano$type <- "p"
    nano$ylim <- rango
    if(is.null(nano$labels)) labels <- 1:nrow(X)
    else labels <- nano$labels
    if(is.null(nano$pch)) nano$pch <- 20
    if(is.null(nano$col)) nano$col <- "black"
    if(is.null(nano$xlab)) nano$xlab <- "Expected quantiles"
    if(is.null(nano$ylab)) nano$ylab <- "Observed quantiles"
    if(is.null(nano$main)) nano$main <- paste0("Normal QQ plot with simulated envelope\n of ",type,"-type residuals")
    if(is.null(nano$labels))  labels <- 1:length(rd)
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    outm <- do.call("qqnorm",nano)
    if(!missingArg(identify)){
      identify(outm$x,outm$y,labels=labels,n=max(1,floor(abs(identify))),labels=labels)
    }
  }
  out_ <- cbind(t(es),rd)
  colnames(out_) <- c("Lower limit","Median","Upper limit","Residuals")
  return(invisible(out_))
}


#' @title Variable selection in Negative Binomial and Beta-Binomial Regression Models
#' @description Performs variable selection in negative binomial and beta-binomial regression models using hybrid versions of forward stepwise and backward stepwise.
#' @param model an object of the class overglm which is obtained from the fit of a negative binomial or beta-binomial regression model.
#' @param direction an (optional) character string indicating the type of procedure which should be used. The available options are: hybrid backward stepwise ("backward") and hybrid forward stepwise ("forward"). By default, \code{direction} is set to be "forward".
#' @param levels an (optional) two-dimensional vector of values in the interval \eqn{(0,1)} indicating the levels at which the variables should in and out from the model. This is only appropiate if \code{criterion}="p-value". By default, \code{levels} is set to be \code{c(0.05,0.05)}.
#' @param test an (optional) character string indicating the statistical test which should be used to compare nested models. The available options are: Wald ("wald"), Rao's score ("score"), likelihood-ratio ("lr") and gradient ("gradient") tests. By default, \code{test} is set to be "wald".
#' @param criterion an (optional) character string indicating the criterion which should be used to compare the candidate models. The available options are: AIC ("aic"), BIC ("bic"), and \emph{p}-value of the \code{test} test ("p-value"). By default, \code{criterion} is set to be "bic".
#' @param ...	further arguments passed to or from other methods. For example, \code{k}, that is, the magnitude of the penalty in the AIC, which by default is set to be 2.
#' @param trace an (optional) logical switch indicating if should the stepwise reports be printed. By default, \code{trace} is set to be TRUE.
#' @param scope an (optional) list containing components \code{lower} and \code{upper}, both formula-type objects, indicating the range of models which should be examined in the stepwise search. By default, \code{lower} is a model with no predictors and \code{upper} is the linear predictor of the model in \code{model}.
#' @return A list which contains the following objects:
#' \itemize{
#' \item{\code{initial}:}{ a character string indicating the linear predictor of the "initial model".}
#' \item{\code{direction}:}{ a character string indicating the type of procedure which was used.}
#' \item{\code{criterion}:}{ a character string indicating the criterion used to compare the candidate models.}
#' \item{\code{final}:}{ a character string indicating the linear predictor of the "final model".}
#' }
#' @seealso \link{stepCriterion.lm}, \link{stepCriterion.glm}, \link{stepCriterion.glmgee}
#' @examples
#' fit <- overglm(infections ~ frequency + location + age + gender, family="nb1(log)", data=swimmers)
#' stepCriterion(fit,direction="forward",criterion="p-value",test="lr")
#' stepCriterion(fit,direction="backward",criterion="p-value",test="lr")
#' stepCriterion(fit,direction="forward",criterion="bic")
#' stepCriterion(fit,direction="backward",criterion="bic")
#' @method stepCriterion overglm
#' @export
#' @references James G., Witten D., Hastie T. and Tibshirani R. (2013, page 210) An Introduction to Statistical Learning with Applications in R, Springer, New York.
stepCriterion.overglm <- function(model, criterion=c("bic","aic","p-value"), test=c("wald","score","lr","gradient"), direction=c("forward","backward"), levels=c(0.05,0.05), trace=TRUE, scope, ...){
  xxx <- list(...)
  if(is.null(xxx$k)) k <- 2 else k <- xxx$k
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
    lower <- formula(paste(deparse(lower[[2]]),"~",attr(terms(lower),"intercept")))
  }else{
    lower <- scope$lower
    upper <- scope$upper
  }
  U <- attr(terms(upper),"term.labels")
  fs <- attr(terms(upper),"factors")
  long <- max(nchar(U)) + 2
  nonename <- paste("<none>",paste(replicate(max(long-6,0)," "),collapse=""),collapse="")
  cambio <- ""
  paso <- 1
  tol <- TRUE
  if(trace){
    cat("\n       Family: ",model$family$family,"\n")
    cat("Link function: ",model$family$link,"\n")
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
      S <- attr(terms(oldformula),"term.labels")
      entran <- seq(1:length(U))[is.na(match(U,S))]
      salen <- seq(1:length(U))[!is.na(match(U,S))]
      mas <- TRUE

      fsalen <- matrix(NA,length(salen),5)
      if(length(salen) > 0){
        nombres <- matrix("",length(salen),1)
        for(i in 1:length(salen)){
          salida <- apply(as.matrix(fs[,salen[i]]*fs[,-c(entran,salen[i])]),2,sum)
          if(all(salida < sum(fs[,salen[i]])) & U[salen[i]]!=cambio){
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
          if(all(salida) & U[entran[i]]!=cambio){
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
      S <- attr(terms(oldformula),"term.labels")
      entran <- seq(1:length(U))[is.na(match(U,S))]
      salen <- seq(1:length(U))[!is.na(match(U,S))]
      menos <- TRUE

      fentran <- matrix(NA,length(entran),5)
      if(length(entran) > 0){
        nombres <- matrix("",length(entran),1)
        for(i in 1:length(entran)){
          salida <- apply(as.matrix(fs[,-c(salen,entran[i])]),2,function(x) sum(fs[,entran[i]]*x)!=sum(x))
          if(all(salida) & U[entran[i]]!=cambio){
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
          if(all(salida < sum(fs[,salen[i]]))){
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
    if(!is.null(xxx$k)) cat("The magnitude of the penalty in the AIC was set to be ",xxx$k)
    cat("\n")
  }
  out_$final <- paste("~",as.character(oldformula)[length(oldformula)],sep=" ")
  return(invisible(out_))
}
