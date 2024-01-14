#'
#' @title Fisher Scoring algorithm in Generalized Linear Models
#' @description This function displays the entire path performed by the
#' Fisher Scoring algorithm for parameter estimation in Generalized Linear
#' Models. It starts with the starting value until convergence is achieved
#' or the maximum number of iterations is exceeded.
#' @param object one object of the class \emph{glm}.
#' @param verbose an (optional) logical indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param digits an (optional) integer value indicating the number of decimal places to be used. As default, \code{digits} is set to \code{max(3, getOption("digits") - 2)}.
#' @return a matrix whose first three columns are the following
#' \tabular{ll}{
#' \code{Iteration} \tab the iteration number,\cr
#' \tab \cr
#' \code{Deviance} \tab value of the (unscaled) deviance computed using the current value of the parameter vector,\cr
#' \tab \cr
#' \code{Tolerance} \tab value of \eqn{|deviance-deviance_{old}|/(deviance_{old} + 0.1)},\cr
#' }
#' @examples
#' ###### Example 1: Fuel efficiency of cars
#' Auto <- ISLR::Auto
#' fit1 <- glm(mpg ~ horsepower + weight + horsepower*weight, family=Gamma(inverse), data=Auto,
#'             control=list(trace=TRUE))
#' FisherScoring(fit1)
#'
#' ###### Example 2: Hill races in Scotland
#' data(races)
#' fit2 <- glm(rtime ~ log(distance) + cclimb, family=Gamma(log), data=races,
#'             control=list(trace=TRUE))
#' FisherScoring(fit2)
#'
#' ###### Example 3:
#' burn1000 <- aplore3::burn1000
#' burn1000 <- within(burn1000, death <- factor(death, levels=c("Dead","Alive")))
#' fit3 <- glm(death ~ age*inh_inj + tbsa*inh_inj, family=binomial("logit"), data=burn1000,
#'             control=list(trace=TRUE))
#' FisherScoring(fit3)
#'
#' ###### Example 4: Skin cancer in women
#' data(skincancer)
#' fit4 <- glm(cases ~ offset(log(population)) + city + age, family=poisson, data=skincancer,
#'             control=list(trace=TRUE))
#' FisherScoring(fit4)
#'
#' ###### Example 5: Agents to stimulate cellular differentiation
#' data(cellular)
#' fit5 <- glm(cbind(cells,200-cells) ~ tnf + ifn, family=binomial(logit), data=cellular,
#'             control=list(trace=TRUE))
#' FisherScoring(fit5)
#'
#' ###### Example 6: Advertising
#' data(advertising)
#' fit6 <- glm(sales ~ log(TV) + radio + log(TV)*radio, family=gaussian(log), data=advertising,
#'             control=list(trace=TRUE))
#' FisherScoring(fit6)
#'
#' @export FisherScoring
FisherScoring <- function(object,verbose=TRUE,digits=max(3, getOption("digits") - 2)){
  options(warn=-1)
  X <- model.matrix(object)
  y <- object$y
  mustart <- object$call$mustart
  etastart <- object$call$etastart
  start <- object$call$start
  nobs <- nrow(X)
  omega <- weights <- object$prior.weights
  if(is.null(object$offset)) offset <- matrix(0,nrow(X),1) else offset <- object$offset
  if(is.null(mustart)){
    if(is.null(etastart)){
      if(is.null(start)){
        family <- object$family
        eval(object$family$initialize)
        mu <- mustart
        eta <- object$family$linkfun(mu)
      }
      else{betanew <- eval(object$call$start)
      eta <- offset + X%*%betanew
      mu <- object$family$linkinv(eta)}
    }else{eta <- eval(object$call$etastart)
    mu <- object$family$linkinv(eta)
    }
  }else{mu <- eval(object$call$mustart)
  eta <- object$family$linkfun(mu)
  }
  epsilon <- object$control$epsilon
  tol <- epsilon + 1
  atras <- matrix(1e-15,1,3+ncol(X))
  i <- 1
  while(tol > epsilon & i <= object$control$maxit){
    if(i > 1){
      betaold <- betanew
      devold <- devnew
      eta <- offset + X%*%betaold
      mu <- object$family$linkinv(eta)
    }
    G <- object$family$mu.eta(eta)
    V <- object$family$variance(mu)
    W <- omega*G^2/V
    X2 <- X*matrix(W,nrow(X),ncol(X))
    z <- eta - offset + (y-mu)/G
    betanew <- chol2inv(chol(t(X)%*%X2))%*%t(X2)%*%z
    eta <- offset + X%*%betanew
    mu <- object$family$linkinv(eta)
    devnew <- sum(object$family$dev.resids(y,mu,omega))
    if(i==1) tol <- epsilon + 1
    else tol <- abs(devnew - devold)/(abs(devold) + 0.1)
    atras <- rbind(atras,c(i,devnew,tol,t(betanew)))
    i <- i + 1
  }
  atras <- atras[-1,]
  colnames(atras) <- c("Iteration","Deviance","Tolerance",colnames(X))
  rownames(atras) <- rep("",nrow(atras))
  cat("Tolerance Limit: ",epsilon,"\n")
  cat("Maximum number of iterations: ",object$control$maxit,"\n\n")
  atras[1,3] <- NA
  atras[,-c(1:3)] <- round(atras[,-c(1:3)],digits=digits)
  if(object$family$family=="gaussian" & object$family$link=="identity") atras[,1] <- atras[,1] - 1
  if(verbose) print(atras,digits=digits)
  if(tol > epsilon) cat("\nConvergence was not achieved!!!\n")
  options(warn=0)
  return(invisible(atras))
}

#' @title Adjusted R-squared
#' @description Computes the adjusted R-squared
#' @param ... one of several model fit objects.
#' @param verbose an (optional) logical indicating if should the report of results be printed.
#' @param digits an (optional) integer value indicating the number of decimal places to be used.
#' @return A matrix with the values of the adjusted R-squared for all model fit objects.
#' @export adjR2
adjR2 <- function(...,digits,verbose){
  UseMethod("adjR2")
}
#'
#' @title Adjusted R-squared in Generalized Linear Models
#' @description Computes the adjusted deviance-based R-squared in generalized linear models.
#' @param ... one or several objects of the class \emph{glm}, which are obtained from the fit of generalized linear models.
#' @param verbose an (optional) logical indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param digits an (optional) integer value indicating the number of decimal places to be used. As default, \code{digits} is set to \code{max(3, getOption("digits") - 2)}.
#' @details The deviance-based R-squared is computed as \eqn{R^2=1 - Deviance/Null.Deviance}. Then,
#' the adjusted deviance-based R-squared is computed as
#' \eqn{1 - \frac{n-1}{n-p}(1-R^2)}, where \eqn{p} is the
#' number of parameters in the linear predictor and \eqn{n} is the sample size.
#' @return a matrix with the following columns
#' \tabular{ll}{
#' \code{Deviance} \tab value of the residual deviance,\cr
#' \tab \cr
#' \code{R-squared} \tab value of the deviance-based R-squared,\cr
#' \tab \cr
#' \code{df}       \tab number of parameters in the linear predictor,\cr
#' \tab \cr
#' \code{adj.R-squared} \tab value of the adjusted deviance-based R-squared,\cr
#' }
#' @method adjR2 glm
#' @export
#' @examples
#' ###### Example 1: Fuel efficiency of cars
#' Auto <- ISLR::Auto
#' fit1 <- glm(mpg ~ horsepower*weight, family=Gamma(inverse), data=Auto)
#' fit2 <- update(fit1, formula=mpg ~ horsepower*weight*cylinders)
#' fit3 <- update(fit1, family=Gamma(log))
#' fit4 <- update(fit2, family=Gamma(log))
#' fit5 <- update(fit1, family=inverse.gaussian(log))
#' fit6 <- update(fit2, family=inverse.gaussian(log))
#'
#' AIC(fit1,fit2,fit3,fit4,fit5,fit6)
#' BIC(fit1,fit2,fit3,fit4,fit5,fit6)
#' adjR2(fit1,fit2,fit3,fit4,fit5,fit6)
#'
adjR2.glm <- function(...,digits=max(3, getOption("digits") - 2),verbose=TRUE){
  x <- list(...)
  if(any(unlist(lapply(x,function(xx) !is(xx,"glm")))))
    stop("Only glm-type objects are supported!!",call.=FALSE)
  if(any(unlist(lapply(x,function(x) names(coef(x))[1]!="(Intercept)"))))
    stop("zero-intercept models are not supported!!",call.=FALSE)
  out_ <- matrix(NA,length(x),4)
  call. <- match.call()
  for(i in 1:length(x)){
    out_[i,1] <- x[[i]]$deviance
    out_[i,3] <- length(x[[i]]$coefficients)
    out_[i,2] <- round(1 - x[[i]]$deviance/x[[i]]$null.deviance,digits=digits)
    out_[i,4] <- round(1 - (x[[i]]$deviance/x[[i]]$df.residual)/(x[[i]]$null.deviance/x[[i]]$df.null),digits=digits)
  }
  rownames(out_) <- as.character(call.[2:(length(x) + 1)])
  colnames(out_) <- c("Deviance","R-squared","df","adj.R-squared")
  if(length(x)==1){
    out_ <- as.numeric(out_[1,4])
    return(out_)
  }
  if(verbose) print(out_)
  return(invisible(out_))
}
#'
#' @title Adjusted R-squared in Normal Linear Models
#' @description Extracts the adjusted R-squared in normal linear models.
#' @param ... one or several objects of the class \emph{lm}, which are obtained from the fit of normal linear models.
#' @param verbose an (optional) logical indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param digits an (optional) integer value indicating the number of decimal places to be used. As default, \code{digits} is set to \code{max(3, getOption("digits") - 2)}.
#' @details The R-squared is computed as \eqn{R^2=1 - RSS/Null.RSS}. Then,
#' the adjusted R-squared is computed as
#' \eqn{1 - \frac{n-1}{n-p}(1-R^2)}, where \eqn{p} is the
#' number of parameters in the linear predictor and \eqn{n} is the sample size.
#' @return a matrix with the following columns
#' \tabular{ll}{
#' \code{RSS} \tab value of the residual sum of squares,\cr
#' \tab \cr
#' \code{R-squared} \tab value of the R-squared,\cr
#' \tab \cr
#' \code{df}       \tab number of parameters in the linear predictor,\cr
#' \tab \cr
#' \code{adj.R-squared} \tab value of the adjusted R-squared,\cr
#' }
#' @method adjR2 lm
#' @export
#' @examples
#' ###### Example 1: Fuel efficiency of cars
#' fit1 <- lm(mpg ~ log(hp) + log(wt) + qsec, data=mtcars)
#' fit2 <- lm(mpg ~ log(hp) + log(wt) + qsec + log(hp)*log(wt), data=mtcars)
#' fit3 <- lm(mpg ~ log(hp)*log(wt)*qsec, data=mtcars)
#'
#' AIC(fit1,fit2,fit3)
#' BIC(fit1,fit2,fit3)
#' adjR2(fit1,fit2,fit3)
#'
adjR2.lm <- function(...,digits=max(3, getOption("digits") - 2),verbose=TRUE){
  x <- list(...)
  if(any(unlist(lapply(x,function(xx) !is(xx,"lm")))))
    stop("Only lm-type objects are supported!!",call.=FALSE)
  if(any(unlist(lapply(x,function(x) names(coef(x))[1]!="(Intercept)"))))
    stop("zero-intercept models are not supported!!",call.=FALSE)
  out_ <- matrix(NA,length(x),4)
  call. <- match.call()
  for(i in 1:length(x)){
    temporal <- summary(x[[i]])
    out_[i,1] <- temporal$sigma^2*x[[i]]$df.residual
    out_[i,3] <- length(x[[i]]$coefficients)
    out_[i,2] <- round(temporal$r.squared,digits=digits)
    out_[i,4] <- round(temporal$adj.r.squared,digits=digits)
  }
  rownames(out_) <- as.character(call.[2:(length(x) + 1)])
  colnames(out_) <- c("RSS","R-squared","df","adj.R-squared")
  if(length(x)==1){
    out_ <- as.numeric(out_[1,4])
    return(out_)
  }
  if(verbose) print(out_)
  return(invisible(out_))
}





#' @title Local Influence
#' @description Computes measures of local influence for a fitted model object.
#' @param object a fitted model object.
#' @param ...	further arguments passed to or from other methods.
#' @return An object with the measures of local influence.
#' @export localInfluence
localInfluence <- function(object,...) {
  UseMethod("localInfluence")
}
#' @title Leverage
#' @description Computes leverage measures for a fitted model object.
#' @param object a fitted model object.
#' @param ...	further arguments passed to or from other methods.
#' @return An object with the values of the leverage measures.
#' @export leverage
leverage <- function(object,...) {
  UseMethod("leverage")
}

#' @title Generalized Variance Inflation Factor
#' @description Computes the generalized variance inflation factor (GVIF) for a fitted model object.
#' @param model a fitted model object.
#' @param ...	further arguments passed to or from other methods.
#' @return An object with the values of the GVIF for all effects in the model.
#' @export gvif
gvif <- function(model,...) {
  UseMethod("gvif")
}


#' @title Function to extract estimating equations
#' @description Extracts estimating equations evaluated at the parameter estimates and the observed data for a fitted model object.
#' @param object a fitted model object.
#' @param ...	further arguments passed to or from other methods.
#' @return A vector with the value of the estimating equations evaluated at the parameter estimates and the observed data.
#' @export estequa
estequa <- function(object,...) {
  UseMethod("estequa")
}

#' @title Variable selection in regression models from a chosen criterion
#' @description Generic function for selecting variables from a fitted regression model using a chosen criterion.
#' @param model a fitted model object.
#' @param ...	further arguments passed to or from other methods.
#' @return A list which includes the descriptions of the linear predictors of the initial and final models as well as the criterion used to compare the candidate models.
#' @export stepCriterion
stepCriterion <- function(model,...){
  UseMethod("stepCriterion")
}

#' @title Normal QQ-plot with simulated envelope of model residuals
#' @description Generic function for building a normal QQ-plot with simulated envelope of residuals obtained from a fitted model.
#' @param object a fitted model object.
#' @param ...	further arguments passed to or from other methods.
#' @return A matrix with the simulated envelope and, optionally, a plot of it.
#' @export envelope
envelope <- function(object,...) {
  UseMethod("envelope")
}

#' @title Test for Varying Dispersion Parameter
#' @description Generic function for testing for varying dispersion parameter from a fitted model.
#' @param model a fitted model object.
#' @param ...	further arguments passed to or from other methods.
#' @return A list which includes the main attributes of the test as, for example, value of the statistic and \emph{p}-value.
#' @export vdtest
vdtest <- function(model,...) {
  UseMethod("vdtest")
}
#'
#' @title Test for Varying Dispersion Parameter in Normal Linear Models
#' @description Performs Rao's score test for varying dispersion parameter in weighted and unweighted normal linear models.
#' @param model an object of the class \emph{lm}.
#' @param varformula an (optional) \code{formula} expression of the form \code{~ z1 + z2 + ... + zq} indicating the potential explanatory variables for the dispersion parameter. As default, the same explanatory variables are taken as in the model for the mean.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param ...	further arguments passed to or from other methods.
#' @details From the heteroskedastic normal linear model in which
#' \eqn{\log(\sigma^2)=\gamma_0 + \gamma_1z_1 + \gamma_2z_2 + ...+ \gamma_qz_q}, where
#' \eqn{\sigma^2} is the dispersion parameter of the distribution of the
#' random errors, the Rao's score test (denoted here as \eqn{S}) to assess the
#' hypothesis \eqn{H_0: \gamma=0} versus \eqn{H_1: \gamma\neq 0} is computed,
#' where \eqn{\gamma=(\gamma_1,\ldots,\gamma_q)}. The corresponding \emph{p}-value is
#' computed from the chi-squared distribution with \eqn{q} degrees of freedom,
#' that is, \emph{p}-value = Prob\eqn{[\chi^2_{q} > S]}. If the object
#' \code{model} corresponds to an unweighted normal linear model, then the
#' test assess the assumption of constant variance, which coincides with the
#' non-studentized Breusch-Pagan test against heteroskedasticity.
#' @return a list list with components including
#' \tabular{ll}{
#' \code{statistic} \tab value of the Rao's score test (\eqn{S}),\cr
#' \tab \cr
#' \code{df}        \tab number of degrees of freedom (\eqn{q}),\cr
#' \tab \cr
#' \code{p.value}   \tab \emph{p}-value of the test,\cr
#' \tab \cr
#' \code{vars}   \tab names of explanatory variables for the dispersion parameter,\cr
#' }
#' @method vdtest lm
#' @export
#' @examples
#' ###### Example 1: Fuel consumption of automobiles
#' fit1 <- lm(mpg ~ log(hp) + log(wt), data=mtcars)
#' vdtest(fit1)
#' vdtest(fit1,varformula = ~ hp + wt)
#' vdtest(fit1,varformula = ~ hp + wt + hp*wt)
#'
#' ###### Example 2: Species richness in plots
#' data(richness)
#' fit2 <- lm(Species ~ Biomass + pH, data=richness)
#' vdtest(fit2)
#'
#' ### The test conclusions change when the outlying observations are excluded
#' fit2a <- lm(Species ~ Biomass + pH, data=richness, subset=-c(1,3,18,20))
#' vdtest(fit2a)
#'
#' ###### Example 3: Gas consumption in a home before and after insulation
#' whiteside <- MASS::whiteside
#' fit3 <- lm(Gas ~ Temp + Insul + Temp*Insul, data=whiteside)
#' vdtest(fit3)
#'
#' ### The test conclusions change when the outlying observations are excluded
#' fit3a <- lm(Gas ~ Temp + Insul + Temp*Insul, data=whiteside, subset=-c(8,9,36,46,55))
#' vdtest(fit3a)
#'
#' @references Breusch T.S., Pagan A.R. (1979) A simple test for heteroscedasticity and random coefficient variation. \emph{Econometrica} 47, 1287–1294.
#' @references Cook R.D., Weisberg S. (1983) Diagnostics for heteroscedasticity in regression. \emph{Biometrika} 70, 1–10.
#' @seealso \link{vdtest.glm}
vdtest.lm <- function(model,varformula,verbose=TRUE,...){
  if(missingArg(varformula)) varformula <- as.formula(model$call$formula)
  if(is.null(model$call$data)) Z <- model.matrix(varformula)
  else Z <- model.matrix(varformula,eval(model$call$data))
  if(!is.null(model$call$subset)) Z <- Z[eval(model$call$subset,eval(model$call$data)),]
  n <- nrow(Z)
  if(colnames(Z)[1]!="(Intercept)"){
    out_ <- colnames(Z)
    Zstar <- cbind(1,Z)
  }else{
    out_ <- colnames(Z)[-1]
    Zstar <- Z
    Z <- Z[,-1]
  }
  p <- ncol(Zstar) - 1
  if(is.null(model$weights)) w <- matrix(1,n,1) else w <- model$weights
  phies <- mean(resid(model)^2*w)
  mus <- fitted(model)
  tau <- resid(model)^2*w/phies - 1
  Zstar <- matrix(1,n,ncol(Zstar))*Zstar
  Zstar2 <- chol2inv(chol(t(Zstar)%*%Zstar))[-1,-1]
  sc = 0.5*(t(tau)%*%Z)%*%Zstar2%*%(t(Z)%*%tau)
  if(verbose){
    cat("\n             Score test for varying dispersion parameter\n\n")
    cat("          Statistic = ",round(sc,digits=5),"\n degrees of freedom = ",p,"\n            p-value = ",format.pval(1-pchisq(sc,p)),"\n\n")
  }
  return(invisible(list(statistic=sc,df=p,p.value=format.pval(1-pchisq(sc,p)),vars=out_)))
}

#' @title Variable Selection in Normal Linear Models
#' @description Performs variable selection in normal linear models using a hybrid versions of forward stepwise and backward stepwise.
#' @param model an object of the class \emph{lm}.
#' @param direction an (optional) character string indicating the type of procedure which should be used. The available options are: hybrid backward stepwise ("backward") and hybrid forward stepwise ("forward"). As default, \code{direction} is set to "forward".
#' @param levels an (optional) two-dimensional vector of values in the interval \eqn{(0,1)} indicating the levels at which the variables should in and out from the model. This is only appropiate if \code{criterion}="p-value". As default, \code{levels} is set to \code{c(0.05,0.05)}.
#' @param criterion an (optional) character string indicating the criterion which should be used to compare the candidate models. The available options are: AIC ("aic"), BIC ("bic"), adjusted R-squared ("adjr2"), predicted R-squared ("prdr2"), Mallows' CP ("cp") and \emph{p}-value of the F test ("p-value"). As default, \code{criterion} is set to "bic".
#' @param ...	further arguments passed to or from other methods. For example, \code{k}, that is, the magnitude of the penalty in the AIC/QICu, which by default is set to 2.
#' @param trace an (optional) logical switch indicating if should the stepwise reports be printed. As default, \code{trace} is set to TRUE.
#' @param scope an (optional) list containing components \code{lower} and \code{upper}, both formula-type objects, indicating the range of models which should be examined in the stepwise search. As default, \code{lower} is a model with no predictors and \code{upper} is the linear predictor of the model in \code{model}.
#' @details The "hybrid forward stepwise" algorithm starts with the
#' simplest model (which may be chosen at the argument \code{scope}, and
#' As default, is a model whose parameters in the linear predictor,
#' except the intercept, if any, are set to 0), and then the candidate
#' models are built by hierarchically including effects in the linear
#' predictor, whose "relevance" and/or "importance" in the model fit is
#' assessed by comparing nested models (that is, by comparing the models
#' with and without the added effect) using a criterion previously
#' specified. If an effect is added to the equation, this strategy may
#' also remove any effect which, according to the previously specified
#' criteria, no longer provides an improvement in the model fit. That
#' process continues until no more effects are included or excluded. The
#' "hybrid backward stepwise" algorithm works similarly.
#' @return a list list with components including
#' \tabular{ll}{
#' \code{initial} \tab  a character string indicating the linear predictor of the "initial model",\cr
#' \tab \cr
#' \code{direction} \tab  a character string indicating the type of procedure which was used,\cr
#' \tab \cr
#' \code{criterion} \tab a character string indicating the criterion used to compare the candidate models,\cr
#' \tab \cr
#' \code{final} \tab a character string indicating the linear predictor of the "final model",\cr
#' }
#' @seealso \link{stepCriterion.glm}, \link{stepCriterion.overglm}, \link{stepCriterion.glmgee}
#' @examples
#' ###### Example 1: New York air quality measurements
#' fit1 <- lm(log(Ozone) ~ Solar.R + Temp + Wind, data=airquality)
#' scope=list(lower=~1, upper=~Solar.R*Temp*Wind)
#' stepCriterion(fit1, direction="forward", criterion="adjr2", scope=scope)
#' stepCriterion(fit1, direction="forward", criterion="bic", scope=scope)
#' stepCriterion(fit1, direction="forward", criterion="p-value", scope=scope)
#'
#' ###### Example 2: Fuel consumption of automobiles
#' fit2 <- lm(mpg ~ log(hp) + log(wt) + qsec, data=mtcars)
#' scope=list(lower=~1, upper=~log(hp)*log(wt)*qsec)
#' stepCriterion(fit2, direction="backward", criterion="bic", scope=scope)
#' stepCriterion(fit2, direction="forward", criterion="cp", scope=scope)
#' stepCriterion(fit2, direction="backward", criterion="prdr2", scope=scope)
#'
#' ###### Example 3: Credit card balance
#' Credit <- ISLR::Credit
#' fit3 <- lm(Balance ~ Cards + Age + Rating + Income + Student + Limit, data=Credit)
#' stepCriterion(fit3, direction="forward", criterion="prdr2")
#' stepCriterion(fit3, direction="forward", criterion="cp")
#' stepCriterion(fit3, direction="forward", criterion="p-value")
#'
#' @seealso \link{stepCriterion.glm}, \link{stepCriterion.overglm}, \link{stepCriterion.glmgee}
#' @method stepCriterion lm
#' @export
#' @references James G., Witten D., Hastie T., Tibshirani R. (2013, page 210) An Introduction to Statistical Learning with Applications in R, Springer, New York.
#'
stepCriterion.lm <- function(model, criterion=c("bic","aic","adjr2","prdr2","cp","p-value"), direction=c("forward","backward"), levels=c(0.05,0.05), trace=TRUE, scope, ...){
  xxx <- list(...)
  if(is.null(xxx$k)) k <- 2 else k <- xxx$k
  criterion <- match.arg(criterion)
  direction <- match.arg(direction)
  criters <- c("aic","bic","adjr2","prdr2","cp","p-value")
  criters2 <- c("AIC","BIC","adj.R-squared","prd.R-squared","Mallows' CP","Pr(>F)(*)")
  sentido <- c(1,1,-1,-1,1,1)
  ids <- criters == criterion
  if(missingArg(scope)){
    upper <- formula(eval(model$call$formula))
    lower <- formula(eval(model$call$formula))
    lower <- formula(paste(deparse(lower[[2]]),"~",attr(terms(lower,data=eval(model$call$data)),"intercept")))
  }else{
    lower <- scope$lower
    upper <- scope$upper
  }
  #formulae <- update(upper, paste(deparse(eval(model$call$formula)[[2]]),"~ ."))
  model <- update(model,formula=upper)
  mf <- model$model
  y <- mf[,attr(model$terms,"response")]
  if(is.null(model$offset)) offset <- matrix(0,length(y),1)	else offset <- model$offset
  if(is.null(model$weights)) weights <- matrix(1,length(y),1) else weights <- model$weights
  n <- sum(weights > 0)
  X <- model.matrix(upper,mf)
  fit0 <- lm.fit(y=sqrt(weights)*(y-offset),x=X*matrix(sqrt(weights),nrow(X),ncol(X)))
  sigma20 <- sum((y-offset-X%*%fit0$coefficients)^2*weights)/(n-ncol(X))
  lmstats <- function(y,X,b,o,w,k){
    p <- ncol(X)
    sigma2 <- sum((y-o-X%*%b)^2*w)/(n-p)
    mu0 <- sum((y-o)*w)/sum(w)
    if(colnames(X)[1]=="(Intercept)") denom <- sum((y-o-mu0)^2*w)/(n-1)
    else denom <- sum((y-o)^2*w)/n
    adjr2 <- 1-sigma2/denom
    aic <- n*log(2*pi*sigma2*(n-p)/n) + n + k*(p+1)
    bic <- n*log(2*pi*sigma2*(n-p)/n) + n + log(n)*(p+1)
    Xw <- X*matrix(sqrt(w),nrow(X),ncol(X))
    salida <- svd(Xw)
    h <- apply(salida$u^2,1,sum)
    numer <- sum(((y-o-X%*%b)*sqrt(w)/(1-h))^2)
    if(colnames(X)[1]=="(Intercept)") denom <- sum(((y-o-mu0)*sqrt(w)/(1-w/sum(w)))^2)
    else denom <- sum((y-o)^2)
    predr2 <- 1-numer/denom
    cp <- (n-p)*(sigma2/sigma20-1) + p
    return(c(aic,bic,adjr2,predr2,cp,sigma2))
  }
  U <- unlist(lapply(strsplit(attr(terms(upper,data=eval(model$call$data)),"term.labels"),":"),function(x) paste(sort(x),collapse =":")))
  fs <- attr(terms(upper,data=eval(model$call$data)),"factors")
  long <- max(nchar(U)) + 2
  nonename <- paste("<none>",paste(replicate(max(long-6,0)," "),collapse=""),collapse="")
  cambio <- ""
  paso <- 1
  tol <- TRUE
  if(trace){
    cat("\n       Family: gaussian\n")
    cat("Link function: identity\n")
    cat("    Criterion:",criters2[ids],"\n")
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
      X <- model.matrix(oldformula,mf)
      fit.x <- lm.fit(y=sqrt(weights)*(y-offset),x=X*matrix(sqrt(weights),nrow(X),ncol(X)))
      none <- c(NA,lmstats(y,X,fit.x$coefficients,offset,weights,k),NA)
      S <- unlist(lapply(strsplit(attr(terms(oldformula),"term.labels"),":"),function(x) paste(sort(x),collapse =":")))
      entran <- seq(1:length(U))[is.na(match(U,S))]
      salen <- seq(1:length(U))[!is.na(match(U,S))]
      mas <- TRUE

      fsalen <- matrix(NA,length(salen),7)
      if(length(salen) > 0){
        nombres <- matrix("",length(salen),1)
        for(i in 1:length(salen)){
          salida <- apply(as.matrix(fs[,salen[i]]*fs[,-c(entran,salen[i])]),2,sum)
          if(all(salida < sum(fs[,salen[i]])) & U[salen[i]]!=cambio){
            newformula <- update(oldformula, paste("~ . -",U[salen[i]]))
            X <- model.matrix(newformula,mf)
            fit.0 <- lm.fit(y=sqrt(weights)*(y-offset),x=X*matrix(sqrt(weights),nrow(X),ncol(X)))
            fsalen[i,1] <- length(fit.x$coefficients) - length(fit.0$coefficients)
            fsalen[i,2:7] <- lmstats(y,X,fit.0$coefficients,offset,weights,k)
            fsalen[i,7] <- ((n-length(fit.0$coefficients))*fsalen[i,7] - (n-length(fit.x$coefficients))*none[7])/(fsalen[i,1]*none[7])
            fsalen[i,7] <- (1 + fsalen[i,1] + n-length(fit.x$coefficients))*log(1 + fsalen[i,7]*fsalen[i,1]/(n-length(fit.x$coefficients)))
            nombres[i] <- U[salen[i]]
          }
        }
        rownames(fsalen) <- paste("-",nombres)
        if(criterion=="p-value" & any(!is.na(fsalen))){
          colnames(fsalen) <- c("df",criters2)
          if(nrow(fsalen) > 1){
            fsalen <- fsalen[order(fsalen[,7]),]
            fsalen <- na.omit(fsalen)
            attr(fsalen,"na.action")	<- attr(fsalen,"class") <- NULL
          }
          u <- fsalen[,1]; v <- n - length(fit.x$coefficients); x <- fsalen[,7]
          fsalen[,7] <- 1 - pf((exp(fsalen[,7]/(1 + u + v)) - 1)*v/u,u,v)
          if(fsalen[1,7] > levels[2]){
            fsalen <- rbind(fsalen,none[-7])
            rownames(fsalen)[nrow(fsalen)] <- nonename
            if(trace){
              printCoefmat(fsalen,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=c(2,3,6),
                           signif.stars=FALSE,tst.ind=c(4,5),dig.tst=4,digits=5)
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
        fentran <- matrix(NA,length(entran),7)
        nombres <- matrix("",length(entran),1)
        for(i in 1:length(entran)){
          salida <- apply(as.matrix(fs[,-c(salen,entran[i])]),2,function(x) sum(fs[,entran[i]]*x)!=sum(x))
          if(all(salida) & U[entran[i]]!=cambio){
            newformula <- update(oldformula, paste("~ . +",U[entran[i]]))
            X <- model.matrix(newformula,mf)
            fit.0 <- lm.fit(y=sqrt(weights)*(y-offset),x=X*matrix(sqrt(weights),nrow(X),ncol(X)))
            fentran[i,1] <- length(fit.0$coefficients) - length(fit.x$coefficients)
            fentran[i,2:7] <- lmstats(y,X,fit.0$coefficients,offset,weights,k)
            fentran[i,7] <- ((n-length(fit.x$coefficients))*none[7]-(n-length(fit.0$coefficients))*fentran[i,7])/(fentran[i,1]*fentran[i,7])
            fentran[i,7] <- (1 + fentran[i,1] + n-length(fit.0$coefficients))*log(1 + fentran[i,7]*fentran[i,1]/(n-length(fit.0$coefficients)))
            nombres[i] <- U[entran[i]]
          }
        }
        rownames(fentran) <- paste("+",nombres)
        if(criterion=="p-value"){
          colnames(fentran) <- c("df",criters2)
          if(nrow(fentran) > 1){
            fentran <- fentran[order(-fentran[,7]),]
            fentran <- na.omit(fentran)
            attr(fentran,"na.action")	<- attr(fentran,"class") <- NULL
          }
          u <- fentran[,1]; v <- n - fentran[,1] - length(fit.x$coefficients); x <- fentran[,7]
          fentran[,7] <- 1 - pf((exp(fentran[,7]/(1 + u + v)) - 1)*v/u,u,v)
          fentran <- rbind(fentran,none[-7])
          rownames(fentran)[nrow(fentran)] <- nonename
          if(trace) printCoefmat(fentran,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=2:3,
                                 signif.stars=FALSE,tst.ind=4:5,dig.tst=4,digits=5)
          if(fentran[1,ncol(fentran)] < levels[1]){
            if(trace) cat("\nStep",paso,":",rownames(fentran)[1],"\n\n")
            paso <- paso + 1
            cambio <- substring(rownames(fentran)[1],3)
            oldformula <- update(oldformula, paste("~ .",rownames(fentran)[1]))
          }else tol <- FALSE
        }
      }
      if(length(entran) > 0 & criterion!="p-value"){
        u <- fentran[,1]; v <- n - fentran[,1] - length(fit.x$coefficients); x <- fentran[,7]
        fentran[,7] <- 1 - pf((exp(fentran[,7]/(1 + u + v)) - 1)*v/u,u,v)
        if(any(!is.na(fsalen))){
          u <- fsalen[,1]; v <- n - length(fit.x$coefficients); x <- fsalen[,7]
          fsalen[,7] <- 1 - pf((exp(fsalen[,7]/(1 + u + v)) - 1)*v/u,u,v)
          fentran <- rbind(fentran,fsalen)
        }
        fentran <- rbind(fentran,c(0,none[-c(1,7,8)],0))
        rownames(fentran)[nrow(fentran)] <- nonename

        colnames(fentran) <- c("df",criters2)
        fentran <- na.omit(fentran)
        attr(fentran,"na.action")	<- attr(fentran,"class") <- NULL
        fentran[nrow(fentran),c(1,7)] <- NA
        fentran <- fentran[order(sentido[ids]*fentran[,c(FALSE,ids)]),]
        if(rownames(fentran)[1]!=nonename){
          if(trace){
            printCoefmat(fentran,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=c(2,3,6),
                         signif.stars=FALSE,tst.ind=c(4,5),dig.tst=4,digits=5)
            cat("\nStep",paso,":",rownames(fentran)[1],"\n\n")
          }
          paso <- paso + 1
          cambio <- substring(rownames(fentran)[1],3)
          oldformula <- update(oldformula, paste("~ .",rownames(fentran)[1]))
        }else{
          tol <- FALSE
          if(trace) printCoefmat(fentran,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=c(2,3,6),
                                 signif.stars=FALSE,tst.ind=c(4,5),dig.tst=4,digits=5)
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
      X <- model.matrix(oldformula,mf)
      fit.x <- lm.fit(y=sqrt(weights)*(y-offset),x=X*matrix(sqrt(weights),nrow(X),ncol(X)))
      none <- c(NA,lmstats(y,X,fit.x$coefficients,offset,weights,k),NA)
      S <- unlist(lapply(strsplit(attr(terms(oldformula),"term.labels"),":"),function(x) paste(sort(x),collapse =":")))
      entran <- seq(1:length(U))[is.na(match(U,S))]
      salen <- seq(1:length(U))[!is.na(match(U,S))]
      menos <- TRUE

      fentran <- matrix(NA,length(entran),7)
      if(length(entran) > 0){
        nombres <- matrix("",length(entran),1)
        for(i in 1:length(entran)){
          salida <- apply(as.matrix(fs[,-c(salen,entran[i])]),2,function(x) sum(fs[,entran[i]]*x)!=sum(x))
          if(all(salida) & U[entran[i]]!=cambio){
            newformula <- update(oldformula, paste("~ . +",U[entran[i]]))
            X <- model.matrix(newformula,mf)
            fit.0 <- lm.fit(y=sqrt(weights)*(y-offset),x=X*matrix(sqrt(weights),nrow(X),ncol(X)))
            fentran[i,1] <- length(fit.0$coefficients) - length(fit.x$coefficients)
            fentran[i,2:7] <- lmstats(y,X,fit.0$coefficients,offset,weights,k)
            fentran[i,7] <- ((n-length(fit.x$coefficients))*none[7]-(n-length(fit.0$coefficients))*fentran[i,7])/(fentran[i,1]*fentran[i,7])
            fentran[i,7] <- (1 + fentran[i,1] + n-length(fit.0$coefficients))*log(1 + fentran[i,7]*fentran[i,1]/(n-length(fit.0$coefficients)))
            nombres[i] <- U[entran[i]]
          }
        }
        rownames(fentran) <- paste("+",nombres)
        if(criterion=="p-value" & any(!is.na(fentran))){
          colnames(fentran) <- c("df",criters2)
          if(nrow(fentran) > 1){
            fentran <- fentran[order(-fentran[,7]),]
            fentran <- na.omit(fentran)
            attr(fentran,"na.action")	<- attr(fentran,"class") <- NULL
          }
          u <- fentran[,1]; v <- n - fentran[,1] - length(fit.x$coefficients); x <- fentran[,7]
          fentran[,7] <- 1 - pf((exp(fentran[,7]/(1 + u + v)) - 1)*v/u,u,v)
          if(fentran[1,7] < levels[1]){
            fentran <- rbind(fentran,none[-7])
            rownames(fentran)[nrow(fentran)] <- nonename
            if(trace){
              printCoefmat(fentran,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=c(2,3,6),
                           signif.stars=FALSE,tst.ind=c(4,5),dig.tst=4,digits=5)
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
        fsalen <- matrix(NA,length(salen),7)
        nombres <- matrix("",length(salen),1)
        for(i in 1:length(salen)){
          salida <- apply(as.matrix(fs[,salen[i]]*fs[,-c(entran,salen[i])]),2,sum)
          if(all(salida < sum(fs[,salen[i]]))){
            newformula <- update(oldformula, paste("~ . -",U[salen[i]]))
            X <- model.matrix(newformula,mf)
            fit.0 <- lm.fit(y=sqrt(weights)*(y-offset),x=X*matrix(sqrt(weights),nrow(X),ncol(X)))
            fsalen[i,1] <- length(fit.x$coefficients) - length(fit.0$coefficients)
            fsalen[i,2:7] <- lmstats(y,X,fit.0$coefficients,offset,weights,k)
            fsalen[i,7] <- ((n-length(fit.0$coefficients))*fsalen[i,7] - (n-length(fit.x$coefficients))*none[7])/(fsalen[i,1]*none[7])
            fsalen[i,7] <- (1 + fsalen[i,1] + n-length(fit.x$coefficients))*log(1 + fsalen[i,7]*fsalen[i,1]/(n-length(fit.x$coefficients)))
            nombres[i] <- U[salen[i]]
          }
        }
        rownames(fsalen) <- paste("-",nombres)
        if(criterion=="p-value"){
          colnames(fsalen) <- c("df",criters2)
          if(nrow(fsalen) > 1){
            fsalen <- fsalen[order(fsalen[,7]),]
            fsalen <- na.omit(fsalen)
            attr(fsalen,"na.action")	<- attr(fsalen,"class") <- NULL
          }
          u <- fsalen[,1]; v <- n - length(fit.x$coefficients); x <- fsalen[,7]
          fsalen[,7] <- 1 - pf((exp(fsalen[,7]/(1 + u + v)) - 1)*v/u,u,v)
          fsalen <- rbind(fsalen,none[-7])
          rownames(fsalen)[nrow(fsalen)] <- nonename
          if(trace) printCoefmat(fsalen,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=c(2,3,6),
                                 signif.stars=FALSE,tst.ind=c(4,5),dig.tst=4,digits=5)
          if(fsalen[1,ncol(fsalen)] > levels[2]){
            if(trace) cat("\nStep",paso,":",rownames(fsalen)[1],"\n\n")
            paso <- paso + 1
            cambio <- substring(rownames(fsalen)[1],3)
            oldformula <- update(oldformula, paste("~ .",rownames(fsalen)[1]))
          }else tol <- FALSE
        }
      }
      if(criterion!="p-value"){
        u <- fsalen[,1]; v <- n - length(fit.x$coefficients); x <- fsalen[,7]
        fsalen[,7] <- 1 - pf((exp(fsalen[,7]/(1 + u + v)) - 1)*v/u,u,v)
        if(any(!is.na(fentran))){
          u <- fentran[,1]; v <- n - fentran[,1] - length(fit.x$coefficients); x <- fentran[,7]
          fentran[,7] <- 1 - pf((exp(fentran[,7]/(1 + u + v)) - 1)*v/u,u,v)
          fentran <- rbind(fentran,fsalen)
        }
        fsalen <- rbind(fsalen,c(0,none[-c(1,7,8)],0))
        rownames(fsalen)[nrow(fsalen)] <- nonename
        colnames(fsalen) <- c("df",criters2)
        fsalen <- na.omit(fsalen)
        attr(fsalen,"na.action")	<- attr(fsalen,"class") <- NULL
        fsalen[nrow(fsalen),c(1,7)] <- NA
        fsalen <- fsalen[order(sentido[ids]*fsalen[,c(FALSE,ids)]),]
        if(rownames(fsalen)[1]!=nonename){
          if(trace){
            printCoefmat(fsalen,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=c(2,3,6),
                         signif.stars=FALSE,tst.ind=c(4,5),dig.tst=4,digits=5)
            cat("\nStep",paso,":",rownames(fsalen)[1],"\n\n")
          }
          paso <- paso + 1
          cambio <- substring(rownames(fsalen)[1],3)
          oldformula <- update(oldformula, paste("~ .",rownames(fsalen)[1]))
        }else{
          tol <- FALSE
          if(trace) printCoefmat(fsalen,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=c(2,3,6),
                                 signif.stars=FALSE,tst.ind=c(4,5),dig.tst=4,digits=5)
        }
      }
      if(length(salen) == 0 & menos) tol <- FALSE
    }
  }
  if(trace){
    cat("\n\nFinal model:\n")
    cat(paste("~",as.character(oldformula)[length(oldformula)],sep=" "),"\n\n")
    cat("********************************************************************************************")
    cat("\n (*) p-values of the F test")
    if(criterion=="p-value"){
      cat("\n Effects are included when their p-values are lower than",levels[1])
      cat("\n Effects are dropped when their p-values are higher than",levels[2])
    }
    if(!is.null(xxx$k)) cat("The magnitude of the penalty in the AIC was set to ",k)
    cat("\n")
  }
  out_$final <- paste("~",as.character(oldformula)[length(oldformula)],sep=" ")
  return(invisible(out_))
}

#'
#' @importFrom stats dbinom delete.response dpois glm AIC BIC logLik fitted quantile
#'             pbinom pgamma ppoints ppois qqplot rchisq binomial poisson resid runif
#'              rgamma rpois rbinom  qqnorm dnbinom residuals pnbinom
#'             qnbinom qpois rbeta rnbinom .getXlevels lm optim simulate
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom numDeriv grad hessian jacobian
#' @importFrom Rfast Digamma Lgamma
#' @importFrom Formula Formula model.part
#' @importFrom methods is
#' @importFrom SuppDists rinvGauss

#' @title Normal QQ-plot with simulated envelope of residuals for normal linear models
#' @description Produces a normal QQ-plot with simulated envelope of residuals obtained from the fit of a normal linear model.
#' @param object an object of the class \emph{lm}.
#' @param rep an (optional) positive integer indicating the number of replicates which should be used to build the simulated envelope. As default, \code{rep} is set to 100.
#' @param conf an (optional) value in the interval (0,1) indicating the confidence level which should be used to build the pointwise confidence intervals, which form the envelope. As default, \code{conf} is set to 0.95.
#' @param type a character string indicating the type of residuals which should be used. The available options are: internally Studentized ("internal") and externally Studentized ("external") residuals. See Cook and Weisberg (1982, pages 18-20).
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
#' \code{Residuals} \tab the observed \code{type}-type residuals,\cr
#' }
#' @details The simulated envelope is built by simulating \code{rep} independent realizations
#' of the response variable for each individual, which is accomplished taking into account
#' the following: (1) the model assumption about the distribution of the response variable;
#' (2) the estimates of the parameters in the linear predictor; and (3) the estimate of the
#' dispersion parameter. The interest model is re-fitted \code{rep} times, as each time the
#' vector of observed responses is replaced by one of the simulated samples. The
#' \code{type}-type residuals are computed and then sorted for each replicate, so that for
#' each \eqn{i=1,2,...,n}, where \eqn{n} is the number of individuals in the sample, there
#' is a random sample of size \code{rep} of the \eqn{i}-th order statistic of the
#' \code{type}-type residuals. Therefore, the simulated envelope is composed of the quantiles
#' (1 - \code{conf})/2 and (1 + \code{conf})/2 of the random sample of size \code{rep} of the
#' \eqn{i}-th order statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n}.
#' @references Atkinson A.C. (1985) \emph{Plots, Transformations and Regression}. Oxford University Press, Oxford.
#' @references Cook R.D., Weisberg S. (1982) \emph{Residuals and Influence in Regression}. Chapman and Hall, New York.
#' @seealso \link{envelope.glm}, \link{envelope.overglm}
#' @examples
#' ###### Example 1: Fuel consumption of automobiles
#' fit1 <- lm(mpg ~ log(hp) + log(wt), data=mtcars)
#' envelope(fit1, rep=100, conf=0.95, type="external", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 2: Species richness in plots
#' data(richness)
#' fit2 <- lm(Species ~ Biomass + pH + Biomass*pH, data=richness)
#' envelope(fit2, rep=100, conf=0.95, type="internal", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 3: Gas consumption in a home before and after insulation
#' whiteside <- MASS::whiteside
#' fit3 <- lm(Gas ~ Temp + Insul + Temp*Insul, data=whiteside)
#' envelope(fit3, rep=100, conf=0.95, type="internal", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' @method envelope lm
#' @export
envelope.lm <- function(object, rep=100, conf=0.95, type=c("external","internal"), plot.it=TRUE, identify, ...){
  type <- match.arg(type)
  p <- length(coef(object))
  X <- model.matrix(object)
  mu <- fitted(object)
  n <- length(mu)
  sigma2 <- summary(object)$sigma^2
  if(is.null(object$offset)) offset <- rep(0,n) else offset <- object$offset
  if(is.null(object$weights)) weights <- rep(1,n) else weights <- object$weights
  if(any(weights == 0)) stop("Only positive weights are supported!!",call.=FALSE)
  Xw <- X*matrix(sqrt(weights),n,p)
  salida <- svd(Xw)
  h <- apply(salida$u^2,1,sum)
  rep <- max(1,floor(abs(rep)))
  e <- matrix(0,n,rep)
  bar <- txtProgressBar(min=0, max=rep, initial=0, width=min(50,rep), char="+", style=3)
  i <- 1
  while(i <= rep){
    resp <- sqrt(sigma2/weights)*rnorm(n) + mu
    fits <- try(lm.wfit(x=X,y=resp,w=weights,offset=offset),silent=TRUE)
    if(is.list(fits)){
      phis <- sum((resp-fits$fitted.values)^2*weights)/(n-p)
      t <- (resp-fits$fitted.values)/sqrt(phis*(1-h))
      if(type=="external") t <- t*sqrt((n-p-1)/(n-p-t^2))
      e[,i] <- sort(t)
      setTxtProgressBar(bar,i)
      i <- i + 1
    }
  }
  close(bar)
  alpha <- 1 - max(0,min(1,abs(conf)))
  e <- as.matrix(e[,1:(i-1)])
  es <- apply(e,1,function(x) return(quantile(x,probs=c(alpha/2,0.5,1-alpha/2))))
  td <- residuals(object,type="response")/sqrt(sigma2*(1-h))
  if(type=="external") td <- td*sqrt((n-p-1)/(n-p-td^2))
  out_ <- as.matrix(cbind(t(es),sort(td)))
  colnames(out_) <- c("Lower limit","Median","Upper limit","Residuals")
  if(plot.it){
    nano <- list(...)
    nano$y <- td
    nano$type <- "p"
    if(is.null(nano$ylim)) nano$ylim <- 1.1*range(out_)
    if(is.null(nano$pch)) nano$pch <- 20
    if(is.null(nano$col)) nano$col <- "black"
    if(is.null(nano$xlab)) nano$xlab <- "Expected quantiles"
    if(is.null(nano$ylab)) nano$ylab <- "Observed quantiles"
    if(is.null(nano$main)) nano$main <- paste0("Normal QQ plot with simulated envelope\n of ",type,"ly studentized residuals")
    if(is.null(nano$labels)) labels <- 1:length(td)
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    outm <- do.call("qqnorm",nano)
    lines(sort(outm$x),es[2,],xlab="",ylab="",main="", type="l",lty=3)
    lines(sort(outm$x),es[1,],xlab="",ylab="",main="", type="l",lty=1)
    lines(sort(outm$x),es[3,],xlab="",ylab="",main="", type="l",lty=1)
    if(!missingArg(identify)) identify(outm$x,outm$y,n=max(1,floor(abs(identify))),labels=labels)
  }
  return(invisible(out_))
}
#'
#'@title The Hosmer-Lemeshow Goodness-of-Fit Test
#' @description Computes the Hosmer-Lemeshow goodness-of-fit test for a generalized linear model fitted to binary responses.
#' @param model an object of the class \emph{glm}, which is obtained from the fit of a generalized linear model where the distribution for the response variable is assumed to be binomial.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param ... further arguments passed to or from other methods.
#' @return A matrix with the following four columns:
#' \tabular{ll}{
#' \code{hm} \tab a matrix with the values of Group, Size, Observed and Expected, which are required to compute the statistic of the test,\cr
#' \tab \cr
#' \code{statistic} \tab the value of the statistic of the test,\cr
#' \tab \cr
#' \code{df} \tab the number of degrees of freedom, given by the number of groups minus 2,\cr
#' \tab \cr
#' \code{p.value} \tab the \emph{p}-value of the test computed using the Chi-square distribution,\cr
#' }
#' @references Hosmer D.W., Lemeshow S. (2000) \emph{Applied Logistic Regression. 2nd ed.} John Wiley & Sons, New York.
#' @examples
#'
#' ###### Example 1: Patients with burn injuries
#' burn1000 <- aplore3::burn1000
#' burn1000 <- within(burn1000, death <- factor(death, levels=c("Dead","Alive")))
#' fit1 <- glm(death ~ age*inh_inj + tbsa*inh_inj, family=binomial("logit"), data=burn1000)
#' hltest(fit1)
#'
#' ###### Example 2: Bladder cancer in mice
#' data(bladder)
#' fit2 <-  glm(cancer/exposed ~ dose, weights=exposed, family=binomial("cloglog"), data=bladder)
#' hltest(fit2)
#'
#' ###### Example 3: Liver cancer in mice
#' data(liver)
#' fit3 <-  glm(cancer/exposed ~ dose, weights=exposed, family=binomial("probit"), data=liver)
#' hltest(fit3)
#'
#' @export hltest
hltest <- function(model,verbose=TRUE,...){
  if(class(model)[1] != "glm") stop("Only glm-type objects are supported!!",call.=FALSE)
  if(model$family$family!="binomial") stop("Only binomial regression models are supported!!",call.=FALSE)
  temp <- data.frame(mus=fitted(model),ys=model$y*model$prior.weights,size=model$prior.weights)
  temp2 <- aggregate(cbind(m=size,events=ys)~mus,data=temp,sum)
  if(nrow(temp2) <= 2) stop("The Hosmer-Lemeshow goodness-of-fit test is not computed because the number of groups should be greater than 2!!",call.=FALSE)
  if(nrow(temp2) >= 10){
    N <- sum(temp2$m)
    M <- floor(0.5 + N/10)
    j <- 1
    i <- 1
    group <- matrix(0,nrow(temp2),1)
    while(j <= nrow(temp2)){
      c <- 0
      while(((c + floor(temp2[j,2]/2) <= M) & (c < M)) || c==0){
        group[j] <- i
        c <- c + temp2[j,2]
        j <- j + 1
        if(j > nrow(temp2)) break
      }
      i <- i + 1
    }
    group2 <- group
    group2[nrow(temp2)] <- group2[nrow(temp2)-1]
    temp3 <- data.frame(temp2,gr=group)
    temp4 <- data.frame(temp2,gr=group2)
    hm <- aggregate(cbind(m,events,mus*m)~gr,data=temp3,sum)
    hm1 <- aggregate(cbind(m,events,mus*m)~gr,data=temp4,sum)
    if(sd(hm[,2]) > sd(hm1[,2])) hm <- hm1
  }
  if(nrow(temp2) < 10){
    hm <- cbind(1:nrow(temp2),temp2[,c(2,3,1)])
    hm[,4] <- temp2[,1]*temp2[,2]
  }
  colnames(hm) <- c("Group","Size","Observed","Expected")
  rownames(hm) <- NULL
  hmt <- sum((hm[,3] - hm[,4])^2/(hm[,4]*(1-hm[,4]/hm[,2])))
  if(verbose){
    cat("\n   The Hosmer-Lemeshow goodness-of-fit test\n\n")
    print(hm,row.names=FALSE)
    cat("\n         Statistic = ",round(hmt,digits=5),"\ndegrees of freedom = ",nrow(hm)-2,"\n           p-value = ",format.pval(1-pchisq(hmt,nrow(hm)-2)),"\n\n")
  }
  return(invisible(list(hm=hm,statistic=hmt,df=nrow(hm)-2,p.value=format.pval(1-pchisq(hmt,nrow(hm)-2),digits=7))))
}

#' @title The Receiver Operating Characteristic (ROC) Curve
#' @description Computes the exact area under the ROC curve (AUROC), the Gini coefficient, and the Kolmogorov-Smirnov (KS) statistic for a binary classifier. Optionally, this function can plot the ROC curve, that is, the plot of the estimates of Sensitivity versus the estimates of 1-Specificity.
#' @param object a matrix with two columns: the first one is a numeric vector of 1's and 0's indicating whether each row is a "success" or a "failure"; the second one is a numeric vector of values indicating the probability (or propensity score) of each row to be a "success". Optionally, \code{object} can be an object of the class glm which is obtained from the fit of a generalized linear model where the distribution of the response variable is assumed to be binomial.
#' @param plot.it an (optional) logical switch indicating if the plot of the ROC curve is required or just the data matrix in which it is based. As default, \code{plot.it} is set to TRUE.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param ... further arguments passed to or from other methods. For example, if \code{plot.it=TRUE} then \code{...} may to include graphical parameters as \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @references Hanley J.A., McNeil B.J. (1982) The Meaning and Use of the Area under a Receiver Operating Characteristic (ROC) Curve. \emph{Radiology} 143, 29–36.
#' @return A list which contains the following objects:
#' \describe{
#' \item{\code{roc}}{ A matrix with the Cutoffs and the associated estimates of Sensitivity and Specificity.}
#' \item{\code{auroc}}{ The exact area under the ROC curve.}
#' \item{\code{gini}}{ The value of the Gini coefficient computed as 2(\code{auroc}-0.5).}
#' \item{\code{ks}}{ The value of the Kolmogorov-Smirnov statistic computed as the maximum value of |1-Sensitivity-Specificity|.}
#' }
#' @examples
#' ###### Example: Patients with burn injuries
#' burn1000 <- aplore3::burn1000
#' burn1000 <- within(burn1000, death2 <- ifelse(death=="Dead",1,0))
#'
#' ### splitting the sample: 70% for the training sample and 30% for the validation sample
#' train <- sample(1:nrow(burn1000),size=nrow(burn1000)*0.7)
#' traindata <- burn1000[train,]
#' testdata <- burn1000[-train,]
#'
#' fit <- glm(death ~ age*inh_inj + tbsa*inh_inj, family=binomial("logit"), data=traindata)
#' probs <- predict(fit, newdata=testdata, type="response")
#'
#' ### ROC curve for the validation sample
#' ROCc(cbind(testdata[,"death2"],probs), col="red", col.lab="blue", col.axis="black",
#'      col.main="black", family="mono")
#' @export ROCc
#' @importFrom stats aggregate sd
#'
ROCc <- function(object,plot.it=TRUE,verbose=TRUE,...){
  if(is(object,"glm")){
    if(object$family$family!="binomial") stop("Only binomial regression models are supported!!",call.=FALSE)
    temp <- data.frame(mus=fitted(object),ys=object$y*object$prior.weights,size=object$prior.weights)
  }else{
    object <- as.matrix(object)
    temp <- data.frame(ys=as.numeric(object[,1])-min(as.numeric(object[,1])),mus=object[,2],size=rep(1,nrow(object)))
    if(any(temp$ys!=0 & temp$ys!=1)) stop("First column of object should contain only 1's or 0's!!",call.=FALSE)
  }
  temp2 <- aggregate(cbind(m=size,events=ys)~mus,data=temp,sum)
  mus <- temp2[,1]
  if(nrow(temp2) > 10000) mus <- quantile(rep(temp2[,1],temp2[,2]), probs=c(0:9999)/10000)
  results <- matrix(0,length(mus),3)
  d1 <- sum(temp2[,3])
  d3 <- sum(temp2[,2])
  d2 <- d3 - d1
  C <- 0
  for(i in 1:length(mus)){
    results[i,1] <- mus[i]
    results[i,2] <- sum(temp2[temp2[,1] >= mus[i],3])/d1
    results[i,3] <- (sum(temp2[temp2[,1] < mus[i],2]) - sum(temp2[temp2[,1] < mus[i],3]))/d2
    if(i >= 2) C <- C + (1/2)*(results[i,3] - results[i-1,3])*(min(results[i,2],results[i-1,2]) + max(results[i,2],results[i-1,2]))
  }
  if(plot.it){
    nano <- list(...)
    nano$x <- 1-results[,3]
    nano$y <- results[,2]
    nano$type <- "l"
    limits <- range(1-results[,3],results[,2])
    if(is.null(nano$col)) nano$col <- "blue"
    if(is.null(nano$xlab)) nano$xlab <- "1-Specificity"
    if(is.null(nano$ylab)) nano$ylab <- "Sensitivity"
    if(is.null(nano$xlim)) nano$xlim <- limits
    if(is.null(nano$ylim)) nano$ylim <- limits
    if(is.null(nano$main)) nano$main <- "Receiver Operating Characteristic Curve"
    do.call("plot",nano)
    abline(0,1,lty=3)
    dev.new()
    kss <- cbind(results[,1],rbind(cbind(1-results[,2],results[,3])[-1,],c(1,1)))
    idks <- max(abs(kss[,2]-kss[,3]))==abs(kss[,2]-kss[,3])
    plot(kss[,1],kss[,2],type="s",ylim=c(0,1),col="blue",main="Kolmogorov-Smirnov Statistic",xlab=expression(hat(mu)),ylab="Empirical Cumulative Distribution Function")
    par(new=TRUE)
    plot(kss[,1],kss[,3],type="s",ylim=c(0,1),col="red",xlab="",ylab="")
    legend("bottomright",legend=c("Zeros","Ones"),col=c("red","blue"),lty=1,bty="n")
    segments(kss[idks,1],kss[idks,2],kss[idks,1],kss[idks,3])
  }
  ks <- max(abs(1-results[,3]-results[,2]))
  if(verbose){
    cat("\n Area Under ROC Curve  = ",round(C,digits=3),"\n")
    cat("     Gini Coefficient  = ",round(2*(C-0.5),digits=3),"\n")
    cat("        K-S Statistic  = ",round(ks,digits=3),"\n")
  }
  results <- as.data.frame(results)
  colnames(results) <- c("Cutoff","Sensitivity","Specificity")
  out_ <- list(roc=results,auroc=C,gini=2*(C-0.5),ks=ks)
  return(invisible(out_))
}


#' @title Comparison of nested Generalized Linear Models
#' @description Allows to compare nested generalized linear models using Wald, score, gradient, and likelihood ratio tests.
#' @param object an object of the class glm which is obtained from the fit of a generalized linear model.
#' @param ... another objects of the class glm which are obtained from the fit of generalized linear models.
#' @param test an (optional) character string indicating the required type of test. The available options are: Wald ("wald"), Rao's score ("score"), Terrell's gradient ("gradient"), and likelihood ratio ("lr") tests. As default, \code{test} is set to "wald".
#' @param verbose an (optional) logical indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @details The Wald, Rao's score and Terrell's gradient tests are performed using the expected Fisher information matrix.
#' @references Buse A. (1982) The Likelihood Ratio, Wald, and Lagrange Multiplier Tests: An Expository Note. \emph{The American Statistician} 36, 153-157.
#' @references Terrell G.R. (2002) The gradient statistic. \emph{Computing Science and Statistics} 34, 206 – 215.
#' @examples
#' ## Example 1
#' Auto <- ISLR::Auto
#' fit1 <- glm(mpg ~ weight, family=inverse.gaussian("log"), data=Auto)
#' fit2 <- update(fit1, . ~ . + horsepower)
#' fit3 <- update(fit2, . ~ . + horsepower:weight)
#' anova2(fit1, fit2, fit3, test="lr")
#' anova2(fit1, fit2, fit3, test="score")
#' anova2(fit1, fit2, fit3, test="wald")
#' anova2(fit1, fit2, fit3, test="gradient")
#'
#' ## Example 2
#' burn1000 <- aplore3::burn1000
#' mod <- death ~ age + tbsa + inh_inj
#' fit1 <- glm(mod, family=binomial("logit"), data=burn1000)
#' fit2 <- update(fit1, . ~ . + inh_inj + age*inh_inj + tbsa*inh_inj)
#' anova2(fit1, fit2, test="lr")
#' anova2(fit1, fit2, test="score")
#' anova2(fit1, fit2, test="wald")
#' anova2(fit1, fit2, test="gradient")
#'
#' ## Example 3
#' data(aucuba)
#' fit <- glm(lesions ~ 1 + time, family=poisson("log"), data=aucuba)
#' anova2(fit, test="lr")
#' anova2(fit, test="score")
#' anova2(fit, test="wald")
#' anova2(fit, test="gradient")
#' @return A matrix with three columns which contains the following:
#' \describe{
#' \item{\code{Chi}}{ The value of the statistic of the test.}
#' \item{\code{Df}}{ The number of degrees of freedom.}
#' \item{\code{Pr(>Chi)}}{ The \emph{p}-value of the test computed using the Chi-square distribution.}
#' }
#' @export anova2

anova2 <- function(object,...,test=c("wald","lr","score","gradient"),verbose=TRUE){
  test <- match.arg(test)
  x <- list(object,...)
  if(any(lapply(x,function(xx) class(xx)[1])!="glm"))
    stop("Only glm-type objects are supported!!",call.=FALSE)
  if(length(x)==1){
    terminos <- attr(object$terms,"term.labels")
    x[[1]] <- update(object,paste(". ~ . -",paste(terminos,collapse="-")))
    for(i in 1:length(terminos)) x[[i+1]] <- update(x[[i]],paste(". ~ . + ",terminos[i]))
  }
  hast <- length(x)
  out_ <- matrix(0,hast-1,3)
  for(i in 2:hast){
    vars0 <- names(coef(x[[i-1]]))
    vars1 <- names(coef(x[[i]]))
    nest <- vars0 %in% vars1
    ids <- is.na(match(vars1,vars0))
    phi <- sum(resid(x[[i]],type="pearson")^2)/x[[i]]$df.residual
    if(x[[i]]$family$family=="poisson" | x[[i]]$family$family=="binomial") phi <- 1
    if(test=="wald") sc <- crossprod(coef(x[[i]])[ids],solve(vcov(x[[i]])[ids,ids]))%*%coef(x[[i]])[ids]
    if(test=="lr") sc <- (x[[i-1]]$deviance - x[[i]]$deviance)/phi
    if(test=="score" | test=="gradient"){
      X <- model.matrix(x[[i]])
      u0 <- crossprod(X[,ids],resid(x[[i-1]],type="pearson")*sqrt(x[[i-1]]$weights))/phi
      if(test=="score"){
        v0 <- phi*chol2inv(chol(crossprod(X,X*matrix(x[[i-1]]$weights,nrow(X),ncol(X)))))[ids,ids]
        sc <- crossprod(u0,v0)%*%u0
      }else sc <- abs(crossprod(u0,coef(x[[i]])[ids]))
    }
    df <- sum(ids)
    out_[i-1,] <- cbind(sc,df,1-pchisq(sc,df))
  }
  colnames(out_) <- c(" Chi  ", " df", " Pr(Chisq>)")
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


#' @title Estimating Equations in Generalized Linear Models
#' @description Extracts estimating equations evaluated at the parameter estimates and the observed data for a generalized linear model fitted to the data.
#' @param object an object of the class glm which is obtained from the fit of a generalized linear model.
#' @param ... further arguments passed to or from other methods.
#' @return A vector with the value of the estimating equations evaluated at the parameter estimates and the observed data.
#' @method estequa glm
#' @export
#' @examples
#' ## Example 1
#' Auto <- ISLR::Auto
#' mod <- mpg ~ cylinders + displacement + acceleration + origin + horsepower*weight
#' fit1 <- glm(mod, family=inverse.gaussian("log"), data=Auto)
#' estequa(fit1)
#'
#' ## Example 2
#' burn1000 <- aplore3::burn1000
#' burn1000 <- within(burn1000, death <- factor(death, levels=c("Dead","Alive")))
#' mod2 <- death ~ age + gender + race + tbsa + inh_inj + flame + age*inh_inj + tbsa*inh_inj
#' fit2 <- glm(mod2, family=binomial("logit"), data=burn1000)
#' estequa(fit2)
#'
#' ## Example 3
#' data(skincancer)
#' fit3 <- glm(cases ~ offset(log(population)) + city + age, family=poisson("log"), data=skincancer)
#' estequa(fit3)
estequa.glm <- function(object,...){
  X <- model.matrix(object)
  y <- object$y
  if(is.null(object$prior.weights)) omega <- matrix(1,nrow(X),1) else omega <- object$prior.weights
  if(is.null(object$offset)) offset <- matrix(0,nrow(X),1) else offset <- object$offset
  beta <- coef(object)
  eta <- offset + X%*%beta
  mu <- object$family$linkinv(eta)
  G <- object$family$mu.eta(eta)
  V <- object$family$variance(mu)
  z <- (y-mu)*omega*G/V
  out_ <- t(X)%*%z/summary(object)$dispersion
  colnames(out_) <- " "
  rownames(out_) <- names(coef(object))
  print(out_)
  return(invisible(out_))
}
#' @title Generalized Variance Inflation Factor
#' @description Computes the generalized variance inflation factor (GVIF) for a weighted or unweighted normal linear model.
#' @param model an object of the class \emph{lm}.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param ...	further arguments passed to or from other methods.
#' @details If the number of degrees of freedom is 1 then the GVIF reduces to the Variance
#' Inflation Factor (VIF).
#' @return A matrix with so many rows as effects in the model and the following columns:
#' \tabular{ll}{
#' \code{GVIF}            \tab the values of GVIF,\cr
#' \tab \cr
#' \code{df}              \tab the number of degrees of freedom,\cr
#' \tab \cr
#' \code{GVIF^(1/(2*df))} \tab the values of GVIF\eqn{^{1/2 df}},\cr
#' }
#' @method gvif lm
#' @export
#' @examples
#' ###### Example 1: New York air quality measurements
#' fit1 <- lm(log(Ozone) ~ Solar.R + Temp + Wind, data=airquality)
#' gvif(fit1)
#'
#' ###### Example 2: Fuel consumption of automobiles
#' fit2 <- lm(mpg ~ log(hp) + log(wt) + qsec, data=mtcars)
#' gvif(fit2)
#'
#' ###### Example 3: Credit card balance
#' Credit <- ISLR::Credit
#' fit3 <- lm(Balance ~ Cards + Age + Rating + Income + Student + Limit, data=Credit)
#' gvif(fit3)
#' @references Fox J., Monette G. (1992) Generalized collinearity diagnostics, \emph{JASA} 87, 178–183.
#' @seealso \link{gvif.glm}
gvif.lm <- function(model,verbose=TRUE,...){
  X <- model.matrix(model)
  postos <- model$assign
  vars <- attr(model$terms,"term.labels")
  vcovar <- vcov(model)
  if(names(coef(model))[1]=="(Intercept)"){
    vcovar <- vcovar[-1,-1]
    postos <- postos[-1]
  }
  nn <- max(postos)
  if(nn==1) stop("At least two effects are required to calculate GVIFs!!",call.=FALSE)
  results <- matrix(0,nn,3)
  cors <- cov2cor(vcovar)
  detx <- det(cors)
  for(i in 1:nn){
    rr2 <- as.matrix(cors[postos==i,postos==i])
    rr3 <- as.matrix(cors[postos!=i,postos!=i])
    results[i,1] <- round(det(rr2)*det(rr3)/detx,digits=4)
    results[i,2] <- ncol(rr2)
    results[i,3] <- round(results[i,1]^(1/(2*ncol(rr2))),digits=4)
  }
  rownames(results) <- vars
  colnames(results) <- c("GVIF", "df", "GVIF^(1/(2*df))")
  if(verbose) print(results)
  return(invisible(results))
}
#'
#' @title Generalized Variance Inflation Factor
#' @description Computes the generalized variance inflation factor (GVIF) for a generalized linear model.
#' @param model an object of the class \emph{glm}.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param ...	further arguments passed to or from other methods.
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
#' @method gvif glm
#' @export
#' @examples
#' ###### Example 1: Fuel consumption of automobiles
#' Auto <- ISLR::Auto
#' Auto2 <- within(Auto, origin <- factor(origin))
#' mod <- mpg ~ cylinders + displacement + acceleration + origin + horsepower*weight
#' fit1 <- glm(mod, family=inverse.gaussian("log"), data=Auto2)
#' gvif(fit1)
#'
#' ###### Example 2: Patients with burn injuries
#' burn1000 <- aplore3::burn1000
#' burn1000 <- within(burn1000, death <- factor(death, levels=c("Dead","Alive")))
#' mod2 <- death ~ gender + race + flame + age*inh_inj + tbsa*inh_inj
#' fit2 <- glm(mod2, family=binomial("logit"), data=burn1000)
#' gvif(fit2)
#'
#' ###### Example 3: Hill races in Scotland
#' data(races)
#' fit3 <- glm(rtime ~ log(distance) + cclimb, family=Gamma(log), data=races)
#' gvif(fit3)
#'
#' @references Fox J., Monette G. (1992) Generalized collinearity diagnostics, \emph{JASA} 87, 178–183.
#' @seealso \link{gvif.lm}
gvif.glm <- function(model,verbose=TRUE,...){
  X <- model.matrix(model)
  postos <- attr(X,"assign")
  vars <- attr(model$terms,"term.labels")
  vcovar <- vcov(model)
  if(names(coef(model))[1]=="(Intercept)"){
    vcovar <- vcovar[-1,-1]
    postos <- postos[-1]
  }
  nn <- max(postos)
  if(nn==1) stop("At least two effects are required to calculate GVIFs!!",call.=FALSE)
  results <- matrix(0,nn,3)
  cors <- cov2cor(vcovar)
  detx <- det(cors)
  for(i in 1:nn){
    rr2 <- as.matrix(cors[postos==i,postos==i])
    rr3 <- as.matrix(cors[postos!=i,postos!=i])
    results[i,1] <- round(det(rr2)*det(rr3)/detx,digits=4)
    results[i,2] <- ncol(rr2)
    results[i,3] <- round(results[i,1]^(1/(2*ncol(rr2))),digits=4)
  }
  rownames(results) <- vars
  colnames(results) <- c("GVIF", "df", "GVIF^(1/(2*df))")
  if(verbose) print(results)
  return(invisible(results))
}

#' @title Confidence Intervals for Generalized Linear Models
#' @description Computes confidence intervals based on Wald, likelihood-ratio, Rao's score or Terrell's gradient tests for a generalized linear model.
#' @param model an object of the class \emph{glm}.
#' @param test an (optional) character string indicating the required type of test. The available options are: Wald ("wald"), Rao's score ("score"), Terrell's gradient ("gradient"), and likelihood ratio ("lr") tests. As default, \code{test} is set to "wald".
#' @param digits an (optional) integer value indicating the number of decimal places to be used. As default, \code{digits} is set to \code{max(3, getOption("digits") - 2)}.
#' @param level an (optional) value indicating the required confidence level. As default, \code{level} is set to 0.95.
#' @param verbose an (optional) logical indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @details The approximate 100(\code{level})\% confidence interval for \eqn{\beta} based on the \code{test} test is the set of values of \eqn{\beta_0} for which the hypothesis \eqn{H_0}: \eqn{\beta=\beta_0} versus \eqn{H_1}: \eqn{\beta!=\beta_0} is not rejected at the approximate significance level of 100(1-\code{level})\%. The Wald, Rao's score and Terrell's gradient tests are performed using the expected Fisher information matrix.
#' @return A matrix with so many rows as parameters in the linear predictor and two columns: "Lower limit" and "Upper limit".
#' @references Buse A. (1982) The Likelihood Ratio, Wald, and Lagrange Multiplier Tests: An Expository Note. \emph{The American Statistician} 36, 153-157.
#' @references Terrell G.R. (2002) The gradient statistic. \emph{Computing Science and Statistics} 34, 206 – 215.
#' @export confint2
#' @examples
#' ###### Example 1: Fuel consumption of automobiles
#' Auto <- ISLR::Auto
#' fit1 <- glm(mpg ~ weight*horsepower, family=inverse.gaussian("log"), data=Auto)
#' confint2(fit1, test="lr")
#' confint2(fit1, test="score")
#'
#' ###### Example 2: Patients with burn injuries
#' burn1000 <- aplore3::burn1000
#' burn1000 <- within(burn1000, death <- factor(death, levels=c("Dead","Alive")))
#' fit2 <- glm(death ~ age*inh_inj + tbsa*inh_inj, family=binomial("logit"), data=burn1000)
#' confint2(fit2, test="lr")
#' confint2(fit2, test="gradient")
#'
confint2 <- function(model, level=0.95, test=c("wald","lr","score","gradient"), digits=max(3, getOption("digits") - 2), verbose=TRUE){
  test <- match.arg(test)
  if(class(model)[1]!="glm")
    stop("Only glm-type objects are supported!!",call.=FALSE)
  alpha <- 1 - level
  X <- model.matrix(model)
  p <- ncol(X)
  n <- nrow(X)
  ee <- sqrt(diag(vcov(model)))
  prefi <- "Approximate"
  if(test=="lr" | test=="score" | test=="gradient"){
    results <- matrix(0,p,2)
    phi <- summary(model)$dispersion
    for(i in 1:p){
      offset0 <- ifelse(is.null(model$offset),0,model$offset)
      Xs <- X[,-i]
      starts <- coef(model)[-i]
      f1 <- function(beta0){
        offsets <- offset0 + X[,i]*beta0
        if(p > 1) fit0s <- glm(model$y ~ -1 + Xs, family=model$family, offset=offsets, weights=model$prior.weights)
        if(p == 1) fit0s <- glm(model$y ~ -1, family=model$family, offset=offsets, weights=model$prior.weights)
        if(test=="lr") salida <- (fit0s$deviance - model$deviance)/phi - qchisq(1-alpha,1)
        if(test=="score"){
          ui <- (crossprod(X,resid(fit0s,type="pearson")*sqrt(fit0s$weights))/phi)[i]
          Xw <- X*matrix(sqrt(fit0s$weights),n,p)
          vi <- phi*solve(t(Xw)%*%Xw)[i,i]
          salida <- ui^2*vi - qchisq(1-alpha,1)
        }
        if(test=="gradient"){
          ui <- (crossprod(X,resid(fit0s,type="pearson")*sqrt(fit0s$weights))/phi)[i]
          salida <- abs(ui*(coef(model)[i]-beta0)) - qchisq(1-alpha,1)
        }
        salida
      }
      betas <- coef(model)[i]
      betai <- betas - 1.05*qnorm(1-alpha/2)*ee[i]
      while(f1(betai) <= 0) betai <- betas + 1.05*(betai - betas)
      results[i,1] <- uniroot(f1, lower=betai, upper=betas)$root
      betai <- coef(model)[i]
      betas <- betas + 1.05*qnorm(1-alpha/2)*ee[i]
      while(f1(betas) <= 0) betas <- betai + 1.05*(betas - betai)
      results[i,2] <- uniroot(f1, lower=betai, upper=betas)$root
    }
  }else{
    if(model$family$family=="gaussian" & model$family$link=="identity"){
      results <- cbind(coef(model) - qt(1-alpha/2,n-p)*ee,coef(model) + qt(1-alpha/2,n-p)*ee)
      prefi <- "Exact"
    }
    else
      results <- cbind(coef(model) - qnorm(1-alpha/2)*ee,coef(model) + qnorm(1-alpha/2)*ee)
  }
  rownames(results) <- colnames(X)
  colnames(results) <- c("Lower limit","Upper limit")
  if(verbose){
    test <- switch(test,"lr"="Likelihood-ratio test",
                   "wald"="Wald test",
                   "score"="Rao's score test",
                   "gradient"="Gradient test")
    cat("\n",prefi,round(100*(1-alpha),digits=1),"percent confidence intervals based on the",test,"\n")
    print(round(results,digits=digits))
  }
  return(invisible(results))
}

#' @title Residuals for Linear and Generalized Linear Models
#' @description Computes residuals for a fitted linear or generalized linear model.
#' @param object a object of the class \emph{lm} or \emph{glm}.
#' @param type an (optional) character string giving the type of residuals which should be returned. The available options for LMs are: (1) externally studentized ("external"); (2) internally studentized ("internal") (default). The available options for GLMs are: (1) "pearson"; (2) "deviance" (default);  (3) "quantile".
#' @param standardized an (optional) logical switch indicating if the residuals should be standardized by dividing by the square root of \eqn{(1-h)}, where \eqn{h} is a measure of leverage. As default, \code{standardized} is set to FALSE.
#' @param plot.it an (optional) logical switch indicating if a plot of the residuals versus the fitted values is required. As default, \code{plot.it} is set to FALSE.
#' @param identify an (optional) integer value indicating the number of individuals to identify on the plot of residuals. This is only appropriate when \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods
#' @return A vector with the observed residuals type \code{type}.
#' @references Atkinson A.C. (1985) \emph{Plots, Transformations and Regression}. Oxford University Press, Oxford.
#' @references Davison A.C., Gigli A. (1989) Deviance Residuals and Normal Scores Plots. \emph{Biometrika} 76, 211-221.
#' @references Dunn P.K., Smyth G.K. (1996) Randomized Quantile Residuals. \emph{Journal of Computational and Graphical Statistics} 5, 236-244.
#' @references Pierce D.A., Schafer D.W. (1986) Residuals in Generalized Linear Models. \emph{Journal of the American Statistical Association} 81, 977-986.
#' @examples
#' ###### Example 1: Species richness in plots
#' data(richness)
#' fit1 <- lm(Species ~ Biomass + pH, data=richness)
#' residuals2(fit1, type="external", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'            col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 2: Lesions of Aucuba mosaic virus
#' data(aucuba)
#' fit2 <- glm(lesions ~ time, family=poisson, data=aucuba)
#' residuals2(fit2, type="quantile", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'            col.axis="blue",col.main="black",family="mono",cex=0.8)
#' @export residuals2
residuals2 <- function(object,type,standardized=FALSE,plot.it=FALSE,identify,...){
  if(class(object)[1]!="glm" & class(object)[1]!="lm") stop("Only lm- and glm-type objects are supported!!",call.=FALSE)
  if(class(object)[1]=="glm" & missingArg(type)) type <- "deviance"
  if(class(object)[1]=="lm" & missingArg(type)) type <- "internal"
  if(class(object)[1]=="glm" & type!="quantile" & type!="pearson" & type!="deviance")
    stop("Only quantile-, pearson- and deviance-type residuals are supported for glm-type objects !!",call.=FALSE)
  if(class(object)[1]=="lm" & type!="internal" & type!="external")
    stop("Only internal-, and external-type residuals are supported for lm-type objects !!",call.=FALSE)
  if(class(object)[1]=="glm" & type=="quantile")
    if(object$family$family %in% c("quasi","quasibinomial","quasipoisson"))
      stop("Quantile-type residuals are not supported for quasi-likelihood models !!",call.=FALSE)
  if(class(object)[1]=="glm"){
    quantileres <- function(family,y,mu,phi){
      resi <- switch(family,
                     Gamma = pgamma(y,shape=1/phi,scale=mu*phi),
                     inverse.gaussian = pnorm((y/mu-1)/sqrt(phi*y)) + exp(2/(mu*phi))*pnorm(-(y/mu+1)/sqrt(y*phi)),
                     gaussian = pnorm((y-mu)/sqrt(phi)),
                     poisson = ppois(y-1,lambda=mu) + dpois(y,lambda=mu)*runif(length(mu)),
                     binomial = pbinom(y/phi-1,size=1/phi,prob=mu) + dbinom(y/phi,size=1/phi,prob=mu)*runif(length(mu)))
      return(qnorm(ifelse(ifelse(resi<1e-16,1e-16,resi)>1-(1e-16),1-(1e-16),resi)))
    }
    phi <- summary(object)$dispersion
    if(type=="quantile")
      rd <- quantileres(object$family$family,object$y,object$fitted.values,phi/object$prior.weights)
    else rd <- residuals(object,type=type)/sqrt(phi)
    if(standardized){
      X <- model.matrix(object)
      Xw <- X*matrix(sqrt(object$weights),nrow(X),ncol(X))
      salida <- svd(Xw)
      h <- apply(salida$u^2,1,sum)
      rd <- rd/sqrt(1-h)
    }
  }
  if(class(object)[1]=="lm"){
    X <- model.matrix(object)
    if(is.null(object$weights)) weights <- rep(1,nrow(X)) else weights <- object$weights
    Xw <- X*matrix(sqrt(weights),nrow(X),ncol(X))
    salida <- svd(Xw)
    h <- apply(salida$u^2,1,sum)
    sigma2 <- summary(object)$sigma^2
    rd <- residuals(object,type="response")/sqrt(sigma2*(1-h))
    if(type=="external") rd <- rd*sqrt((nrow(X)-ncol(X)-1)/(nrow(X)-ncol(X)-rd^2))
  }
  if(plot.it){
    nano <- list(...)
    nano$x <- object$fitted.values
    nano$y <- rd
    if(is.null(nano$ylim)) nano$ylim <- c(min(-3.5,min(rd)),max(+3.5,max(rd)))
    if(is.null(nano$xlab)) nano$xlab <- "Fitted values"
    if(class(object)[1]=="lm") nano$ylab <- paste0(type,"lly studentized residual",sep="")
    else if(is.null(nano$ylab)) nano$ylab <- paste(type," - type residual",sep="")
    if(is.null(nano$pch))  nano$pch  <- 20
    if(is.null(nano$labels))  labels <- 1:length(rd)
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    do.call("plot",nano)
    abline(h=-3,lty=3)
    abline(h=+3,lty=3)
    if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=labels)
  }
  rd <- as.matrix(rd)
  colnames(rd) <- type
  return(invisible(rd))
}

#' @title Variable Selection in Generalized Linear Models
#' @description Performs variable selection in generalized linear models using hybrid versions of forward stepwise and backward stepwise.
#' @param model an object of the class \emph{glm}.
#' @param direction an (optional) character string indicating the type of procedure which should be used. The available options are: hybrid backward stepwise ("backward") and hybrid forward stepwise ("forward"). As default, \code{direction} is set to "forward".
#' @param levels an (optional) two-dimensional vector of values in the interval \eqn{(0,1)} indicating the levels at which the variables should in and out from the model. This is only appropiate if \code{criterion}="p-value". As default, \code{levels} is set to \code{c(0.05,0.05)}.
#' @param test an (optional) character string indicating the statistical test which should be used to compare nested models. The available options are: Wald ("wald"), Rao's score ("score"), likelihood-ratio ("lr") and gradient ("gradient") tests. As default, \code{test} is set to "wald".
#' @param criterion an (optional) character string indicating the criterion which should be used to compare the candidate models. The available options are: AIC ("aic"), BIC ("bic"), adjusted deviance-based R-squared ("adjr2"), and \emph{p}-value of the \code{test} test ("p-value"). As default, \code{criterion} is set to "adjr2".
#' @param ...	further arguments passed to or from other methods. For example, \code{k}, that is, the magnitude of the penalty in the AIC/QICu, which by default is set to 2.
#' @param trace an (optional) logical switch indicating if should the stepwise reports be printed. As default, \code{trace} is set to TRUE.
#' @param scope an (optional) list, containing components \code{lower} and \code{upper}, both formula-type objects, indicating the range of models which should be examined in the stepwise search. As default, \code{lower} is a model with no predictors and \code{upper} is the linear predictor of the model in \code{model}.
#' @details The "hybrid forward stepwise" algorithm starts with the simplest model (which may
#' be chosen at the argument \code{scope}, and As default, is a model whose parameters in the
#' linear predictor, except the intercept, if any, are set to 0), and then the candidate
#' models are built by hierarchically including effects in the linear predictor, whose
#' "relevance" and/or "importance" in the model fit is assessed by comparing nested models
#' (that is, by comparing the models with and without the added effect) using a criterion
#' previously specified. If an effect is added to the equation, this strategy may also remove
#' any effect which, according to the previously specified criterion, no longer provides
#' improvement in the model fit. That process continues until no more effects are included
#' or excluded. The "hybrid backward stepwise" algorithm works similarly.
#'
#' @return a list list with components including
#' \tabular{ll}{
#' \code{initial} \tab  a character string indicating the linear predictor of the "initial model",\cr
#' \tab \cr
#' \code{direction} \tab  a character string indicating the type of procedure which was used,\cr
#' \tab \cr
#' \code{criterion} \tab a character string indicating the criterion used to compare the candidate models,\cr
#' \tab \cr
#' \code{final} \tab a character string indicating the linear predictor of the "final model",\cr
#' }
#' @seealso \link{stepCriterion.lm}, \link{stepCriterion.overglm}, \link{stepCriterion.glmgee}
#' @examples
#' ###### Example 1: Fuel consumption of automobiles
#' Auto <- ISLR::Auto
#' Auto2 <- within(Auto, origin <- factor(origin))
#' mod <- mpg ~ cylinders + displacement + acceleration + origin + horsepower*weight
#' fit1 <- glm(mod, family=inverse.gaussian("log"), data=Auto2)
#' stepCriterion(fit1, direction="forward", criterion="p-value", test="lr")
#' stepCriterion(fit1, direction="backward", criterion="bic")
#'
#' ###### Example 2: Patients with burn injuries
#' burn1000 <- aplore3::burn1000
#' burn1000 <- within(burn1000, death <- factor(death, levels=c("Dead","Alive")))
#' upper <- ~ age + gender + race + tbsa + inh_inj + flame + age*inh_inj + tbsa*inh_inj
#' lower <- ~ 1
#' fit2 <- glm(death ~ age + gender + race + tbsa + inh_inj, family=binomial("logit"), data=burn1000)
#' stepCriterion(fit2, direction="backward", criterion="bic", scope=list(lower=lower,upper=upper))
#' stepCriterion(fit2, direction="forward", criterion="p-value", test="score")
#'
#' ###### Example 3: Skin cancer in women
#' data(skincancer)
#' upper <- cases ~ city + age + city*age
#' fit3 <- glm(upper, family=poisson("log"), offset=log(population), data=skincancer)
#' stepCriterion(fit3, direction="backward", criterion="aic", scope=list(lower=~ 1,upper=upper))
#' stepCriterion(fit3, direction="forward", criterion="p-value", test="lr")
#' @method stepCriterion glm
#' @export
#' @references James G., Witten D., Hastie T., Tibshirani R. (2013, page 210) An Introduction to Statistical Learning with Applications in R, Springer, New York.

stepCriterion.glm <- function(model, criterion=c("adjr2","bic","aic","p-value","qicu"), test=c("wald","lr","score","gradient"), direction=c("forward","backward"), levels=c(0.05,0.05), trace=TRUE, scope, ...){
  xxx <- list(...)
  if(is.null(xxx$k)) k <- 2 else k <- xxx$k
  criterion <- match.arg(criterion)
  direction <- match.arg(direction)
  test <- match.arg(test)
  if(test=="wald") test2 <- "Wald test"
  if(test=="score") test2 <- "Rao's score test"
  if(test=="lr") test2 <- "likelihood-ratio test"
  if(test=="gradient") test2 <- "Gradient test"

  quasi <- FALSE
  if(model$family$family %in% c("quasi","quasibinomial","quasipoisson")){
    quasi <- TRUE
    if(model$family$family=="quasi"){
      familia <- switch(model$family$varfun,"constant"="gaussian","mu(1-mu)"="binomial",
                        "mu"="poisson","mu^2"="Gamma","mu^3"="inverse.gaussian")
    }else familia <- switch(model$family$family,"quasibinomial"="binomial","quasipoisson"="poisson")
  }
  if(quasi & criterion %in% c("aic","bic")) stop("The AIC and BIC are not supported for quasi-likelihood models!!",call.=FALSE)
  if(!quasi & criterion=="qicu") stop("The QICu is not supported for likelihood-based models!!",call.=FALSE)
  if(quasi & criterion=="qicu") criterion <- "aic"

  quasilik <- function(familia,modelo){
    mu <- modelo$fitted.values
    y <- model$y
    weights <- model$prior.weights
    phi <- sum((y-mu)^2*weights/model$family$variance(mu))/modelo$df.residual
    if(familia=="gaussian") qll <- sum(-weights*(y - mu)^2/2)
    if(familia=="binomial") qll <- sum(weights*(y*log(mu) + (1-y)*log(1-mu)))
    if(familia=="poisson") qll <- sum(weights*(y*log(mu) - mu))
    if(familia=="Gamma") qll <- sum(-weights*(y/mu + log(mu)))
    if(familia=="inverse.gaussian") qll <- sum(weights*(mu - y/2)/mu^2)
    return(qll/phi)
  }
  tests <- function(fitred,fitcom,type){
    phi <- sum((model$y-fitcom$fitted.values)^2*model$prior.weights/model$family$variance(fitcom$fitted.values))
    phi <- phi/fitcom$df.residual
    if(fitcom$family$family %in% c("poisson","binomial")) phi <- 1
    if(type=="lr") sc <- (fitred$deviance - fitcom$deviance)/phi
    cuales <- is.na(match(names(fitcom$coefficients),names(fitred$coefficients)))
    if(type=="wald"){
      vcovar <- phi*chol2inv(fitcom$R)
      sc <- t(fitcom$coefficients[cuales])%*%chol2inv(chol(vcovar[cuales,cuales]))%*%fitcom$coefficients[cuales]
    }
    if(type=="score"){
      X <- qr.X(fitcom$qr)/sqrt(fitcom$weights)
      Xw <- X*matrix(sqrt(fitred$weights),nrow(X),ncol(X))
      vcovar0 <- phi*chol2inv(chol(t(Xw)%*%Xw))
      U0 <- t(Xw)%*%((model$y-fitred$fitted.values)*sqrt(model$prior.weights/model$family$variance(fitred$fitted.values)))/phi
      sc <- t(U0[cuales])%*%vcovar0[cuales,cuales]%*%U0[cuales]
    }
    if(type=="gradient"){
      X <- qr.X(fitcom$qr)/sqrt(fitcom$weights)
      Xw <- X*matrix(sqrt(fitred$weights),nrow(X),ncol(X))
      U0 <- t(Xw)%*%((model$y-fitred$fitted.values)*sqrt(model$prior.weights/model$family$variance(fitred$fitted.values)))/phi
      sc <- abs(t(U0[cuales])%*%fitcom$coefficients[cuales])
    }
    return(sc)
  }

  criters <- c("aic","bic","adjr2","p-value")
  criters2 <- c("AIC","BIC","adj.R-squared","P(Chisq>)(*)")
  sentido <- c(1,1,-1,1)
  if(names(coef(model))[1]!="(Intercept)") sentido[3] <- 1
  pen <- attr(logLik(model),"df")-length(model$coefficients)
  if(missingArg(scope)){
    upper <- formula(eval(model$call$formula))
    lower <- formula(eval(model$call$formula))
    lower <- formula(paste(deparse(lower[[2]]),"~",attr(terms(lower,data=eval(model$call$data)),"intercept")))
  }else{
    lower <- scope$lower
    upper <- scope$upper
  }
  if(is.null(model$call$data)) datas <- get_all_vars(upper,environment(eval(model$call$formula)))
  else datas <- get_all_vars(upper,eval(model$call$data))
  if(!is.null(model$call$subset)) datas <- datas[eval(model$call$subset,datas),]
  datas <- na.omit(datas)

  U <- attr(terms(upper,data=datas),"term.labels")
  U <- lapply(lapply(strsplit(U,":"),sort),paste,collapse=":")
  fs <- attr(terms(upper,data=datas),"factors")
  long <- max(nchar(U)) + 2
  nonename <- paste("<none>",paste(replicate(max(long-6,0)," "),collapse=""),collapse="")
  cambio <- ""
  paso <- 1
  tol <- TRUE
  if(trace){
    if(quasi){
      if(model$family$family!="quasi") model$family$varfun <- switch(model$family$family,"quasibinomial"="mu(1-mu)","quasipoisson"="mu")
      if(model$family$varfun!="constant") cat("     Variance:  proportional to",model$family$varfun,"\n")
      else cat("     Variance: ",model$family$varfun,"\n")
    }else cat("\n       Family: ",model$family$family,"\n")
  cat("Link function: ",model$family$link,"\n")
  cat("    Criterion: ",criters2[criters==criterion],"\n")
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
      X <- model.matrix(oldformula,data=datas)
      fit.x <- glm.fit(y=model$y,x=X,family=model$family,offset=model$offset,weights=model$prior.weights)
      S <- attr(terms(oldformula),"term.labels")
      S <- lapply(lapply(strsplit(S,":"),sort),paste,collapse=":")
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
            X <- model.matrix(newformula,data=datas)
            fit.0 <- glm.fit(y=model$y,x=X,family=model$family,offset=model$offset,weights=model$prior.weights)
            fsalen[i,1] <- fit.0$df.residual - fit.x$df.residual
            fsalen[i,5] <- tests(fit.0,fit.x,type=test)
            fsalen[i,5] <- sqrt(9*fsalen[i,1]/2)*((fsalen[i,5]/fsalen[i,1])^(1/3) - 1 + 2/(9*fsalen[i,1]))
            fsalen[i,2] <- ifelse(quasi,-2*quasilik(familia,fit.0) + k*length(fit.0$coefficients),fit.0$aic + (k-2)*(length(fit.0$coefficients)+pen))
            fsalen[i,3] <- fsalen[i,2] + (log(length(fit.0$y))-k)*(length(fit.0$coefficients)+pen)
            fsalen[i,4] <- fit.0$deviance/fit.0$df.residual
            if(names(coef(model))[1]=="(Intercept)") fsalen[i,4] <- 1 - fsalen[i,4]/(model$null.deviance/model$df.null)
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
            aic <- ifelse(quasi,-2*quasilik(familia,fit.x) + k*length(fit.x$coefficients),fit.x$aic + (k-2)*(length(fit.x$coefficients)+pen))
            none <- c(aic,aic + (log(length(fit.x$y))-k)*(length(fit.x$coefficients)+pen),fit.x$deviance/fit.x$df.residual)
            if(names(coef(model))[1]=="(Intercept)") none[3] <- 1 - none[3]/(model$null.deviance/model$df.null)
            fsalen <- rbind(fsalen,c(NA,none,NA))
            rownames(fsalen)[nrow(fsalen)] <- nonename
            if(quasi){
              fsalen <- fsalen[,-3]
              colnames(fsalen)[2] <- "QICu"
            }
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
            X <- model.matrix(newformula,data=datas)
            fit.0 <- glm.fit(y=model$y,x=X,family=model$family,offset=model$offset,weights=model$prior.weights)
            fentran[i,1] <- fit.x$df.residual - fit.0$df.residual
            fentran[i,5] <- tests(fit.x,fit.0,type=test)
            fentran[i,5] <- sqrt(9*fentran[i,1]/2)*((fentran[i,5]/fentran[i,1])^(1/3) - 1 + 2/(9*fentran[i,1]))
            fentran[i,2] <- ifelse(quasi,-2*quasilik(familia,fit.0) + k*length(fit.0$coefficients),fit.0$aic + (k-2)*(length(fit.0$coefficients)+pen))
            fentran[i,3] <- fentran[i,2] + (log(length(fit.0$y))-k)*(length(fit.0$coefficients)+pen)
            fentran[i,4] <- fit.0$deviance/fit.0$df.residual
            if(names(coef(model))[1]=="(Intercept)") fentran[i,4] <- 1 - fentran[i,4]/(model$null.deviance/model$df.null)
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
          pen <- length(fit.x$coefficients) + attr(logLik(model),"df")-length(model$coefficients)
          aic <- ifelse(quasi,-2*quasilik(familia,fit.x) + k*length(fit.x$coefficients),fit.x$aic + (k-2)*(length(fit.x$coefficients)+pen))
          none <- c(aic,aic + (log(length(fit.x$y))-k)*(length(fit.x$coefficients)+pen),fit.x$deviance/fit.x$df.residual)
          if(names(coef(model))[1]=="(Intercept)") none[3] <- 1 - none[3]/(model$null.deviance/model$df.null)
          fentran <- rbind(fentran,c(NA,none,NA))
          rownames(fentran)[nrow(fentran)] <- nonename
          if(quasi){
            fentran <- fentran[,-3]
            colnames(fentran)[2] <- "QICu"
          }
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
        aic <- ifelse(quasi,-2*quasilik(familia,fit.x) + k*length(fit.x$coefficients),fit.x$aic + (k-2)*(length(fit.x$coefficients)+pen))
        none <- c(aic,aic + (log(length(fit.x$y))-k)*(length(fit.x$coefficients)+pen),fit.x$deviance/fit.x$df.residual)
        if(names(coef(model))[1]=="(Intercept)") none[3] <- 1 - none[3]/(model$null.deviance/model$df.null)
        fentran <- rbind(fentran,c(0,none,0))
        rownames(fentran)[nrow(fentran)] <- nonename
        ids <- criters == criterion
        colnames(fentran) <- c("df",criters2)
        fentran <- na.omit(fentran)
        attr(fentran,"na.action")	<- attr(fentran,"class") <- NULL
        fentran[nrow(fentran),c(1,5)] <- NA
        fentran <- fentran[order(sentido[ids]*fentran[,c(FALSE,ids)]),]
        if(rownames(fentran)[1]!=nonename){
          if(quasi){
            fentran <- fentran[,-3]
            colnames(fentran)[2] <- "QICu"
          }
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
          if(quasi){
            fentran <- fentran[,-3]
            colnames(fentran)[2] <- "QICu"
          }
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
      X <- model.matrix(oldformula,data=datas)
      fit.x <- glm.fit(y=model$y,x=X,family=model$family,offset=model$offset,weights=model$prior.weights)
      S <- attr(terms(oldformula),"term.labels")
      S <- lapply(lapply(strsplit(S,":"),sort),paste,collapse=":")
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
            X <- model.matrix(newformula,data=datas)
            fit.0 <- glm.fit(y=model$y,x=X,family=model$family,offset=model$offset,weights=model$prior.weights)
            fentran[i,1] <- fit.x$df.residual - fit.0$df.residual
            fentran[i,5] <- tests(fit.x,fit.0,type=test)
            fentran[i,5] <- sqrt(9*fentran[i,1]/2)*((fentran[i,5]/fentran[i,1])^(1/3) - 1 + 2/(9*fentran[i,1]))
            fentran[i,2] <- ifelse(quasi,-2*quasilik(familia,fit.0) + k*length(fit.0$coefficients),fit.0$aic + (k-2)*(length(fit.0$coefficients)+pen))
            fentran[i,3] <- fentran[i,2] + (log(length(fit.0$y))-k)*(length(fit.0$coefficients)+pen)
            fentran[i,4] <- fit.0$deviance/fit.0$df.residual
            if(names(coef(model))[1]=="(Intercept)") fentran[i,4] <- 1 - fentran[i,4]/(model$null.deviance/model$df.null)
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
            aic <- ifelse(quasi,-2*quasilik(familia,fit.x) + k*length(fit.x$coefficients),fit.x$aic + (k-2)*(length(fit.x$coefficients)+pen))
            none <- c(aic,aic + (log(length(fit.x$y))-k)*(length(fit.x$coefficients)+pen),fit.x$deviance/fit.x$df.residual)
            if(names(coef(model))[1]=="(Intercept)") none[3] <- 1 - none[3]/(model$null.deviance/model$df.null)
            fentran <- rbind(fentran,c(NA,none,NA))
            rownames(fentran)[nrow(fentran)] <- nonename
            if(quasi){
              fentran <- fentran[,-3]
              colnames(fentran)[2] <- "QICu"
            }
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
            X <- model.matrix(newformula,data=datas)
            fit.0 <- glm.fit(y=model$y,x=X,family=model$family,offset=model$offset,weights=model$prior.weights)
            fsalen[i,1] <- fit.0$df.residual - fit.x$df.residual
            fsalen[i,5] <- tests(fit.0,fit.x,type=test)
            fsalen[i,5] <- sqrt(9*fsalen[i,1]/2)*((fsalen[i,5]/fsalen[i,1])^(1/3) - 1 + 2/(9*fsalen[i,1]))
            fsalen[i,2] <- ifelse(quasi,-2*quasilik(familia,fit.0) + k*length(fit.0$coefficients),fit.0$aic + (k-2)*(length(fit.0$coefficients)+pen))
            fsalen[i,3] <- fsalen[i,2] + (log(length(fit.0$y))-k)*(length(fit.0$coefficients)+pen)
            fsalen[i,4] <- fit.0$deviance/fit.0$df.residual
            if(names(coef(model))[1]=="(Intercept)") fsalen[i,4] <- 1 - fsalen[i,4]/(model$null.deviance/model$df.null)
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
          aic <- ifelse(quasi,-2*quasilik(familia,fit.x) + k*length(fit.x$coefficients),fit.x$aic + (k-2)*(length(fit.x$coefficients)+pen))
          none <- c(aic,aic + (log(length(fit.x$y))-k)*(length(fit.x$coefficients)+pen),fit.x$deviance/fit.x$df.residual)
          if(names(coef(model))[1]=="(Intercept)") none[3] <- 1 - none[3]/(model$null.deviance/model$df.null)
          fsalen <- rbind(fsalen,c(NA,none,NA))
          rownames(fsalen)[nrow(fsalen)] <- nonename
          if(quasi){
            fsalen <- fsalen[,-3]
            colnames(fsalen)[2] <- "QICu"
          }
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
        aic <- ifelse(quasi,-2*quasilik(familia,fit.x) + k*length(fit.x$coefficients),fit.x$aic + (k-2)*(length(fit.x$coefficients)+pen))
        none <- c(aic,aic + (log(length(fit.x$y))-k)*(length(fit.x$coefficients)+pen),fit.x$deviance/fit.x$df.residual)
        if(names(coef(model))[1]=="(Intercept)") none[3] <- 1 - none[3]/(model$null.deviance/model$df.null)
        fsalen <- rbind(fsalen,c(0,none,0))
        rownames(fsalen)[nrow(fsalen)] <- nonename
        ids <- criters == criterion
        colnames(fsalen) <- c("df",criters2)
        fsalen <- na.omit(fsalen)
        attr(fsalen,"na.action")	<- attr(fsalen,"class") <- NULL
        fsalen[nrow(fsalen),c(1,5)] <- NA
        fsalen <- fsalen[order(sentido[ids]*fsalen[,c(FALSE,ids)]),]
        if(rownames(fsalen)[1]!=nonename){
          if(quasi){
            fsalen <- fsalen[,-3]
            colnames(fsalen)[2] <- "QICu"
          }
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
          if(quasi){
            fsalen <- fsalen[,-3]
            colnames(fsalen)[2] <- "QICu"
          }
          if(trace) printCoefmat(fsalen,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=2:(ncol(fsalen)-2),
                                 signif.stars=FALSE,tst.ind=ncol(fsalen)-1,dig.tst=4,digits=5)
        }
      }
      if(length(salen) == 0 & menos) tol <- FALSE
    }
  }
  if(trace){
    cat("\nFinal model:\n")
    cat(paste("~",as.character(oldformula)[length(oldformula)],sep=" "),"\n\n")
    cat("****************************************************************************")
    cat("\n(*) p-values of the",test2)
    if(criterion=="p-value"){
      cat("\n Effects are included when their p-values are lower than",levels[1])
      cat("\n Effects are dropped when their p-values are higher than",levels[2])
    }
    if(!is.null(xxx$k)) cat("The magnitude of the penalty in the AIC/QICu was set to ",xxx$k)
    cat("\n")
  }
  out_$final <- paste("~",as.character(oldformula)[length(oldformula)],sep=" ")
  return(invisible(out_))
}
#' @title Local Influence for Generalized Linear Models
#' @description Computes some measures and, optionally, display	graphs of them to perform
#' influence analysis based on the approaches described by Cook (1986).
#' @param object an object of class \emph{glm}.
#' @param type an (optional) character string indicating the type of approach to study the
#' local influence. The options are: the absolute value of the elements of the eigenvector which corresponds to the maximum absolute eigenvalue ("local"); and the absolute value of the elements of the main diagonal ("total"). As default, \code{type} is set to "total".
#' @param perturbation an (optional) character string indicating the perturbation scheme
#' to apply. The options are: case weight perturbation of observations ("case-weight"); perturbation of covariates ("covariate"); and perturbation of response ("response"). As default, \code{perturbation} is set to "case-weight".
#' @param plot.it an (optional) logical indicating if the plot of the measures of local
#' influence is required or just the data matrix in which that plot is based. By default,
#' \code{plot.it} is set to FALSE.
#' @param covariate an character string which (partially) match with the names of one of
#' the parameters in the linear predictor. This is only appropriate if \code{perturbation="covariate"}.
#' @param coefs	an (optional) character string which (partially) match with the names of
#' some of the parameters in the linear predictor.
#' @param identify an (optional) integer indicating the number of observations to identify
#' on the plot of the measures of local influence. This is only appropriate if
#' \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE}
#' then \code{...} may be used to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix as many rows as observations in the sample and one column with the values of the measures of local influence.
#' @method localInfluence glm
#' @export
#' @references Cook D. (1986) Assessment of Local Influence. \emph{Journal of the Royal Statistical Society: Series B (Methodological)} 48, 133-155.
#' @references Thomas W., Cook D. (1989) Assessing Influence on Regression Coefficients in Generalized Linear Models. \emph{Biometrika} 76, 741-749.
localInfluence.glm <- function(object,type=c("total","local"),perturbation=c("case-weight","response","covariate"),covariate,coefs,plot.it=FALSE,identify,...){
  type <- match.arg(type)
  perturbation <- match.arg(perturbation)
  if(perturbation=="response" & object$family$family %in% c("binomial","poisson")) warning("response perturbation scheme is not appropiate for discrete response models!!",call.=FALSE)
  beta <- object$coefficients
  phi <- summary(object)$dispersion
  X <- model.matrix(object)
  p <- ncol(X)
  n <- nrow(X)
  y <- object$y
  w <- object$prior.weights
  if(is.null(object$offset)) eta <- X%*%beta
  else eta <- X%*%beta + object$offset
  mu <- object$family$linkinv(eta)
  vmu <- object$family$variance(mu)
  subst <- NULL
  if(!missingArg(coefs)){
    ids <- grepl(coefs,rownames(object$coefficients),ignore.case=TRUE)
    if(sum(ids) > 0) subst <- rownames(object$coefficients)[ids]
    else stop(paste("There are no coefficients with the name",coefs,collapse=""),call.=FALSE)
  }
  if(perturbation=="covariate"){
    if(missingArg(covariate)) stop("Under this perturbation scheme a covariate should be specified!!",call.=FALSE)
    covar <- grep(covariate,names(coef(object)),ignore.case=TRUE)[1]
    if(is.na(covar)) stop(paste("The covariate '",covariate,"' was not found!!",sep=""),call.=FALSE)
    Xr <- matrix(0,n,p);Xr[,covar] <- rep(1,n)
    br <- coef(object)[covar]
  }
  kp1 <- matrix(object$family$mu.eta(eta),n,1)
  kp2 <- grad(object$family$mu.eta,eta)
  vp <- grad(object$family$variance,mu)
  vmu <- object$family$variance(mu)
  Xw <- X*matrix(w*(vmu*((y - mu)*kp2 - kp1^2) - (y - mu)*kp1^2*vp)/vmu^2,n,p)
  Qpp <- -t(X)%*%Xw
  if(perturbation=="case-weight") Delta <- matrix(w*(y - mu)*kp1/vmu,n,p)*X
  if(perturbation=="response") Delta <- matrix(kp1/sqrt(vmu/w),n,p)*X
  if(perturbation=="covariate") Delta <- Xw*br + matrix(w*(y - mu)*kp1/vmu,n,p)*Xr

  Qpp2 <- try(chol(Qpp),silent=TRUE)
  if(is.matrix(Qpp2)) Qpp2 <- chol2inv(Qpp2) else Qpp2 <- solve(Qpp)
  if(!is.null(subst)) Qpp2[-ids,-ids] <- Qpp2[-ids,-ids] - solve(Qpp[-ids,-ids])
  li <- tcrossprod(Delta,t(Qpp2))
  if(type=="local"){
    tol <- 1
    bnew <- matrix(rnorm(nrow(li)),nrow(li),1)
    while(tol > 0.000001){
      bold <- bnew
      bnew <- tcrossprod(li,t(crossprod(Delta,bold)))
      bnew <- bnew/sqrt(sum(bnew^2))
      tol <- max(abs((bnew - bold)/bold))
    }
    out_ <- abs(bnew/sqrt(sum(bnew^2)))
  }else out_ <- apply(li*Delta,1,sum)
  out_ <- matrix(out_,nrow=length(out_))
  rownames(out_) <- 1:n
  colnames(out_) <- type
  if(plot.it){
    nano <- list(...)
    nano$x <- 1:length(out_)
    nano$y <- out_
    if(is.null(nano$xlab)) nano$xlab <- "Observation Index"
    if(is.null(nano$type)) nano$type <- "h"
    if(is.null(nano$ylab)) nano$ylab <- ifelse(type=="local",expression(d[max]),expression(diag[i]))
    if(is.null(nano$main) & perturbation=="covariate") nano$main <- names(coef(object))[covar]
    if(is.null(nano$labels)) labels <- 1:n
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    do.call("plot",nano)
    if(any(out_>0)) abline(h=3*mean(out_[out_>0]),lty=3)
    if(any(out_<0)) abline(h=3*mean(out_[out_<0]),lty=3)
    if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=labels)
  }
  if(!is.null(subst))
    message("The coefficients included in the measures of local influence are: ",paste(subst,sep=""),"\n")
  return(invisible(out_))
}

#' @title Box-Tidwell transformations
#' @description Computes the Box-Tidwell power transformations of the predictors in a regression model.
#' @param object a model fit object.
#' @param transf an one-sided formula giving the predictors that are candidates for transformation.
#' @param epsilon an (optional) numerical value. If the maximum relative change in coefficients is less than
#'                \emph{epsilon}, then convergence is declared. As default, \emph{epsilon} is set to 0.0001.
#' @param maxiter an (optional) positive integer value indicating the maximum number of iterations. By default,
#'                \emph{maxiter} is set to 30.
#' @param trace an (optional) logical indicating if should the record of iterations be printed. By default,
#'                \emph{trace} is set to FALSE.
#' @param digits an (optional) integer value indicating the number of decimal places to be used.
#' @param ...	further arguments passed to or from other methods.
#' @return Two matrices with the values of marginal and omnibus tests.
#' @export BoxTidwell
BoxTidwell <- function(object,transf,epsilon=0.0001,maxiter=30,trace=FALSE,digits=max(3, getOption("digits") - 2),...){
  UseMethod("BoxTidwell")
}


#' @title Box-Tidwell transformations in Normal Linear Models
#' @description Computes the Box-Tidwell power transformations of the predictors in a normal linear model.
#' @param object an object of the class \emph{lm}.
#' @param transf an one-sided formula giving the quantitative predictors that are candidates for transformation.
#' @param epsilon an (optional) numerical value. If the maximum relative change in coefficients is less than
#'                \emph{epsilon}, then convergence is declared. As default, \emph{epsilon} is set to 0.0001.
#' @param maxiter an (optional) positive integer value indicating the maximum number of iterations. By default,
#'                \emph{maxiter} is set to 30.
#' @param trace an (optional) logical indicating if should the record of iterations be printed. By default,
#'              \emph{trace} is set to FALSE.
#' @param digits an (optional) integer value indicating the number of decimal places to be used.
#' @param ...	further arguments passed to or from other methods.
#' @return a list list with components including
#' \tabular{ll}{
#' \code{marginal} \tab a matrix with estimates and standard errors of the estimated powers, as well as the statistic
#'                       and the p-value of the Wald test to assess the hypothesis \eqn{H_0:\tau=1} versus \eqn{H_1:\tau\neq 1},\cr
#' \tab \cr
#' \code{omnibus} \tab a matrix with the statistic and the p-value of the Wald test for null hypothesis that all powers
#'                      are 1,\cr
#' }
#' @references Box G.E.P., Tidwell P.W. (1962) Transformation of the independent variables. \emph{Technometrics} 4, 531-550.
#' @references Fox J. (2016) \emph{Applied Regression Analysis and Generalized Linear Models}, Third Edition. Sage.
#' @seealso \link{BoxTidwell.glm}
#' @method BoxTidwell lm
#' @export
#' @examples
#' ###### Example 1: Hill races in Scotland
#' data(races)
#' fit1 <- lm(rtime ~ distance + cclimb, data=races)
#' AIC(fit1)
#' BoxTidwell(fit1, transf= ~ distance + cclimb)
#' fit1 <- update(fit1,formula=rtime ~ distance + I(cclimb^2))
#' AIC(fit1)
#'
#' ###### Example 2: Gasoline yield
#' fit2 <- lm(mpg ~ hp + wt + am, data=mtcars)
#' AIC(fit2)
#' BoxTidwell(fit2, transf= ~ hp + wt)
#' fit2 <- update(fit2,formula=mpg ~ log(hp) + log(wt) + am)
#' AIC(fit2)
#'
#' ###### Example 3: New York Air Quality Measurements
#' fit3 <- lm(log(Ozone) ~ Solar.R + Wind + Temp, data=airquality)
#' AIC(fit3)
#' BoxTidwell(fit3, transf= ~ Solar.R + Wind + Temp)
#' fit3 <- update(fit3,formula=log(Ozone) ~ log(Solar.R) + Wind + Temp)
#' AIC(fit3)
#'
#' ###### Example 4: Heat capacity of hydrobromic acid
#' data(heatcap,package="GLMsData")
#' fit4 <- lm(Cp ~ Temp, data=heatcap)
#' AIC(fit4)
#' BoxTidwell(fit4, transf= ~ Temp)
#' fit4 <- update(fit4,formula=Cp ~ I(Temp^5))
#' AIC(fit4)
#'
#' ###### Example 5: Age and Eye Lens Weight of Rabbits in Australia
#' data(rabbits)
#' fit5 <- lm(log(wlens) ~ age, data=rabbits)
#' AIC(fit5)
#' BoxTidwell(fit5, transf= ~ age)
#' fit5 <- update(fit5,formula=log(wlens) ~ I(age^(-1/3)))
#' AIC(fit5)
#'
BoxTidwell.lm <- function(object,transf,epsilon=0.0001,maxiter=30,trace=FALSE,digits=max(3, getOption("digits") - 2),...){
  X <- model.matrix(object)
  n <- nrow(X)
  transf.terms <- attr(terms(transf),"term.labels")
  transf.terms <- !is.na(match(colnames(X),transf.terms))
  if(sum(transf.terms)==0) stop("The variables to be transformed were not found in the fitted model",call.=FALSE)
  X1 <- as.matrix(X[,!transf.terms],nrow=n)
  X2 <- as.matrix(X[,transf.terms],nrow=n)
  nv <- ncol(X2)
  if(any(X2 <= 0)) stop("The variables to be transformed must have only positive values",call.=FALSE)
  if(colnames(X)[1]!="(Intercept)") stop("Models without Intercept are not supported",call.=FALSE)
  if(is.null(object$offset)) offset <- rep(0,n) else offset <- object$offset
  if(is.null(object$weights)) weights <- rep(1,n) else weights <- object$weights
  y <- object$residuals + object$fitted.values
  epsilon <- 0.0001
  tol <- epsilon + 1
  new.powers <- rep(1,nv)
  iter <- 1
  while(tol > epsilon & iter < maxiter){
    old.powers <- new.powers
    X22 <- X2^matrix(old.powers,nrow=n,ncol=nv,byrow=TRUE)
    X2.log <- X22*log(X22)
    mod.1 <- lm.wfit(x=cbind(X22,X1),y=y,w=weights,offset=offset)
    mod.2 <- lm.wfit(x=cbind(X2.log,X22,X1),y=y,w=weights,offset=offset)
    new.powers <- old.powers*(1 + coef(mod.2)[1:nv]/coef(mod.1)[1:nv])
    tol <- max(abs(new.powers-old.powers)/abs(new.powers))
    if(trace) cat("Iteration=",iter,"   taus=",new.powers,"\n")
    iter <- iter + 1
  }
  if(iter==maxiter) warning("Iteration limit exceeded!!\n",call.=FALSE)
  X22 <- X2^matrix(new.powers,nrow=n,ncol=nv,byrow=TRUE)
  X2.log <- X22*log(X2)
  mod <- lm(y ~ 0 + cbind(X22,X1),weights=weights,offset=offset)
  XX <- cbind(X2.log*matrix(coef(mod)[1:nv],nrow=n,ncol=nv,byrow=TRUE),X22,X1)
  XX <- XX*matrix(sqrt(weights),nrow=n,ncol=ncol(XX))
  vc <- summary(mod)$sigma^2*chol2inv(chol(crossprod(XX)))[1:nv,1:nv]
  TAB	<- cbind(Estimate <- new.powers,
               StdErr <- sqrt(diag(if(nv==1) matrix(vc) else vc)),
               zval <- (Estimate-1)/StdErr,
               p.value <- 1-pchisq(zval^2,df=1))
  TAB <- matrix(TAB,nrow=nv,ncol=4)
  colnames(TAB) <- c("Estimate", "Std.Error", "z-value", "Pr(>|z|)")
  rownames(TAB) <- colnames(X)[transf.terms]
  cat("\n")
  printCoefmat(TAB, P.values=TRUE, signif.stars=FALSE, has.Pvalue=TRUE, digits=digits, dig.tst=digits, signif.legend=FALSE, tst.ind=c(1,2,3), na.print="")
  chi <- t(new.powers-1)%*%chol2inv(chol(vc))%*%(new.powers-1)
  omnibus <- cbind(chi,nv,1-pchisq(chi,df=nv))
  colnames(omnibus) <- c("Chi","df","Pr(>Chi)"); rownames(omnibus) <- ""
  cat("\nWald test for null hypothesis that all taus are 1:\n")
  cat("chi = ",format(omnibus[1],digits=digits),", df = ",omnibus[2],", Pr(>chi) = ",format.pval(omnibus[3],digits=digits),"\n")
  return(invisible(list(marginal=TAB,omnibus=omnibus)))
}

#' @title Box-Tidwell transformations in Generalized Linear Models
#' @description Computes the Box-Tidwell power transformations of the predictors in a generalized linear model.
#' @param object an object of the class \emph{glm}.
#' @param transf an one-sided formula giving the quantitative predictors that are candidates for transformation.
#' @param epsilon an (optional) numerical value. If the maximum relative change in coefficients is less than
#'                \emph{epsilon}, then convergence is declared. As default, \emph{epsilon} is set to 0.0001.
#' @param maxiter an (optional) positive integer value indicating the maximum number of iterations. By default,
#'                \emph{maxiter} is set to 30.
#' @param trace an (optional) logical indicating if should the record of iterations be printed. By default,
#'              \emph{trace} is set to FALSE.
#' @param digits an (optional) integer value indicating the number of decimal places to be used.
#' @param ...	further arguments passed to or from other methods.
#' @return a list list with components including
#' \tabular{ll}{
#' \code{marginal} \tab a matrix with estimates and standard errors of the estimated powers, as well as the statistic
#'                       and the p-value of the Wald test to assess the hypothesis \eqn{H_0:\tau=1} versus \eqn{H_1:\tau\neq 1},\cr
#' \tab \cr
#' \code{omnibus} \tab a matrix with the statistic and the p-value of the Wald test for null hypothesis that all powers
#'                      are 1,\cr
#' }
#' @seealso \link{BoxTidwell.lm}
#' @method BoxTidwell glm
#' @export
#' @references Box G.E.P., Tidwell P.W. (1962) Transformation of the independent variables. \emph{Technometrics} 4, 531-550.
#' @references Fox J. (2016) \emph{Applied Regression Analysis and Generalized Linear Models}, Third Edition. Sage.
#' @examples
#' ###### Example 1: Skin cancer in women
#' data(skincancer)
#' fit1 <- glm(cases ~ age + city, offset=log(population), family=poisson(log), data=skincancer)
#' AIC(fit1)
#' BoxTidwell(fit1, transf= ~ age)
#' fit1 <- update(fit1,formula=. ~ I(age^(-1/2)) + city)
#' AIC(fit1)
#'
#' ###### Example 3: Gas mileage
#' data(Auto, package="ISLR")
#' fit3 <- glm(mpg ~ horsepower + weight, family=inverse.gaussian(log), data=Auto)
#' AIC(fit3)
#' BoxTidwell(fit3, transf= ~ horsepower + weight)
#' fit3 <- update(fit3,formula=. ~ I(horsepower^(-1/3)) + weight)
#' AIC(fit3)
#'
#' ###### Example 4: Advertising
#' data(advertising)
#' fit4 <- glm(sales ~ TV + radio, family=gaussian(log), data=advertising)
#' AIC(fit4)
#' BoxTidwell(fit4, transf= ~ TV)
#' fit4 <- update(fit4,formula=. ~ I(TV^(1/10)) + radio)
#' AIC(fit4)
#'
#' ###### Example 5: Leukaemia Patients
#' data(leuk, package="MASS")
#' fit5 <- glm(ifelse(time>=52,1,0) ~ ag + wbc, family=binomial, data=leuk)
#' AIC(fit5)
#' BoxTidwell(fit5, transf= ~ wbc)
#' fit5 <- update(fit5,formula=. ~ ag + I(wbc^(-0.18)))
#' AIC(fit5)
#'

BoxTidwell.glm <- function(object,transf,epsilon=0.0001,maxiter=30,trace=FALSE,digits=max(3, getOption("digits") - 2),...){
  X <- model.matrix(object)
  n <- nrow(X)
  transf.terms <- attr(terms(transf),"term.labels")
  transf.terms <- !is.na(match(colnames(X),transf.terms))
  if(sum(transf.terms)==0) stop("The variables to be transformed were not found in the fitted model",call.=FALSE)
  X1 <- as.matrix(X[,!transf.terms],nrow=n)
  X2 <- as.matrix(X[,transf.terms],nrow=n)
  nv <- ncol(X2)
  if(any(X2 <= 0)) stop("The variables to be transformed must have only positive values",call.=FALSE)
  if(colnames(X)[1]!="(Intercept)") stop("Models without Intercept are not supported",call.=FALSE)
  if(is.null(object$offset)) offset <- rep(0,n) else offset <- object$offset
  epsilon <- 0.0001
  tol <- epsilon + 1
  new.powers <- rep(1,nv)
  iter <- 1
  while(tol > epsilon & iter < maxiter){
    old.powers <- new.powers
    X22 <- X2^matrix(old.powers,nrow=n,ncol=nv,byrow=TRUE)
    X2.log <- X22*log(X22)
    mod.1 <- glm.fit(x=cbind(X22,X1),y=object$y,family=object$family,weights=object$prior.weights,offset=offset)
    mod.2 <- glm.fit(x=cbind(X2.log,X22,X1),y=object$y,family=object$family,weights=object$prior.weights,offset=offset)
    new.powers <- old.powers*(1 + coef(mod.2)[1:nv]/coef(mod.1)[1:nv])
    tol <- max(abs(new.powers-old.powers)/abs(new.powers))
    if(trace) cat("Iteration=",iter,"   taus=",new.powers,"\n")
    iter <- iter + 1
  }
  if(iter==maxiter) warning("Iteration limit exceeded!!\n",call.=FALSE)
  X22 <- X2^matrix(new.powers,nrow=n,ncol=nv,byrow=TRUE)
  X2.log <- X22*log(X2)
  mod <- glm(object$y ~ 0 + cbind(X22,X1),family=object$family,weights=object$prior.weights,offset=offset)
  XX <- cbind(X2.log*matrix(coef(mod)[1:nv],nrow=n,ncol=nv,byrow=TRUE),X22,X1)
  XX <- XX*matrix(sqrt(mod$weights),nrow=n,ncol=ncol(XX))
  vc <- summary(mod)$dispersion*chol2inv(chol(crossprod(XX)))[1:nv,1:nv]
  TAB	<- cbind(Estimate <- new.powers,
               StdErr <- sqrt(diag(if(nv==1) matrix(vc) else vc)),
               zval <- (Estimate-1)/StdErr,
               p.value <- 1-pchisq(zval^2,df=1))
  TAB <- matrix(TAB,nrow=nv,ncol=4)
  colnames(TAB) <- c("Estimate", "Std.Error", "z-value", "Pr(>|z|)")
  rownames(TAB) <- colnames(X)[transf.terms]
  cat("\n")
  printCoefmat(TAB, P.values=TRUE, signif.stars=FALSE, has.Pvalue=TRUE, digits=digits, dig.tst=digits, signif.legend=FALSE, tst.ind=c(1,2,3), na.print="")
  chi <- t(new.powers-1)%*%chol2inv(chol(vc))%*%(new.powers-1)
  omnibus <- cbind(chi,nv,1-pchisq(chi,df=nv))
  colnames(omnibus) <- c("Chi","df","Pr(>Chi)"); rownames(omnibus) <- ""
  cat("\nWald test for null hypothesis that all taus are 1:\n")
  cat("chi = ",format(omnibus[1],digits=digits),", df = ",omnibus[2],", Pr(>chi) = ",format.pval(omnibus[3],digits=digits),"\n")
  return(invisible(list(marginal=TAB,omnibus=omnibus)))
}



