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
#' @param varformula an (optional) \code{formula} expression of the form \code{~ z1 + z2 + ... + zq} indicating the potential explanatory variables for the dispersion parameter. By default, the same explanatory variables are taken as in the model for the mean.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @param ...	further arguments passed to or from other methods.
#' @details From the heteroskedastic normal lineal model in which
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
#' }
#' @method vdtest lm
#' @export
#' @examples
#' ###### Example 1: Fuel consumption of automobiles
#' fit1 <- lm(mpg ~ log(hp) + log(wt), data=mtcars)
#' vdtest(fit1)
#'
#' ###### Example 2: Species richness in plots
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
#' @references Breusch, T.S. and Pagan, A.R. (1979) A simple test for heteroscedasticity and random coefficient variation. \emph{Econometrica} 47, 1287–1294.
#' @references Cook, R.D. and Weisberg, S. (1983) Diagnostics for heteroscedasticity in regression. \emph{Biometrika} 70, 1–10.
#' @seealso \link{vdtest.glm}
vdtest.lm <- function(model,varformula,verbose=TRUE,...){
  if(!missingArg(varformula)){
    if(is.null(model$call$data)) m <- get_all_vars(eval(varformula))
    else m <- get_all_vars(eval(varformula),eval(model$call$data))
    Z <- model.matrix(varformula,m)
    if(!is.null(model$call$subset)) Z <- Z[eval(model$call$subset,eval(model$call$data)),]
  }else Z <- model.matrix(model)
  if(colnames(Z)[1]=="(Intercept)") Z <- as.matrix(Z[,-1])
  n <- nrow(Z)
  p <- ncol(Z)
  if(is.null(model$weights)) w <- matrix(1,n,1) else w <- model$weights
  phies <- mean(resid(model)^2*w)
  mus <- fitted(model)
  tau <- resid(model)^2*w/phies - 1
  Zstar <- cbind(1,Z)
  Zstar <- matrix(1,n,ncol(Zstar))*Zstar
  Zstar2 <- (solve(t(Zstar)%*%Zstar))[-1,-1]
  sc = 0.5*(t(tau)%*%Z)%*%Zstar2%*%(t(Z)%*%tau)
  if(verbose){
    cat("\n             Score test for varying dispersion parameter\n\n")
    cat("          Statistic = ",round(sc,digits=5),"\n degrees of freedom = ",p,"\n            p-value = ",format.pval(1-pchisq(sc,p)),"\n\n")
  }
  return(invisible(list(statistic=sc,df=p,p.value=format.pval(1-pchisq(sc,p)))))
}

#' @title Test for Varying Dispersion Parameter in Generalized Linear Models
#' @description Performs Rao's score test for varying dispersion parameter in
#' weighted and unweighted generalized linear models in which the response
#' distribution is assumed to be gaussian, Gamma or inverse gaussian.
#' @param model an object of the class \emph{glm} where the distribution of the response
#' variable is assumed to be \code{gaussian}, \code{Gamma} or \code{inverse.gaussian}.
#' @param varformula an (optional) \code{formula} expression of the form \code{~ z1 + z2 + ... + zq} describing only the potential explanatory variables for the dispersion. By default, the same explanatory variables are taken as in the model for the mean.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @param ...	further arguments passed to or from other methods.
#' @details From the generalized lineal model with varying dispersion in which
#' \eqn{\log(\phi)=\gamma_0 + \gamma_1z_1 + \gamma_2z_2 + ... + \gamma_qz_q}, where
#' \eqn{\phi} is the dispersion parameter of the distribution used to describe the
#' response variable, the Rao's score test (denoted here as \eqn{S}) to assess the
#' hypothesis \eqn{H_0: \gamma=0} versus \eqn{H_1: \gamma\neq 0} is computed,
#' where \eqn{\gamma=(\gamma_1,\ldots,\gamma_q)}.  The corresponding \emph{p}-value is
#' computed from the chi-squared distribution with \eqn{q} degrees of freedom,
#' that is, \emph{p}-value = Prob\eqn{[\chi^2_{q} > S]}. If the object
#' \code{model} corresponds to an unweighted generalized linear model then
#' this test assess assumptions of constant variance and constant coefficient
#' of variation on models in which the response distribution is assumed to be
#' gaussian and Gamma, respectively.
#' @return a list list with components including
#' \tabular{ll}{
#' \code{statistic} \tab value of the Rao's score test (\eqn{S}),\cr
#' \tab \cr
#' \code{df}        \tab number of degrees of freedom (\eqn{q}),\cr
#' \tab \cr
#' \code{p.value}   \tab \emph{p}-value of the test,\cr
#' }
#' @examples
#' ###### Example 1: Fuel consumption of automobiles
#' Auto <- ISLR::Auto
#' fit1 <- glm(mpg ~ weight*horsepower, family=inverse.gaussian("log"), data=Auto)
#' vdtest(fit1)
#'
#' ###### Example 2: Hill races in Scotland
#' fit2 <- glm(rtime ~ log(distance) + log(cclimb), family=Gamma("log"), data=races)
#' vdtest(fit2)
#'
#' ###### Example 3: Mammal brain and body weights
#' fit3 <- glm(BrainWt ~ log(BodyWt), family=Gamma("log"), data=brains)
#' vdtest(fit3)
#' @method vdtest glm
#' @export
#' @references Wei, B.-C. and Shi, J.-Q. and Fung, W.-K. and Hu, Y.-Q. (1998) Testing for Varying Dispersion in Exponential Family Nonlinear Models. \emph{Annals of the Institute of Statistical Mathematics} 50, 277–294.
#'
#' @seealso \link{vdtest.lm}
vdtest.glm <- function(model,varformula,verbose=TRUE,...){
  if(model$family$family!="gaussian" & model$family$family!="Gamma" & model$family$family!="inverse.gaussian")
    stop("Only gaussian, Gamma and inverse.gaussian families are supported!!",call.=FALSE)
  if(!missingArg(varformula)){
    if(is.null(model$call$data)) m <- get_all_vars(eval(varformula))
    else m <- get_all_vars(eval(varformula),eval(model$call$data))
    Z <- model.matrix(varformula,m)
    if(!is.null(model$call$subset)) Z <- Z[eval(model$call$subset,eval(model$call$data)),]
  }else Z <- model.matrix(model)
  if(colnames(Z)[1]=="(Intercept)") Z <- as.matrix(Z[,-1])
  n <- nrow(Z)
  p <- ncol(Z)
  y <- model$y
  mus <- fitted(model)
  w <- model$prior.weights
  if(model$family$family=="gaussian"){
    phies <- mean((y-mus)^2*w)
    tau <- (y-mus)^2*w/phies - 1
  }
  if(model$family$family=="inverse.gaussian"){
    phies <- mean((y-mus)^2*w/(mus^2*y))
    tau <- (y-mus)^2*w/(mus^2*y*phies) - 1
  }
  if(model$family$family=="Gamma"){
    phies <- sum(resid(model,type="pearson")^2)/model$df.residual
    phies <- uniroot(function(x) sum((y/mus + log(mus*x/(w*y)) + psigamma(w/x) - 1)*w), lower=phies*(0.1), upper=phies*(1.9))$root
    tau <- 2*(y/mus + log(mus*phies/(w*y)) + psigamma(w/phies) - 1)*(w/phies)
    pes <- sqrt((w/phies)*(2*psigamma(w/phies,1)*(w/phies) - 2))
  }
  Zstar <- cbind(1,Z)
  Zstar <- matrix(1,n,ncol(Zstar))*Zstar
  Zstar2 <- chol2inv(chol(t(Zstar)%*%Zstar))[-1,-1]
  sc = 0.5*(t(tau)%*%Z)%*%Zstar2%*%(t(Z)%*%tau)
  if(verbose){
    cat("\n             Score test for varying dispersion parameter\n\n")
    cat("          Statistic = ",round(sc,digits=5),"\n degrees of freedom = ",p,"\n            p-value = ",format.pval(1-pchisq(sc,p)),"\n\n")
  }
  return(invisible(list(statistic=sc,df=p,p.value=format.pval(1-pchisq(sc,p)))))
}

#' @title Variable Selection in Normal Linear Models
#' @description Performs variable selection in normal linear models using a hybrid versions of forward stepwise and backward stepwise.
#' @param model an object of the class \emph{lm}.
#' @param direction an (optional) character string indicating the type of procedure which should be used. The available options are: hybrid backward stepwise ("backward") and hybrid forward stepwise ("forward"). By default, \code{direction} is set to be "forward".
#' @param levels an (optional) two-dimensional vector of values in the interval \eqn{(0,1)} indicating the levels at which the variables should in and out from the model. This is only appropiate if \code{criterion}="p-value". By default, \code{levels} is set to be \code{c(0.05,0.05)}.
#' @param criterion an (optional) character string indicating the criterion which should be used to compare the candidate models. The available options are: AIC ("aic"), BIC ("bic"), adjusted R-squared ("adjr2"), predicted R-squared ("prdr2"), Mallows' CP ("cp") and \emph{p}-value of the F test ("p-value"). By default, \code{criterion} is set to be "bic".
#' @param ...	further arguments passed to or from other methods. For example, \code{k}, that is, the magnitude of the penalty in the AIC/QICu, which by default is set to be 2.
#' @param trace an (optional) logical switch indicating if should the stepwise reports be printed. By default, \code{trace} is set to be TRUE.
#' @param scope an (optional) list containing components \code{lower} and \code{upper}, both formula-type objects, indicating the range of models which should be examined in the stepwise search. By default, \code{lower} is a model with no predictors and \code{upper} is the linear predictor of the model in \code{model}.
#' @details The "hybrid forward stepwise"  algorithm starts with the simplest
#' model (which may be specified at the argument \code{scope}, and by default,
#' is a model whose parameters in the linear predictor, except the intercept,
#' if any, are set to be 0), and then the candidate models are builded by
#' hierarchically adding effects in the linear predictor, whose "relevance"
#' and/or "importance" in the model fit is assessed by comparing nested models
#' (that is, by comparing the models with and without the added effect) using a
#' criterion previously specified. If an effect is added to the model then this strategy may
#' also remove any effect which, according to the criterion previously specified, no
#' longer provide an improvement in the model fit. That process remain until no more effects may be included or excluded.
#'
#' The "hybrid backward stepwise" algorithm works similarly.
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
#' @references James, G. and Witten, D. and Hastie, T. and Tibshirani, R. (2013, page 210) An Introduction to Statistical Learning with Applications in R, Springer, New York.
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
    lower <- formula(paste(deparse(lower[[2]]),"~",attr(terms(lower),"intercept")))
  }else{
    lower <- scope$lower
    upper <- scope$upper
  }
  formulae <- update(upper, paste(deparse(eval(model$call$formula)[[2]]),"~ ."))
  model <- update(model,formula=formulae)
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
  U <- unlist(lapply(strsplit(attr(terms(upper),"term.labels"),":"),function(x) paste(sort(x),collapse =":")))
  fs <- attr(terms(upper),"factors")
  long <- max(nchar(U)) + 2
  nonename <- paste("<none>",paste(replicate(max(long-6,0)," "),collapse=""),collapse="")
  cambio <- ""
  paso <- 1
  tol <- TRUE
  if(trace){
    cat("\n       Family: gaussian\n")
    cat("Link function: identity\n")
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
    if(!is.null(xxx$k)) cat("The magnitude of the penalty in the AIC was set to be ",k)
    cat("\n")
  }
  out_$final <- paste("~",as.character(oldformula)[length(oldformula)],sep=" ")
  return(invisible(out_))
}


#' @title Normal QQ-plot with simulated envelope of residuals in GLMs
#' @description Produces a normal QQ-plot with simulated envelope of residuals
#' for generalized linear models.
#' @param object an object of the class \emph{glm}.
#' @param rep an (optional) positive integer which allows to specify the number of replicates which should be used to build the simulated envelope. By default, \code{rep} is set to be 25.
#' @param conf an (optional) value in the interval (0,1) indicating the confidence level which should be used to build the pointwise confidence intervals, which form the envelope. By default, \code{conf} is set to be 0.95.
#' @param type a character string indicating the type of residuals which should be used. The available options are: randomized quantile ("quantile"), deviance ("deviance") and pearson ("pearson") residuals. By default, \code{type} is set to be "quantile".
#' @param standardized an (optional) logical switch indicating if the residuals should be standardized by dividing by the square root of \eqn{(1-h)}, where \eqn{h} is a measure of leverage. By default, \code{standardized} is set to be FALSE.
#' @param plot.it an (optional) logical switch indicating if the normal QQ-plot with simulated envelope of residuals is required or just the data matrix in which it is based. By default, \code{plot.it} is set to be TRUE.
#' @param identify an (optional) positive integer indicating the number of individuals to identify on the QQ-plot with simulated envelope of residuals. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used to include graphical parameters to customize the plot. For example,  \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
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
#' @details The simulated envelope is builded by simulating \code{rep} independent realizations of the response variable for each
#' individual, which is accomplished taking into account the following: (1) the model assumption about the distribution of
#' the response variable; (2) the estimates of the parameters in the linear predictor; and (3) the estimate of the
#' dispersion parameter. The interest model is re-fitted \code{rep} times, as each time the vector of observed responses
#' is replaced by one of the simulated samples. The \code{type}-type residuals are computed and then sorted for each
#' replicate, so that for each \eqn{i=1,2,...,n}, where \eqn{n} is the number of individuals in the sample, there is a random
#' sample of size \code{rep} of the \eqn{i}-th order statistic of the  \code{type}-type residuals. Therefore, the simulated
#' envelope is composed of the quantiles (1 - \code{conf})/2 and (1 + \code{conf})/2 of the random sample of size \code{rep} of
#' the \eqn{i}-th order statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n}.
#' @references Atkinson, A.C. (1985) \emph{Plots, Transformations and Regression}. Oxford University Press, Oxford.
#' @references Davison, A.C. and Gigli, A. (1989) Deviance Residuals and Normal Scores Plots. \emph{Biometrika} 76, 211-221.
#' @references Dunn, P.K. and Smyth, G.K. (1996) Randomized Quantile Residuals. \emph{Journal of Computational and Graphical Statistics} 5, 236-244.
#' @references Pierce, D.A. and Schafer, D.W. (1986) Residuals in Generalized Linear Models. \emph{Journal of the American Statistical Association} 81, 977-986.
#' @seealso \link{envelope.lm}, \link{envelope.overglm}
#' @examples
#'
#' ###### Example 1:
#' burn1000 <- aplore3::burn1000
#' burn1000 <- within(burn1000, death <- factor(death, levels=c("Dead","Alive")))
#' fit1 <- glm(death ~ age*inh_inj + tbsa*inh_inj, family=binomial("logit"), data=burn1000)
#' envelope(fit1, rep=50, conf=0.95, type="pearson", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 2: Fuel consumption of automobiles
#' Auto <- ISLR::Auto
#' fit2 <- glm(mpg ~ horsepower*weight, family=inverse.gaussian("log"), data=Auto)
#' envelope(fit2, rep=50, conf=0.95, type="pearson", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 3: Skin cancer in women
#' fit3 <- glm(cases ~ offset(log(population)) + city + age, family=poisson, data=skincancer)
#' envelope(fit3, rep=100, conf=0.95, type="quantile", col="red", pch=20,col.lab="blue",
#'          col.axis="blue",col.main="black",family="mono",cex=0.8)
#'
#' ###### Example 4: Self diagnozed ear infections in swimmers
#' fit4 <- glm(infections ~ frequency + location, family=poisson(log), data=swimmers)
#' envelope(fit4, rep=100, conf=0.95, type="quantile", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 5: Agents to stimulate cellular differentiation
#' fit5 <- glm(cbind(cells,200-cells) ~ tnf + ifn, family=binomial(logit), data=cellular)
#' envelope(fit5, rep=100, conf=0.95, type="quantile", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' @importFrom stats dbinom delete.response dpois glm AIC BIC logLik fitted quantile
#'             pbinom pgamma ppoints ppois qqplot rchisq binomial poisson resid runif
#'              rgamma rpois rbinom  qqnorm dnbinom residuals pnbinom
#'             qnbinom qpois rbeta rnbinom .getXlevels lm optim
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom numDeriv grad hessian
#' @importFrom Rfast Digamma Lgamma
#' @importFrom Formula Formula model.part
#' @importFrom methods is
#' @method envelope glm
#' @export
envelope.glm <- function(object, rep=25, conf=0.95, type=c("quantile","deviance","pearson"), standardized=FALSE, plot.it=TRUE, identify, ...){
  if(object$family$family=="quasi" | object$family$family=="quasipoisson" | object$family$family=="quasibinomial")
    stop("Quasi-likelihood models are not supported!!",call.=FALSE)
  if(any(object$prior.weights == 0)) stop("Only positive weights are supported!!",call.=FALSE)
  type <- match.arg(type)
  p <- length(coef(object))
  X <- model.matrix(object)
  mu <- fitted(object)
  n <- length(mu)
  weights <- object$prior.weights
  if(is.null(object$offset)) offs <- rep(0,n) else offs <- object$offset
  phi <- summary(object)$dispersion
  rep <- max(1,floor(abs(rep)))
  e <- matrix(0,n,rep)
  quantileres <- function(family,y,mu,phi){
    resi <- switch(family,
                   Gamma = pgamma(y,shape=1/phi,scale=mu*phi),
                   inverse.gaussian = pnorm((y/mu-1)/sqrt(y*phi)) + exp(2/(mu*phi))*pnorm(-(y/mu+1)/sqrt(y*phi)),
                   gaussian = pnorm((y-mu)/sqrt(phi)),
                   poisson = ppois(y-1,lambda=mu) + dpois(y,lambda=mu)*runif(length(mu)),
                   binomial = pbinom(y/phi-1,size=1/phi,prob=mu) + dbinom(y/phi,size=1/phi,prob=mu)*runif(length(mu)))
    return(qnorm(ifelse(ifelse(resi<1e-16,1e-16,resi)>1-(1e-16),1-(1e-16),resi)))
  }
  bar <- txtProgressBar(min=0, max=rep, initial=0, width=min(50,rep), char="+", style=3)
  i <- 1
  while(i <= rep){
    if(object$family$family=="inverse.gaussian"){
      w <- rchisq(n,df=1)
      u <- mu + mu^2*w*phi/2 - (mu*phi/2)*sqrt(4*mu*w/phi + mu^2*w^2)
    }
    resp <- switch(object$family$family,
                   Gamma = rgamma(n,shape=object$prior.weights/phi,scale=mu*phi/object$prior.weights),
                   inverse.gaussian = ifelse(runif(n)<=mu/(mu+u),u,mu^2/u),
                   gaussian = sqrt(phi/object$prior.weights)*rnorm(n) + mu,
                   poisson = rpois(n,lambda=mu),
                   binomial = rbinom(n,size=object$prior.weights,prob=mu)/object$prior.weights)
    fits <- try(glm.fit(x=X,y=resp,family=object$family,offset=offs,start=coef(object),weights=weights),silent=TRUE)
    if(is.list(fits)){
      if(fits$converged==TRUE){
        phis <- sum((resp-fits$fitted.values)^2*weights/object$family$variance(fits$fitted.values))/fits$df.residual
        if(object$family$family=="binomial" | object$family$family=="poisson") phis <- 1
        if(type=="quantile")
          rs <- quantileres(object$family$family,resp,fits$fitted.values,phis/weights)
        if(type=="deviance"){
          rs <- sqrt(object$family$dev.resids(resp,fits$fitted.values,weights)/phis)
          rs <- ifelse(resp >= fits$fitted.values,1,-1)*rs
        }
        if(type=="pearson")
          rs <- (resp-fits$fitted.values)*sqrt(weights/(object$family$variance(fits$fitted.values)*phis))
        if(standardized){
          Xw <- X*matrix(sqrt(fits$weights),n,p)
          salida <- svd(Xw)
          h <- apply(salida$u^2,1,sum)
          rs <- rs/sqrt(1-h)
        }
        e[,i] <- sort(rs)
        setTxtProgressBar(bar,i)
        i <- i + 1
      }
    }
  }
  close(bar)
  alpha <- 1 - max(0,min(1,abs(conf)))
  e <- as.matrix(e[,1:(i-1)])
  es <- apply(e,1,function(x) return(quantile(x,probs=c(alpha/2,0.5,1-alpha/2))))
  if(type=="quantile")
    rd <- quantileres(object$family$family,object$y,mu,phi/object$prior.weights)
  else rd <- residuals(object,type=type)/sqrt(phi)
  if(standardized){
    Xw <- X*matrix(sqrt(object$weights),n,p)
    salida <- svd(Xw)
    h <- apply(salida$u^2,1,sum)
    rd <- rd/sqrt(1-h)
  }
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

#' @title Normal QQ-plot with simulated envelope of residuals for normal linear models
#' @description Produces a normal QQ-plot with simulated envelope of residuals obtained from the fit of a normal linear model.
#' @param object an object of the class \emph{lm}.
#' @param rep an (optional) positive integer indicating the number of replicates which should be used to build the simulated envelope. By default, \code{rep} is set to be 100.
#' @param conf an (optional) value in the interval (0,1) indicating the confidence level which should be used to build the pointwise confidence intervals, which form the envelope. By default, \code{conf} is set to be 0.95.
#' @param type a character string indicating the type of residuals which should be used. The available options are: internally Studentized ("internal") and externally Studentized ("external") residuals. See Cook and Weisberg (1982, pages 18-20).
#' @param plot.it an (optional) logical switch indicating if the normal QQ-plot with simulated envelope of residuals is required or just the data matrix in which it is based. By default, \code{plot.it} is set to be TRUE.
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
#' @details The simulated envelope is builded by simulating \code{rep} independent realizations of the response variable for each
#' individual, which is accomplished taking into account the following: (1) the model assumption about the distribution of
#' the response variable; (2) the estimates of the parameters in the linear predictor; and (3) the estimate of the
#' dispersion parameter. The interest model is re-fitted \code{rep} times, as each time the vector of observed responses
#' is replaced by one of the simulated samples. The \code{type}-type residuals are computed and then sorted for each
#' replicate, so that for each \eqn{i=1,2,...,n}, where \eqn{n} is the number of individuals in the sample, there is a random
#' sample of size \code{rep} of the \eqn{i}-th order statistic of the  \code{type}-type residuals. Therefore, the simulated
#' envelope is composed of the quantiles (1 - \code{conf})/2 and (1 + \code{conf})/2 of the random sample of size \code{rep} of
#' the \eqn{i}-th order statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n}.
#' @references Atkinson, A.C. (1985) \emph{Plots, Transformations and Regression}. Oxford University Press, Oxford.
#' @references Cook, R.D. and Weisberg, S. (1982) \emph{Residuals and Influence in Regression}. Chapman and Hall, New York.
#' @seealso \link{envelope.glm}, \link{envelope.overglm}
#' @examples
#' ###### Example 1: Fuel consumption of automobiles
#' fit1 <- lm(mpg ~ log(hp) + log(wt), data=mtcars)
#' envelope(fit1, rep=100, conf=0.95, type="external", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 2: Species richness in plots
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
    resp <- sqrt(sigma2/weights)*rnorm(n) + mu - offset
    fits <- try(lm.fit(x=X,y=resp,w=weights,offset=offset),silent=TRUE)
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
  if(plot.it){
    rango <- 1.1*range(td)
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
    nano$y <- td
    nano$type <- "p"
    nano$ylim <- rango
    if(is.null(nano$labels)) labels <- 1:nrow(X)
    else labels <- nano$labels
    if(is.null(nano$pch)) nano$pch <- 20
    if(is.null(nano$col)) nano$col <- "black"
    if(is.null(nano$xlab)) nano$xlab <- "Expected quantiles"
    if(is.null(nano$ylab)) nano$ylab <- "Observed quantiles"
    if(is.null(nano$main)) nano$main <- paste0("Normal QQ plot with simulated envelope\n of ",type,"lly studentized residuals")
    if(is.null(nano$labels))  labels <- 1:length(td)
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    outm <- do.call("qqnorm",nano)
    if(!missingArg(identify)){
      identify(outm$x,outm$y,labels=labels,n=max(1,floor(abs(identify))),labels=labels)
    }
  }
  out_ <- cbind(t(es),td)
  colnames(out_) <- c("Lower limit","Median","Upper limit","Residuals")
  return(invisible(out_))
}
#'
#'@title The Hosmer-Lemeshow Goodness-of-Fit Test
#' @description Computes the Hosmer-Lemeshow goodness-of-fit test for a generalized linear model fitted to binary responses.
#' @param model an object of the class \emph{glm}, which is obtained from the fit of a generalized linear model where the distribution for the response variable is assumed to be binomial.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
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
#' @references Hosmer, D.W. and Lemeshow, S. (2000) \emph{Applied Logistic Regression. 2nd ed.} John Wiley & Sons, New York.
#' @examples
#'
#' ###### Example 1: Patients with burn injuries
#' burn1000 <- aplore3::burn1000
#' burn1000 <- within(burn1000, death <- factor(death, levels=c("Dead","Alive")))
#' fit1 <- glm(death ~ age*inh_inj + tbsa*inh_inj, family=binomial("logit"), data=burn1000)
#' hltest(fit1)
#'
#' ###### Example 2: Bladder cancer in mice
#' fit2 <-  glm(cancer/exposed ~ dose, weights=exposed, family=binomial("cloglog"), data=bladder)
#' hltest(fit2)
#'
#' ###### Example 3: Liver cancer in mice
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
    hm <- aggregate(cbind(m,events,mus*m/sum(m))~gr,data=temp3,sum)
    hm1 <- aggregate(cbind(m,events,mus*m/sum(m))~gr,data=temp4,sum)
    if(sd(hm[,2]) > sd(hm1[,2])) hm <- hm1
  }
  if(nrow(temp2) < 10) hm <- cbind(1:nrow(temp2),temp2[,c(2,3,1)])
  hm2 <- cbind(hm[,1],hm[,2],hm[,3],hm[,2]*hm[,4])
  colnames(hm2) <- c("Group","Size","Observed","Expected")
  rownames(hm2) <- rep("",nrow(hm2))
  hmt <- sum((hm[,3] - hm[,2]*hm[,4])^2/(hm[,2]*hm[,4]*(1-hm[,4])))
  if(verbose){
    cat("\n   The Hosmer-Lemeshow goodness-of-fit test\n\n")
    printCoefmat(hm2,digits=4,na.print="")
    cat("\n         Statistic = ",round(hmt,digits=5),"\ndegrees of freedom = ",nrow(hm)-2,"\n           p-value = ",format.pval(1-pchisq(hmt,nrow(hm)-2)),"\n\n")
  }
  return(invisible(list(hm=hm2,statistic=hmt,df=nrow(hm)-2,p.value=format.pval(1-pchisq(hmt,nrow(hm)-2)))))
}

#' @title The Receiver Operating Characteristic (ROC) Curve
#' @description Computes the exact area under the ROC curve (AUROC), the Gini coefficient, and the Kolmogorov-Smirnov (KS) statistic for a binary classifier. Optionally, this function can plot the ROC curve, that is, the plot of the estimates of Sensitivity versus the estimates of 1-Specificity.
#' @param object a matrix with two columns: the first one is a numeric vector of 1's and 0's indicating whether each row is a "success" or a "failure"; the second one is a numeric vector of values indicating the probability (or propensity score) of each row to be a "success". Optionally, \code{object} can be an object of the class glm which is obtained from the fit of a generalized linear model where the distribution of the response variable is assumed to be binomial.
#' @param plot.it an (optional) logical switch indicating if the plot of the ROC curve is required or just the data matrix in which it is based. By default, \code{plot.it} is set to be TRUE.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @param ... further arguments passed to or from other methods. For example, if \code{plot.it=TRUE} then \code{...} may to include graphical parameters as \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @references Hanley, J.A. and McNeil, B.J. (1982) The Meaning and Use of the Area under a Receiver Operating Characteristic (ROC) Curve. \emph{Radiology} 143, 29–36.
#' @return A list which contains the following objects:
#' \itemize{
#' \item{\code{roc:}}{ A matrix with the Cutoffs and the associated estimates of Sensitivity and Specificity.}
#' \item{\code{auroc:}}{ The exact area under the ROC curve.}
#' \item{\code{gini:}}{ The value of the Gini coefficient computed as 2(\code{auroc}-0.5).}
#' \item{\code{ks:}}{ The value of the Kolmogorov-Smirnov statistic computed as the maximum value of |1-Sensitivity-Specificity|.}
#' }
#' @examples
#' ###### Example: Patients with burn injuries
#' burn1000 <- aplore3::burn1000
#'
#' ### splitting the sample: 70% for the training sample and 30% for the validation sample
#' burn1000 <- within(burn1000, sampleof <- "validation")
#' s <- sample(nrow(burn1000),nrow(burn1000)*0.7)
#' burn1000$sampleof[s] <- "training"
#'
#' training <- subset(burn1000,sampleof=="training")
#' fit <- glm(death ~ age*inh_inj + tbsa*inh_inj, family=binomial("logit"), data=training)
#'
#' ### ROC curve for the training sample
#' ROCc(fit, col="red", col.lab="blue", col.axis="black", col.main="black", family="mono")
#'
#' validation <- subset(burn1000, sampleof=="validation")
#' probs <- predict(fit, newdata=validation, type="response")
#' responses <- with(validation, ifelse(death=="Dead",1,0))
#'
#' ### ROC curve for the validation sample
#' ROCc(cbind(responses,probs), col="red", col.lab="blue", col.axis="black",
#'      col.main="black", family="mono")
#' @export ROCc
#' @importFrom stats aggregate sd
#'
ROCc <- function(object,plot.it=TRUE,verbose=TRUE,...){
  if(is.list(object)){
    if(class(object)[1]!="glm") stop("Only glm-type objects are supported!!",call.=FALSE)
    if(object$family$family!="binomial") stop("Only binomial regression models are supported!!",call.=FALSE)
    temp <- data.frame(mus=fitted(object),ys=object$y*object$prior.weights,size=object$prior.weights)
  }else{
    temp <- data.frame(ys=object[,1],mus=object[,2],size=rep(1,nrow(object)))
    if(any(temp$ys!=0 & temp$ys!=1)) stop("First column of object should contain only 1's or 0's!!",call.=FALSE)
  }
  temp2 <- aggregate(cbind(m=size,events=ys)~mus,data=temp,sum)
  mus <- temp2[,1]
  if(nrow(temp2) > 1000) mus <- quantile(rep(temp2[,1],temp2[,2]), probs=0:1001/1001)
  results <- matrix(0,length(mus),3)
  d1 <- sum(temp2[,3])
  d3 <- sum(temp2[,2])
  d2 <- d3 - d1
  C <- 0
  for(i in 1:length(mus)){
    results[i,1] <- mus[i]
    results[i,2] <- sum(temp2[temp2[,1] >= mus[i],3])/d1
    results[i,3] <- (sum(temp2[temp2[,1] < mus[i],2]) - sum(temp2[temp2[,1] < mus[i],3]))/d2
    if(i>=2) C <- C +  (1/2)*(results[i,3] - results[i-1,3])*(min(results[i,2],results[i-1,2]) + max(results[i,2],results[i-1,2]))
  }
  if(plot.it){
    nano <- list(...)
    nano$x <- 1-results[,3]
    nano$y <- results[,2]
    nano$type <- "l"
    limits <- range(1-results[,3],results[,2])
    print(limits)
    if(is.null(nano$col)) nano$col <- "blue"
    if(is.null(nano$xlab)) nano$xlab <- "1-Specificity"
    if(is.null(nano$ylab)) nano$ylab <- "Sensitivity"
    if(is.null(nano$xlim)) nano$xlim <- limits
    if(is.null(nano$ylim)) nano$ylim <- limits
    if(is.null(nano$main)) nano$main <- "Receiver Operating Characteristic Curve"
    do.call("plot",nano)
    abline(0,1,lty=3)
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
#' @param test an (optional) character string indicating the required type of test. The available options are: Wald ("wald"), Rao's score ("score"), Terrell's gradient ("gradient"), and likelihood ratio ("lr") tests. By default, \code{test} is set to be "wald".
#' @param verbose an (optional) logical indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @details The Wald, Rao's score and Terrell's gradient tests are performed using the expected Fisher information matrix.
#' @references Buse, A. (1982) The Likelihood Ratio, Wald, and Lagrange Multiplier Tests: An Expository Note. \emph{The American Statistician} 36, 153-157.
#' @references Terrell, G.R. (2002) The gradient statistic. \emph{Computing Science and Statistics} 34, 206 – 215.
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
#' fit <- glm(lesions ~ 1 + time, family=poisson("log"), data=aucuba)
#' anova2(fit, test="lr")
#' anova2(fit, test="score")
#' anova2(fit, test="wald")
#' anova2(fit, test="gradient")
#' @return A matrix with three columns which contains the following:
#' \itemize{
#' \item \code{Chi:}{ The value of the statistic of the test.}
#' \item \code{Df:}{ The number of degrees of freedom.}
#' \item \code{Pr(>Chi):}{ The \emph{p}-value of the test computed using the Chi-square distribution.}
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
#' fit3 <- glm(cases ~ offset(log(population)) + city + age, family=poisson("log"), data=skincancer)
#' estequa(fit3)
estequa.glm <- function(object,...){
  xx <- model.matrix(object)
  n <- nrow(xx)
  p <- ncol(xx)
  delta <- ifelse(object$family$mu.eta(object$linear.predictor)>=0,1,-1)
  salida <- crossprod(xx,delta*resid(object,type="pearson")*sqrt(object$weights))
  colnames(salida) <- " "
  rownames(salida) <- names(coef(object))
  return(salida)
}
#' @title Generalized Variance Inflation Factor
#' @description Computes the generalized variance inflation factor (GVIF) for a weighted or unweighted normal linear model.
#' @param model an object of the class \emph{lm}.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
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
#' @references Fox, J. and Monette, G. (1992) Generalized collinearity diagnostics, \emph{JASA} 87, 178–183.
#' @seealso \link{gvif.glm}
gvif.lm <- function(model,verbose=TRUE,...){
  X <- model.matrix(model)
  postos <- model$assign
  vars <- attr(model$terms,"term.labels")
  if(!is.null(model$weights))
    X <- matrix(sqrt(model$weights),nrow(X),ncol(X))*X
  vcovar <- solve(t(X)%*%X)
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
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
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
#' fit3 <- glm(rtime ~ log(distance) + log(cclimb), family=Gamma("log"), data=races)
#' gvif(fit3)
#'
#' @references Fox, J. and Monette, G. (1992) Generalized collinearity diagnostics, \emph{JASA} 87, 178–183.
#' @seealso \link{gvif.lm}
gvif.glm <- function(model,verbose=TRUE,...){
  X <- model.matrix(model)
  postos <- attr(X,"assign")
  vars <- attr(model$terms,"term.labels")
  X <- matrix(sqrt(model$weights),nrow(X),ncol(X))*X
  vcovar <- solve(t(X)%*%X)
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
#' @param test an (optional) character string indicating the required type of test. The available options are: Wald ("wald"), Rao's score ("score"), Terrell's gradient ("gradient"), and likelihood ratio ("lr") tests. By default, \code{test} is set to be "wald".
#' @param digits an (optional) integer value indicating the number of decimal places to be used. By default, \code{digits} is set to be 5.
#' @param level an (optional) value indicating the required confidence level. By default, \code{level} is set to be 0.95.
#' @param verbose an (optional) logical indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @details The approximate 100(\code{level})\% confidence interval for \eqn{\beta} based on the \code{test} test is the set of values of \eqn{\beta_0} for which the hypothesis \eqn{H_0}: \eqn{\beta=\beta_0} versus \eqn{H_1}: \eqn{\beta!=\beta_0} is not rejected at the approximate significance level of 100(1-\code{level})\%. The Wald, Rao's score and Terrell's gradient tests are performed using the expected Fisher information matrix.
#' @return A matrix with so many rows as parameters in the linear predictor and two columns: "Lower limit" and "Upper limit".
#' @references Buse, A. (1982) The Likelihood Ratio, Wald, and Lagrange Multiplier Tests: An Expository Note. \emph{The American Statistician} 36, 153-157.
#' @references Terrell, G.R. (2002) The gradient statistic. \emph{Computing Science and Statistics} 34, 206 – 215.
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
confint2 <- function(model, level=0.95, test=c("wald","lr","score","gradient"), digits=5, verbose=TRUE){
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
#' @param type an (optional) character string giving the type of residuals which should be returned. The available options for LMs are: (1) externally studentized ("external"); (2) internally studentized ("internal") (default). The available options for GLMs are: (1) "pearson"; (2) "deviance";  (3) "quantile" (default).
#' @param standardized an (optional) logical switch indicating if the residuals should be standardized by dividing by the square root of \eqn{(1-h)}, where \eqn{h} is a measure of leverage. By default, \code{standardized} is set to be FALSE.
#' @param plot.it an (optional) logical switch indicating if a plot of the residuals is required. By default, \code{plot.it} is set to be FALSE.
#' @param identify an (optional) integer value indicating the number of individuals to identify on the plot of residuals. This is only appropriate when \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods
#' @return A vector with the observed residuals type \code{type}.
#' @examples
#' ###### Example 1: Species richness in plots
#' fit1 <- lm(Species ~ Biomass + pH, data=richness)
#' residuals2(fit1, type="external", col="red", pch=20, col.lab="blue", plot.it=TRUE,
#'            col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 2: Lesions of Aucuba mosaic virus
#' fit2 <- glm(lesions ~ time, family=poisson, data=aucuba)
#' residuals2(fit2, type="quantile", col="red", pch=20, col.lab="blue", plot.it=TRUE,
#'            col.axis="blue",col.main="black",family="mono",cex=0.8)
#' @export residuals2
residuals2 <- function(object,type,standardized=FALSE,plot.it=TRUE,identify,...){
  if(class(object)[1]!="glm" & class(object)[1]!="lm") stop("Only lm- and glm-type objects are supported!!",call.=FALSE)
  if(class(object)[1]=="glm" & missingArg(type)) type <- "quantile"
  if(class(object)[1]=="lm" & missingArg(type)) type <- "internal"
  if(class(object)[1]=="glm" & type!="quantile" & type!="pearson" & type!="deviance")
    stop("Only quantile-, pearson- and deviance-type residuals are supported for glm-type objects !!",call.=FALSE)
  if(class(object)[1]=="lm" & type!="internal" & type!="external")
    stop("Only internal-, and external-type residuals are supported for lm-type objects !!",call.=FALSE)
  if(class(object)[1]=="glm"){
    quantileres <- function(family,y,mu,phi){
      resi <- switch(family,
                     Gamma = pgamma(y,shape=1/phi,scale=mu*phi),
                     inverse.gaussian = pnorm((y/mu-1)/sqrt(y*phi)) + exp(2/(mu*phi))*pnorm(-(y/mu+1)/sqrt(y*phi)),
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
#' @param direction an (optional) character string indicating the type of procedure which should be used. The available options are: hybrid backward stepwise ("backward") and hybrid forward stepwise ("forward"). By default, \code{direction} is set to be "forward".
#' @param levels an (optional) two-dimensional vector of values in the interval \eqn{(0,1)} indicating the levels at which the variables should in and out from the model. This is only appropiate if \code{criterion}="p-value". By default, \code{levels} is set to be \code{c(0.05,0.05)}.
#' @param test an (optional) character string indicating the statistical test which should be used to compare nested models. The available options are: Wald ("wald"), Rao's score ("score"), likelihood-ratio ("lr") and gradient ("gradient") tests. By default, \code{test} is set to be "wald".
#' @param criterion an (optional) character string indicating the criterion which should be used to compare the candidate models. The available options are: AIC ("aic"), BIC ("bic"), adjusted deviance-based R-squared ("adjr2"), and \emph{p}-value of the \code{test} test ("p-value"). By default, \code{criterion} is set to be "bic".
#' @param ...	further arguments passed to or from other methods. For example, \code{k}, that is, the magnitude of the penalty in the AIC/QICu, which by default is set to be 2.
#' @param trace an (optional) logical switch indicating if should the stepwise reports be printed. By default, \code{trace} is set to be TRUE.
#' @param scope an (optional) list, containing components \code{lower} and \code{upper}, both formula-type objects, indicating the range of models which should be examined in the stepwise search. By default, \code{lower} is a model with no predictors and \code{upper} is the linear predictor of the model in \code{model}.
#' @details The "hybrid forward stepwise"  algorithm starts with the simplest
#' model (which may be specified at the argument \code{scope}, and by default,
#' is a model whose parameters in the linear predictor, except the intercept,
#' if any, are set to be 0), and then the candidate models are builded by
#' hierarchically adding effects in the linear predictor, whose "relevance"
#' and/or "importance" in the model fit is assessed by comparing nested models
#' (that is, by comparing the models with and without the added effect) using a
#' criterion previously specified. If an effect is added to the model then this strategy may
#' also remove any effect which, according to the criterion previously specified, no
#' longer provide an improvement in the model fit. That process remain until no more effects may be included or excluded.
#'
#' The "hybrid backward stepwise" algorithm works similarly.
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
#' upper <- cases ~ city + age + city*age
#' fit3 <- glm(upper, family=poisson("log"), offset=log(population), data=skincancer)
#' stepCriterion(fit3, direction="backward", criterion="aic", scope=list(lower=~ 1,upper=upper))
#' stepCriterion(fit3, direction="forward", criterion="p-value", test="lr")
#' @method stepCriterion glm
#' @export
#' @references James, G. and Witten, D. and Hastie, T. and Tibshirani, R. (2013, page 210) An Introduction to Statistical Learning with Applications in R, Springer, New York.

stepCriterion.glm <- function(model, criterion=c("bic","aic","adjr2","p-value","qicu"), test=c("wald","lr","score","gradient"), direction=c("forward","backward"), levels=c(0.05,0.05), trace=TRUE, scope, ...){
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
    lower <- formula(paste(deparse(lower[[2]]),"~",attr(terms(lower),"intercept")))
  }else{
    lower <- scope$lower
    upper <- scope$upper
  }
  if(is.null(model$call$data)) datas <- na.omit(get_all_vars(upper,environment(eval(model$call$formula))))
  else datas <- na.omit(get_all_vars(upper,eval(model$call$data)))

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
  if(quasi){
    if(model$family$varfun!="constant") cat("     Variance: proportional to",model$family$varfun,"\n")
    else cat("     Variance: ",model$family$varfun,"\n")
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
    if(!is.null(xxx$k)) cat("The magnitude of the penalty in the AIC/QICu was set to be ",xxx$k)
    cat("\n")
  }
  out_$final <- paste("~",as.character(oldformula)[length(oldformula)],sep=" ")
  return(invisible(out_))
}
#' @title Local Influence for Generalized Linear Models
#' @description Computes some measures and, optionally, display	graphs of them to perform
#' influence analysis based on the approaches described in Cook (1986).
#' @param object an object of class \emph{glm}.
#' @param type an (optional) character string indicating the type of approach to study the
#' local influence. The options are: the absolute value of the elements of the eigenvector which corresponds to the maximum absolute eigenvalue ("local"); and the absolute value of the elements of the main diagonal ("total"). By default, \code{type} is set to be "total".
#' @param perturbation an (optional) character string indicating the perturbation scheme
#' to apply. The options are: case weight perturbation of observations ("case-weight"); perturbation of covariates ("covariate"); and perturbation of response ("response"). By default, \code{perturbation} is set to be "case-weight".
#' @param plot.it an (optional) logical indicating if the plot of the measures of local
#' influence is required or just the data matrix in which that plot is based. By default,
#' \code{plot.it} is set to be FALSE.
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
#' @references Cook, D. (1986) Assessment of Local Influence. \emph{Journal of the Royal Statistical Society: Series B (Methodological)} 48, 133-155.
#' @references Thomas, W. and Cook, D. (1989) Assessing Influence on Regression Coefficients in Generalized Linear Models. \emph{Biometrika} 76, 741-749.
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
