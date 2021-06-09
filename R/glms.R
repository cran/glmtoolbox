#' @title Generalized Variance Inflaction Factor
#' @description Computes the generalized variance inflaction factor (GVIF) for a fitted model object.
#' @param model a fitted model object.
#' @param ...	further arguments passed to or from other methods.
#' @return An object with the values of the GVIF for all effects in the model.
#' @export gvif
gvif <- function(model,...) {
  UseMethod("gvif")
}


#' @title Function to extract estimating equations
#' @description Extracts estimating equations evaluated at the parameter estimates and the observed data for a fitted model object.
#' @param model a fitted model object.
#' @param ...	further arguments passed to or from other methods.
#' @return A vector with the value of the estimating equations evaluated at the parameter estimates and the observed data.
#' @export estequa
estequa <- function(model,...) {
  UseMethod("estequa")
}

#' @title Variable selection in regression models from a chosen criterion
#' @description Generic function for selecting variables from a fitted regression model using a chosen criterion.
#' @param model a fitted model object.
#' @param ...	further arguments passed to or from other methods.
#' @return A list which includes the descriptions of the linear predictors of the initial and final models as well as the criterion used to compare the candidate models.
#' @export stepCriterion
stepCriterion <- function(model,...) {
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
#' @param model an object of the class lm which is obtained from the fit of a weighted or unweighted normal linear model.
#' @param varformula an (optional) \code{formula} expression of the form \code{~ predictors} describing only the potential explanatory variables for the dispersion. By default, the same explanatory variables are taken as in the model for the mean.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @param ...	further arguments passed to or from other methods.
#' @details If the object \code{model} corresponds to an unweighted normal linear model then this test assess the assumption of constant variance, which
#' coincides with the (non-studentized) Breusch-Pagan test against heteroskedasticity.
#' @return A list which includes the three main attributes of the test for varying dispersion parameter: statistic ("statistic"), degrees of freedom ("df") and \emph{p}-value ("p.value").
#' @method vdtest lm
#' @export
#' @examples
#' ## Example 1
#' fit1 <- lm(mpg ~ log(hp) + log(wt), data=mtcars)
#' vdtest(fit1)
#'
#' ## Example 2
#' fit2 <- lm(Species ~ Biomass + pH, data=richness)
#' vdtest(fit2)
#'
#' fit2a <- lm(Species ~ Biomass + pH, data=richness, subset=-c(1,3,18,20))
#' vdtest(fit2a)
#'
#' ## Example 3
#' whiteside <- MASS::whiteside
#' fit3 <- lm(Gas ~ Temp + Insul + Temp*Insul, data=whiteside)
#' vdtest(fit3)
#'
#' fit3a <- lm(Gas ~ Temp + Insul + Temp*Insul, data=whiteside, subset=-c(8,9,36,46,55))
#' vdtest(fit3a)
#'
#' @references Breusch T.S. and Pagan A.R. (1979) A simple test for heteroscedasticity and random coefficient variation. \emph{Econometrica} 47, 1287–1294.
#'
#' Cook R.D. and Weisberg S. (1983) Diagnostics for heteroscedasticity in regression. \emph{Biometrika} 70, 1–10.
#'
#' @seealso \link{vdtest.glm}
vdtest.lm <- function(model,varformula,verbose=TRUE,...){
  if(!missingArg(varformula)){
    if(is.null(model$call$data)) m <- get_all_vars(eval(varformula))
    else m <- get_all_vars(eval(varformula),eval(model$varformula))
    Z <- model.matrix(varformula,m)
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
#' @description Performs Rao's score test for varying dispersion parameter in weighted and unweighted generalized linear models in which the response distribution is assumed to be gaussian, Gamma or inverse gaussian.
#' @param model an object of the class glm which is obtained from the fit of a weighted or unweighted generalized linear model in which the response distribution is assumed to be gaussian, Gamma or inverse gaussian.
#' @param varformula an (optional) \code{formula} expression of the form \code{~ predictors} describing only the potential explanatory variables for the dispersion. By default, the same explanatory variables are taken as in the model for the mean.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @param ...	further arguments passed to or from other methods.
#' @details The aim of this test is to assess the assumption of constant dispersion parameter in generalized linear models. If the object \code{model} corresponds to an unweighted generalized linear model then this test assess assumptions of constant variance and constant coefficient of variation on models in which the response distribution is assumed to be gaussian and Gamma, respectively.
#' @return A list which includes the three main attributes of the test for varying dispersion parameter: statistic ("statistic"), degrees of freedom ("df") and \emph{p}-value ("p.value").
#' @examples
#' ## Example 1
#' Auto <- ISLR::Auto
#' fit1 <- glm(mpg ~ weight*horsepower, family=inverse.gaussian("log"), data=Auto)
#' vdtest(fit1)
#'
#' ## Example 2
#' fit2 <- glm(rtime ~ log(distance) + log(cclimb), family=Gamma("log"), data=races)
#' vdtest(fit2)
#' @method vdtest glm
#' @export
#' @references Wei BC., Shi JQ., Fung WK. and Hu YQ. (1998) Testing for Varying Dispersion in Exponential Family Nonlinear Models. \emph{Annals of the Institute of Statistical Mathematics} 50, 277–294.
#'
#' @seealso \link{vdtest.lm}
vdtest.glm <- function(model,varformula,verbose=TRUE,...){
  if(model$family$family!="gaussian" & model$family$family!="Gamma" & model$family$family!="inverse.gaussian")
    stop("Only gaussian, Gamma and inverse.gaussian families are supported!!",call.=FALSE)
  if(!missingArg(varformula)){
    if(is.null(model$call$data)) m <- get_all_vars(eval(varformula))
    else m <- get_all_vars(eval(varformula),eval(model$varformula))
    Z <- model.matrix(varformula,m)
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
  Zstar2 <- (solve(t(Zstar)%*%Zstar))[-1,-1]
  sc = 0.5*(t(tau)%*%Z)%*%Zstar2%*%(t(Z)%*%tau)
  if(verbose){
    cat("\n             Score test for varying dispersion parameter\n\n")
    cat("          Statistic = ",round(sc,digits=5),"\n degrees of freedom = ",p,"\n            p-value = ",format.pval(1-pchisq(sc,p)),"\n\n")
  }
  return(invisible(list(statistic=sc,df=p,p.value=format.pval(1-pchisq(sc,p)))))
}

#' @title Variable Selection in Normal Linear Models
#' @description Performs variable selection in normal linear models.
#' @param model an object of the class lm which is obtained from the fit of a normal linear model. The linear predictor of the model whose fit is stored in this lm object is the more complex candidate which should be considered by the variable selection procedure.  The more simple model which should be considered by the variable selection procedure is that with just the Intercept, if there is.
#' @param direction an (optional) character string indicating the mode of variable selection which should be used. The available options are: deleting variables ("backward")  and adding variables ("forward"). By default, \code{direction} is set to be "backward".
#' @param level an (optional) numeric value in the interval (0,1) indicating the significance level chosen to perform the F tests. This is only appropiate if \code{criterion="p-value"}. By default, \code{level} is set to be 0.05.
#' @param criterion an (optional) character string indicating the criterion which should be used to compare the candidate models. The available options are: AIC ("aic"), BIC ("bic"), adjusted R-squared ("adjr2"), predicted R-squared ("predr2"), Mallows' CP ("cp") and \emph{p}-value of the F test ("p-value"). By default, \code{criterion} is set to be "bic".
#' @param ...	further arguments passed to or from other methods. For example, \code{k}, that is, the magnitude of the penalty in the AIC, which by default is set to be 2.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @return A list which contains the following objects:
#' \itemize{
#' \item{\code{initial}:}{ an expression describing the linear predictor of the "initial" model.}
#' \item{\code{final}:}{ an expression describing the linear predictor of the "final" model.}
#' \item{\code{criterion}:}{ a character string describing the criterion chosen to compare the candidate models.}
#' }
#' @examples
#' ## Example 1
#' fit1 <- lm(log(Ozone) ~ Solar.R*Temp*Wind, data=airquality)
#' stepCriterion(fit1, direction="forward", criterion="adjr2")
#' stepCriterion(fit1, direction="forward", criterion="bic")
#' stepCriterion(fit1, direction="forward", criterion="p-value", level=0.05)
#'
#' ## Example 2
#' fit2 <- lm(mpg ~ log(hp)*log(wt)*qsec, data=mtcars)
#' stepCriterion(fit2, direction="backward", criterion="bic")
#' stepCriterion(fit2, direction="forward", criterion="cp")
#' stepCriterion(fit2, direction="backward", criterion="p-value", level=0.05)
#' @seealso \link{stepCriterion.glm}, \link{stepCriterion.overglm}, \link{stepCriterion.glmgee}
#' @method stepCriterion lm
#' @export

stepCriterion.lm <- function(model, criterion=c("bic","aic","adjr2","predr2","cp","p-value"), direction=c("backward","forward"), level=0.05, verbose=TRUE, ...){
  xxx <- list(...)
  if(is.null(xxx$k)) k <- 2 else k <- xxx$k
  criterion <- match.arg(criterion)
  direction <- match.arg(direction)

  if(is.null(model$call$data)) datas <- na.omit(get_all_vars(eval(model$call$formula)))
  else datas <- na.omit(get_all_vars(eval(model$call$formula),eval(model$call$data)))
  out <- function(fitt,names.effets){
    left <- lapply(as.list(strsplit(attr(fitt$terms,"term.labels"),":",fixed=TRUE)),function(x) paste(sort(x),collapse=":"))
    right <- lapply(as.list(strsplit(names.effets,":",fixed=TRUE)),function(x) paste(sort(x),collapse=":"))
    !(left %in% right)
  }
  constr.formula <- function(fitt,inter,term,action){
    if(missingArg(term)) names.effects <- attr(fitt$terms,"term.labels")
    else{if(action=="-") names.effects <- attr(fitt$terms,"term.labels")[attr(fitt$terms,"term.labels")!=term]
    else names.effects <- c(attr(fitt$terms,"term.labels"),term)}
    if(length(names.effects)>0)
      paste(attr(fitt$terms,"variables")[2],ifelse(inter,"~ 1 +","~ 0 +"),paste(names.effects,collapse=" + "))
    else paste(attr(fitt$terms,"variables")[2],ifelse(inter,"~ 1 ","~ 0 "))
  }
  lmstats <- function(fitt){
    X <- model.matrix(fitt)
    nano <- summary(fitt)
    sigma2 <- nano$sigma^2
    if(length(X)!=0){
      Xw <- X*matrix(sqrt(weights),nrow(X),ncol(X))
      salida <- svd(Xw)
      h <- apply(salida$u^2,1,sum)
      press <- sum((resid(fitt)/(1-h))^2)
    }else press <- sum(resid(fitt)^2)
    salida <- c(round(AIC(fitt,k=k),digits=3),round(BIC(fitt),digits=3),nano$adj.r.squared,1-press/sigma20p,round(fitt$df.residual*(sigma2/sigma20-1) + ncol(X),digits=3))
    salida
  }
  tol <- TRUE
  count <- 0
  inter <- ifelse(length(coef(model))>0,names(coef(model))[1]=="(Intercept)",FALSE)
  oldformula <- constr.formula(model,inter)
  if(direction=="forward") oldformula <- constr.formula(model,inter,term=attr(model$terms,"term.labels"),action="-")

  ps <- ifelse(criterion=="p-value",FALSE,TRUE)
  if(is.null(model$offset)) offset <- matrix(0,length(model$residuals),1) else offset <- model$offset
  if(is.null(model$weights)) weights <- matrix(1,length(model$residuals),1) else weights <- model$weights
  if(verbose){
    cat("\n               Family: gaussian\n")
    cat("                 Link: identity")
    cat("\n****************************************************************************")
    cat("\nInitial model:\n")
  }
  fit.x <- model
  if(direction=="forward") fit.x <- lm(as.formula(oldformula),offset=offset,weights=weights,data=datas)
  if(verbose) cat(oldformula,"\n")
  out_ <- list(initial=oldformula,criterion=criterion)
  newformula <- oldformula
  sale <- " "
  inter <- ifelse(length(coef(model))>0,names(coef(model))[1]=="(Intercept)",FALSE)
  if(!inter & direction=="forward")
    stop("The forward variable selection and non-intercept models are not compatible!!",call.=FALSE)
  names.col <- c("df"," BIC "," AIC ","adj.r2(^)","pred.r2(~)"," CP(&) "," Pr(F>)(*)")
  delta <- c(1,1,1,-1,-1,1,-1)
  id0 <-  c(" ","bic","aic","adjr2","predr2","cp","p-value")==criterion
  if(direction=="forward") delta[length(delta)] <- 1
  long <- max(nchar(attr(model$terms,"term.labels")))+2
  nano <- summary(model)
  sigma20 <- nano$sigma^2
  sigma20p <- nano$sigma^2*model$df.residual/(1 - nano$r.squared)
  while(tol){
    if(length(attr(fit.x$terms,"term.labels"))==0) names.effects <- "" else names.effects <- attr(fit.x$terms,"term.labels")
    if(direction=="backward"){
      results <- matrix(NA,length(names.effects)+1,length(delta))
      if(criterion=="p-value") results[1,ncol(results)] <- -1
      if(count == 0) results[1,2:(length(delta)-1)] <- lmstats(fit.x) else results[1,2] <- 0
      s <- attr(fit.x$terms,"factors")
      for(i in 1:length(names.effects)){
        if(all(apply(as.matrix(s[,-i]*s[,i]),2,sum) < sum(s[,i]))){
          formula0 <- constr.formula(fit.x,inter,term=names.effects[i],action="-")
          fit0 <- lm(as.formula(formula0),offset=offset,weights=weights,data=datas)
          results[i+1,1] <- length(coef(fit.x)) - length(coef(fit0))
          results[i+1,2:(length(delta)-1)] <- lmstats(fit0)
          results[i+1,length(delta)] <- anova(fit.x,fit0,test="F")$Pr[2]
        }
      }
      names.effects <- cbind("-",names.effects)
    }
    if(direction=="forward"){
      outs <- out(model,names.effects)
      s2 <- as.matrix(attr(model$terms,"factors")[,outs])
      names.effects <- attr(model$terms,"term.labels")[outs]
      results <- matrix(NA,length(names.effects)+1,length(delta))
      if(criterion=="p-value") results[1,length(delta)] <- 1
      if(count == 0) results[1,2:(length(delta)-1)] <- lmstats(fit.x) else results[1,2] <- 0
      for(i in 1:length(names.effects)){
        if(sum(apply(as.matrix(s2[,-i]),2,function(x) sum(s2[,i]*x)==sum(x)))==0){
          formula0 <- constr.formula(fit.x,inter,term=names.effects[i],action="+")
          fit0 <- lm(as.formula(formula0),offset=offset,weights=weights,data=datas)
          results[i+1,1] <- length(coef(fit0)) - length(coef(fit.x))
          results[i+1,2:(length(delta)-1)] <- lmstats(fit0)
          results[i+1,length(delta)] <- anova(fit.x,fit0,test="F")$Pr[2]
        }
      }
      names.effects <- cbind("+",names.effects)
    }
    shows <- !is.na(results[,2])
    results <- cbind(results[shows,1],results[shows,id0],results[shows,!id0])
    colnames(results) <- c(names.col[1],names.col[id0],names.col[!id0])
    results <- results[,-3]
    if(count > 0){
      if(criterion=="p-value") results[1,-c(1,2)] <- recicla
      else results[1,-c(1,length(delta))] <- recicla
    }
    names.effects2 <- cbind(names.effects[shows[-1],1],names.effects[shows[-1],2])
    names.effects3 <- c("<none>",paste(names.effects2[,1],names.effects2[,2]))
    rownames(results) <- names.effects3
    charac <- rownames(results)[2]
    rownames(results)[2] <- paste(charac,paste(replicate(max(long-nchar(charac),0)," "),collapse=""),collapse="")
    ids <- min(results[,2]*delta[id0])==(results[,2]*delta[id0])
    if(verbose) cat("\nStep",count,":",sale,"\n")
    indexes <- order(results[,2]*delta[id0],na.last=TRUE)
    if(sum(ids) > 1){
      nexto <- c(1:length(delta))[id0]
      nexto <- ifelse(nexto==length(delta),2,nexto+1)
      indexes <- order(delta[nexto]*results[,3],na.last=TRUE)
    }
    ids <- c(1:length(indexes))==indexes[1]
    if(criterion=="p-value"){
      recicla <- results[names.effects3==names.effects3[ids],-c(1,2)]
      if(direction=="backward") ps <- ifelse(max(results[,2]) > level,TRUE,FALSE)
      if(direction=="forward") ps <- ifelse(min(results[,2]) < level,TRUE,FALSE)
      results[names.effects3=="<none>",2] <- NA
    }else recicla <- results[names.effects3==names.effects3[ids],-c(1,length(delta))]
    tst.ind <- c(2:7)[colnames(results)[-1]!=names.col[2] & colnames(results)[-1]!=names.col[3] & colnames(results)[-1]!=names.col[6]]
    if(verbose) printCoefmat(results[indexes,],cs.ind=1,tst.ind=tst.ind,dig.tst=4,na.print=" ",signif.stars=FALSE)
    count <- count + 1
    if(names.effects3[ids]!="<none>" & ps==TRUE){
      ids <- ids[-1]
      sale <- paste(names.effects2[ids,1],names.effects2[ids,2])
      newformula <- constr.formula(fit.x,inter,term=names.effects2[ids,2],action=names.effects2[ids,1])
      if(nrow(names.effects)==1 | (nrow(names.effects)==2 & !inter)){
        tol <- FALSE
        if(verbose) cat("\nStep",count,":",sale,"\n")
      }
      fit.x <- lm(as.formula(newformula),offset=offset,weights=weights,data=datas)
    }else tol <- FALSE
  }
  if(verbose){
    cat("\nFinal model:\n")
    cat(newformula,"\n")
    cat("****************************************************************************\n")
    cat("(^)  adjusted R-squared\n")
    cat("(~)  predicted R-squared\n")
    cat("(&)  Mallows' CP\n")
    cat("(*)  p-value of the F test")
    if(criterion=="p-value" & direction=="backward") cat(" ( effects are dropped when their p-values are higher than",level,")")
    if(criterion=="p-value" & direction=="forward")  cat(" ( effects are included when their p-values are lower than",level,")")
    if(!is.null(xxx$k)) cat("The magnitude of the penalty in the AIC was set to be ",xxx$k)
    cat("\n")
  }
  out_$final <- newformula
  return(invisible(out_))
}


#' @title Normal QQ-plot with simulated envelope of model residuals
#' @description Produces a normal QQ-plot with simulated envelope of residuals obtained from the fit of a generalized linear model.
#' @param object an object of the class glm which is obtained from the fit of a generalized linear model.
#' @param rep an (optional) positive integer indicating the number of replicates which should be used to build the simulated envelope. By default, \code{rep} is set to be 100.
#' @param conf an (optional) value in the interval (0,1) indicating the confidence level which should be used to build the pointwise confidence intervals, which form the envelope. By default, \code{conf} is set to be 0.95.
#' @param type a character string indicating the type of residuals which should be used. The available options are: randomized quantile ("quantile"), deviance ("deviance") and pearson ("pearson") residuals. By default, \code{type} is set to be "quantile".
#' @param standardized an (optional) logical switch indicating if the residuals should be standardized by dividing by the square root of \eqn{(1-h)}, where \eqn{h} is a measure of leverage. By default, \code{standardized} is set to be FALSE.
#' @param plot.it an (optional) logical switch indicating if the normal QQ-plot with simulated envelope of residuals is required or just the data matrix in which it is based. By default, \code{plot.it} is set to be TRUE.
#' @param identify an (optional) positive integer indicating the number of individuals to identify on the QQ-plot with simulated envelope of residuals. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used to include graphical parameters to customize the plot. For example,  \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix with \eqn{n} rows and four columns: the first three (Lower limit, Median, and Upper limit) describe the simulated envelope, that is, each row corresponds to the quantiles (1-\code{conf})/2, 0.5 and (1+\code{conf})/2 of the random sample of size \code{rep} of the \eqn{i}-th order statistic of the residuals type \code{type} for \eqn{i=1,2,...,n}; and the last one column (Residuals) contains the observed type \code{type} residuals.
#' @details The simulated envelope is builded by simulating \code{rep} independent realizations of the response variable for each
#' individual, which is accomplished taking into account the following: (1) the model assumption about the distribution of the response variable; (2) the estimates of the parameters in the linear predictor; and (3) the estimate of the dispersion parameter. The interest model is re-fitted \code{rep} times, as each time the vector of observed responses is replaced by one of the simulated samples. The residuals type \emph{type} are computed and then ordered for each replicate, so that for each \eqn{i=1,2,...,n}, where \eqn{n} is the number of individuals in the sample, there is a random sample of size \code{rep} of the \eqn{i}-th order statistic of the residuals type \emph{type}. Therefore, the simulated envelope is composed of the quantiles (1-\code{conf})/2 and (1+\code{conf})/2 of the random sample of size \code{rep} of the \eqn{i}-th order statistic of the residuals type \emph{type} for \eqn{i=1,2,...,n}. Families \code{quasi()}, \code{quasipoisson} and \code{quasibinomial} are not supported.
#' @references Atkinson A.C. (1985) \emph{Plots, Transformations and Regression}. Oxford University Press, Oxford.
#' @references Davison A.C. and Gigli A. (1989) Deviance Residuals and Normal Scores Plots. \emph{Biometrika} 76, 211-221.
#' @references Dunn P.K. and Smyth G.K. (1996) Randomized Quantile Residuals. \emph{Journal of Computational and Graphical Statistics} 5, 236-244.
#' @references Pierce D.A. and Schafer D.W. (1986) Residuals in Generalized Linear Models. \emph{Journal of the American Statistical Association} 81, 977-986.
#' @seealso \link{envelope.lm}, \link{envelope.overglm}
#' @examples
#' # Example 1
#' fit1 <- glm(infections ~ frequency + location, family=poisson, data=swimmers)
#' envelope(fit1, type="quantile", col="red", pch=20,col.lab="blue",
#'          col.axis="blue",col.main="black",family="mono",cex=0.8)
#'
#' # Example 2
#' fit2 <- glm(cbind(cells,200-cells) ~ tnf + ifn + tnf*ifn, family=binomial, data=cellular)
#' envelope(fit2, type="deviance",col="red", pch=20,col.lab="blue",
#'          col.axis="blue",col.main="black",family="mono",cex=0.8)
#'
#' # Example 3
#' fit3 <- glm(cancer/exposed ~ dose, family=binomial, weights=exposed, data=bladder)
#' envelope(fit3, type="pearson", col="red", pch=20,col.lab="blue",
#'          col.axis="blue",col.main="black",family="mono",cex=0.8)
#'
#' # Example 4
#' fit4 <- glm(cases ~ offset(log(population)) + city + age, family=poisson("log"), data=skincancer)
#' envelope(fit4, type="quantile", col="red", pch=20,col.lab="blue",
#'          col.axis="blue",col.main="black",family="mono",cex=0.8)
#' @importFrom stats dbinom delete.response dpois glm
#'             pbinom pgamma ppoints ppois qqplot rchisq
#' @method envelope glm
#' @export
envelope.glm <- function(object, rep=100, conf=0.95, type=c("quantile","deviance","pearson"), standardized=FALSE, plot.it=TRUE, identify, ...){
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
        phis <- summary((resp-fits$fitted.values)^2*weights/object$family$variance(fits$fitted.values))/fits$df.residual
        if(object$family$family=="binomial" | object$family$family=="poisson") phis <- 1
        if(type=="quantile")
          rs <- quantileres(object$family$family,resp,fits$fitted.values,phis/weights)
        if(type=="deviance"){
          rs <- sqrt(object$family$dev.resids(resp,fits$fitted.values,weights)/sqrt(phis))
          rs <- ifelse(resp >= fits$fitted.values,1,-1)*rs
        }
        if(type=="pearson")
          rs <- (resp-fits$fitted.values)*sqrt(weights/object$family$variance(fits$fitted.values)*phis)
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
  rd <- if(type=="quantile")
    rd <- quantileres(object$family$family,object$y,mu,phi/object$prior.weights)
  else
    rd <- residuals(object,type=type)/sqrt(phi)
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
    outm <- do.call("qqnorm",nano)
    if(!missingArg(identify)){
      identify(outm$x,outm$y,labels=labels,n=max(1,floor(abs(identify))))
    }
  }
  out_ <- cbind(t(es),rd)
  colnames(out_) <- c("Lower limit","Median","Upper limit","Residuals")
  return(invisible(out_))
}

#' @title Normal QQ-plot with simulated envelope of model residuals
#' @description Produces a normal QQ-plot with simulated envelope of residuals obtained from the fit of a normal linear model.
#' @param object an object of the class lm which is obtained from the fit of a normal linear model.
#' @param rep an (optional) positive integer indicating the number of replicates which should be used to build the simulated envelope. By default, \code{rep} is set to be 100.
#' @param conf an (optional) value in the interval (0,1) indicating the confidence level which should be used to build the pointwise confidence intervals, which form the envelope. By default, \code{conf} is set to be 0.95.
#' @param type a character string indicating the type of residuals which should be used. The available options are: internally Studentized ("internal") and externally Studentized ("external") residuals. See Cook and Weisberg (1982, pages 18-20).
#' @param plot.it an (optional) logical switch indicating if the normal QQ-plot with simulated envelope of residuals is required or just the data matrix in which it is based. By default, \code{plot.it} is set to be TRUE.
#' @param identify an (optional) positive integer value indicating the number of individuals to identify on the QQ-plot with simulated envelope of residuals. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix with \eqn{n} rows and four columns: the first three (Lower limit, Median, and Upper limit) describe the simulated envelope, that is, each row corresponds to the quantiles (1-\code{conf})/2, 0.5 and (1+\code{conf})/2 of the random sample of size \code{rep} of the \eqn{i}-th order statistic of the residuals type \code{type} for \eqn{i=1,2,...,n}; and the last one column (Residuals) contains the observed type \code{type} residuals.
#' @details The simulated envelope is builded by simulating rep independent realizations of the response variable for each individual, which is accomplished taking into account the following: (1) the estimates of the parameters in the linear predictor; and (2) the estimate of the dispersion parameter. The interest model is re-fitted \code{rep} times, as each time the vector of observed responses is replaced by one of the simulated samples. The residuals type \code{type} are computed and then ordered for each replicate, so that for each \eqn{i=1,2,...,n}, where n is the number of individuals in the sample, there is a random sample of size \code{rep} of the \eqn{i}-th order statistic of the residuals type \code{type}. Therefore, the simulated envelope is composed of the quantiles (1-\code{conf})/2 and (1+\code{conf})/2 of the random sample of size \code{rep} of the \eqn{i}-th order statistic of the residuals type \code{type} for \eqn{i=1,2,...,n}.
#' @references Atkinson A.C. (1985) \emph{Plots, Transformations and Regression}. Oxford University Press, Oxford.
#' @references Cook R.D. and Weisberg S. (1982) \emph{Residuals and Influence in Regression}. Chapman and Hall, New York.
#' @seealso \link{envelope.glm}, \link{envelope.overglm}
#' @examples
#' # Example 1
#' fit1 <- lm(Species ~ Biomass + pH + Biomass*pH, data=richness)
#' envelope(fit1, type="internal", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' # Example 2
#' fit2 <- lm(mpg ~ log(hp) + log(wt), data=mtcars)
#' envelope(fit2, type="external", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
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
    outm <- do.call("qqnorm",nano)
    if(!missingArg(identify)){
      identify(outm$x,outm$y,labels=labels,n=max(1,floor(abs(identify))))
    }
  }
  out_ <- cbind(t(es),td)
  colnames(out_) <- c("Lower limit","Median","Upper limit","Residuals")
  return(invisible(out_))
}



#' @title The Hosmer-Lemeshow Goodness-of-Fit Test
#' @description Computes the Hosmer-Lemeshow goodness-of-fit test for a generalized linear model fitted to binary responses.
#' @param model an object of the class glm which is obtained from the fit of a generalized linear model where the distribution for the response variable is assumed to be binomial.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @param ... further arguments passed to or from other methods.
#' @return A list which contains the following objects:
#' \itemize{
#' \item \code{hm:}{ A matrix with the values of Group, Size, Observed and Expected, which are required to compute the statistic of the test.}
#' \item \code{statistic:}{ The value of the statistic of the test.}
#' \item \code{df:}{ The number of degrees of freedom, given by the number of groups minus 2.}
#' \item \code{p.value:}{ The \emph{p}-value of the test computed using the Chi-square distribution.}
#' }
#' @references Hosmer, D.W. and Lemeshow, S. (2000) \emph{Applied Logistic Regression. 2nd ed.} John Wiley & Sons, New York.
#' @examples
#' ## Example 1
#' fit1 <-  glm(cancer/exposed ~ dose, weights=exposed, family=binomial("logit"), data=bladder)
#' hltest(fit1)
#'
#' ## Example 2
#' fit2 <-  glm(cancer/exposed ~ dose, weights=exposed, family=binomial("logit"), data=liver)
#' hltest(fit2)
#'
#' ## Example 3
#' burn1000 <- aplore3::burn1000
#' mod <- death ~ age + tbsa + inh_inj + age*inh_inj + tbsa*inh_inj
#' fit3 <- glm(mod, family=binomial("logit"), data=burn1000)
#' hltest(fit3)
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
#' @references Hanley J.A. and McNeil B.J. (1982) The Meaning and Use of the Area under a Receiver Operating Characteristic (ROC) Curve. \emph{Radiology} 143, 29–36.
#' @return A list which contains the following objects:
#' \itemize{
#' \item{\code{roc:}}{ A matrix with the Cutoffs and the associated estimates of Sensitivity and Specificity.}
#' \item{\code{auroc:}}{ The exact area under the ROC curve.}
#' \item{\code{gini:}}{ The value of the Gini coefficient computed as 2(\code{auroc}-0.5).}
#' \item{\code{ks:}}{ The value of the Kolmogorov-Smirnov statistic computed as the maximum value of |1-Sensitivity-Specificity|.}
#' }
#' @examples
#' burn1000 <- aplore3::burn1000
#'
#' ## splitting the sample
#' ## 70% for the training sample and 30% for the validation sample
#' burn1000 <- within(burn1000, sampleof <- "validation")
#' s <- sample(nrow(burn1000),nrow(burn1000)*0.7)
#' burn1000$sampleof[s] <- "training"
#'
#' mod <- death ~ age + tbsa + inh_inj + age*inh_inj + tbsa*inh_inj
#' training <- subset(burn1000,sampleof=="training")
#' fit <- glm(mod, family=binomial("logit"), data=training)
#'
#' ## ROC curve for the training sample
#' ROCc(fit, col="red", col.lab="blue", col.axis="black",
#'      col.main="black", family="mono")
#'
#' validation <- subset(burn1000, sampleof=="validation")
#' probs <- predict(fit, newdata=validation, type="response")
#' responses <- with(validation, ifelse(death=="Dead",1,0))
#'
#' ## ROC curve for the validation sample
#' ROCc(cbind(responses,probs), col="red", col.lab="blue",
#'      col.axis="black", col.main="black", family="mono")
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
#' @param test an (optional) character string indicating the required type of test. The available options are: Wald ("wald"), Rao's score ("score"), Terrell's gradient ("gradient"), and likelihood ratio ("lrt") tests. By default, \code{test} is set to be "wald".
#' @param verbose an (optional) logical indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @details The Wald, Rao's score and Terrell's gradient tests are performed using the expected Fisher information matrix.
#' @references Buse A. (1982) The Likelihood Ratio, Wald, and Lagrange Multiplier Tests: An Expository Note. \emph{The American Statistician} 36, 153-157.
#' @references Terrell G.R. (2002) The gradient statistic. \emph{Computing Science and Statistics} 34, 206 – 215.
#' @examples
#' ## Example 1
#' Auto <- ISLR::Auto
#' fit1 <- glm(mpg ~ weight, family=inverse.gaussian("log"), data=Auto)
#' fit2 <- update(fit1, . ~ . + horsepower)
#' fit3 <- update(fit2, . ~ . + horsepower:weight)
#' anova2(fit1, fit2, fit3, test="lrt")
#' anova2(fit1, fit2, fit3, test="score")
#' anova2(fit1, fit2, fit3, test="wald")
#' anova2(fit1, fit2, fit3, test="gradient")
#'
#' ## Example 2
#' burn1000 <- aplore3::burn1000
#' mod <- death ~ age + tbsa + inh_inj
#' fit1 <- glm(mod, family=binomial("logit"), data=burn1000)
#' fit2 <- update(fit1, . ~ . + inh_inj + age*inh_inj + tbsa*inh_inj)
#' anova2(fit1, fit2, test="lrt")
#' anova2(fit1, fit2, test="score")
#' anova2(fit1, fit2, test="wald")
#' anova2(fit1, fit2, test="gradient")
#'
#' ## Example 3
#' fit <- glm(lesions ~ 1 + time, family=poisson("log"), data=aucuba)
#' anova2(fit, test="lrt")
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

anova2 <- function(object,...,test=c("wald","lrt","score","gradient"),verbose=TRUE){
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
    if(test=="lrt") sc <- (x[[i-1]]$deviance - x[[i]]$deviance)/phi
    if(test=="score" | test=="gradient"){
      X <- model.matrix(x[[i]])
      u0 <- crossprod(X[,ids],resid(x[[i-1]],type="pearson")*sqrt(x[[i-1]]$weights))/phi
      if(test=="score"){
        v0 <- solve(crossprod(X,X*matrix(x[[i-1]]$weights,nrow(X),ncol(X))))[ids,ids]
        sc <- crossprod(u0,v0)%*%u0
      }else sc <- abs(crossprod(u0,coef(x[[i]])[ids]))
    }
    df <- sum(ids)
    out_[i-1,] <- cbind(sc,df,1-pchisq(sc,df))
  }
  colnames(out_) <- c(" Chi  ", " Df", "  Pr(>Chi)")
  rownames(out_) <- paste(1:(hast-1),"vs",2:hast)
  if(verbose){
    test <- switch(test,"lrt"="Likelihood-ratio test",
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

#' @title Variable Selection in Generalized Linear Models
#' @description Performs variable selection in generalized linear models.
#' @param model an object of the class glm which is obtained from the fit of a generalized linear model. The linear predictor of the model whose fit is stored in this glm object is the more complex candidate which should be considered by the variable selection procedure.  The more simple model which should be considered by the variable selection procedure is that with just the Intercept, if there is.
#' @param direction an (optional) character string indicating the mode of variable selection which should be used. The available options are: deleting variables ("backward")  and adding variables ("forward"). By default, \code{direction} is set to be "backward".
#' @param level an (optional) numeric value in the interval (0,1) indicating the significance level chosen to perform the F tests. This is only appropiate if \code{criterion}="p-value". By default, \code{level} is set to be 0.05.
#' @param test an (optional) character string indicating the statistical test which should be used to compare nested models. The available options are: Wald ("wald"), Rao's score ("score"), likelihood ratio ("lrt") and gradient ("gradient") tests. By default, \code{test} is set to be "wald".
#' @param criterion an (optional) character string indicating the criterion which should be used to compare the candidate models. The available options are: AIC ("aic"), BIC ("bic"), adjusted deviance-based R-squared ("adjr2"), and \emph{p}-value of the \code{test} test ("p-value"). By default, \code{criterion} is set to be "bic".
#' @param ...	further arguments passed to or from other methods. For example, \code{k}, that is, the magnitude of the penalty in the AIC, which by default is set to be 2.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @return A list which contains the following objects:
#' \itemize{
#' \item{\code{initial}:}{ an expression describing the linear predictor of the "initial" model.}
#' \item{\code{final}:}{ an expression describing the linear predictor of the "final" model.}
#' \item{\code{criterion}:}{ a character string describing the criterion chosen to compare the candidate models.}
#' }
#' @seealso \link{stepCriterion.lm}, \link{stepCriterion.overglm}, \link{stepCriterion.glmgee}
#' @examples
#' ## Example 1
#' Auto <- ISLR::Auto
#' mod <- mpg ~ cylinders + displacement + acceleration + origin + horsepower*weight
#' fit1 <- glm(mod, family=inverse.gaussian("log"), data=Auto)
#' stepCriterion(fit1, direction="forward", criterion="p-value", test="lrt")
#' stepCriterion(fit1, direction="backward", criterion="adjr2")
#'
#' ## Example 2
#' burn1000 <- aplore3::burn1000
#' burn1000 <- within(burn1000, death <- factor(death, levels=c("Dead","Alive")))
#' mod2 <- death ~ age + gender + race + tbsa + inh_inj + flame + age*inh_inj + tbsa*inh_inj
#' fit2 <- glm(mod2, family=binomial("logit"), data=burn1000)
#' stepCriterion(fit2, direction="backward", criterion="bic")
#' stepCriterion(fit2, direction="forward", criterion="p-value", test="score")
#'
#' ## Example 3
#' fit3 <- glm(cases ~ offset(log(population)) + city*age, family=poisson("log"), data=skincancer)
#' stepCriterion(fit3, direction="backward", criterion="adjr2")
#' stepCriterion(fit3, direction="forward", criterion="p-value", test="lrt")
#' @method stepCriterion glm
#' @export

stepCriterion.glm <- function(model, criterion=c("bic","aic","adjr2","p-value"), test=c("wald","lrt","score","gradient"), direction=c("backward","forward"), level=0.05, verbose=TRUE, ...){
  xxx <- list(...)
  if(is.null(xxx$k)) k <- 2 else k <- xxx$k
  criterion <- match.arg(criterion)
  direction <- match.arg(direction)
  test <- match.arg(test)
  if(test=="wald") test2 <- "Wald test"
  if(test=="score") test2 <- "Rao's score test"
  if(test=="lrt") test2 <- "likelihood-ratio test"
  if(test=="gradient") test2 <- "Gradient test"

  if(is.null(model$call$data)) datas <- na.omit(get_all_vars(eval(model$call$formula)))
  else datas <- na.omit(get_all_vars(eval(model$call$formula),eval(model$call$data)))
  out <- function(fitt,names.effets){
    left <- lapply(as.list(strsplit(attr(fitt$terms,"term.labels"),":",fixed=TRUE)),function(x) paste(sort(x),collapse=":"))
    right <- lapply(as.list(strsplit(names.effets,":",fixed=TRUE)),function(x) paste(sort(x),collapse=":"))
    !(left %in% right)
  }
  constr.formula <- function(fitt,inter,term,action){
    if(missingArg(term)) names.effects <- attr(fitt$terms,"term.labels")
    else{if(action=="-") names.effects <- attr(fitt$terms,"term.labels")[attr(fitt$terms,"term.labels")!=term]
    else names.effects <- c(attr(fitt$terms,"term.labels"),term)}
    if(length(names.effects)>0)
      paste(attr(fitt$terms,"variables")[2],ifelse(inter,"~ 1 +","~ 0 +"),paste(names.effects,collapse=" + "))
    else paste(attr(fitt$terms,"variables")[2],ifelse(inter,"~ 1 ","~ 0 "))
  }
  glmstats <- function(fitt){
    if(inter) r2 <- 1 - (fitt$deviance/fitt$df.residual)/(fitt$null.deviance/fitt$df.null)
    else r2 <- fitt$deviance/fitt$df.residual
    if(quasi) return(c(0,0,r2))
    else return(c(AIC(fitt,k=k),BIC(fitt),r2))
  }
  tol <- TRUE
  count <- 0
  inter <- ifelse(length(coef(model))>0,names(coef(model))[1]=="(Intercept)",FALSE)
  oldformula <- constr.formula(model,inter)
  if(direction=="forward") oldformula <- constr.formula(model,inter,term=attr(model$terms,"term.labels"),action="-")

  ps <- ifelse(criterion=="p-value",FALSE,TRUE)
  if(is.null(model$offset)) offset <- matrix(0,length(model$residuals),1) else offset <- model$offset
  if(is.null(model$weights)) weights <- matrix(1,length(model$residuals),1) else weights <- model$weights
  if(verbose){
    cat("\n  Family: ",model$family$family,"\n")
    cat("    Link: ",model$family$link,"\n")
    if(model$family$family=="quasi"){
      if(model$family$varfun!="constant") cat("Variance:  proportional to",model$family$varfun,"\n")
      else cat("Variance: ",model$family$varfun,"\n")
    }
    cat("\nInitial model:\n")
  }
  fit.x <- model
  if(direction=="forward") fit.x <- glm(as.formula(oldformula),weights=model$prior.weights,offset=model$offset,family=model$family,data=datas)
  cat(oldformula,"\n")
  out_ <- list(initial=oldformula,criterion=criterion)
  newformula <- oldformula
  sale <- " "
  inter <- ifelse(length(coef(model))>0,names(coef(model))[1]=="(Intercept)",FALSE)
  if(model$family$family=="quasi" | model$family$family=="quasibinomial" | model$family$family=="quasipoisson"){
    quasi <- TRUE
    if(criterion=="bic" | criterion=="aic") criterion <- "adjr2"
  }else quasi <- FALSE
  if(!inter & direction=="forward")
    stop("The forward variable selection and non-intercept models are not compatible!!",call.=FALSE)
  names.col <- c("Df","AIC  ","BIC  ","adj.r2(^)","p-value(*)")
  delta <- c(1,1,1,-1,-1)
  id0 <-  c(" ","aic","bic","adjr2","p-value")==criterion
  if(!inter) delta <- c(1,1,1,1,-1)
  if(direction=="forward") delta[5] <- 1
  long <- max(nchar(attr(model$terms,"term.labels")))+2
  while(tol){
    if(length(attr(fit.x$terms,"term.labels"))==0) names.effects <- "" else names.effects <- attr(fit.x$terms,"term.labels")
    if(length(attr(model.matrix(fit.x),"assign"))==0) num.effects <- 0 else num.effects <- attr(model.matrix(fit.x),"assign")
    if(direction=="backward"){
      results <- matrix(NA,max(num.effects)+1,5)
      if(criterion=="p-value") results[1,ncol(results)] <- -1
      if(count==0) results[1,2:(ncol(results)-1)] <- glmstats(fit.x) else results[1,2] <- 0
      s <- attr(fit.x$terms,"factors")
      for(i in 1:max(num.effects)){
        if(all(apply(as.matrix(s[,-i]*s[,i]),2,sum) < sum(s[,i]))){
          formula0 <- constr.formula(fit.x,inter,term=names.effects[i],action="-")
          fit0 <- try(glm(as.formula(formula0),weights=fit.x$prior.weights,offset=fit.x$offset,family=fit.x$family,data=datas),silent=TRUE)
          if(!is.list(fit0))	fit0 <- glm(as.formula(formula0),weights=fit.x$prior.weights,offset=fit.x$offset,family=fit.x$family,data=datas,mustart=fitted(fit.x))
          results[i+1,1] <- fit0$df.residual - fit.x$df.residual
          results[i+1,2:4] <- glmstats(fit0)
          results[i+1,5] <- anova2(fit0,fit.x,test=test,verbose=FALSE)[1,3]
        }
      }
      names.effects <- cbind("-",names.effects)
    }
    if(direction=="forward"){
      outs <- out(model,names.effects)
      s2 <- as.matrix(attr(model$terms,"factors")[,outs])
      names.effects <- attr(model$terms,"term.labels")[outs]
      results <- matrix(NA,length(names.effects)+1,5)
      if(criterion=="p-value") results[1,ncol(results)] <- 1
      if(count==0) results[1,2:(ncol(results)-1)] <- glmstats(fit.x) else results[1,2] <- 0
      for(i in 1:length(names.effects)){
        if(sum(apply(as.matrix(s2[,-i]),2,function(x) sum(s2[,i]*x)==sum(x)))==0){
          formula0 <- constr.formula(fit.x,inter,term=names.effects[i],action="+")
          fit0 <- try(glm(as.formula(formula0),weights=fit.x$prior.weights,offset=fit.x$offset,family=fit.x$family,data=datas),silent=TRUE)
          if(!is.list(fit0)) fit0 <- glm(as.formula(formula0),weights=fit.x$prior.weights,offset=fit.x$offset,family=fit.x$family,data=datas,mustart=fitted(fit.x))
          results[i+1,1] <- fit.x$df.residual - fit0$df.residual
          results[i+1,2:4] <- glmstats(fit0)
          results[i+1,5] <- anova2(fit.x,fit0,test=test,verbose=FALSE)[1,3]
        }
      }
      names.effects <- cbind("+",names.effects)
    }
    shows <- !is.na(results[,2])
    results <- cbind(results[shows,1],results[shows,id0],results[shows,!id0])
    colnames(results) <- c(names.col[1],names.col[id0],names.col[!id0])
    results <- results[,-3]
    if(count > 0){
      if(criterion=="p-value") results[1,-c(1,2)] <- recicla
      else results[1,-c(1,5)] <- recicla
    }
    names.effects2 <- cbind(names.effects[shows[-1],1],names.effects[shows[-1],2])
    names.effects3 <- c("<none>",paste(names.effects2[,1],names.effects2[,2]))
    rownames(results) <- names.effects3
    charac <- rownames(results)[2]
    rownames(results)[2] <- paste(charac,paste(replicate(max(long-nchar(charac),0)," "),collapse=""),collapse="")

    ids <- min(results[,2]*delta[id0])==(results[,2]*delta[id0])
    if(sum(ids)>1) ids <- min(results[,3])==(results[,3])
    if(verbose) cat("\nStep",count,":",sale,"\n")
    indexes <- sort(results[,2]*delta[id0],index=TRUE)$ix
    if(criterion=="p-value"){
      recicla <- results[names.effects3==names.effects3[ids],-c(1,2)]
      if(direction=="backward") ps <- ifelse(max(results[,2]) > level,TRUE,FALSE)
      if(direction=="forward") ps <- ifelse(min(results[,2]) < level,TRUE,FALSE)
      results[names.effects3=="<none>",2] <- NA
    }else recicla <- results[names.effects3==names.effects3[ids],-c(1,ncol(results))]
    results2 <- cbind(results[,1],NA,results[,-1])
    colnames(results2) <- c(colnames(results)[1],"",colnames(results)[-1])
    if(verbose){
      if(quasi)
        printCoefmat(results2[indexes,colnames(results2)!="AIC  " & colnames(results2)!="BIC  "],cs.ind=1,tst.ind=3,dig.tst=3,na.print=" ",signif.stars=FALSE)
      else
        printCoefmat(results2[indexes,],cs.ind=1,tst.ind=c(3:5),dig.tst=3,na.print=" ",signif.stars=FALSE)
    }
    count <- count + 1
    if(names.effects3[ids]!="<none>" & ps==TRUE){
      ids <- ids[-1]
      sale <- paste(names.effects2[ids,1],names.effects2[ids,2])
      newformula <- constr.formula(fit.x,inter,term=names.effects2[ids,2],action=names.effects2[ids,1])
      if(nrow(names.effects)==1 | (nrow(names.effects)==2 & !inter)){
        tol <- FALSE
        if(verbose) cat("\nStep",count,":",sale,"\n")
      }
      fit.x <- glm(as.formula(newformula),weights=fit.x$prior.weights,offset=fit.x$offset,family=fit.x$family,data=datas)
    }else tol <- FALSE
  }
  if(verbose){
    cat("\n\nFinal model:\n")
    cat(newformula,"\n\n")
    cat("****************************************************************************\n")
    if(inter) cat("\n(^) Adjusted R-squared based on the residual deviance")
    else cat("\n(^) Deviance-based estimate of the dispersion parameter")
    cat("\n(*) p-value of the",test2)
    if(criterion=="p-value" & direction=="backward") cat("\n ( effects are dropped when their p-values are higher than",level,")")
    if(criterion=="p-value" & direction=="forward")  cat("\n ( effects are included when their p-values are lower than",level,")")
    if(!is.null(xxx$k)) cat("The magnitude of the penalty in the AIC was set to be ",xxx$k)
    cat("\n")
  }
  out_$final <- newformula
  return(invisible(out_))
}

#' @title Estimating Equations in Generalized Linear Models
#' @description Extracts estimating equations evaluated at the parameter estimates and the observed data for a generalized linear model fitted to the data.
#' @param model an object of the class glm which is obtained from the fit of a generalized linear model.
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
estequa.glm <- function(model,...){
  xx <- model.matrix(model)
  n <- nrow(xx)
  p <- ncol(xx)
  delta <- ifelse(model$family$mu.eta(model$linear.predictor)>=0,1,-1)
  salida <- crossprod(xx,delta*resid(model,type="pearson")*sqrt(model$weights))
  colnames(salida) <- " "
  rownames(salida) <- names(coef(model))
  return(salida)
}

#' @title Generalized Variance Inflaction Factor
#' @description Computes the generalized variance inflaction factor (GVIF) for a normal linear model.
#' @param model an object of the class lm which is obtained from the fit of a normal linear model.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @param ...	further arguments passed to or from other methods.
#' @return A matrix with so many rows as effects in the model and three columns: \emph{(1)} the values of GVIF; \emph{(2)} the number of degrees of freedom; and \emph{(3)} the values of GVIF^{1/(2*df)}.
#' @return If the number of degrees of freedom is 1 then the GVIF reduces to the Variance Inflation Factor (VIF).
#' @method gvif lm
#' @export
#' @examples
#' ## Example 1
#' fit1 <- lm(mpg ~ log(hp) + log(wt), data=mtcars)
#' gvif(fit1)
#'
#' ## Example 2
#' fit2 <- lm(Species ~ Biomass + pH, data=richness)
#' gvif(fit2)
#'
#' ## Example 3
#' whiteside <- MASS::whiteside
#' fit3 <- lm(Gas ~ Temp + Insul + Temp*Insul, data=whiteside)
#' gvif(fit3)
#' @references Fox J. and Monette G. (1992) Generalized collinearity diagnostics, \emph{JASA} 87, 178–183.
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
    results[i,1] <- det(rr2)*det(rr3)/detx
    results[i,2] <- ncol(rr2)
    results[i,3] <- results[i,1]^(1/(2*ncol(rr2)))
  }
  rownames(results) <- vars
  colnames(results) <- c("GVIF", "df", "GVIF^(1/(2*df))")
  if(verbose) print(results)
  return(invisible(results))
}


#' @title Generalized Variance Inflaction Factor
#' @description Computes the generalized variance inflaction factor (GVIF) for a generalized linear model.
#' @param model an object of the class glm which is obtained from the fit of a generalized linear model.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @param ...	further arguments passed to or from other methods.
#' @return A matrix with so many rows as effects in the model and three columns: \emph{(1)} the values of GVIF; \emph{(2)} the number of degrees of freedom; and \emph{(3)} the values of GVIF^{1/(2*df)}.
#' @return If the number of degrees of freedom is 1 then the GVIF reduces to the Variance Inflation Factor (VIF).
#' @method gvif glm
#' @export
#' @examples
#' ## Example 1
#' Auto <- ISLR::Auto
#' fit1 <- glm(mpg ~ weight*horsepower, family=inverse.gaussian("log"), data=Auto)
#' gvif(fit1)
#'
#' ## Example 2
#' burn1000 <- aplore3::burn1000
#' mod <- death ~ age + tbsa + inh_inj + age*inh_inj + tbsa*inh_inj
#' fit2 <- glm(mod, family=binomial("logit"), data=burn1000)
#' gvif(fit2)
#'
#' ## Example 3
#' fit3 <- glm(rtime ~ log(distance) + log(cclimb), family=Gamma("log"), data=races)
#' gvif(fit3)
#'
#' @references Fox J. and Monette G. (1992) Generalized collinearity diagnostics, \emph{JASA} 87, 178–183.
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
    results[i,1] <- det(rr2)*det(rr3)/detx
    results[i,2] <- ncol(rr2)
    results[i,3] <- results[i,1]^(1/(2*ncol(rr2)))
  }
  rownames(results) <- vars
  colnames(results) <- c("GVIF", "df", "GVIF^(1/(2*df))")
  if(verbose) print(results)
  return(invisible(results))
}



#' @title Confidence Intervals for Generalized Linear Models
#' @description Computes confidence intervals based on Wald, likelihood-ratio, Rao's score or Terrell's gradient tests for a generalized linear model.
#' @param model an object of the class glm which is obtained from the fit of a generalized linear model.
#' @param test an (optional) character string indicating the required type of test. The available options are: Wald ("wald"), Rao's score ("score"), Terrell's gradient ("gradient"), and likelihood ratio ("lrt") tests. By default, \code{test} is set to be "wald".
#' @param digits an (optional) integer value indicating the number of decimal places to be used. By default, \code{digits} is set to be 4.
#' @param level an (optional) value indicating the required confidence level. By default, \code{level} is set to be 0.95.
#' @param verbose an (optional) logical indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @details The approximate 100(\code{level})\% confidence interval for \eqn{\beta} based on the \code{test} test is the set of values of \eqn{\beta_0} for which the hypothesis \eqn{H_0}: \eqn{\beta=\beta_0} versus \eqn{H_1}: \eqn{\beta!=\beta_0} is not rejected at the approximate significance level of 100(1-\code{level})\%. The Wald, Rao's score and Terrell's gradient tests are performed using the expected Fisher information matrix.
#' @return A matrix with so many rows as parameters in the linear predictor and two columns: "Lower limit" and "Upper limit".
#' @references Buse A. (1982) The Likelihood Ratio, Wald, and Lagrange Multiplier Tests: An Expository Note. \emph{The American Statistician} 36, 153-157.
#' @references Terrell G.R. (2002) The gradient statistic. \emph{Computing Science and Statistics} 34, 206 – 215.
#' @export confint2
#' @examples
#' ## Example 1
#' Auto <- ISLR::Auto
#' fit1 <- glm(mpg ~ weight*horsepower, family=inverse.gaussian("log"), data=Auto)
#' confint2(fit1)
#'
#' ## Example 2
#' burn1000 <- aplore3::burn1000
#' mod <- death ~ age + tbsa + inh_inj + age*inh_inj + tbsa*inh_inj
#' fit2 <- glm(mod, family=binomial("logit"), data=burn1000)
#' confint2(fit2)
#'
confint2 <- function(model, level=0.95, test=c("wald","lrt","score","gradient"), digits=4, verbose=TRUE){
  test <- match.arg(test)
  if(class(model)[1]!="glm")
    stop("Only glm-type objects are supported!!",call.=FALSE)
  alpha <- 1 - level
  X <- model.matrix(model)
  p <- ncol(X)
  n <- nrow(X)
  ee <- sqrt(diag(vcov(model)))
  prefi <- "Approximate "
  if(test=="lrt" | test=="score" | test=="gradient"){
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
        if(test=="lrt") salida <- (fit0s$deviance - model$deviance)/phi - qchisq(1-alpha,1)
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
      prefi <- "Exact "
    }
    else
      results <- cbind(coef(model) - qnorm(1-alpha/2)*ee,coef(model) + qnorm(1-alpha/2)*ee)
  }
  rownames(results) <- colnames(X)
  colnames(results) <- c("Lower limit","Upper limit")
  if(verbose){
    test <- switch(test,"lrt"="Likelihood-ratio test",
                   "wald"="Wald test",
                   "score"="Rao's score test",
                   "gradient"="Gradient test")
    cat("\n",prefi,round(100*(1-alpha),digits=1),"percent confidence intervals based on the",test,"\n")
    print(round(results,digits=digits))
  }
  return(invisible(results))
}
