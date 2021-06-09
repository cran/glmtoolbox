#' @title Generalized Estimating Equations
#' @description Produces an object of the class glmgee in which are stored the main results of the generalized estimating equation fitted to the data.
#' @param formula a \code{formula} expression of the form \code{response ~ predictors}, which is a symbolic description of the linear predictor of the model to be fitted to the data.
#' @param family a \code{family} object, that is, a list of functions and expressions for defining link and variance functions. Families supported are gaussian, binomial, poisson, Gamma, inverse gaussian and quasi. See the \link{glm} and \link{family} documentation. By default, \code{family} is set to be \code{gaussian("identity")}.
#' @param id a vector which identifies the clusters. The length of \code{id} should be the same as the number of observations.
#' @param weights an (optional) vector of positive "prior weights" to be used in the fitting process. The length of \code{weights} should be the same as the number of observations.
#' @param data an (optional) \code{data frame} in which to look for variables involved in the \code{formula} expression, as well as the variables specified in the arguments \code{id} and \code{weights}. The data are assumed to be sorted by \code{id} and time.
#' @param subset an (optional) vector specifying a subset of observations to be used in the fitting process.
#' @param corstr a character string specifying the (working) correlation structure. The available options are: "Independence", "Unstructured", "Stationary-M-dependent(m)", "Non-Stationary-M-dependent(m)", "AR-1", "Exchangeable" and "User-defined", where \emph{m} represents the lag of the dependence. By default, \code{corstr} is set to be "Independence".
#' @param corr an (optional) square matrix of the same dimension of the maximum cluster size containing the user specified correlation. This is only appropriate if \code{corstr} is specified to be "User-defined".
#' @param start an (optional) vector of starting values for the parameters in the linear predictor.
#' @param maxit an (optional) integer value which represents the maximum number of iterations allowed for the fitting algorithm. By default, \code{maxit} is set to be 50.
#' @param toler an (optional) positive value which represents the convergence tolerance. The convergence is reached when the maximum of the relative differences between the values of the parameters in the linear predictor in consecutive iterations of the fitting algorithm is lower than \code{toler}. By default, \code{toler} is set to be 0.0000001.
#' @param adjr2 an (optional) logical variable. If TRUE, the adjusted deviance-based R-squared is calculated. By default, \code{adjr2} is set to be FALSE.
#' @param scale.fix an (optional) logical variable. If TRUE, the scale parameter is fixed at the value of \code{scale.value}. By default, \code{scale.fix} is set to be FALSE.
#' @param scale.value an (optional) numeric variable giving the value to which the scale parameter should be fixed. This is only appropriate if \code{scale.fix=TRUE}. By default, \code{scale.value} is set to be 1.
#' @param waves an (optional) a positive integer-valued variable that is used to identify the order and spacing of observations within clusters. This argument is crucial when there are missing values and gaps in the data. By default, \code{waves} is equal to the integers from 1 to the size of each cluster.
#' @param ...	further arguments passed to or from other methods.
#' @details If the value of \code{waves} for a cluster of size 4 is 2, 4, 5, 6 then it means that the data on times 1 and 3 are missing, which should be taken into account by \code{glmgee} when the structure of the correlation matrix is assumed to be "Unstructured", "Stationary-M-dependent", "Non-Stationary-M-dependent" or "AR-1".  If in this scenario \code{waves} is not specified then \code{glmgee} assumes that the available data for this cluster were taken on point times 1, 2, 3 and 4.
#' @return an object of the class glmgee in which are stored the main results of the generalized estimating equation fitted to the data. Some of those results can be easily accessed using functions as, for example, \code{print()}, \code{summary()}, \code{model.matrix()}, \code{estequa()}, \code{coef()}, \code{vcov()}, \code{logLik()}, \code{fitted()}, \code{confint()} and \code{predict()}. In addition, the model fitted to the data
#' can be assessed using functions as, for instance, \link{anova.glmgee}, \link{residuals.glmgee}, \link{dfbeta.glmgee} and \link{cooks.distance.glmgee}. The variable selection may be accomplished using \link{stepCriterion.glmgee} whereas the working–correlation–structure can be chosen by using criteria as \link{QIC}, \link{CIC}, \link{GHYC} and \link{RJC}.
#' @export glmgee
#' @importFrom graphics abline par
#' @importFrom methods missingArg
#' @importFrom stats as.formula coef gaussian get_all_vars rnorm update qt
#'             glm.fit model.extract model.frame model.matrix uniroot lm.fit
#'             model.offset model.response model.weights pnorm cov2cor qchisq
#'             printCoefmat pchisq vcov cooks.distance dfbeta qnorm anova na.omit
#' @examples
#' ## Example 1
#' mod1 <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod1, id=tree, family=Gamma("log"), corstr="AR-1", data=spruce)
#' summary(fit1)
#'
#' ## Example 2
#' mod2 <- depressd ~ visit + group
#' fit2 <- glmgee(mod2, id=subj, family=binomial("logit"), corstr="Exchangeable", data=depression)
#' summary(fit2)
#'
#' ## Example 3
#' mod3 <- dep ~ visit + group
#' fit3 <- glmgee(mod3, id=subj, corstr="Non-Stationary-M-dependent(2)", data=depression)
#' summary(fit3)
#' @references Liang K.Y. and Zeger S.L. (1986) Longitudinal data analysis using generalized linear models.
#' \emph{Biometrika} 73, 13-22.
#' @references Zeger S.L. and Liang K.Y. (1986) Longitudinal data analysis for discrete and continuous outcomes.
#' \emph{Biometrics} 42, 121-130.
#' @references Hardin J.W. and Hilbe J.M. (2013). \emph{Generalized Estimating Equations}. Chapman & Hall, London.

glmgee <- function(formula,family=gaussian(),weights,id,waves,data,subset,corstr,corr,start,scale.fix=FALSE,scale.value=1,toler=0.0000001,maxit=50,adjr2=FALSE,...){
  if(missingArg(data)) data <- environment(formula)
  if(missingArg(corstr)) corstr <- "Independence"
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "weights", "waves", "data", "subset", "id"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- as.matrix(model.response(mf, "any"))
  weights <- as.vector(model.weights(mf))
  if(class(family)=="function") family <- family()
  if(family$family=="quasi"){
    family2 <- family
    family$family <- switch(family$varfun,"constant"="gaussian","mu(1-mu)"="binomial",
                            "mu"="poisson","mu^2"="Gamma","mu^3"="inverse.gaussian")
    family <- do.call(family$family,list(link=family$link))
  }
  else{
    varfun <- switch(family$family,"gaussian"="constant","binomial"="mu(1-mu)","quasibinomial"="mu(1-mu)",
                     "poisson"="mu","quasipoisson"="mu","Gamma"="mu^2","inverse.gaussian"="mu^3")
    family$family <- gsub("quasi","",family$family)
    family2 <- do.call("quasi",list(variance=varfun,link=family$link))
  }
  if(ncol(y)==2 & family$family=="binomial"){
    weights <- as.matrix(y[,1]+y[,2])
    y <- as.matrix(y[,1]/weights)
  }
  y <- as.matrix(as.numeric(y))
  offset <- as.vector(model.offset(mf))
  X <- model.matrix(mt, mf)
  p <- ncol(X)
  q <- p
  n <- nrow(X)
  if(is.null(offset)) offs <- matrix(0,n,1) else offs <- as.matrix(offset)
  if(is.null(weights)) weights <- matrix(1,n,1) else weights <- as.matrix(weights)
  waves <- as.vector(model.extract(mf,waves))
  if(any(weights <= 0)) stop("Only positive weights are allowed!!",call.=FALSE)
  mustart <- y
  if(family$family=="binomial") mustart <- pmax(0.01, pmin(0.99, mustart))
  if(family$family=="poisson") mustart <- pmax(0.01, mustart)
  if(family$family=="Gamma") mustart <- pmax(0.01, mustart)
  if(family$family=="inverse.gaussian") mustart <- pmax(0.01, mustart)
  id <- factor(model.extract(mf,id))
  id2 <- as.matrix(levels(id))
  nclus <- length(id2)
  sizes <- matrix(0,nclus,1)
  datas <- list()
  for(i in 1:nclus){
    places <- id==id2[i]
    sizes[i] <- sum(places)
    if(is.null(waves))
      datas[[i]] <- cbind(matrix(X[places,],ncol=p),y[places,],offs[places],weights[places],1:sizes[i],i)
    else
      datas[[i]] <- cbind(matrix(X[places,],ncol=p),y[places,],offs[places],weights[places],waves[places],i)
  }
  maxsize <- ifelse(is.null(waves),max(sizes),max(waves))
  Rhat <- function(D,beta){
    etai <- tcrossprod(D[,1:p],t(beta)) + D[,q+2]
    mui <- family$linkinv(etai)
    wi <- sqrt(family$variance(mui)/D[,q+3])
    es <- rep(0,maxsize)
    es2 <- es
    es[D[,q+4]] <- (D[,q+1]-mui)/wi
    es2[D[,q+4]] <- 1
    nums <- tcrossprod(es)
    nums2 <- tcrossprod(es2)
    cbind(nums,nums2)
  }
  temp <- strsplit(gsub("[[:space:]]","",corstr),"[()]")[[1]]
  args <- c("Independence","Unstructured","Stationary-M-dependent",
            "Non-Stationary-M-dependent","AR-1","Exchangeable","User-defined")
  temp2 <- grep(tolower(temp[1]),tolower(args))
  if(length(temp)>0){
    corstr <- args[temp2[1]]
    if(corstr=="Stationary-M-dependent" | corstr=="Non-Stationary-M-dependent"){
      M <- min(max(1,floor(abs(as.numeric(temp[2])))),maxsize-1)
      if(is.na(M)) M <- 1
      attr(corstr,"M") <- M
    }
  }else stop("Correlation structure should be one of Independence, Unstructured, Stationary-M-dependent,
 		            Non-Stationary-M-dependent, AR-1, Exchangeable and User-defined!!",call.=FALSE)
  Rout <- function(resume,corstr){
    if(corstr=="Unstructured"){
      R <- resume[1:maxsize,1:maxsize]/(resume[1:maxsize,-c(1:maxsize)]-p)
      R <- R/phi
      diag(R) <- rep(1,maxsize)
    }
    if(corstr=="Stationary-M-dependent"){
      alphas <- matrix(0,M,1)
      for(i in 1:M){
        inds <- cbind(1:(maxsize-i),(i+1):maxsize)
        inds2 <- cbind(inds[,1],maxsize+inds[,2])
        alphas[i] <- sum(resume[inds])/(sum(resume[inds2])-p)
      }
      alphas <- alphas/phi
      R <- matrix(1,maxsize,maxsize)
      for(i in 1:(maxsize-1)){
        for(j in (i+1):maxsize){
          if((j-i) <= M) R[i,j] <- alphas[j-i] else R[i,j] <- 0
          R[j,i] <- R[i,j]
        }
      }
    }
    if(corstr=="Non-Stationary-M-dependent"){
      R <- resume[1:maxsize,1:maxsize]/(resume[1:maxsize,-c(1:maxsize)]-p)
      R <- R/phi
      diag(R) <- rep(1,maxsize)
      for(i in 1:maxsize){
        for(j in 1:maxsize){
          if(abs(j-i) > M) R[i,j] <- 0
        }
      }
    }
    if(corstr=="AR-1"){
      inds <- cbind(1:(maxsize-1),2:maxsize)
      inds2 <- cbind(inds[,1],maxsize+inds[,2])
      alpha <- sum(resume[inds])/(sum(resume[inds2])-p)
      alpha <- alpha/phi
      R <- matrix(1,maxsize,maxsize)
      for(i in 1:(maxsize-1)){
        for(j in (i+1):maxsize){
          R[i,j] <- alpha^(j-i)
          R[j,i] <- alpha^(j-i)
        }
      }
    }
    if(corstr=="Exchangeable"){
      diag(resume[1:maxsize,1:maxsize]) <- rep(0,maxsize)
      alpha <- sum(resume[1:maxsize,1:maxsize])/(sum(sizes*(sizes-1))-2*p)
      alpha <- alpha/phi
      R <- matrix(alpha,maxsize,maxsize)
      diag(R) <- rep(1,maxsize)
    }
    R <- matrix(mapply(function(x) max(min(x,1),-1),R),nrow=maxsize,ncol=maxsize)
    return(R)
  }
  score <- function(D,beta,out){
    Xi <- as.matrix(D[,1:p])
    if(ncol(Xi)!=p) Xi <- as.matrix(t(Xi))
    yi <- D[,q+1]
    ni <- nrow(Xi)
    etai <- tcrossprod(Xi,t(beta)) + D[,q+2]
    mui <- family$linkinv(etai)
    wi <- sqrt(family$variance(mui)/D[,q+3])
    Vi <- t(R[D[,q+4],D[,q+4]]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
    Xiw <- Xi*matrix(family$mu.eta(etai),nrow(Xi),p)
    if(is.null(attr(rchol,"class"))) Vi2 <- chol2inv(chol(Vi))
    else Vi2 <- solve(Vi)
    if(out) cbind(crossprod(Xiw,Vi2%*%(yi-mui)),crossprod(Xiw,Vi2%*%Xiw))
    else cbind(crossprod(Xiw,Vi2%*%(yi-mui)),crossprod(Xiw,Vi2%*%Xiw),crossprod(Xiw,Vi2%*%(tcrossprod(yi-mui))%*%Vi2%*%Xiw))
  }
  if(missingArg(start)){
    beta_new <- solve(t(X)%*%X)%*%t(X)%*%(family2$linkfun(mustart)-offs)
  }else beta_new <- start
  tol <- 1
  niter <- 0
  if(missingArg(corr)) corr <- diag(maxsize)
  while(tol > toler & niter < maxit){
    beta_old <- beta_new
    #resume <- Reduce('+',lapply(datas,Rhat,beta=beta_old))
    resume <- Rhat(datas[[1]],beta=beta_old)
    for(i in 2:nclus) resume <- resume + Rhat(datas[[i]],beta=beta_old)
    phi <- sum(diag(resume[1:maxsize,1:maxsize]))/(sum(sizes)-p)
    if(corstr!="User-defined" & corstr!="Independence") R <- Rout(resume,corstr)
    else R <- as.matrix(corr)
    rchol <- try(chol(R),silent=TRUE)
    #resume2 <- Reduce('+',lapply(datas,score,beta=beta_old,out=TRUE))
    resume2 <- score(datas[[1]],beta=beta_old,out=TRUE)
    for(i in 2:nclus) resume2 <- resume2 + score(datas[[i]],beta=beta_old,out=TRUE)
    kchol <- try(chol(resume2[,-1]),silent=TRUE)
    if(is.null(attr(kchol,"class"))) kchol <- chol2inv(kchol) else kchol <- solve(resume2[,-1])
    beta_new <- beta_old + kchol%*%resume2[,1]
    tol <- max(abs((beta_new-beta_old)/beta_old))
    niter <- niter + 1
  }
  rownames(beta_new) <- colnames(X)
  colnames(beta_new) <- ""
  if(niter==maxit) cat("Iteration limit exceeded!!\n",call.=FALSE)
  eta <- tcrossprod(X,t(beta_new)) + offs
  mu <- family$linkinv(eta)
  sera <- try(chol(R),silent=TRUE)
  if(!is.null(attr(sera,"class"))) warning("Estimate of correlation matrix is not positive definite",call.=FALSE)
  nano <- list(...)
  if(is.null(nano$si.mu.la.ti.on)){
    #resume2 <- Reduce('+',lapply(datas,score,beta=beta_new,out=FALSE))
    resume2 <- score(datas[[1]],beta=beta_new,out=FALSE)
    for(i in 2:nclus) resume2 <- resume2 + score(datas[[i]],beta=beta_new,out=FALSE)
    I0 <- solve(resume2[1:p,2:(p+1)])
    I1 <- resume2[1:p,(p+2):(2*p+1)]
    RJC <- I0%*%I1
    vcovs <- RJC%*%I0
    rownames(vcovs) <- colnames(X)
    colnames(vcovs) <- colnames(X)
    if(family$family=="gaussian") qll <- -weights*(y - mu)^2/2
    if(family$family=="binomial") qll <- weights*(y*log(mu) + (1-y)*log(1-mu))
    if(family$family=="poisson") qll <- weights*(y*log(mu) - mu)
    if(family$family=="Gamma") qll <- -weights*(y/mu + log(mu))
    if(family$family=="inverse.gaussian") qll <- weights*(mu - y/2)/mu^2
    w <- sqrt(weights*family2$mu.eta(eta)^2/family2$variance(mu))
    phi0 <- sum((y-mu)^2*weights/family2$variance(mu))/(length(y)-p)
    Xw <- matrix(w,nrow(X),ncol(X))*X
    vcov0 <- phi0*solve(crossprod(Xw,Xw))
    CIC <- sum(diag(solve(vcov0)%*%vcovs))
    phi <- ifelse(scale.fix,scale.value,phi)
    RJC <- sqrt((1 - sum(diag(RJC))/(p*phi))^2 + (1 - sum(diag(RJC%*%RJC))/(p*phi^2))^2)
    logLik <- sum(qll)/phi
    rownames(R) <- paste("[",1:maxsize,"]",sep="")
    colnames(R) <- paste("[",1:maxsize,"] ",sep="")
    estfun <- as.matrix(resume2[,1])
    rownames(estfun) <- colnames(X)
    colnames(estfun) <- ""
    ytrunc <- y
    if(family$family=="inverse.gaussian" | family$family=="Gamma") ytrunc <- ifelse(y<=0,0.01,y)
    if(family$family=="poisson") ytrunc <- ifelse(y<0,0,y)
    out_ <- list(coefficients=beta_new,fitted.values=mu,linear.predictors=eta,sizes=sizes,arrangedata=datas,
                 prior.weights=weights,y=y,formula=formula,call=match.call(),offset=offs,waves=waves,model=mf,data=data,score=score,ids=id2,
                 converged=ifelse(niter<maxit,TRUE,FALSE),estfun=estfun,R=vcovs,terms=mt,family=family,Rout=Rout,Rhat=Rhat,id=id,
                 phi=phi,CIC=CIC,RJC=RJC,logLik=logLik,corr=R,clusters=c(length(sizes),max(sizes),min(sizes)),corstr=corstr,
                 deviance=sum(family$dev.resids(ytrunc,mu,weights)),df.residual=length(y)-length(beta_new))
    class(out_) <- "glmgee"
    if(colnames(X)[1]=="(Intercept)" & adjr2==TRUE){
      p <- 1
      beta_new <- mean(family2$linkfun(mustart)-offs)
      tol <- 1
      niter <- 0
      while(tol > toler & niter < maxit){
        beta_old <- beta_new
        #resume <- Reduce('+',lapply(datas,Rhat,beta=beta_old))
        resume <- Rhat(datas[[1]],beta=beta_old)
        for(i in 2:nclus) resume <- resume + Rhat(datas[[i]],beta=beta_old)
        phi <- sum(diag(resume[1:maxsize,1:maxsize]))/(sum(sizes)-p)
        if(corstr!="User-defined") R <- Rout(resume,corstr)
        else R <- as.matrix(corr)
        #resume2 <- Reduce('+',lapply(datas,score,beta=beta_old,out=TRUE))
        resume2 <- score(datas[[1]],beta=beta_new,out=TRUE)
        for(i in 2:nclus) resume2 <- resume2 + score(datas[[i]],beta=beta_old,out=TRUE)
        beta_new <- beta_old + spdinv(resume2[,-1])%*%resume2[,1]
        tol <- max(abs((beta_new-beta_old)/beta_old))
        niter <- niter + 1
      }
      if(niter < maxit){
        mu0 <- family$linkinv(rep(beta_new,length(out_$y)) + offs)
        out_$null.deviance <- sum(family$dev.resids(ytrunc,mu0,weights))
        out_$df.null <- length(y)-length(beta_new)
      }
    }
    return(out_)
  }else{out_ <- list(coefficients=beta_new,converged=TRUE,arrangedata=datas,phi=phi,family=family,fitted.values=mu,corr=R,ids=id2,y=y)
  class(out_) <- "glmgee"
  return(out_)
  }
}
#' @method summary glmgee
#' @export
summary.glmgee <- function(object, ...,digits=4){
  cat("\nSample size")
  cat("\n       Number of clusters: ",object$clusters[1])
  cat("\n     Minimum cluster size: ",object$clusters[3])
  cat("\n     Maximum cluster size: ",object$clusters[2])
  cat("\n*************************************************************")
  cat("\nModel")
  cat("\n        Variance function: ",object$family$family)
  cat("\n            Link function: ",object$family$link)
  cat("\n    Correlation structure: ",ifelse(grepl("M-dependent",object$corstr),paste(object$corstr,"(",attr(object$corstr,"M"),")",sep=""),object$corstr))
  cat("\n*************************************************************\n")
  cat("Coefficients\n")
  TAB	<- cbind(Estimate <- object$coefficients,
               StdErr <- sqrt(diag(object$R)),
               tval <- Estimate/StdErr,
               p.value <- 2*pnorm(-abs(tval)))
  colnames(TAB) <- c("Estimate", "Std.Error", "z-value", "Pr(>|z|)")
  rownames(TAB) <- rownames(object$coefficients)
  printCoefmat(TAB, P.values=TRUE, signif.stars=FALSE, has.Pvalue=TRUE, digits=5, dig.tst=5, signif.legend=FALSE, tst.ind=c(1,2,3))
  cat("\nDispersion ",round(object$phi,digits=5),"\n")
  cat("*************************************************************\n")
  cat("Goodness-of-fit statistics\n")
  cat("\n      -2*quasi-likelihood: ",round(-2*object$logLik,digits=3),"\n")
  cat("                      QIC: ",round(-2*object$logLik+2*object$CIC,digits=3),"\n")
  cat("                     QICu: ",round(-2*object$logLik+2*length(object$coefficients),digits=3),"\n")
  if(!is.null(object$null.deviance)){
    cat("       adjusted R-squared: ",round(1-(object$deviance/object$df.residual)/(object$null.deviance/object$df.null),digits=4),"\n")}
  cat("*************************************************************\n")
  cat("Working correlation\n")
  print(round(object$corr,digits=digits))
  return(invisible(round(TAB,digits=digits)))
}

#' @method confint glmgee
#' @export
confint.glmgee <- function(object,parm,level=0.95,digits=4,verbose=TRUE,...){
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


#' @title Dfbeta for Generalized Estimating Equations
#' @description Produces an approximation, better known as the \emph{one-step approximation}, of the effect of deleting each cluster in turn on the estimates of the parameters in the linear predictor of a generalized estimating equation. This function also can produce a plot of those effects for a subset of the parameters in the linear predictor.
#' @param model an object of class glmgee which is obtained from the fit of a generalized estimating equation.
#' @param coefs	an (optional) character string which (partially) match with the names of some parameters in the linear predictor.
#' @param identify an (optional) integer indicating the number of clusters to identify on the plot of dfbeta. This is only appropriate if \code{coefs} is specified.
#' @param ... further arguments passed to or from other methods. If \code{coefs} is specified then \code{...} may be used to include graphical parameters to customize the plot. For example,  \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix with so many rows as clusters in the sample and so many columns as parameters in the linear predictor. The \eqn{i}-th row of that matrix corresponds to the difference between the estimates of the parameters in the linear predictor using all clusters and the \emph{one-step approximation} of those estimates when the \emph{i}-th cluster is excluded from the dataset.
#' @details The \emph{one-step approximation} of the estimates of the parameters in the linear predictor of a GEE when the \emph{i}-th cluster is excluded from the dataset is given by the vector obtained as the result of the first iteration of the fitting algorithm of that GEE when it is performed using:  (1) a dataset in which the \emph{i}-th cluster is excluded; and (2) a starting value which is the solution to the same GEE but based on the dataset inluding all clusters.
#' @references Pregibon D. (1981). Logistic regression diagnostics. \emph{The Annals of Statistics}, 9, 705-724.
#' @method dfbeta glmgee
#' @export
#' @examples
#' mod <- size ~ poly(days,4) + treat
#' fit <- glmgee(mod, id=tree, family=Gamma("log"), data=spruce, corstr="Exchangeable")
#' dfbs <- dfbeta(fit, coefs="treat" ,col="red", lty=1, lwd=1, col.lab="blue",
#'                col.axis="blue", col.main="black", family="mono", cex=0.8, main="Dfbeta")
#'
#' # Calculation by hand of dfbeta for the tree labeled by "N1T01"
#' idtree <- "N1T01"
#' onestep <- glmgee(mod, id=tree, family=Gamma("log"), data=spruce, corstr="Exchangeable",
#'                   start=coef(fit), subset=c(tree!=idtree), maxit=1)
#' coef(fit)-coef(onestep)
#' dfbs[rownames(dfbs)==idtree,]
#'
dfbeta.glmgee <- function(model, coefs, identify,...){
  p <- length(model$coefficients)
  envir <- environment(model$score)
  environment(model$Rout) <- envir
  environment(model$Rhat) <- envir
  envir$p <- p
  envir$q <- p
  envir$family <- model$family
  if(grepl("M-dependent",model$corstr))	envir$M <- attr(model$corstr,"M")
  dfbetas <- matrix(0,length(model$ids),p)
  envir$sizes <- model$sizes
  envir$maxsize <- ifelse(is.null(model$waves),max(envir$sizes),max(model$waves))
  datax <- model$arrangedata
  datas <- lapply(datax,model$Rhat,beta=model$coefficients)
  for(i in 1:length(model$ids)){
    if(model$corstr!="User-defined" & model$corstr!="Independence"){
      mus <- model$family$linkinv(datax[[i]][,1:envir$p]%*%model$coefficients + datax[[i]][,envir$p+2])
      pr <- as.matrix((datax[[i]][,envir$p+1] - mus)/sqrt(model$family$variance(mus)/datax[[i]][,envir$p+3]))
      prs <- tcrossprod(pr)
      phi <- (model$phi*(sum(envir$sizes)-p) - sum(diag(prs)))/(sum(envir$sizes[-i])-p)
      if(model$corstr=="Exchangeable"){
        alpha <- (model$corr[1,2]*(0.5*sum(envir$sizes*(envir$sizes-1))-p)*model$phi -
                    0.5*(sum(prs)-sum(diag(prs))))/((0.5*sum(envir$sizes[-i]*(envir$sizes[-i]-1))-p)*phi)
        R <- matrix(alpha,envir$maxsize,envir$maxsize)
        diag(R) <- rep(1,envir$maxsize)
      }
      if(model$corstr=="AR-1"){
        refe <- matrix(1:envir$maxsize,envir$maxsize,envir$maxsize,byrow=TRUE) - matrix(1:envir$maxsize,envir$maxsize,envir$maxsize)
        refe2 <- refe[datax[[i]][,envir$p+4],datax[[i]][,envir$p+4]]
        alpha <- (model$corr[1,2]*(sum(envir$sizes-1)-p)*model$phi -
                    sum(ifelse(refe2==1,1,0)*prs))/((sum(envir$sizes[-i]-1)-p)*phi)
        R <- alpha^abs(refe)
      }
      if(model$corstr!="Exchangeable" & model$corstr!="AR-1"){
        envir$sizes <- model$sizes[-i]
        envir$maxsize <- max(envir$sizes)
        datas2 <- datas
        datas2[[i]] <- NULL
        resume <- Reduce('+',datas2)
        envir$phi <- sum(diag(resume[1:envir$maxsize,1:envir$maxsize]))/(sum(envir$sizes)-p)
        R <- model$Rout(resume,model$corstr)
      }
    }else R <- model$corr
    envir$R <- R
    envir$rchol <- try(chol(R),silent=TRUE)
    datas2 <- datax
    datas2[[i]] <- NULL
    resume <- Reduce('+',lapply(datas2,model$score,beta=model$coefficients,out=TRUE))
    kchol <- try(chol(resume[,-1]),silent=TRUE)
    if(is.null(attr(kchol,"class"))) kchol <- chol2inv(kchol) else kchol <- solve(resume[,-1])
    dfbetas[i,] <- -kchol%*%resume[,1]
  }
  colnames(dfbetas) <- rownames(model$coefficients)
  rownames(dfbetas) <- model$ids
  if(!missingArg(coefs)){
    ids <- grep(coefs,colnames(dfbetas),ignore.case=TRUE)
    if(length(ids) > 0){
      nano <- list(...)
      if(is.null(nano$labels)) labels <- 1:nrow(dfbetas) else{
        labels <- nano$labels
        nano$labels <-NULL
      }
      nano$x <- 1:nrow(dfbetas)
      if(is.null(nano$xlab)) nano$xlab <- "Cluster Index"
      if(is.null(nano$type)) nano$type <- "h"
      if(is.null(nano$ylab)) nano$ylab <- expression(hat(beta)-hat(beta)[("- i")])
      if(is.null(nano$main)) main <- colnames(dfbetas)[ids]
      else main <- matrix(nano$main,length(ids),1)
      oldpar <- par(no.readonly=TRUE)
      on.exit(par(oldpar))
      par(mfrow=c(1,length(ids)))
      for(i in 1:length(ids)){
        nano$y <- dfbetas[,ids[i]]
        nano$main <- main[i]
        do.call("plot",nano)
        lim <- adjboxStats(nano$y[nano$y>0])
        abline(h=lim$stats[5],lty=3)
        lim <- adjboxStats(nano$y[nano$y<0])
        abline(h=lim$stats[1],lty=3)
        if(!missingArg(identify)){
          identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=model$ids)
        }
      }
    }
  }
  return(invisible(dfbetas))
}

#' @title Cook's Distance for Generalized Estimating Equations
#' @description Produces an approximation, better known as the \emph{one-step aproximation}, of the Cook's distance, which is aimed to measure the effect on the estimates of the parameters in the linear predictor of deleting each cluster in turn. This function also can produce a cluster-index plot of the Cook's distance for all parameters in the linear predictor or for some subset of them.
#' @param model an object of class glmgee obtained from the fit of a generalized estimating equation.
#' @param plot.it an (optional) logical indicating if the plot of Cook's distance is required or just the data matrix in which that plot is based. By default, \code{plot.it} is set to be TRUE.
#' @param coefs	an (optional) character string which (partially) match with the names of some of the parameters in the linear predictor.
#' @param identify an (optional) integer indicating the number of clusters to identify on the plot of Cook's distance. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used to include graphical parameters to customize the plot. For example,  \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix as many rows as clusters in the sample and one column with the values of the Cook's distance.
#' @details The Cook's distance consists of the \emph{distance} between two estimates of the parameters in the linear predictor using a metric based on the (estimate of the) variance-covariance matrix. The first one set of estimates is computed from a dataset including all clusters, and the second one is computed from a dataset in which the \emph{i}-th cluster is excluded. To avoid computational burden, the second set of estimates is replaced by its \emph{one-step approximation}. See the \link{dfbeta.glmgee} documentation.
#' @method cooks.distance glmgee
#' @export
#' @importFrom robustbase adjboxStats
#' @examples
#' ## Cook's distance for all parameters in the linear predictor
#' mod <- size ~ poly(days,4) + treat
#' fit <- glmgee(mod, id=tree, family=Gamma("log"), data=spruce, corstr="Exchangeable")
#' cooks.distance(fit, col="red", lty=1, lwd=1, col.lab="blue", main="Cook's distance",
#'                col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ## Cook's distance for the parameter associated to the variable treat
#' cooks.distance(fit, coef="treat", col="red", lty=1, lwd=1, col.lab="blue",
#'                main="Cook's distance", col.axis="blue", col.main="black",
#'                family="mono", cex=0.8)
cooks.distance.glmgee <- function(model, plot.it=TRUE, coefs, identify,...){
  dfbetas <- dfbeta(model)
  met <- vcov(model)
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
    if(is.null(nano$xlab)) nano$xlab <- "Cluster Index"
    if(is.null(nano$type)) nano$type <- "h"
    if(is.null(nano$ylab)) nano$ylab <- expression((hat(beta)-hat(beta)[{(-~~i)}])^{T}~(Var(hat(beta)))^{-1}~(hat(beta)-hat(beta)[{(-~~i)}]))
    do.call("plot",nano)
    lim <- adjboxStats(CD)
    abline(h=lim$stats[5],lty=3)
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

#' @method coef glmgee
#' @export
coef.glmgee <- function(object,...){
  out_ <- object$coefficients
  colnames(out_) <- ""
  return(out_)
}


#' @method model.matrix glmgee
#' @export
model.matrix.glmgee <-	function(object,...){
  if(is.null(object$call$data)) m <- get_all_vars(eval(object$call$formula))
  else m <- get_all_vars(eval(object$call$formula),eval(object$call$data))
  modelframe <- model.frame(object$call,m)
  X <- model.matrix(modelframe,m)
  return(X)
}

#' @method logLik glmgee
#' @export
logLik.glmgee <- function(object,...){
  out_ <- object$logLik
  attr(out_,"df") <- length(object$coefficients)
  class(out_) <- "logLik"
  return(out_)
}

#' @method fitted glmgee
#' @export
fitted.glmgee <- function(object,...) return(object$fitted.values)

#' @method print glmgee
#' @export
print.glmgee <- function(x, ...){
  cat("\n        Variance function: ",x$family$family)
  cat("\n                     Link: ",x$family$link)
  cat("\n    Correlation structure: ",ifelse(grepl("M-dependent",x$corstr),paste(x$corstr,"(",attr(x$corstr,"M"),")",sep=""),x$corstr))
  cat("\n********************************************************")
  cat("\n      -2*quasi-likelihood: ",round(-2*x$logLik,digits=3),"\n")
  cat("                      QIC: ",round(-2*x$logLik+2*x$CIC,digits=3),"\n")
  cat("                     QICu: ",round(-2*x$logLik+2*length(x$coefficients),digits=3),"\n")
  if(!is.null(x$null.deviance)){
    cat("       adjusted R-squared: ",round(1-(x$deviance/x$df.residual)/(x$null.deviance/x$df.null),digits=4),"\n")}
}

#' @title Predictions for Generalized Estimating Equations
#' @description Produces predictions and optionally estimates standard errors of those predictions from a fitted generalized estimating equation.
#' @param object an object of the class glmgee which is obtained from the fit of a generalized estimating equation.
#' @param newdata	an (optional) \code{data frame} in which to look for variables with which to predict. If omitted, the fitted linear predictors are used.
#' @param type an (optional) character string giving the type of prediction required. The default, "link", is on the scale of the linear predictors, and the alternative, "response", is on the scale of the response variable.
#' @param se.fit	an (optional) logical switch indicating if standard errors are required. By default, \code{se.fit} is set to be FALSE.
#' @param ... further arguments passed to or from other methods.
#' @return A matrix with so many rows as \code{newdata} and one column with the predictions. If \code{se.fit=}TRUE then a second column with estimates standard errors is included.
#' @examples mod <- size ~ poly(days,4) + treat
#' fit <- glmgee(mod, id=tree, family=Gamma("log"), data=spruce, corstr="Stationary-M-dependent(2)")
#' newdata <- data.frame(days=c(556,556),treat=as.factor(c("normal","ozone_enriched")))
#' predict(fit,newdata=newdata,type="response",se.fit=TRUE)
#' @method predict glmgee
#' @export
predict.glmgee <- function(object, ...,newdata, se.fit=FALSE, type=c("link","response")){
  type <- match.arg(type)
  if(missingArg(newdata)){
    predicts <- object$linear.predictors
    X <- model.matrix(object)
  }
  else{
    newdata <- data.frame(newdata)
    mf <- model.frame(delete.response(object$terms),newdata)
    X <- model.matrix(delete.response(object$terms),mf)
    predicts <- tcrossprod(X,t(object$coefficients))
    offs <- model.offset(mf)
    if(!is.null(offs)) predicts <- predicts + offs
  }
  if(type=="response") predicts <- object$family$linkinv(predicts)
  if(se.fit){
    se <- sqrt(apply((tcrossprod(X,object$R))*X,1,sum))
    if(type=="response") se <- se*abs(object$family$mu.eta(object$family$linkfun(predicts)))
    predicts <- cbind(predicts,se)
    colnames(predicts) <- c("fit","se.fit")
  }else colnames(predicts) <- c("fit")
  rownames(predicts) <- rep(" ",nrow(predicts))
  return(predicts)
}

#' @title Residuals for Generalized Estimating Equations
#' @description Calculates residuals for a fitted generalized estimating equation.
#' @param object a object of the class glmgee obtained from the fit of a generalized estimating equation.
#' @param type an (optional) character string giving the type of residuals which should be returned. The available options are: (1) "pearson"; (2) "deviance";  (3) the distance between the observed response vector and the fitted mean vector using a metric based on the product between the cluster size and fitted variance-covariance matrix ("mahalanobis"). By default, \code{type} is set to be "mahalanobis".
#' @param plot.it an (optional) logical switch indicating if a plot of the residuals is required. By default, \code{plot.it} is set to be TRUE.
#' @param identify an (optional) integer value indicating the number of individuals/clusters to identify on the plot of residuals. This is only appropriate when \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods
#' @return A vector with the observed residuals type \code{type}.
#' @examples
#' mod <- size ~ poly(days,4) + treat
#' fit <- glmgee(mod, id=tree, family=Gamma("log"), data=spruce, corstr="AR-1")
#' residuals(fit, type="mahalanobis", col="red", pch=20, col.lab="blue",
#'           col.axis="blue", col.main="black", family="mono", cex=0.8)
#' @method residuals glmgee
#' @export
residuals.glmgee <- function(object,..., type=c("mahalanobis","pearson","deviance"), plot.it=TRUE, identify){
  type <- match.arg(type)
  beta <- object$coefficients
  mhd <- function(D){
    p <- length(beta)
    yi <- D[,p+1]
    Xi <- as.matrix(D[,1:p])
    ni <- nrow(Xi)
    etai <- tcrossprod(Xi,t(beta)) + D[,p+2]
    mui <- object$family$linkinv(etai)
    wi <- sqrt(object$family$variance(mui)/D[,p+3])
    Vi <- t(object$corr[D[,p+4],D[,p+4]]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
    uu <- try(chol(Vi),silent=TRUE)
    if(is.null(attr(uu,"class"))) Vi2 <- chol2inv(uu) else Vi2 <- solve(Vi)
    crossprod((yi-mui),Vi2%*%(yi-mui))/ni
  }
  res0 <- object$y-object$fitted.values
  res <- switch(type,
                deviance = ifelse(res0>=0,1,-1)*sqrt(object$family$dev.resids(object$y,object$fitted.values,object$prior.weights)/object$phi),
                pearson = res0/sqrt(object$phi*object$family$variance(object$fitted.values)/object$prior.weights),
                mahalanobis = as.matrix(unlist(lapply(object$arrangedata,mhd))/object$phi))
  if(plot.it){
    nano <- list(...)
    if(type!="mahalanobis"){
      nano$x <- object$fitted.values
      nano$y <- res
      if(is.null(nano$ylim)) nano$ylim <- c(min(-3.5,min(res)),max(+3.5,max(res)))
      if(is.null(nano$xlab)) nano$xlab <- "Fitted values"
      if(is.null(nano$ylab)) nano$ylab <- paste(type," - type residual",sep="")
      if(is.null(nano$pch))  nano$pch  <- 20
      do.call("plot",nano)
      abline(h=-3,lty=3)
      abline(h=+3,lty=3)
      if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))))
    }else{nano$x <- 1:length(object$ids)
    nano$y <- res
    if(is.null(nano$ylim)) nano$ylim <- c(0,max(3.5,max(res)))
    if(is.null(nano$xlab)) nano$xlab <- "Cluster Index"
    if(is.null(nano$ylab)) nano$ylab <- paste(type," - type residual",sep="")
    if(is.null(nano$type)) nano$type  <- "h"
    do.call("plot",nano)
    lim <- adjboxStats(res)
    abline(h=lim$stats[5],lty=3)
    if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=object$ids)
    }
  }
  colnames(res) <- type
  return(invisible(res))
}
#'
#' @title Estimating Equations in Generalized Estimating Equations
#' @description Extracts estimating equations evaluated at the parameter estimates and the observed data for a generalized estimating equation fitted to the data.
#' @param model an object of class glmgee which is obtained from the fit of a generalized estimating equation.
#' @param ... further arguments passed to or from other methods.
#' @return A vector with the value of the estimating equations evaluated at the parameter estimates and the observed data.
#' @method  estequa glmgee
#' @export
#' @examples
#' mod <- size ~ poly(days,4) + treat
#' fit <- glmgee(mod, id=tree, family=Gamma("log"), data=spruce, corstr="AR-1")
#' estequa(fit)
estequa.glmgee <- function(model,...){
  salida <- model$estfun
  colnames(salida) <- " "
  return(salida)
}
#'
#' @title Comparison of nested Generalized Estimating Equations
#' @description Allows to compare nested generalized estimating equations using the Wald and generalized score tests.
#' @param object an (object) of the class glmgee which is obtained from the fit of a generalized estimating equation.
#' @param ... another objects of the class glmgee which are obtained from the fit of generalized estimating equations.
#' @param test an (optional) character string indicating the required test. The available options are: Wald ("wald") and generalized score ("score") tests. By default, \code{test} is set to be "wald".
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @return A matrix with three columns which contains the following:
#' \itemize{
#' \item \code{Chi:}{ The value of the statistic of the test.}
#' \item \code{df:}{ The number of degrees of freedom.}
#' \item \code{Pr(>Chi):}{ The \emph{p}-value of the test computed using the Chi-square distribution.}
#' }
#' @method anova glmgee
#' @export
#' @examples
#' ## Example 1
#' mod <- size ~ poly(days,4)
#' fit1 <- glmgee(mod, id=tree, family=Gamma("log"), data=spruce, corstr="AR-1")
#' fit2 <- update(fit1, . ~ . + treat)
#' fit3 <- update(fit2, . ~ . + poly(days,4):treat)
#' anova(fit1,fit2,fit3,test="wald")
#' anova(fit3,test="wald")
#' anova(fit1,fit2,fit3,test="score")
#' anova(fit3,test="score")
#'
#' ## Example 2
#' mod2 <- depressd ~ group
#' fit1 <- glmgee(mod2, id=subj, family=binomial("logit"), corstr="Exchangeable", data=depression)
#' fit2 <- update(fit1, . ~ . + visit)
#' fit3 <- update(fit2, . ~ . + group:visit)
#' anova(fit1,fit2,fit3,test="wald")
#' anova(fit3,test="wald")
#' anova(fit1,fit2,fit3,test="score")
#' anova(fit3,test="score")
#'
#' ## Example 3
#' mod3 <- dep ~ group
#' fit1 <- glmgee(mod3, id=subj, family=gaussian("identity"), corstr="AR-1", data=depression)
#' fit2 <- update(fit1, . ~ . + visit)
#' fit3 <- update(fit2, . ~ . + group:visit)
#' anova(fit1,fit2,fit3,test="wald")
#' anova(fit3,test="wald")
#' anova(fit1,fit2,fit3,test="score")
#' anova(fit3,test="score")
#' @references Boos D. (1992) On Generalized Score Tests. \emph{American Statistician} 46, 327–33.
#' @references Rotnitzky A. and Jewell N.P. (1990). Hypothesis Testing of Regression Parameters in Semiparametric Generalized Linear Models for Cluster Correlated Data. \emph{Biometrika} 77, 485-497.
anova.glmgee <- function(object,...,test=c("wald","score"),verbose=TRUE){
  test <- match.arg(test)
  x <- list(object,...)
  if(any(lapply(x,function(xx) class(xx)[1])!="glmgee"))
    stop("Only glmgee-type objects are supported!!",call.=FALSE)
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
    if(test=="wald"){
      sc <- crossprod(coef(x[[i]])[ids],solve(vcov(x[[i]])[ids,ids]))%*%coef(x[[i]])[ids]
    }
    if(test=="score"){
      p <- length(x[[i]]$coefficients)
      envir <- environment(x[[i]]$score)
      envir$p <- p
      envir$q <- p
      envir$family <- x[[i]]$family
      envir$R <- x[[i-1]]$corr
      beta0 <- coef(x[[i]])
      beta0[ids] <- rep(0,sum(ids))
      beta0[!ids] <- coef(x[[i-1]])
      resume2 <- Reduce('+',lapply(x[[i]]$arrangedata,x[[i]]$score,beta=beta0,out=FALSE))
      I0 <- solve(resume2[1:p,2:(p+1)])
      I1 <- resume2[1:p,(p+2):(2*p+1)]
      vcovs <- I0%*%I1%*%I0
      sc <- crossprod((I0%*%resume2[,1])[ids],solve(vcovs[ids,ids]))%*%((I0%*%resume2[,1])[ids])
    }
    df <- sum(ids)
    out_[i-1,] <- cbind(sc,df,1-pchisq(sc,df))
  }
  colnames(out_) <- c(" Chi  ", " df", "  Pr(>Chi)")
  rownames(out_) <- paste(1:(hast-1),"vs",2:hast)
  if(verbose){
    if(test=="wald") test <- "Wald test" else test <- "Generalized score test"
    cat("\n ",test,"\n\n")
    for(i in 1:hast) cat(paste("Model", i,": ",x[[i]]$formula[2],x[[i]]$formula[1],x[[i]]$formula[3:length(x[[i]]$formula)],collapse=""),"\n")
    cat("\n")
    printCoefmat(out_, P.values=TRUE, has.Pvalue=TRUE, digits=5, signif.legend=TRUE, cs.ind=2)
  }
  return(invisible(out_))
}

#' @title QIC for Generalized Estimating Equations
#' @description Computes the quasi-likelihood under the independence model criterion (QIC) for one or more objects of the class glmgee.
#' @param ...	one or several objects of the class glmgee which are obtained from the fit of generalized estimating equations.
#' @param k an (optional) non-negative value giving the magnitude of the penalty. By default, \code{k} is set to be 2.
#' @param u an (optional) logical switch indicating if QIC should be replaced by QICu. By default, \code{u} is set to be FALSE.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @return A \code{data.frame} with the values of -2*quasi-likelihood, the number of parameters in the linear predictor, and the value of QIC (or QICu if \code{u}=TRUE) for each glmgee object in the input.
#' @seealso \link{CIC}, \link{GHYC}, \link{RJC}, \link{AGPC}, \link{SGPC}
#' @export QIC
#' @examples
#' ## Example 1
#' mod <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod, id=tree, family=Gamma("log"), data=spruce, corstr="Exchangeable")
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' QIC(fit1, fit2, fit3, fit4)
#'
#' ## Example 2
#' mod <- dep ~ visit + group
#' fit1 <- glmgee(mod, id=subj, family=gaussian, corstr="Exchangeable", data=depression)
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Non-Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' QIC(fit1, fit2, fit3, fit4)
#'
#' ## Example 3
#' mod <- depressd ~ visit + group
#' fit1 <- glmgee(mod, id=subj, family=binomial, corstr="Exchangeable", data=depression)
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' QIC(fit1, fit2, fit3, fit4)
#' @references Pan W. (2001) Akaike's information criterion in generalized estimating equations, \emph{Biometrics} 57, 120-125.
#' @references Hin L-Y, Carey V.J., Wang Y-G (2007) Criteria for Working–Correlation–Structure Selection in GEE:
#' Assessment via Simulation. \emph{The American Statistician} 61, 360–364.

QIC <- function(...,k=2,u=FALSE,verbose=TRUE){
  x <- list(...)
  if(any(lapply(x,function(xx) class(xx)[1])!="glmgee"))
    stop("Only glmgee-type objects are supported!!",call.=FALSE)
  results <- matrix(NA,length(x),3)
  results2 <- matrix(NA,length(x),5)
  rows <-  matrix(NA,length(x),1)
  call. <- match.call()
  for(i in 1:length(x)){
    results[i,1] <- round(-2*x[[i]]$logLik,digits=3)
    results[i,2] <- length(coef(x[[i]]))
    results[i,3] <- round(-2*x[[i]]$logLik+ifelse(u,k*length(x[[i]]$coefficients),k*x[[i]]$CIC),digits=3)
    results2[i,1] <- as.character(call.[i+1])
    results2[i,2] <- x[[i]]$family$family
    results2[i,3] <- x[[i]]$family$link
    results2[i,4] <- paste(c(attr(x[[i]]$terms,"intercept"),attr(x[[i]]$terms,"term.labels")),collapse=" + ")
    results2[i,5] <- ifelse(grepl("M-dependent",x[[i]]$corstr),paste(x[[i]]$corstr,"(",attr(x[[i]]$corstr,"M"),")",sep=""),x[[i]]$corstr)
  }
  if(nrow(results) > 1){
    if(all(results2[,2]==results2[1,2]))
      if(verbose) cat("\n        Variance function: ",results2[1,2],"\n")
    if(all(results2[,3]==results2[1,3]))
      if(verbose) cat("            Link function: ",results2[1,3],"\n")
    if(all(results2[,4]==results2[1,4]))
      if(verbose) cat("         Linear predictor: ",results2[1,4],"\n")
    if(all(results2[,5]==results2[1,5]))
      if(verbose) cat("    Correlation structure: ",results2[1,5],"\n")
    if(verbose) cat("\n")
    ids <- c(TRUE,!all(results2[,2]==results2[1,2]),!all(results2[,3]==results2[1,3]),!all(results2[,4]==results2[1,4]),!all(results2[,5]==results2[1,5]))
    temp <- as.matrix(results2[,ids])
    out_ <- data.frame(temp,results)
    colnames(out_) <- c(c("Object","Variance function","Link function","Predictor","Correlation")[ids],"-2*quasi-likelihood"," Parameters",ifelse(u,"QICu  ","QIC  "))
    if(verbose) print(out_,row.names=FALSE)
    return(invisible(out_))
  }else return(results[,3])
}

#' @title Correlation Information Criterion for Generalized Estimating Equations
#' @description Computes the Correlation Information Criterion (CIC) for one or more objects of the class glmgee.
#' @param ...	one or several objects of the class glmgee which are obtained from the fit of generalized estimating equations.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @return A \code{data.frame} with the values of the CIC for each glmgee object in the input.
#' @export CIC
#' @seealso \link{QIC}, \link{GHYC}, \link{RJC}, \link{AGPC}, \link{SGPC}
#' @examples
#' ## Example 1
#' mod <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod, id=tree, family=Gamma("log"), data=spruce, corstr="Exchangeable")
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' CIC(fit1, fit2, fit3, fit4)
#'
#' ## Example 2
#' mod <- dep ~ visit + group
#' fit1 <- glmgee(mod, id=subj, family=gaussian, corstr="Exchangeable", data=depression)
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Non-Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' CIC(fit1, fit2, fit3, fit4)
#'
#' ## Example 3
#' mod <- depressd ~ visit + group
#' fit1 <- glmgee(mod, id=subj, family=binomial, corstr="Exchangeable", data=depression)
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' CIC(fit1, fit2, fit3, fit4)
#' @references Hin L.-Y. and Wang Y.-G. (2009) Working-Correlation-Structure Identification in Generalized Estimating Equations. \emph{Statistics in Medicine}, 28, 642-658.
#' @references Hin L.-Y., Carey V.J., Wang Y.-G. (2007) Criteria for Working–Correlation–Structure Selection in GEE:
#' Assessment via Simulation. \emph{The American Statistician} 61, 360–364.

CIC <- function(...,verbose=TRUE){
  x <- list(...)
  if(any(lapply(x,function(xx) class(xx)[1])!="glmgee"))
    stop("Only glmgee-type objects are supported!!",call.=FALSE)
  results <- matrix(NA,length(x),1)
  results2 <- matrix(NA,length(x),5)
  rows <-  matrix(NA,length(x),1)
  call. <- match.call()
  for(i in 1:length(x)){
    results[i,1] <- round(x[[i]]$CIC,digits=3)
    results2[i,1] <- as.character(call.[i+1])
    results2[i,2] <- x[[i]]$family$family
    results2[i,3] <- x[[i]]$family$link
    results2[i,4] <- paste(c(attr(x[[i]]$terms,"intercept"),attr(x[[i]]$terms,"term.labels")),collapse=" + ")
    results2[i,5] <- ifelse(grepl("M-dependent",x[[i]]$corstr),paste(x[[i]]$corstr,"(",attr(x[[i]]$corstr,"M"),")",sep=""),x[[i]]$corstr)
  }
  if(nrow(results) > 1){
    if(all(results2[,2]==results2[1,2]))
      if(verbose) cat("\n        Variance function: ",results2[1,2],"\n")
    if(all(results2[,3]==results2[1,3]))
      if(verbose) cat("            Link function: ",results2[1,3],"\n")
    if(all(results2[,4]==results2[1,4]))
      if(verbose) cat("         Linear predictor: ",results2[1,4],"\n")
    if(all(results2[,5]==results2[1,5]))
      if(verbose) cat("    Correlation structure: ",results2[1,5],"\n")
    if(verbose) cat("\n")
    ids <- c(TRUE,!all(results2[,2]==results2[1,2]),!all(results2[,3]==results2[1,3]),!all(results2[,4]==results2[1,4]),!all(results2[,5]==results2[1,5]))
    temp <- as.matrix(results2[,ids])
    out_ <- data.frame(temp,results)
    colnames(out_) <- c(c("Object","Variance function","Link function","Predictor","Correlation")[ids],"CIC ")
    if(verbose) print(out_,row.names=FALSE)
    return(invisible(out_))
  }else return(results[,1])
}

#' @title Gosho-Hamada-Yoshimura's Criterion for Generalized Estimating Equations
#' @description Computes the Gosho-Hamada-Yoshimura's criterion (GHYC) for one or more objects of the class glmgee.
#' @param ...	one or several objects of the class glmgee which are obtained from the fit of generalized estimating equations.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @return A \code{data.frame} with the values of the GHYC for each glmgee object in the input.
#' @export GHYC
#' @seealso \link{QIC}, \link{CIC}, \link{RJC}, \link{AGPC}, \link{SGPC}
#' @examples
#' ## Example 1
#' mod <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod, id=tree, family=Gamma("log"), data=spruce, corstr="Exchangeable")
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' GHYC(fit1, fit2, fit3, fit4)
#'
#' ## Example 2
#' mod <- dep ~ visit + group
#' fit1 <- glmgee(mod, id=subj, family=gaussian, corstr="Exchangeable", data=depression)
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Non-Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' GHYC(fit1, fit2, fit3, fit4)
#'
#' ## Example 3
#' mod <- depressd ~ visit + group
#' fit1 <- glmgee(mod, id=subj, family=binomial, corstr="Exchangeable", data=depression)
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' GHYC(fit1, fit2, fit3, fit4)
#' @references Gosho M., Hamada C., Yoshimura I. (2011) Criterion for the Selection of a Working Correlation Structure in the Generalized Estimating Equation Approach for Longitudinal Balanced
#' Data. \emph{Communications in Statistics – Theory and Methods} 40, 3839–3856.
#' @references Gosho M. (2014) Criteria to Select a Working Correlation Structure in SAS. \emph{Journal of Statistical Software, Code Snippets} 57.

GHYC <- function(...,verbose=TRUE){
  x <- list(...)
  if(any(lapply(x,function(xx) class(xx)[1])!="glmgee"))
    stop("Only glmgee-type objects are supported!!",call.=FALSE)
  results <- matrix(NA,length(x),1)
  results2 <- matrix(NA,length(x),5)
  rows <-  matrix(NA,length(x),1)
  call. <- match.call()
  Gosho <- function(model){
    p <- length(model$coefficients)
    family <- model$family
    maxsize <- ifelse(is.null(model$waves),max(model$sizes),max(model$waves))
    phi <- model$phi
    beta <- model$coefficients
    VSi <- function(D){
      Xi <- as.matrix(D[,1:p])
      yi <- D[,p+1]
      ni <- nrow(Xi)
      etai <- tcrossprod(Xi,t(beta)) + D[,p+2]
      mui <- family$linkinv(etai)
      wi <- sqrt(family$variance(mui)/D[,p+3])
      Vi <- matrix(0,maxsize,maxsize)
      Si <- matrix(0,maxsize,maxsize)
      es <- rep(0,maxsize)
      Si[D[,p+4],D[,p+4]] <- tcrossprod(yi - mui)
      Vi[D[,p+4],D[,p+4]] <- t(model$corr[D[,p+4],D[,p+4]]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
      es[D[,p+4]] <- 1
      nums <- tcrossprod(es)
      cbind(model$phi*Vi,Si,nums)
    }
    resume <- Reduce('+',lapply(model$arrangedata,VSi))
    a <- (resume[,(maxsize+1):(2*maxsize)]/resume[,(2*maxsize+1):(3*maxsize)])%*%solve(resume[,1:maxsize]/resume[,(2*maxsize+1):(3*maxsize)]) - diag(maxsize)
    cr <- sum(diag(a%*%a))
    return(cr)
  }
  for(i in 1:length(x)){
    results[i,1] <- round(Gosho(x[[i]]),digits=3)
    results2[i,1] <- as.character(call.[i+1])
    results2[i,2] <- x[[i]]$family$family
    results2[i,3] <- x[[i]]$family$link
    results2[i,4] <- paste(c(attr(x[[i]]$terms,"intercept"),attr(x[[i]]$terms,"term.labels")),collapse=" + ")
    results2[i,5] <- ifelse(grepl("M-dependent",x[[i]]$corstr),paste(x[[i]]$corstr,"(",attr(x[[i]]$corstr,"M"),")",sep=""),x[[i]]$corstr)
  }
  if(nrow(results) > 1){
    if(all(results2[,2]==results2[1,2]))
      if(verbose) cat("\n        Variance function: ",results2[1,2],"\n")
    if(all(results2[,3]==results2[1,3]))
      if(verbose) cat("            Link function: ",results2[1,3],"\n")
    if(all(results2[,4]==results2[1,4]))
      if(verbose) cat("         Linear predictor: ",results2[1,4],"\n")
    if(all(results2[,5]==results2[1,5]))
      if(verbose) cat("    Correlation structure: ",results2[1,5],"\n")
    if(verbose) cat("\n")
    ids <- c(TRUE,!all(results2[,2]==results2[1,2]),!all(results2[,3]==results2[1,3]),!all(results2[,4]==results2[1,4]),!all(results2[,5]==results2[1,5]))
    temp <- as.matrix(results2[,ids])
    out_ <- data.frame(temp,results)
    colnames(out_) <- c(c("Object","Variance function","Link function","Predictor","Correlation")[ids],"GHYC ")
    if(verbose) print(out_,row.names=FALSE)
    return(invisible(out_))
  }else return(results[,1])
}

#' @title Rotnitzky–Jewell's Criterion for Generalized Estimating Equations
#' @description Computes the Rotnitzky–Jewell's criterion (RJC) for one or more objects of the class glmgee.
#' @param ...	one or several objects of the class glmgee which are obtained from the fit of generalized estimating equations.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @return A \code{data.frame} with the values of the RJC for each glmgee object in the input.
#' @export RJC
#' @seealso \link{QIC}, \link{CIC}, \link{GHYC}, \link{AGPC}, \link{SGPC}
#' @examples
#' ## Example 1
#' mod <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod, id=tree, family=Gamma("log"), data=spruce, corstr="Exchangeable")
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' RJC(fit1, fit2, fit3, fit4)
#'
#' ## Example 2
#' mod <- dep ~ visit + group
#' fit1 <- glmgee(mod, id=subj, family=gaussian, corstr="Exchangeable", data=depression)
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Non-Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' RJC(fit1, fit2, fit3, fit4)
#'
#' ## Example 3
#' mod <- depressd ~ visit + group
#' fit1 <- glmgee(mod, id=subj, family=binomial, corstr="Exchangeable", data=depression)
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' RJC(fit1, fit2, fit3, fit4)
#' @references Hin L-Y, Carey V.J., Wang Y-G (2007) Criteria for Working–Correlation–Structure Selection in GEE:
#' Assessment via Simulation. \emph{The American Statistician} 61, 360–364.

RJC <- function(...,verbose=TRUE){
  x <- list(...)
  if(any(lapply(x,function(xx) class(xx)[1])!="glmgee"))
    stop("Only glmgee-type objects are supported!!",call.=FALSE)
  results <- matrix(NA,length(x),1)
  results2 <- matrix(NA,length(x),5)
  rows <-  matrix(NA,length(x),1)
  call. <- match.call()
  for(i in 1:length(x)){
    results[i,1] <- round(x[[i]]$RJC,digits=3)
    results2[i,1] <- as.character(call.[i+1])
    results2[i,2] <- x[[i]]$family$family
    results2[i,3] <- x[[i]]$family$link
    results2[i,4] <- paste(c(attr(x[[i]]$terms,"intercept"),attr(x[[i]]$terms,"term.labels")),collapse=" + ")
    results2[i,5] <- ifelse(grepl("M-dependent",x[[i]]$corstr),paste(x[[i]]$corstr,"(",attr(x[[i]]$corstr,"M"),")",sep=""),x[[i]]$corstr)
  }
  if(nrow(results) > 1){
    if(all(results2[,2]==results2[1,2]))
      if(verbose) cat("\n        Variance function: ",results2[1,2],"\n")
    if(all(results2[,3]==results2[1,3]))
      if(verbose) cat("            Link function: ",results2[1,3],"\n")
    if(all(results2[,4]==results2[1,4]))
      if(verbose) cat("         Linear predictor: ",results2[1,4],"\n")
    if(all(results2[,5]==results2[1,5]))
      if(verbose) cat("    Correlation structure: ",results2[1,5],"\n")
    if(verbose) cat("\n")
    ids <- c(TRUE,!all(results2[,2]==results2[1,2]),!all(results2[,3]==results2[1,3]),!all(results2[,4]==results2[1,4]),!all(results2[,5]==results2[1,5]))
    temp <- as.matrix(results2[,ids])
    out_ <- data.frame(temp,results)
    colnames(out_) <- c(c("Object","Variance function","Link function","Predictor","Correlation")[ids],"RJC ")
    if(verbose) print(out_,row.names=FALSE)
    return(invisible(out_))
  }else return(results[,1])
}


#' @title Variable selection in Generalized Estimating Equations
#' @description Performs variable selection in generalized estimating equations using a chosen model fit criterion.
#' @param model an object of the class glmgee which is obtained from the fit of a generalized estimating equation. The linear predictor of the model whose fit is stored in this glmgee object is the more complex candidate which should be considered by the variable selection procedure. The more simple model which should be considered by the variable selection procedure is that with just the Intercept, if there is.
#' @param direction an (optional) character string indicating the mode of variable selection which should be used. The available options are: deleting variables ("backward")  and adding variables ("forward"). By default, \code{direction} is set to be "backward".
#' @param test an (optional) character string indicating the statistical test which should be used to compare nested models. The available options are: Wald ("wald") and generalized score ("score") tests. By default, \code{test} is set to be "wald".
#' @param level an (optional) numeric value in the interval (0,1) indicating the significance level chosen to perform the statistical tests. This is only appropiate if \code{criterion="p-value"}. By default, \code{level} is set to be 0.05.
#' @param criterion an (optional) character string indicating the criterion which should be used to compare the candidate models. The available options are: QIC ("qic"), QICu ("qicu"), \emph{p}-value of a statistical test ("p-value") and adjusted deviance-based R-squared ("adjr2"). By default, \code{criterion} is set to be "p-value".
#' @param ...	further arguments passed to or from other methods. For example, \code{k}, that is, the magnitude of the penalty in the QIC (or the QICu), which by default is set to be 2.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @seealso \link{stepCriterion.lm}, \link{stepCriterion.glm}, \link{stepCriterion.overglm}
#' @return A list with the following objects:
#' \itemize{
#' \item{\code{initial}:}{ an expression describing the linear predictor of the "initial" model.}
#' \item{\code{final}:}{ an expression describing the linear predictor of the "final" model.}
#' \item{\code{criterion}:}{ a character string describing the criterion chosen to compare the candidate models.}
#' }
#' @examples
#' ## Example 1
#' mod <- size ~ poly(days,4)*treat
#' fit1 <- glmgee(mod, id=tree, family=Gamma("log"), data=spruce, corstr="AR-1")
#' stepCriterion(fit1, criterion="p-value", direction="forward")
#'
#' ## Example 2
#' mod <- depressd ~ visit*group
#' fit2 <- glmgee(mod, id=subj, family=binomial("logit"), corstr="AR-1", data=depression)
#' stepCriterion(fit2, criterion="adjr2", direction="forward")
#' @method stepCriterion glmgee
#' @export
stepCriterion.glmgee <- function(model, criterion=c("p-value","qic","qicu","adjr2"),
                          direction=c("backward","forward"), test=c("wald","score"), level=0.05, verbose=TRUE, ...){
  xxx <- list(...)
  if(is.null(xxx$k)) k <- 2 else k <- xxx$k
  criterion <- match.arg(criterion)
  direction <- match.arg(direction)
  test <- match.arg(test)

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
  tol <- TRUE
  count <- 0
  inter <- ifelse(length(coef(model))>0,rownames(coef(model))[1]=="(Intercept)",FALSE)
  oldformula <- constr.formula(model,inter)
  if(direction=="forward") oldformula <- constr.formula(model,inter,term=attr(model$terms,"term.labels"),action="-")
  if(test=="wald") test2 <- "Wald test"
  if(test=="score") test2 <- "generalized score test"

  nullmodel <- constr.formula(model,inter,term=attr(model$terms,"term.labels"),action="-")
  ps <- ifelse(criterion=="p-value",FALSE,TRUE)
  corstr <- ifelse(grepl("M-dependent",model$corstr),paste(model$corstr,"(",attr(model$corstr,"M"),")",sep=""),model$corstr)
  if(verbose){
    cat("\n    Variance function: ",model$family$family,"\n")
    cat("        Link function: ",model$family$link,"\n")
    cat("Correlation structure: ",corstr)
    cat("\n****************************************************************************")
    cat("\nInitial model:\n")
  }
  fit.x <- model
  oldformula2 <- oldformula
  oldformula <- paste(oldformula,"+ offset(model$offset)")
  if(direction=="forward") fit.x <- glmgee(as.formula(oldformula),weights=model$prior.weights,family=model$family,id=model$id,corstr=corstr,data=datas,adjr2=FALSE)
  if(verbose) cat(oldformula2,"\n\n")
  out_ <- list(initial=oldformula2,criterion=criterion)
  newformula2 <- oldformula2
  newformula <- oldformula
  sale <- " "
  inter <- ifelse(length(coef(model))>0,rownames(coef(model))[1]=="(Intercept)",FALSE)
  if(!inter & direction=="forward")
    stop("The forward variable selection and non-intercept models are not compatible!!",call.=FALSE)
  names.col <- c("df"," adj.R-squared"," QIC "," Pr(Chisq>)(*)")
  if(criterion=="qicu") names.col[3] <- " QICu "
  delta <- c(1,-1,1,-1)
  if(!inter){
    names.col[2] <- "Deviance(^)"
    delta[2] <- 1
  }
  if(criterion=="qicu") id0 <- c(" ","adjr2","qicu","p-value")==criterion
  else id0 <-  c(" ","adjr2","qic","p-value")==criterion
  if(direction=="forward") delta[length(delta)] <- 1
  long <- max(nchar(attr(model$terms,"term.labels")))+2
  if(is.null(model$adj.rsquared) & inter){
    fit00 <- glmgee(as.formula(nullmodel),weights=model$prior.weights,family=model$family,id=model$id,corstr=corstr,data=datas,adjr2=TRUE)
    model$null.deviance <- fit00$deviance
    model$df.null <- fit00$df.residual
  }
  while(tol){
    if(length(attr(fit.x$terms,"term.labels"))==0) names.effects <- "" else names.effects <- attr(fit.x$terms,"term.labels")
    if(direction=="backward"){
      results <- matrix(NA,length(names.effects)+1,length(delta))
      if(criterion=="p-value") results[1,ncol(results)] <- -1
      if(count == 0){
        if(inter) results[1,2:(length(delta)-1)] <- c(1-(fit.x$deviance/fit.x$df.residual)/(model$null.deviance/model$df.null),QIC(fit.x,k=k))
        else results[1,2:(length(delta)-1)] <- c(fit.x$deviance/fit.x$df.residual,QIC(fit.x,k=k))
      }
      else results[1,2] <- 0
      s <- attr(fit.x$terms,"factors")
      for(i in 1:length(names.effects)){
        if(all(apply(as.matrix(s[,-i]*s[,i]),2,sum) < sum(s[,i]))){
          formula0 <- constr.formula(fit.x,inter,term=names.effects[i],action="-")
          formula0 <- paste(formula0,"+ offset(model$offset)")
          fit0 <- glmgee(as.formula(formula0),weights=model$prior.weights,id=model$id,family=model$family,corstr=corstr,data=datas,adjr2=TRUE)
          results[i+1,1] <- length(coef(fit.x)) - length(coef(fit0))
          results[i+1,3] <-  -2*fit0$logLik + ifelse(criterion=="qicu",k*length(fit0$coefficients),k*fit0$CIC)
          if(inter) results[i+1,2] <- 1-(fit0$deviance/fit0$df.residual)/(model$null.deviance/model$df.null)
          else results[i+1,2] <- fit0$deviance/fit0$df.residual
          results[i+1,4] <- anova(fit0,fit.x,test=test,verbose=FALSE)[1,3]
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
      if(count == 0) results[1,2:(length(delta)-1)] <- c(1-(fit.x$deviance/fit.x$df.residual)/(model$null.deviance/model$df.null),QIC(fit.x,k=k)) else results[1,2] <- 0
      for(i in 1:length(names.effects)){
        if(sum(apply(as.matrix(s2[,-i]),2,function(x) sum(s2[,i]*x)==sum(x)))==0){
          formula0 <- constr.formula(fit.x,inter,term=names.effects[i],action="+")
          formula0 <- paste(formula0,"+ offset(model$offset)")
          fit0 <- glmgee(as.formula(formula0),weights=model$prior.weights,id=model$id,family=model$family,corstr=corstr,data=datas,adjr2=TRUE)
          results[i+1,1] <- length(coef(fit0)) - length(coef(fit.x))
          results[i+1,3] <- -2*fit0$logLik + ifelse(criterion=="qicu",k*length(fit0$coefficients),k*fit0$CIC)
          if(inter) results[i+1,2] <- 1-(fit0$deviance/fit0$df.residual)/(model$null.deviance/model$df.null)
          else results[i+1,2] <- fit0$deviance/fit0$df.residual
          results[i+1,4] <- anova(fit.x,fit0,test=test,verbose=FALSE)[1,3]
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
    if(inter) tst.ind <- c(2:4)[colnames(results)[-1]!=names.col[3]]
    else tst.ind <- c(2:4)[colnames(results)[-1]==names.col[4]]
    if(verbose) print(round(results[indexes,],digits=5),na.print=" ")
    count <- count + 1

    if(names.effects3[ids]!="<none>" & ps==TRUE){
      ids <- ids[-1]
      sale <- paste(names.effects2[ids,1],names.effects2[ids,2])
      newformula2 <- constr.formula(fit.x,inter,term=names.effects2[ids,2],action=names.effects2[ids,1])
      newformula <- paste(newformula2,"+ offset(model$offset)")
      if(nrow(names.effects)==1 | (nrow(names.effects)==2 & !inter)){
        tol <- FALSE
        if(verbose) cat("\nStep",count,":",sale,"\n")
      }
      fit.x <- glmgee(as.formula(newformula),weights=model$prior.weights,id=model$id,family=model$family,corstr=corstr,data=datas,adjr2=TRUE)
    }else tol <- FALSE
  }
  if(verbose){
    cat("\n\nFinal model:\n")
    cat(newformula2,"\n")
    cat("****************************************************************************\n")
    cat("(*) p-values of the",test2)
    if(criterion=="p-value" & direction=="backward") cat(" ( effects are dropped when their p-values are higher than",level,")")
    if(criterion=="p-value" & direction=="forward")  cat(" ( effects are included when their p-values are lower than",level,")")
    if(!inter) cat("\n(^) Deviance divided by its degrees of freedom")
    if(!is.null(xxx$k)) cat("The magnitude of the penalty in the ",ifelse(criterion=="qicu","QICu","QIC")," was set to be ",xxx$k)
    cat("\n")
  }
  out_$final <- newformula2
  return(invisible(out_))
}



#' @title AGPC for Generalized Estimating Equations
#' @description Computes the Akaike-type penalized Gaussian pseudo-likelihood criterion (AGPC) for one or more objects of the class glmgee.
#' @param ...	one or several objects of the class glmgee which are obtained from the fit of generalized estimating equations.
#' @param k an (optional) non-negative value giving the magnitude of the penalty. By default, \code{k} is set to be 2.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @return A \code{data.frame} with the values of the gaussian pseudo-likelihood, the number of parameters in the linear predictor plus the number of parameters in the correlation matrix, and the value of AGPC for each glmgee object in the input.
#' @details If \code{k} is set to be 0 then the AGPC reduces to the Gaussian pseudo-likelihood criterion (GPC), proposed by Carey and Wang (2011), which corresponds to the logarithm of the multivariate normal density function.
#' @export AGPC
#' @seealso \link{QIC}, \link{CIC}, \link{RJC}, \link{GHYC}, \link{SGPC}
#' @examples
#' ## Example 1
#' mod <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod, id=tree, family=Gamma("log"), data=spruce, corstr="Exchangeable")
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' AGPC(fit1, fit2, fit3, fit4)
#'
#' ## Example 2
#' mod <- dep ~ visit + group
#' fit1 <- glmgee(mod, id=subj, family=gaussian, corstr="Exchangeable", data=depression)
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Non-Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' AGPC(fit1, fit2, fit3, fit4)
#'
#' ## Example 3
#' mod <- depressd ~ visit + group
#' fit1 <- glmgee(mod, id=subj, family=binomial, corstr="Exchangeable", data=depression)
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' AGPC(fit1, fit2, fit3, fit4)
#' @references Carey V.J. and Wang Y.-G. (2011) Working covariance model selection for generalized estimating equations. \emph{Statistics in Medicine} 30, 3117–3124.
#' @references Zhu X. and Zhu Z. (2013) Comparison of criteria to select working correlation matrix in generalized estimating
#' equations. \emph{Chinese Journal of Applied Probability and Statistics} 29, 515-530.
#' @references Fu L., Hao Y. and Wang Y.-G. (2018) Working correlation structure selection in generalized
#' estimating equations. \emph{Computational Statistics} 33, 983-996.

AGPC <- function(...,k=2,verbose=TRUE){
  x <- list(...)
  if(any(lapply(x,function(xx) class(xx)[1])!="glmgee"))
    stop("Only glmgee-type objects are supported!!",call.=FALSE)
  results <- matrix(NA,length(x),3)
  results2 <- matrix(NA,length(x),5)
  rows <-  matrix(NA,length(x),1)
  call. <- match.call()
  Gosho <- function(model){
    p <- length(model$coefficients)
    family <- model$family
    phi <- model$phi
    beta <- model$coefficients
    maxsize <- ifelse(is.null(model$waves),max(model$sizes),max(model$waves))
    VSi <- function(D){
      Xi <- as.matrix(D[,1:p])
      yi <- D[,p+1]
      ni <- nrow(Xi)
      etai <- tcrossprod(Xi,t(beta)) + D[,p+2]
      mui <- family$linkinv(etai)
      wi <- sqrt(family$variance(mui)/D[,p+3])
      Vi <- phi*t(model$corr[D[,p+4],D[,p+4]]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
      return(ni*log(2*pi) + t(yi - mui)%*%solve(Vi)%*%(yi - mui) + log(det(Vi)))
    }
    resume <- Reduce('+',lapply(model$arrangedata,VSi))
    q <- switch (model$corstr,
                 "AR-1" = 1,
                 "Independence" = 0,
                 "Exchangeable" = 1,
                 "Unstructured" = maxsize*(maxsize-1)/2,
                 "Stationary-M-dependent" = attr(model$corstr,"M"),
                 "Non-Stationary-M-dependent" = attr(model$corstr,"M")*maxsize - attr(model$corstr,"M")*(attr(model$corstr,"M")+1)/2)
    cr1 <- resume + k*(p+q)
    cr2 <- p+q
    return(list(cr0=resume,cr1=cr1,cr2=cr2))
  }
  for(i in 1:length(x)){
    temporal <- Gosho(x[[i]])
    results[i,1] <- round(temporal$cr0,digits=4)
    results[i,2] <- temporal$cr2
    results[i,3] <- round(temporal$cr1,digits=4)
    results2[i,1] <- as.character(call.[i+1])
    results2[i,2] <- x[[i]]$family$family
    results2[i,3] <- x[[i]]$family$link
    results2[i,4] <- paste(c(attr(x[[i]]$terms,"intercept"),attr(x[[i]]$terms,"term.labels")),collapse=" + ")
    results2[i,5] <- ifelse(grepl("M-dependent",x[[i]]$corstr),paste(x[[i]]$corstr,"(",attr(x[[i]]$corstr,"M"),")",sep=""),x[[i]]$corstr)
  }
  if(nrow(results) > 1){
    if(all(results2[,2]==results2[1,2]))
      if(verbose) cat("\n        Variance function: ",results2[1,2],"\n")
    if(all(results2[,3]==results2[1,3]))
      if(verbose) cat("            Link function: ",results2[1,3],"\n")
    if(all(results2[,4]==results2[1,4]))
      if(verbose) cat("         Linear predictor: ",results2[1,4],"\n")
    if(all(results2[,5]==results2[1,5]))
      if(verbose) cat("    Correlation structure: ",results2[1,5],"\n")
    if(verbose) cat("\n")
    ids <- c(TRUE,!all(results2[,2]==results2[1,2]),!all(results2[,3]==results2[1,3]),!all(results2[,4]==results2[1,4]),!all(results2[,5]==results2[1,5]))
    temp <- as.matrix(results2[,ids])
    out_ <- data.frame(temp,results)
    colnames(out_) <- c(c("Object","Variance function","Link function","Predictor","Correlation")[ids],"GPL","Parameters","AGPC ")
    if(verbose) print(out_,row.names=FALSE)
    return(invisible(out_))
  }else return(results[,2])
}

#' @title SGPC for Generalized Estimating Equations
#' @description Computes the Schwarz-type penalized Gaussian pseudo-likelihood criterion (SGPC) for one or more objects of the class glmgee.
#' @param ...	one or several objects of the class glmgee which are obtained from the fit of generalized estimating equations.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. By default, \code{verbose} is set to be TRUE.
#' @return A \code{data.frame} with the values of the gaussian pseudo-likelihood, the number of parameters in the linear predictor plus the number of parameters in the correlation matrix, and the value of SGPC for each glmgee object in the input.
#' @export SGPC
#' @seealso \link{QIC}, \link{CIC}, \link{RJC}, \link{GHYC}, \link{AGPC}
#' @examples
#' ## Example 1
#' mod <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod, id=tree, family=Gamma("log"), data=spruce, corstr="Exchangeable")
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' SGPC(fit1, fit2, fit3, fit4)
#'
#' ## Example 2
#' mod <- dep ~ visit + group
#' fit1 <- glmgee(mod, id=subj, family=gaussian, corstr="Exchangeable", data=depression)
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Non-Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' SGPC(fit1, fit2, fit3, fit4)
#'
#' ## Example 3
#' mod <- depressd ~ visit + group
#' fit1 <- glmgee(mod, id=subj, family=binomial, corstr="Exchangeable", data=depression)
#' fit2 <- update(fit1, corstr="AR-1")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Independence")
#' SGPC(fit1, fit2, fit3, fit4)
#' @references Carey V.J. and Wang Y.-G. (2011) Working covariance model selection for generalized estimating equations. \emph{Statistics in Medicine} 30, 3117–3124.
#' @references Zhu X. and Zhu Z. (2013) Comparison of criteria to select working correlation matrix in generalized estimating
#' equations. \emph{Chinese Journal of Applied Probability and Statistics} 29, 515-530.
#' @references Fu L., Hao Y. and Wang Y.-G. (2018) Working correlation structure selection in generalized
#' estimating equations. \emph{Computational Statistics} 33, 983-996.

SGPC <- function(...,verbose=TRUE){
  x <- list(...)
  if(any(lapply(x,function(xx) class(xx)[1])!="glmgee"))
    stop("Only glmgee-type objects are supported!!",call.=FALSE)
  results <- matrix(NA,length(x),3)
  results2 <- matrix(NA,length(x),5)
  rows <-  matrix(NA,length(x),1)
  call. <- match.call()
  Gosho <- function(model){
    p <- length(model$coefficients)
    family <- model$family
    phi <- model$phi
    beta <- model$coefficients
    maxsize <- ifelse(is.null(model$waves),max(model$sizes),max(model$waves))
    VSi <- function(D){
      Xi <- as.matrix(D[,1:p])
      yi <- D[,p+1]
      ni <- nrow(Xi)
      etai <- tcrossprod(Xi,t(beta)) + D[,p+2]
      mui <- family$linkinv(etai)
      wi <- sqrt(family$variance(mui)/D[,p+3])
      Vi <- phi*t(model$corr[D[,p+4],D[,p+4]]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
      return(ni*log(2*pi) + t(yi - mui)%*%solve(Vi)%*%(yi - mui) + log(det(Vi)))
    }
    resume <- Reduce('+',lapply(model$arrangedata,VSi))
    q <- switch (model$corstr,
                 "AR-1" = 1,
                 "Independence" = 0,
                 "Exchangeable" = 1,
                 "Unstructured" = maxsize*(maxsize-1)/2,
                 "Stationary-M-dependent" = attr(model$corstr,"M"),
                 "Non-Stationary-M-dependent" = attr(model$corstr,"M")*maxsize - attr(model$corstr,"M")*(attr(model$corstr,"M")+1)/2)
    cr1 <- resume + log(length(model$sizes))*(p+q)
    cr2 <- p+q
    return(list(cr0=resume,cr1=cr1,cr2=cr2))
  }
  for(i in 1:length(x)){
    temporal <- Gosho(x[[i]])
    results[i,1] <- round(temporal$cr0,digits=4)
    results[i,2] <- temporal$cr2
    results[i,3] <- round(temporal$cr1,digits=4)
    results2[i,1] <- as.character(call.[i+1])
    results2[i,2] <- x[[i]]$family$family
    results2[i,3] <- x[[i]]$family$link
    results2[i,4] <- paste(c(attr(x[[i]]$terms,"intercept"),attr(x[[i]]$terms,"term.labels")),collapse=" + ")
    results2[i,5] <- ifelse(grepl("M-dependent",x[[i]]$corstr),paste(x[[i]]$corstr,"(",attr(x[[i]]$corstr,"M"),")",sep=""),x[[i]]$corstr)
  }
  if(nrow(results) > 1){
    if(all(results2[,2]==results2[1,2]))
      if(verbose) cat("\n        Variance function: ",results2[1,2],"\n")
    if(all(results2[,3]==results2[1,3]))
      if(verbose) cat("            Link function: ",results2[1,3],"\n")
    if(all(results2[,4]==results2[1,4]))
      if(verbose) cat("         Linear predictor: ",results2[1,4],"\n")
    if(all(results2[,5]==results2[1,5]))
      if(verbose) cat("    Correlation structure: ",results2[1,5],"\n")
    if(verbose) cat("\n")
    ids <- c(TRUE,!all(results2[,2]==results2[1,2]),!all(results2[,3]==results2[1,3]),!all(results2[,4]==results2[1,4]),!all(results2[,5]==results2[1,5]))
    temp <- as.matrix(results2[,ids])
    out_ <- data.frame(temp,results)
    colnames(out_) <- c(c("Object","Variance function","Link function","Predictor","Correlation")[ids],"GPL","Parameters","SGPC ")
    if(verbose) print(out_,row.names=FALSE)
    return(invisible(out_))
  }else return(results[,2])
}

#' @title Estimate of the variance-covariance matrix in GEEs
#' @description Computes the type-\code{type} estimate of the variance-covariance matrix from an object of the class glmgee.
#' @param object An object of the class glmgee which is obtained from the fit of a generalized estimating equation.
#' @param type an (optional) character string indicating the type of estimator which should be used. The available options are: robust estimator ("robust"), jackknife estimator computed from the one-step approximations of the ``leave-one-out'' estimates of the parameter vector ("jackknife"). By default, \code{type} is set to be "robust".
#' @param ...	further arguments passed to or from other methods. For example, \code{k}, that is, the magnitude of the penalty in the QIC (or the QICu), which by default is set to be 2.
#' @return A \code{matrix} with the type-\code{type} estimate of the variance-covariance matrix.
#' @method vcov glmgee
#' @export
#' @examples
#' ## Example 1
#' mod <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod, id=tree, family=Gamma("log"), data=spruce, corstr="Exchangeable")
#' vcov(fit1)
#' vcov(fit1,type="jackknife")
#'
#' ## Example 2
#' mod <- dep ~ visit + group
#' fit2 <- glmgee(mod, id=subj, family=gaussian, corstr="AR-1", data=depression)
#' vcov(fit2)
#' vcov(fit2,type="jackknife")
#'
#' ## Example 3
#' mod <- depressd ~ visit + group
#' fit3 <- glmgee(mod, id=subj, family=binomial, corstr="Stationary-M-dependent(3)", data=depression)
#' vcov(fit3)
#' vcov(fit3,type="jackknife")
#'
#' @references Lipsitz S.R., Laird N.M. and Harrington D.P. (1990) Using the jackknife to estimate the variance of regression estimators from repeated measures studies. \emph{Communications in Statistics - Theory and Methods} 19, 821–845.
#'
vcov.glmgee <- function(object,...,type=c("robust","jackknife")){
  type <- match.arg(type)
  if(type=="jackknife"){
    dfbetas <- dfbeta(object)
    out <- crossprod(dfbetas)
    out <- (1 - length(object$coefficients)/length(object$sizes))*out
    return(out)
  }
  if(type=="robust") return(object$R)
}

