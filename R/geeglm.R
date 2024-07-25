#' @title Fit Generalized Estimating Equations
#' @description Produces an object of the class \code{glmgee} in which the main results of a Generalized Estimating Equation (GEE) fitted to the data are stored.
#' @param formula a \code{formula} expression of the form \code{response ~ x1 + x2 + ...}, which is a symbolic description of the linear predictor of the model to be fitted to the data.
#' @param family an (optional) \code{family} object, that is, a list of functions and expressions for defining link and variance functions. Families (and links) supported are the same supported by \link{glm} using its \link{family} argument, that is,
#' \code{gaussian}, \code{binomial}, \code{poisson}, \code{Gamma}, \code{inverse.gaussian}, and \code{quasi}. The family \code{negative.binomial} in the library \pkg{MASS} are also available. As default, the argument \code{family} is set to \code{gaussian(identity)}.
#' @param id a vector which identifies the subjects or clusters. The length of \code{id} should be the same as the number of observations.
#' @param weights an (optional) vector of positive "prior weights" to be used in the fitting process. The length of \code{weights} should be the same as the total number of observations.
#' @param data an (optional) \code{data frame} in which to look for variables involved in the \code{formula} expression, as well as for variables specified in the arguments \code{id} and \code{weights}. The data are assumed to be sorted by \code{id} and time.
#' @param subset an (optional) vector specifying a subset of observations to be used in the fitting process.
#' @param corstr an (optional) character string which allows to specify the working-correlation structure. The available options are: "Independence", "Unstructured", "Stationary-M-dependent(\emph{m})", "Non-Stationary-M-dependent(\emph{m})", "AR-M-dependent(\emph{m})", "Exchangeable" and "User-defined", where \emph{m} represents the lag of the dependence. As default, \code{corstr} is set to "Independence".
#' @param corr an (optional) square matrix of the same dimension of the maximum cluster size containing the user specified correlation. This is only appropriate if \code{corstr} is specified to be "User-defined".
#' @param start an (optional) vector of starting values for the parameters in the linear predictor.
#' @param maxit an (optional) integer value which represents the maximum number of iterations allowed for the fitting algorithm. As default, \code{maxit} is set to 50.
#' @param toler an (optional) positive value which represents the \emph{convergence tolerance}. The convergence is reached when the maximum of the absolute relative differences between the values of the parameters in the linear predictor in consecutive iterations of the fitting algorithm is lower than \code{toler}. As default, \code{toler} is set to 0.00001.
#' @param trace an (optional) logical variable. If TRUE, output is produced for each iteration of the estimating algorithm.
#' @param scale.fix an (optional) logical variable. If TRUE, the scale parameter is fixed at the value of \code{scale.value}. As default, \code{scale.fix} is set to \code{FALSE}.
#' @param scale.value an (optional) numeric value at which the scale parameter should be fixed. This is only appropriate if \code{scale.fix=TRUE}. As default, \code{scale.value} is set to 1.
#' @param waves an (optional) positive integer-valued variable that is used to identify the order and spacing of observations within clusters. This argument is crucial when there are missing values and gaps in the data. As default, \code{waves} is equal to the integers from 1 to the size of each cluster.
#' @param ...	further arguments passed to or from other methods.
#' @details The values of the multivariate response variable measured on \eqn{n} subjects or clusters,
#' denoted by \eqn{y_{i}=(y_{i1},\ldots,y_{in_i})^{\top}} for \eqn{i=1,\ldots,n}, are assumed to be
#' realizations of independent random vectors denoted by \eqn{Y_{i}=(Y_{i1},\ldots,Y_{in_i})^{\top}}
#' for \eqn{i=1,\ldots,n}. The random variables associated to the \eqn{i}-th subject or
#' cluster, \eqn{Y_{ij}} for \eqn{j=1,\ldots,n_i}, are assumed to satisfy
#' \eqn{\mu_{ij}=} E\eqn{(Y_{ij})},Var\eqn{(Y_{ij})=\frac{\phi}{\omega_{ij}}}V\eqn{(\mu_{ij})}
#' and Corr\eqn{(Y_{ij},Y_{ik})=r_{jk}(\rho)},
#' where \eqn{\phi>0} is the dispersion parameter,
#' V\eqn{(\mu_{ij})} is the variance function, \eqn{\omega_{ij}>0} is a known weight, and
#' \eqn{\rho=(\rho_1,\ldots,\rho_q)^{\top}} is a parameter vector.
#' In addition, \eqn{\mu_{ij}} is assumed to be dependent on the regressors vector \eqn{x_{ij}}
#' by \eqn{g(\mu_{ij})=z_{ij} + x_{ij}^{\top}\beta}, where \eqn{g(\cdot)} is the link function,
#' \eqn{z_{ij}} is a known \emph{offset} and \eqn{\beta=(\beta_1,\ldots,\beta_p)^{\top}} is
#' a vector of regression parameters. The parameter estimates are obtained by iteratively
#' solving the estimating equations described by Liang and Zeger (1986).
#'
#' If the maximum cluster size is 6 and for a cluster of size 4 the value
#' of \code{waves} is set to 2, 4, 5, 6, then it means that the data at
#' times 1 and 3 are missing, which should be taken into account by
#' \code{glmgee} when the structure of the correlation matrix is assumed
#' to be "Unstructured", "Stationary-M-dependent", "Non-Stationary-M-dependent"
#' or "AR-M-dependent". If in this scenario \code{waves} is not specified
#' then \code{glmgee} assumes that the available data for this cluster
#' were taken at times 1, 2, 3 and 4.
#'
#' A set of standard extractor functions for fitted model objects is
#' available for objects of class \emph{glmgee}, including methods to generic functions such as  \code{print}, \code{summary},	\code{model.matrix}, \code{estequa},
#' \code{coef}, \code{vcov}, \code{logLik}, \code{fitted}, \code{confint} and \code{predict}.
#' In addition, the model may be assessed using functions such as \link{anova.glmgee},
#' \link{residuals.glmgee}, \link{dfbeta.glmgee}, \link{cooks.distance.glmgee}, \link{localInfluence.glmgee},
#' \link{tidy.glmgee} and \link{glance.glmgee}.
#' The variable selection may be accomplished using the routine
#' \link{stepCriterion.glmgee}.
#'
#' @return an object of class \emph{glmgee} in which the main results of the GEE model fitted to the data are stored, i.e., a
#' list with components including
#' \tabular{ll}{
#' \code{coefficients} \tab a vector with the estimates of \eqn{\beta_1,\ldots,\beta_p},\cr
#' \tab \cr
#' \code{fitted.values}\tab a vector with the estimates of \eqn{\mu_{ij}} for \eqn{i=1,\ldots,n} and \eqn{j=1,\ldots,n_i},\cr
#' \tab \cr
#' \code{start}        \tab a vector with the starting values used,\cr
#' \tab \cr
#' \code{iter}         \tab a numeric constant with the number of iterations,\cr
#' \tab \cr
#' \code{prior.weights}\tab a vector with the values of \eqn{\omega_{ij}} for \eqn{i=1,\ldots,n} and \eqn{j=1,\ldots,n_i},\cr
#' \tab \cr
#' \code{offset}       \tab a vector with the values of \eqn{z_{ij}} for \eqn{i=1,\ldots,n} and \eqn{j=1,\ldots,n_i},\cr
#' \tab \cr
#' \code{terms}        \tab an object containing the terms objects,\cr
#' \tab \cr
#' \code{loglik}       \tab the value of the quasi-log-likelihood function evaluated at the parameter\cr
#'                     \tab estimates and the observed data,\cr
#' \tab \cr
#' \code{estfun}       \tab a vector with the estimating equations evaluated at the parameter\cr
#'                     \tab estimates and the observed data,\cr
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
#' \code{y}            \tab a vector with the values of \eqn{y_{ij}} for \eqn{i=1,\ldots,n} and \eqn{j=1,\ldots,n_i},\cr
#' \tab \cr
#' \code{family}       \tab an object containing the \link{family} object used,\cr
#' \tab \cr
#' \code{linear.predictors} \tab a vector with the estimates of \eqn{g(\mu_{ij})} for \eqn{i=1,\ldots,n} and \eqn{j=1,\ldots,n_i},\cr
#' \tab \cr
#' \code{R}            \tab a matrix with the (robust) estimate of the variance-covariance,\cr
#' \tab \cr
#' \code{corr}         \tab a matrix with the estimate of the working-correlation,\cr
#' \tab \cr
#' \code{corstr}       \tab a character string specifying the working-correlation structure,\cr
#' \tab \cr
#' \code{id}           \tab a vector which identifies the subjects or clusters,\cr
#' \tab \cr
#' \code{sizes}        \tab a vector with the values of \eqn{n_i} for \eqn{i=1,\ldots,n},\cr
#' \tab \cr
#' \code{call}         \tab the original function call,\cr
#' }
#' @seealso \link{gnmgee}, \link{wglmgee}
#' @export glmgee
#' @importFrom grDevices dev.new
#' @importFrom MASS negative.binomial
#' @importFrom statmod tweedie
#' @importFrom splines bs ns
#' @importFrom graphics abline par lines legend segments
#' @importFrom methods missingArg
#' @importFrom stats as.formula coef gaussian get_all_vars rnorm update qt var
#'             glm.fit model.extract model.frame model.matrix uniroot lm.fit lm.wfit
#'             model.offset model.response model.weights pnorm cov2cor qchisq
#'             printCoefmat pchisq vcov cooks.distance dfbeta qnorm anova na.omit
#'             formula terms pf quasibinomial quasipoisson toeplitz SSasymp
#'             SSasympOff SSasympOrig SSbiexp SSfol SSfpl SSgompertz SSlogis
#'             SSmicmen SSweibull nls na.pass
#' @examples
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod1 <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod1, id=tree, family=Gamma(log), corstr="AR-M-dependent(1)", data=spruces)
#' summary(fit1, corr.digits=2)
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod2 <- depressd ~ visit + group
#' fit2 <- glmgee(mod2, id=subj, family=binomial(logit), corstr="AR-M-dependent(1)", data=depression)
#' summary(fit2, corr.digits=2)
#'
#' ###### Example 3: Treatment for severe postnatal depression (2)
#' data(depression)
#' mod3 <- dep ~ visit*group
#' fit3 <- glmgee(mod3, id=subj, family=gaussian, corstr="AR-M-dependent(1)", data=depression)
#' summary(fit3, corr.digits=2)
#'
#' ###### Example 4: Dental Clinical Trial
#' data(rinse)
#' mod4 <- score/3.6 ~ rinse*time
#' fit4 <- glmgee(mod4, family=binomial(log), id=subject, corstr="Exchangeable", data=rinse)
#' summary(fit4, corr.digits=2)
#'
#' ###### Example 5: Shoulder Pain after Laparoscopic Cholecystectomy
#' data(cholecystectomy)
#' mod5 <- pain2 ~ treatment + age + time
#' corstr <- "Stationary-M-dependent(2)"
#' fit5 <- glmgee(mod5, family=binomial(logit), id=id, corstr=corstr, data=cholecystectomy)
#' summary(fit5,varest="bias-corrected")
#'
#' ###### Example 6: Guidelines for Urinary Incontinence Discussion and Evaluation
#' data(GUIDE)
#' mod6 <- bothered ~ gender + age + dayacc + severe + toilet
#' fit6 <- glmgee(mod6, family=binomial(logit), id=practice, corstr="Exchangeable", data=GUIDE)
#' summary(fit6)
#'
#' ###### Example 7: Tests of Auditory Perception in Children with OME
#' OME <- MASS::OME
#' mod7 <- cbind(Correct, Trials-Correct) ~ Loud + Age + OME
#' fit7 <- glmgee(mod7, family = binomial(cloglog), id = ID, corstr = "Exchangeable", data = OME)
#' summary(fit7, corr=FALSE)
#'
#' ###### Example 8: Epileptic seizures
#' data(Seizures)
#' Seizures2 <- within(Seizures, time4 <- ifelse(time==4,1,0))
#' mod8 <- seizures ~ log(age) + time4 + log(base/4)*treatment
#' fit8 <- glmgee(mod8, family=poisson(log), id=id, corstr="Exchangeable", data=Seizures2)
#' summary(fit8)
#'
#' @references Liang K.Y., Zeger S.L. (1986) Longitudinal data analysis using generalized linear models.
#' \emph{Biometrika} 73:13-22.
#' @references Zeger S.L., Liang K.Y. (1986) Longitudinal data analysis for discrete and continuous outcomes.
#' \emph{Biometrics} 42:121-130.
#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
#'
glmgee <- function(formula,family=gaussian(),weights,id,waves,data,subset,corstr,corr,start=NULL,scale.fix=FALSE,scale.value=1,toler=0.00001,maxit=50,trace=FALSE,...){
  if(missingArg(data)) data <- environment(eval(formula))
  if(missingArg(corstr)) corstr <- "Independence"
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "weights", "waves", "data", "subset", "id"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- na.omit
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- as.matrix(model.response(mf, "any"))
  weights <- as.vector(model.weights(mf))
  if(is(family,"function")) family <- family()

  if(family$family %in% c("quasi","quasibinomial","quasipoisson")){
    if(family$family=="quasi"){
      family$family <- switch(family$varfun,"constant"="gaussian","mu(1-mu)"="binomial",
                              "mu"="poisson","mu^2"="Gamma","mu^3"="inverse.gaussian")
    }
    else{
      family$family <- switch(family$family,"quasibinomial"="binomial","quasipoisson"="poisson")
    }
    family <- do.call(family$family,list(link=family$link))
    family2 <- family
  }
  else{
    if(family$family %in% c("gaussian","binomial","poisson","Gamma","inverse.gaussian")){
      varfun <- switch(family$family,"gaussian"="constant","binomial"="mu(1-mu)",
                       "poisson"="mu","Gamma"="mu^2","inverse.gaussian"="mu^3")
      family2 <- do.call("quasi",list(variance=varfun,link=family$link))
    if(family$family=="binomial") family2 <- do.call("quasibinomial",list(link=family$link))
    if(family$family=="poisson") family2 <- do.call("quasipoisson",list(link=family$link))
    }else family2 <- family
  }
  if(ncol(y)==2 & family$family=="binomial"){
    weights <- as.matrix(y[,1]+y[,2])
    y <- as.matrix(y[,1]/weights)
  }
  y <- as.matrix(as.numeric(y))
  offset <- as.vector(model.offset(mf))
  X <- model.matrix(mt, mf)
  p <- ncol(X)
  n <- nrow(X)
  if(is.null(offset)) offs <- matrix(0,n,1) else offs <- as.matrix(offset)
  if(is.null(weights)) weights <- matrix(1,n,1) else weights <- as.matrix(weights)
  if(any(weights <= 0)) stop("Only positive weights are allowed!!",call.=FALSE)
  waves <- as.vector(model.extract(mf,waves))
  if(!is.null(waves)) if(any(waves!=floor(waves)) | any(waves<=0)) stop("Only positive integers are allowed in waves!!",call.=FALSE)
  id <- model.extract(mf,id)
  if(is.null(id)) id <- matrix(1:n,n,1)
  id2 <- as.matrix(unique(id))
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
  if(maxsize==1) corstr <- "Independence"
  Rhat <- function(D,beta){
    etai <- tcrossprod(D[,1:p],t(beta)) + D[,p+2]
    mui <- family$linkinv(etai)
    wi <- sqrt(family$variance(mui)/D[,p+3])
    es <- rep(0,maxsize)
    es2 <- es
    es[D[,p+4]] <- (D[,p+1]-mui)/wi
    es2[D[,p+4]] <- 1
    nums <- tcrossprod(es)
    nums2 <- tcrossprod(es2)
    cbind(nums,nums2)
  }
  temp <- strsplit(gsub("[[:space:]]","",corstr),"[()]")[[1]]
  args <- c("Independence","Unstructured","Stationary-M-dependent",
            "Non-Stationary-M-dependent","AR-M-dependent","Exchangeable","User-defined")
  temp2 <- grep(tolower(temp[1]),tolower(args))
  if(length(temp)>0){
    corstr <- args[temp2[1]]
    if(corstr=="Stationary-M-dependent" | corstr=="Non-Stationary-M-dependent" | corstr=="AR-M-dependent"){
      M <- min(max(1,floor(abs(as.numeric(temp[2])))),maxsize-1)
      if(is.na(M)) M <- 1
      attr(corstr,"M") <- M
    }
  }else stop("Correlation structure should be one of Independence, Unstructured, Stationary-M-dependent,
 		            Non-Stationary-M-dependent, AR-M-dependent, Exchangeable and User-defined!!",call.=FALSE)
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
    if(corstr=="AR-M-dependent"){
      alphas <- matrix(0,M,1)
      R <- matrix(0,maxsize,maxsize)
      for(i in 1:M){
        inds <- cbind(1:(maxsize-i),(1+i):maxsize)
        inds2 <- cbind(inds[,1],maxsize+inds[,2])
        alphas[i] <- sum(resume[inds])/(sum(resume[inds2])-p)
      }
      alphas <- alphas/phi
      alphasf <- function(x) if(x==0) return(1) else return(alphas[x])
      phises <- crossprod(chol2inv(chol(apply(toeplitz(0:(M-1)),c(1,2),alphasf))),alphas)
      rhos <- matrix(0,maxsize-1,1)
      rhos[1:M] <- alphas
      for(i in (M+1):(maxsize-1)) rhos[i] <- sum(phises*rhos[i-c(1:M)])
      R <- toeplitz(c(1,rhos))
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
    Xi <- matrix(D[,1:p],ncol=p)
    yi <- D[,p+1]
    ni <- nrow(Xi)
    etai <- Xi%*%beta + D[,p+2]
    mui <- family$linkinv(etai)
    wi <- sqrt(family$variance(mui)/D[,p+3])
    Xiw <- Xi*matrix(family$mu.eta(etai),nrow(Xi),p)
    Vi <- t(R[D[,p+4],D[,p+4]]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
    if(corstr=="Independence") Vi2 <- diag(as.vector(1/wi[D[,p+4]]^2),ni,ni)
    else{Vi2 <- try(chol(Vi),silent=TRUE)
    if(is.matrix(Vi2)) Vi2 <- chol2inv(Vi2) else Vi2 <- solve(Vi)}
    Xiw2 <- crossprod(Vi2,Xiw)
    if(out) cbind(crossprod(Xiw2,(yi-mui)),crossprod(Xiw2,Xiw))
    else cbind(crossprod(Xiw2,(yi-mui)),crossprod(Xiw2,Xiw),crossprod(Xiw2,(tcrossprod(yi-mui))%*%Xiw2))
  }
  options(warn=-1)
  if(is.null(start)){
    beta_new <- try(glm.fit(y=y,x=X,family=family2,weights=weights,offset=offs),silent=TRUE)
    if(is.list(beta_new)){
      if(beta_new$converged) beta_new <- beta_new$coefficients
      else{beta_new <- try(glm.fit(y=y,x=X,family=family,weights=weights,offset=offs),silent=TRUE)
           if(is.list(beta_new)){
             if(beta_new$converged) beta_new <- beta_new$coefficients
             else stop("cannot find valid starting values: please specify some!!",call.=FALSE)
           }
      }
    }else{beta_new <- try(glm.fit(y=y,x=X,family=family,weights=weights,offset=offs),silent=TRUE)
          if(is.list(beta_new)){
            if(beta_new$converged) beta_new <- beta_new$coefficients
            else stop("cannot find valid starting values: please specify some!!",call.=FALSE)
          }
    }
  }else beta_new <- start
  options(warn=0)
  start <- beta_new
  tol <- 1
  niter <- 0
  if(missingArg(corr)) corr <- diag(maxsize)
  if(trace) message("\nIteration  Convergence Criterion")
  while(tol > toler & niter < maxit){
    beta_old <- beta_new
    resume <- 0
    for(i in 1:nclus) resume <- resume + Rhat(datas[[i]],beta=beta_old)
    phi <- sum(diag(matrix(resume[1:maxsize,1:maxsize],maxsize,maxsize)))/(sum(sizes)-p)
    if(corstr!="User-defined" & corstr!="Independence") R <- Rout(resume,corstr)
    else R <- as.matrix(corr)
    resume2 <- 0
    for(i in 1:nclus) resume2 <- resume2 + score(datas[[i]],beta=beta_old,out=FALSE)
    kchol <- try(chol(resume2[1:p,2:(p+1)]),silent=TRUE)
    if(is.matrix(kchol)) kchol <- chol2inv(kchol) else kchol <- solve(resume2[1:p,2:(p+1)])
    beta_new <- beta_old + crossprod(kchol,resume2[,1])
    tol <- max(abs((beta_new-beta_old)/beta_old))
    niter <- niter + 1
    if(trace) message("    ",niter,"            ",signif(tol,digits=5))
  }
  rownames(beta_new) <- colnames(X)
  colnames(beta_new) <- ""
  if(niter==maxit) warning("Iteration limit exceeded!!\n",call.=FALSE)
  eta <- X%*%beta_new + offs
  mu <- family$linkinv(eta)
  phi <- sum(diag(matrix(resume[1:maxsize,1:maxsize],maxsize,maxsize)))/(sum(sizes)-p)
  sera <- try(chol(R),silent=TRUE)
  if(!is.matrix(sera)) warning("Estimate of correlation matrix is not positive definite",call.=FALSE)
    I0 <- try(chol(resume2[1:p,2:(p+1)]),silent=TRUE)
    if(is.matrix(I0)) I0 <- chol2inv(I0) else I0 <- solve(resume2[1:p,2:(p+1)])
    I1 <- resume2[1:p,(p+2):(2*p+1)]
    RJC <- crossprod(I0,I1)
    vcovs <- RJC%*%I0
    rownames(vcovs) <- colnames(X)
    colnames(vcovs) <- colnames(X)
    qll <- NULL
    if(family$family=="gaussian") qll <- -weights*(y - mu)^2/2
    if(family$family=="binomial") qll <- weights*(y*log(mu) + (1-y)*log(1-mu))
    if(family$family=="poisson") qll <- weights*(y*log(mu) - mu)
    if(family$family=="Gamma") qll <- -weights*(y/mu + log(mu))
    if(family$family=="inverse.gaussian") qll <- weights*(mu - y/2)/mu^2
    if(family$family=="Negative Binomial"){
      .Theta <- function() return(.Theta)
      environment(.Theta) <- environment(family$variance)
      .Theta <- .Theta()
      qll <- weights*(y*log(mu/(mu + .Theta)) + .Theta*log(.Theta/(.Theta + mu)))
    }
    if(family$family=="Tweedie"){
      .Theta <- function() return(p)
      environment(.Theta) <- environment(family$variance)
      .Theta <- .Theta()
      if(.Theta %in% c(0,1,2)){
        if(.Theta==0) qll <- -weights*(y - mu)^2/2
        if(.Theta==1) qll <- weights*(y*log(mu) - mu)
        if(.Theta==2) qll <- -weights*(y/mu + log(mu))
      }else qll <- mu^(-.Theta)*(mu*y/(1 - .Theta) - mu^2/(2 - .Theta))
    }
    if(is.null(qll)) qll <- family$dev.resids(y,mu,weights)
    w <- sqrt(weights*family2$mu.eta(eta)^2/family2$variance(mu))
    Xw <- matrix(w,nrow(X),ncol(X))*X
    CIC <- sum(diag((crossprod(Xw)/phi)%*%vcovs))
    phi <- ifelse(scale.fix,scale.value,phi)
    RJC <- sqrt((1 - sum(diag(RJC))/(p*phi))^2 + (1 - sum(diag(RJC%*%RJC))/(p*phi^2))^2)
    logLik <- sum(qll)/phi
    rownames(R) <- paste("[",1:maxsize,"]",sep="")
    colnames(R) <- paste("[",1:maxsize,"] ",sep="")
    estfun <- as.matrix(resume2[,1])
    rownames(estfun) <- colnames(X)
    colnames(estfun) <- ""
    out_ <- list(coefficients=beta_new,fitted.values=mu,linear.predictors=eta,sizes=sizes,arrangedata=datas,
                 prior.weights=weights,y=y,formula=formula,call=match.call(),offset=offs,waves=waves,model=mf,data=data,score=score,ids=id2,
                 converged=ifelse(niter<maxit,TRUE,FALSE),estfun=estfun,R=vcovs,naive=I0,terms=mt,family=family,Rout=Rout,Rhat=Rhat,id=id,
                 phi=phi,CIC=CIC,RJC=RJC,logLik=logLik,corr=R,clusters=c(length(sizes),max(sizes),min(sizes)),corstr=corstr,
                 deviance=sum(family$dev.resids(y,mu,weights)),df.residual=length(y)-length(beta_new),levels=.getXlevels(attr(mf,"terms"),mf),
                 contrasts=attr(X,"contrasts"),start=start,iter=niter,linear=TRUE)
    class(out_) <- "glmgee"
    return(out_)
}
#' @method summary glmgee
#' @export
summary.glmgee <- function(object, ...,digits=max(3, getOption("digits") - 2),corr.digits=3,varest=c("robust","df-adjusted","bias-corrected","model"),corr=TRUE){
  varest <- match.arg(varest)
  .Theta <- function() return(.Theta)
  environment(.Theta) <- environment(object$family$variance)
  cat("\nSample size")
  cat("\n   Number of observations: ",sum(object$sizes))
  cat("\n       Number of clusters: ",length(object$sizes),"\n")
  if(var(range(object$sizes)) > 0){
  out_ <- matrix(quantile(object$sizes,probs=c(0,0.25,0.5,0.75,1)),1,5)
  rownames(out_)[1] <- "            Cluster sizes:";colnames(out_) <- c(" Min"," 25%"," 50%"," 75%"," Max")
  print(out_,digits=1)
  }else cat("             Cluster size: ",object$sizes[1],"\n")
  cat("*************************************************************")
  cat("\nModel")
  cat("\n        Variance function: ",object$family$family)
  cat("\n            Link function: ",object$family$link)
  cat("\n    Correlation structure: ",ifelse(grepl("M-dependent",object$corstr),paste(object$corstr,"(",attr(object$corstr,"M"),")",sep=""),object$corstr))
  cat("\n*************************************************************\n")
  cat("Coefficients\n")
  TAB	<- rbind(cbind(Estimate <- object$coefficients,
                     StdErr <- sqrt(diag(vcov(object,type=varest))),
                     tval <- Estimate/StdErr,
                     p.value <- 2*pnorm(-abs(tval))),
                     rep(NA,4),
                     c(object$phi,NA,NA,NA))
  colnames(TAB) <- c("Estimate", "Std.Error", "z-value", "Pr(>|z|)")
  rownames(TAB) <- c(rownames(object$coefficients),"","Dispersion")
  printCoefmat(TAB, P.values=TRUE, signif.stars=FALSE, has.Pvalue=TRUE, digits=digits, dig.tst=digits, signif.legend=FALSE, tst.ind=c(1,2,3), na.print="")
  cat("*************************************************************\n")
  if(corr){
    cat("Working correlation\n")
    print(round(object$corr,digits=corr.digits))
  }
  return(invisible(list(coefficients=round(TAB,digits=digits),dispersion=object$phi,corr=round(object$corr,digits=corr.digits))))
}

#' @method confint glmgee
#' @export
confint.glmgee <- function(object,parm,level=0.95,contrast,digits=max(3, getOption("digits") - 2),verbose=TRUE,varest=c("robust","df-adjusted","model","bias-corrected"),...){
varest <- match.arg(varest)
name.s <- rownames(object$coefficients)
if(missingArg(contrast)){
  bs <- coef(object)
  ee <- sqrt(diag(vcov(object,type=varest)))
}else{
  contrast <- as.matrix(contrast)
  if(ncol(contrast)!=length(name.s)) stop(paste("Number of columns of contrast matrix must to be",length(name.s)),call.=FALSE)
  bs <- contrast%*%coef(object)
  ee <- sqrt(diag(contrast%*%vcov(object,type=varest)%*%t(contrast)))
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
return(invisible(round(results,digits=digits)))
}
#' @title Dfbeta for Generalized Estimating Equations
#' @description Produces an approximation, better known as the \emph{one-step approximation},
#' of the effect on the parameter estimates of deleting each cluster/observation in turn. This function also can produce
#' an index plot of the Dfbeta Statistic for some parameters via the argument \code{coefs}.
#' @param model an object of class \emph{glmgee}.
#' @param coefs	an (optional) character string which (partially) match with the names of some parameters in the linear predictor.
#' @param method an (optional) character string indicating the method of calculation for the \emph{one-step approximation}. The options are: the \emph{one-step approximation} described by Preisser and Qaqish (1996) in which the working-correlation matrix is assumed to be known ("Preisser-Qaqish"); and the "authentic" \emph{one-step approximation} ("full"). As default, \code{method} is set to "Preisser-Qaqish".
#' @param level an (optional) character string indicating the level for which the Dfbeta statistic is required. The options are: cluster-level ("clusters") and observation-level ("observations"). As default, \code{level} is set to "clusters".
#' @param identify an (optional) integer indicating the number of clusters/observations to identify on the plot of the Dfbeta statistic. This is only appropriate if \code{coefs} is specified.
#' @param ... further arguments passed to or from other methods. If \code{coefs} is specified then \code{...} may be used to include graphical parameters to customize the plot. For example,  \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix with so many rows as clusters/observations in the sample and so many
#' columns as parameters in the linear predictor. For clusters, the \eqn{i}-th row of that matrix corresponds to the
#' difference between the estimates of the parameters in the linear predictor using all clustersand the \emph{one-step approximation} of those estimates when the \emph{i}-th cluster is excluded from the dataset.
#' @details The \emph{one-step approximation} (with the \code{method} "full") of the estimates of the parameters in the linear
#' predictor of a GEE when the \emph{i}-th cluster is excluded from the dataset is given by the
#' vector obtained as the result of the first iteration of the fitting algorithm of that GEE
#' when it is performed using:  (1) a dataset in which the \emph{i}-th cluster is excluded; and
#' (2) a starting value which is the solution to the same GEE but based on the dataset inluding all clusters.
#' @references Pregibon D. (1981). Logistic regression diagnostics. \emph{The Annals of Statistics} 9, 705-724.
#' @references Preisser J.S., Qaqish B.F. (1996) Deletion diagnostics for generalised estimating equations.
#' \emph{Biometrika} 83:551–562.
#' @references Hammill B.G., Preisser J.S. (2006) A SAS/IML software program for GEE and regression diagnostics.
#' \emph{Computational Statistics & Data Analysis} 51:1197-1212.
#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
#' @method dfbeta glmgee
#' @export
#' @examples
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod1 <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod1, id=tree, family=Gamma(log), corstr="AR-M-dependent", data=spruces)
#' dfbs1 <- dfbeta(fit1, method="full", coefs="treat", col="red", lty=1, lwd=1, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8, main="treat")
#'
#' ### Calculation by hand of dfbeta for the tree labeled by "N1T01"
#' onestep1 <- glmgee(mod1, id=tree, family=Gamma(log), corstr="AR-M-dependent",
#'             data=spruces, start=coef(fit1), subset=c(tree!="N1T01"), maxit=1)
#'
#' coef(fit1)-coef(onestep1)
#' dfbs1[rownames(dfbs1)=="N1T01",]
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod2 <- depressd ~ visit + group
#' fit2 <- glmgee(mod2, id=subj, family=binomial(logit), corstr="AR-M-dependent",
#'                data=depression)
#'
#' dfbs2 <- dfbeta(fit2, method="full", coefs="group" ,col="red", lty=1, lwd=1, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8, main="group")
#'
#' ### Calculation by hand of dfbeta for the woman labeled by "18"
#' onestep2 <- glmgee(mod2, id=subj, family=binomial(logit), corstr="AR-M-dependent",
#'             data=depression, start=coef(fit2), subset=c(subj!=18), maxit=1)
#'
#' coef(fit2)-coef(onestep2)
#' dfbs2[rownames(dfbs2)==18,]
#'
#' ###### Example 3: Treatment for severe postnatal depression (2)
#' mod3 <- dep ~ visit*group
#' fit3 <- glmgee(mod3, id=subj, family=gaussian(identity), corstr="AR-M-dependent",
#'                data=depression)
#'
#' dfbs3 <- dfbeta(fit3, method="full", coefs="visit:group" ,col="red", lty=1, lwd=1, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8, main="visit:group")
#'
#' ### Calculation by hand of dfbeta for the woman labeled by "18"
#' onestep3 <- glmgee(mod3, id=subj, family=gaussian(identity), corstr="AR-M-dependent",
#'             data=depression, start=coef(fit3), subset=c(subj!=18), maxit=1)
#'
#' coef(fit3)-coef(onestep3)
#' dfbs3[rownames(dfbs3)==18,]
#'
dfbeta.glmgee <- function(model, level=c("clusters","observations"), method=c("Preisser-Qaqish","full"), coefs, identify,...){
  method <- match.arg(method)
  level <- match.arg(level)
  if(level=="observations" & method=="full") stop("Dfbeta statistic at observation-level is only available for the 'Preisser-Qaqish' method!!",call.=FALSE)
  if(model$corstr %in% c("User-defined","Independence") & level=="clusters") method <- "Preisser-Qaqish"
  p <- length(model$coefficients)
  if(method=="full"){
    dfbetas <- matrix(0,length(model$ids),p)
    envir <- environment(model$score)
    environment(model$Rout) <- envir
    environment(model$Rhat) <- envir
    envir$p <- p
    envir$family <- model$family
    if(grepl("M-dependent",model$corstr))	envir$M <- attr(model$corstr,"M")
    envir$sizes <- model$sizes
    envir$maxsize <- ifelse(is.null(model$waves),max(envir$sizes),max(model$waves))
    datax <- model$arrangedata
    datas <- lapply(datax,model$Rhat,beta=model$coefficients)
    for(i in 1:length(model$ids)){
      if(model$corstr!="User-defined" & model$corstr!="Independence"){
        mus <- model$family$linkinv(model$linear.predictors[model$ids[i]==model$id])
        pr <- as.matrix((datax[[i]][,envir$p+1] - mus)/sqrt(model$family$variance(mus)/datax[[i]][,envir$p+3]))
        prs <- tcrossprod(pr)
        phi <- (model$phi*(sum(envir$sizes)-p) - sum(diag(prs)))/(sum(envir$sizes[-i])-p)
        if(model$corstr=="Exchangeable"){
          alpha <- (model$corr[1,2]*(0.5*sum(envir$sizes*(envir$sizes-1))-p)*model$phi -
                      0.5*(sum(prs)-sum(diag(prs))))/((0.5*sum(envir$sizes[-i]*(envir$sizes[-i]-1))-p)*phi)
          R <- matrix(alpha,envir$maxsize,envir$maxsize)
          diag(R) <- rep(1,envir$maxsize)
        }
        else{
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
      datas2 <- datax
      datas2[[i]] <- NULL
      if(!model$linear) envir$Ders <- model.matrix(model)
      resume <- Reduce('+',lapply(datas2,model$score,beta=model$coefficients,out=TRUE))
      kchol <- try(chol(resume[,-1]),silent=TRUE)
      if(is.matrix(kchol)) kchol <- chol2inv(kchol) else kchol <- solve(resume[,-1])
      dfbetas[i,] <- -crossprod(kchol,resume[,1])
    }
  }else{
    dfbetaCO <- function(D){
      beta <- model$coefficients
      p <- length(beta)
      yi <- D[,p+1]
      ni <- length(yi)
      if(model$linear){
        Xi <- matrix(D[,1:p],ncol=p)
        etai <- Xi%*%beta + D[,p+2]
      }else{
        places <- model$ids[D[1,p+5]]==model$id
        Xi <- matrix(Ders[places,],ncol=p)
        etai <- etas[places]
      }
      mui <- model$family$linkinv(etai)
      wi <- sqrt(model$family$variance(mui)/D[,p+3])
      Vi <- t(model$corr[D[,p+4],D[,p+4]]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
      Xiw <- Xi*matrix(model$family$mu.eta(etai),nrow(Xi),p)
      if(level=="clusters" | ni==1){
        if(model$corstr=="Independence") Vi2 <- diag(as.vector(1/wi[D[,p+4]]^2),ni,ni)
        else{Vi2 <- try(chol(Vi),silent=TRUE)
        if(is.matrix(Vi2)) Vi2 <- chol2inv(Vi2) else Vi2 <- solve(Vi)}
        Xiw2 <- crossprod(Vi2,Xiw)
        Hi <-  Xiw%*%tcrossprod(model$naive,Xiw2)
        dfbetaCi <- tcrossprod(model$naive,Xiw2)%*%solve(diag(ni)-Hi)%*%(yi-mui)
        return(dfbetaCi)
      }else{
        dfbetaOij <- matrix(0,ni,p)
        ri <- yi - mui
        for(j in 1:ni){
          a <- Vi[j,-j]%*%solve(Vi[-j,-j])
          Dt <- Xiw[j,] - a%*%Xiw[-j,]
          rt <- ri[j] - a%*%ri[-j]
          vt <- Vi[j,j] - a%*%Vi[-j,j]
          ht <- (Dt%*%tcrossprod(model$naive,Dt))/vt
          dfbetaOij[j,] <- as.numeric(rt/(vt*(1-ht)))*(Dt%*%model$naive)
        }
        return(dfbetaOij)
      }
    }
    dfbetas <- 0;rowsn <- ""
    if(!model$linear){
      etas <- model$linear.predictors
      Ders <- model.matrix(model)
    }
    for(i in 1:length(model$arrangedata)){
      dfbetas <- rbind(dfbetas,matrix(dfbetaCO(model$arrangedata[[i]]),ncol=p))
      if(level=="observations") rowsn <- c(rowsn,paste(model$ids[i],"(",model$arrangedata[[i]][,p+4],")",sep=""))
    }
    dfbetas <- dfbetas[-1,]
  }
  colnames(dfbetas) <- rownames(model$coefficients)
  if(level=="observations") rownames(dfbetas) <- rowsn[-1] else rownames(dfbetas) <- model$ids
  if(!missingArg(coefs)){
    ids <- grep(coefs,colnames(dfbetas),ignore.case=TRUE)
    if(length(ids) > 0){
      nano <- list(...)
      nano$x <- 1:nrow(dfbetas)
      if(is.null(nano$xlab)) nano$xlab <- ifelse(level=="observations","Observation Index","Cluster Index")
      if(is.null(nano$type)) nano$type <- "h"
      if(is.null(nano$ylab)) nano$ylab <- ifelse(level=="observations",expression(hat(beta)-hat(beta)[("- ij")]),expression(hat(beta)-hat(beta)[("- i")]))
      if(is.null(nano$main)) main <- colnames(dfbetas)[ids]
      else main <- matrix(nano$main,length(ids),1)
      oldpar <- par(no.readonly=TRUE)
      on.exit(par(oldpar))
      par(mfrow=c(1,length(ids)))
      for(i in 1:length(ids)){
        nano$y <- dfbetas[,ids[i]]
        nano$main <- main[i]
        do.call("plot",nano)
        if(any(nano$y>0)) abline(h=3*mean(nano$y[nano$y>0]),lty=3)
        if(any(nano$y<0)) abline(h=3*mean(nano$y[nano$y<0]),lty=3)
        if(!missingArg(identify)){
          identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=rownames(dfbetas))
        }
      }
    }else stop(paste("There are no variables with the name",coefs,collapse=""),call.=FALSE)
  }
  return(dfbetas)
}

#' @title Cook's Distance for Generalized Estimating Equations
#' @description Produces an approximation, better known as the \emph{one-step aproximation},
#' of the Cook's distance, which is aimed to measure the effect on the estimates of the parameters in the linear predictor
#' of deleting each cluster/observation in turn. This function also can produce a cluster/observation-index plot of the
#' Cook's distance for all parameters in the linear predictor or for some subset of them (via the argument \code{coefs}).
#' @param model an object of class \emph{glmgee}.
#' @param method an (optional) character string indicating the method of calculation for the \emph{one-step approximation}. The options are: the \emph{one-step approximation} described by Preisser and Qaqish (1996) in which the working-correlation matrix is assumed to be known ("Preisser-Qaqish"); and the "authentic" \emph{one-step approximation} ("full"). As default, \code{method} is set to "Preisser-Qaqish".
#' @param level an (optional) character string indicating the level for which the Cook's distance is required. The options are: cluster-level ("clusters") and observation-level ("observations"). As default, \code{level} is set to "clusters".
#' @param plot.it an (optional) logical indicating if the plot of Cook's distance is required or just the data matrix in which that plot is based. As default, \code{plot.it} is set to \code{FALSE}.
#' @param coefs	an (optional) character string which (partially) match with the names of some of the parameters in the linear predictor.
#' @param identify an (optional) integer indicating the number of clusters to identify on the plot of Cook's distance. This is only appropriate if \code{plot.it=TRUE}.
#' @param varest an (optional) character string indicating the type of estimator which should be used to the variance-covariance matrix of the interest parameters. The available options are: robust sandwich-type estimator ("robust"), degrees-of-freedom-adjusted estimator ("df-adjusted"), bias-corrected estimator ("bias-corrected"), and the model-based or naive estimator ("model"). As default, \code{varest} is set to "robust".
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used to include graphical parameters to customize the plot. For example,  \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix as many rows as clusters/observations in the sample and one column with the values of the Cook's distance.
#' @details The Cook's distance consists of the \emph{distance} between two estimates of the
#' parameters in the linear predictor using a metric based on the (estimate of the) variance-covariance matrix. For the cluster-level,
#' the first one set of estimates is computed from a dataset including all clusters/observations, and the second one is computed from a dataset in which the \emph{i}-th cluster is excluded. To avoid computational burden, the second set of estimates is replaced by its \emph{one-step approximation}. See the \link{dfbeta.glmgee} documentation.
#' @method cooks.distance glmgee
#' @export
#' @examples
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod1 <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod1, id=tree, family=Gamma(log), data=spruces, corstr="AR-M-dependent")
#'
#' ### Cook's distance for all parameters in the linear predictor
#' cooks.distance(fit1, method="full", plot.it=TRUE, col="red", lty=1, lwd=1, cex=0.8,
#'                col.lab="blue", col.axis="blue", col.main="black", family="mono")
#'
#' ### Cook's distance for the parameter associated to the variable 'treat'
#' cooks.distance(fit1, coef="treat", method="full", plot.it=TRUE, col="red", lty=1,
#'                lwd=1, col.lab="blue", col.axis="blue", col.main="black", cex=0.8)
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod2 <- depressd ~ visit + group
#' fit2 <- glmgee(mod2, id=subj, family=binomial(logit), corstr="AR-M-dependent", data=depression)
#'
#' ### Cook's distance for all parameters in the linear predictor
#' cooks.distance(fit2, method="full", plot.it=TRUE, col="red", lty=1, lwd=1, cex=0.8,
#'                col.lab="blue", col.axis="blue", col.main="black", family="mono")
#'
#' ### Cook's distance for the parameter associated to the variable 'group'
#' cooks.distance(fit2, coef="group", method="full", plot.it=TRUE, col="red", lty=1,
#'                lwd=1, col.lab="blue", col.axis="blue", col.main="black", cex=0.8)
#'
#' @references Pregibon D. (1981). Logistic regression diagnostics. \emph{The Annals of Statistics} 9, 705-724.
#' @references Preisser J.S., Qaqish B.F. (1996) Deletion diagnostics for generalised estimating equations.
#' \emph{Biometrika} 83:551–562.
#' @references Hammill B.G., Preisser J.S. (2006) A SAS/IML software program for GEE and regression diagnostics.
#' \emph{Computational Statistics & Data Analysis} 51:1197-1212.
#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
cooks.distance.glmgee <- function(model, method=c("Preisser-Qaqish","full"), level=c("clusters","observations"), plot.it=FALSE, coefs, identify, varest=c("robust","df-adjusted","model","bias-corrected"),...){
  method <- match.arg(method)
  level <- match.arg(level)
  varest <- match.arg(varest)
  dfbetas <- dfbeta(model,method=method,level=level)
  met <- vcov(model,type=varest)
  subst <- NULL
  if(!missingArg(coefs)){
    ids <- grepl(coefs,colnames(dfbetas),ignore.case=TRUE)
    if(sum(ids) > 0){
      subst <- colnames(dfbetas)[ids]
      dfbetas <- as.matrix(dfbetas[,ids])
      met <- as.matrix(met[ids,ids])
    }else stop(paste("There are no variables with the name",coefs,collapse=""),call.=FALSE)
  }
  met2 <- try(chol(met),silent=TRUE)
  if(is.matrix(met2)) met2 <- chol2inv(met2) else met2 <- solve(met)
  CD <- as.matrix(apply((tcrossprod(dfbetas,met2))*dfbetas,1,sum))/ncol(met2)
  colnames(CD) <- "Cook's distance"
  if(plot.it){
    nano <- list(...)
    if(is.null(nano$labels)) labels <- rownames(dfbetas)
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    nano$x <- 1:nrow(dfbetas)
    nano$y <- CD
    if(is.null(nano$xlab)) nano$xlab <- ifelse(level=="clusters","Cluster Index","Observation Index")
    if(is.null(nano$type)) nano$type <- "h"
    if(is.null(nano$ylab)) nano$ylab <- ifelse(level=="clusters",expression((hat(beta)-hat(beta)[{(-~~i)}])^{T}~(Var(hat(beta)))^{-1}~(hat(beta)-hat(beta)[{(-~~i)}])),expression(frac(1,p)(hat(beta)-hat(beta)[{(-~~ij)}])^{T}~(Var(hat(beta)))^{-1}~(hat(beta)-hat(beta)[{(-~~ij)}])))
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

#' @method coef glmgee
#' @export
coef.glmgee <- function(object,...){
  out_ <- object$coefficients
  colnames(out_) <- "Estimates"
  return(out_)
}


#' @method model.matrix glmgee
#' @export
model.matrix.glmgee <-	function(object,...){
  if(object$linear)
  out_ <- model.matrix(object$terms, object$model, contrasts=object$contrasts)
  else{
      X <- model.matrix(object$terms, object$model, contrasts=object$contrasts)
      p <- length(object$coefficients)
      pars <- rownames(object$coefficients)
      etaf <- function(beta){
              temp <- deparse(object$formula[[3]])
              for(i in 1:p) temp <- gsub(pars[i],beta[i],temp)
              matrix(eval(parse(text=temp),data.frame(X)),ncol=1)
      }
      Der <- function(beta) matrix(jacobian(etaf,beta),ncol=p)
      out_ <- Der(object$coefficients)
      colnames(out_) <- pars
  }
  return(out_)
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
  .Theta <- function() return(.Theta)
  environment(.Theta) <- environment(x$family$variance)
  cat("\n        Variance function: ",x$family$family)
  cat("\n                     Link: ",x$family$link)
  cat("\n    Correlation structure: ",ifelse(grepl("M-dependent",x$corstr),paste(x$corstr,"(",attr(x$corstr,"M"),")",sep=""),x$corstr),"\n")
}

#' @title Predictions for Generalized Estimating Equations
#' @description Produces predictions and optionally estimates standard errors of those predictions from a fitted generalized estimating equation.
#' @param object an object of the class \emph{glmgee}.
#' @param newdata	an (optional) \code{data frame} in which to look for variables with which to predict. If omitted, the fitted linear predictors are used.
#' @param type an (optional) character string giving the type of prediction required. The default, "link", is on the scale of the linear predictors, and the alternative, "response", is on the scale of the response variable.
#' @param se.fit	an (optional) logical switch indicating if standard errors are required. As default, \code{se.fit} is set to \code{FALSE}.
#' @param varest an (optional) character string indicating the type of estimator which should be used to the variance-covariance matrix of the interest parameters. The available options are: robust sandwich-type estimator ("robust"), degrees-of-freedom-adjusted estimator ("df-adjusted"), bias-corrected estimator ("bias-corrected"), and the model-based or naive estimator ("model"). As default, \code{varest} is set to "robust".
#' @param ... further arguments passed to or from other methods.
#' @return A matrix with so many rows as \code{newdata} and one column with the predictions. If \code{se.fit=}TRUE then a second column with estimates standard errors is included.
#' @examples
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod1 <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod1, id=tree, family=Gamma(log), data=spruces, corstr="AR-M-dependent")
#' newdata1 <- data.frame(days=c(556,556),treat=as.factor(c("normal","ozone-enriched")))
#' predict(fit1,newdata=newdata1,type="response",se.fit=TRUE)
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod2 <- depressd ~ visit + group
#' fit2 <- glmgee(mod2, id=subj, family=binomial(logit), corstr="AR-M-dependent", data=depression)
#' newdata2 <- data.frame(visit=c(6,6),group=as.factor(c("placebo","oestrogen")))
#' predict(fit2,newdata=newdata2,type="response",se.fit=TRUE)
#'
#' ###### Example 3: Treatment for severe postnatal depression (2)
#' mod3 <- dep ~ visit*group
#' fit3 <- glmgee(mod3, id=subj, family=gaussian(identity), corstr="AR-M-dependent", data=depression)
#' newdata3 <- data.frame(visit=c(6,6),group=as.factor(c("placebo","oestrogen")))
#' predict(fit3,newdata=newdata3,type="response",se.fit=TRUE)
#'
#' @method predict glmgee
#' @export
predict.glmgee <- function(object, ...,newdata, se.fit=FALSE, type=c("link","response"),varest=c("robust","df-adjusted","model","bias-corrected")){
  type <- match.arg(type)
  varest <- match.arg(varest)
  if(missingArg(newdata)){
    predicts <- object$linear.predictors
  }
  else{
    newdata <- data.frame(newdata)
    mf <- model.frame(delete.response(object$terms),newdata,xlev=object$levels)
    X <- model.matrix(delete.response(object$terms),mf,contrasts=object$contrasts)
    if(object$linear) predicts <- X%*%object$coefficients
    else{
      p <- length(object$coefficients)
      pars <- rownames(object$coefficients)
      etaf <- function(beta){
        temp <- deparse(object$formula[[3]])
        for(i in 1:p) temp <- gsub(pars[i],beta[i],temp)
        matrix(eval(parse(text=temp),data.frame(X)),ncol=1)
      }
      predicts <- etaf(object$coefficients)
    }
    offs <- model.offset(mf)
    if(!is.null(offs)) predicts <- predicts + offs
  }
  if(type=="response") predicts <- object$family$linkinv(predicts)
  if(se.fit){
    if(!object$linear) X <- matrix(jacobian(etaf,object$coefficients),nrow=nrow(X))
    varhat <- vcov(object,type=varest)
    se <- matrix(sqrt(apply(tcrossprod(X,varhat)*X,1,sum)),ncol=1)
    if(type=="response") se <- se*abs(object$family$mu.eta(object$family$linkfun(predicts)))
    predicts <- cbind(predicts,se)
    colnames(predicts) <- c("fit","se.fit")
  }else colnames(predicts) <- c("fit")
  return(predicts)
}

#' @title Residuals for Generalized Estimating Equations
#' @description Calculates residuals for a fitted generalized estimating equation.
#' @param object a object of the class \emph{glmgee}.
#' @param type an (optional) character string giving the type of residuals which should be returned. The available options are: (1) "pearson"; (2) "deviance";  (3) the distance between the observed response vector and the fitted mean vector using a metric based on the product between the cluster size and fitted variance-covariance matrix ("mahalanobis"). As default, \code{type} is set to "mahalanobis".
#' @param plot.it an (optional) logical switch indicating if a plot of the residuals is required. As default, \code{plot.it} is set to \code{FALSE}.
#' @param identify an (optional) integer value indicating the number of individuals/clusters to identify on the plot of residuals. This is only appropriate when \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods
#' @return A vector with the observed residuals type \code{type}.
#' @examples
#'
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod1 <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod1, id=tree, family=Gamma(log), data=spruces, corstr="AR-M-dependent")
#' ### Plot to assess the adequacy of the chosen variance function
#' residuals(fit1, type="deviance", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'           col.axis="blue", col.main="black", family="mono", cex=0.8)
#' ### Plot to identify trees suspicious to be outliers
#' residuals(fit1, type="mahalanobis", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'           col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod2 <- depressd ~ visit + group
#' fit2 <- glmgee(mod2, id=subj, family=binomial(logit), corstr="AR-M-dependent", data=depression)
#' ### Plot to identify women suspicious to be outliers
#' residuals(fit2, type="mahalanobis", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'           col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 3: Treatment for severe postnatal depression (2)
#' mod3 <- dep ~ visit*group
#' fit3 <- glmgee(mod3, id=subj, family=gaussian(identity), corstr="AR-M-dependent", data=depression)
#' ### Plot to assess the adequacy of the chosen variance function
#' residuals(fit3, type="pearson", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'           col.axis="blue", col.main="black", family="mono", cex=0.8)
#' ### Plot to identify women suspicious to be outliers
#' residuals(fit3, type="mahalanobis", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'           col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' @method residuals glmgee
#' @export
residuals.glmgee <- function(object,..., type=c("mahalanobis","pearson","deviance"), plot.it=FALSE, identify){
  type <- match.arg(type)
  beta <- object$coefficients
  etas <-
  mhd <- function(D){
    p <- length(beta)
    yi <- D[,p+1]
    ni <- length(yi)
    places <- object$ids[D[1,p+5]]==object$id
    etai <- object$linear.predictors[places]
    mui <- object$family$linkinv(etai)
    wi <- sqrt(object$family$variance(mui)/D[,p+3])
    Vi <- t(object$corr[D[,p+4],D[,p+4]]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
    if(object$corstr=="Independence") Vi2 <- diag(as.vector(1/wi[D[,p+4]]^2),ni,ni)
    else{Vi2 <- try(chol(Vi),silent=TRUE)
    if(is.matrix(Vi2)) Vi2 <- chol2inv(Vi2) else Vi2 <- solve(Vi)}
    crossprod(yi-mui,Vi2%*%(yi-mui))/ni
  }
  res0 <- object$y-object$fitted.values
  res <- switch(type,
                deviance = ifelse(res0 >= 0,1,-1)*sqrt(object$family$dev.resids(object$y,object$fitted.values,object$prior.weights)/object$phi),
                pearson = res0/sqrt(object$phi*object$family$variance(object$fitted.values)/object$prior.weights),
                mahalanobis = as.matrix(unlist(lapply(object$arrangedata,mhd))/object$phi))
  if(plot.it){
    nano <- list(...)
    p <- length(object$coefficients)
    if(type!="mahalanobis"){
      rowsn <- ""
      for(i in 1:length(object$arrangedata)) rowsn <- c(rowsn,paste(object$ids[i],"(",object$arrangedata[[i]][,p+4],")",sep=""))
      nano$x <- object$fitted.values
      nano$y <- res
      if(is.null(nano$ylim)) nano$ylim <- c(min(-3.5,min(res)),max(+3.5,max(res)))
      if(is.null(nano$xlab)) nano$xlab <- "Fitted values"
      if(is.null(nano$ylab)) nano$ylab <- paste(type," - type residual",sep="")
      if(is.null(nano$pch))  nano$pch  <- 20
      if(is.null(nano$labels)) labels <- rowsn[-1]
      else{
        labels <- nano$labels
        nano$labels <- NULL
      }
      do.call("plot",nano)
      abline(h=-3,lty=3)
      abline(h=+3,lty=3)
      if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=labels)
    }else{nano$x <- 1:length(object$ids)
    nano$y <- res
    if(is.null(nano$ylim)) nano$ylim <- c(0,max(3.5,max(res)))
    if(is.null(nano$xlab)) nano$xlab <- "Cluster Index"
    if(is.null(nano$ylab)) nano$ylab <- paste(type," - type residual",sep="")
    if(is.null(nano$type)) nano$type  <- "h"
    if(is.null(nano$labels)) labels <- object$ids
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    do.call("plot",nano)
    abline(h=3*mean(res),lty=3)
    if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=labels)
    }
  }
  colnames(res) <- type
  return(res)
}
#'
#' @title Estimating Equations in Generalized Estimating Equations
#' @description Extracts estimating equations evaluated at the parameter estimates and the observed data for a generalized estimating equation fitted to the data.
#' @param object an object of class \emph{glmgee}.
#' @param ... further arguments passed to or from other methods.
#' @return A vector with the value of the estimating equations evaluated at the parameter estimates and the observed data.
#' @method estequa glmgee
#' @export
#' @examples
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod1 <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod1, id=tree, family=Gamma(log), corstr="AR-M-dependent", data=spruces)
#' estequa(fit1)
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod2 <- depressd ~ visit + group
#' fit2 <- glmgee(mod2, id=subj, family=binomial(logit), corstr="AR-M-dependent", data=depression)
#' estequa(fit2)
#'
#' ###### Example 3: Treatment for severe postnatal depression (2)
#' mod3 <- dep ~ visit*group
#' fit3 <- glmgee(mod3, id=subj, family=gaussian(identity), corstr="AR-M-dependent", data=depression)
#' estequa(fit3)
#'
#' ###### Example 4: Dental Clinical Trial
#' data(rinse)
#' mod4 <- score/3.6 ~ rinse*time
#' fit4 <- glmgee(mod4, family=binomial(log), id=subject, corstr="Exchangeable", data=rinse)
#' estequa(fit4)
#'
#' ###### Example 5: Shoulder Pain after Laparoscopic Cholecystectomy
#' data(cholecystectomy)
#' mod5 <- pain2 ~ treatment + age + time
#' corstr <- "Stationary-M-dependent(2)"
#' fit5 <- glmgee(mod5, family=binomial(logit), id=id, corstr=corstr, data=cholecystectomy)
#' estequa(fit5)
#'
#' ###### Example 6: Guidelines for Urinary Incontinence Discussion and Evaluation
#' data(GUIDE)
#' mod6 <- bothered ~ gender + age + dayacc + severe + toilet
#' fit6 <- glmgee(mod6, family=binomial(logit), id=practice, corstr="Exchangeable", data=GUIDE)
#' estequa(fit6)
#'
#' ###### Example 7: Tests of Auditory Perception in Children with OME
#' OME <- MASS::OME
#' mod7 <- cbind(Correct, Trials-Correct) ~ Loud + Age + OME
#' fit7 <- glmgee(mod7, family = binomial(cloglog), id = ID, corstr = "Exchangeable", data = OME)
#' estequa(fit7)
estequa.glmgee <- function(object,...){
  out_ <- object$estfun/object$phi
  colnames(out_) <- " "
  return(out_)
}
#'
#' @title Comparison of nested Generalized Estimating Equations
#' @description Allows to compare nested generalized estimating equations using the Wald and generalized score tests.
#' @param object an object of the class \emph{glmgee}.
#' @param ... another objects of the class glmgee which are obtained from the fit of generalized estimating equations.
#' @param test an (optional) character string indicating the required test. The available options are: Wald ("wald") and generalized score ("score") tests. As default, \code{test} is set to "wald".
#' @param varest an (optional) character string indicating the type of estimator which should be used to the variance-covariance matrix of the interest parameters in the Wald test. The available options are: robust sandwich-type estimator ("robust"), degrees-of-freedom-adjusted estimator ("df-adjusted"), bias-corrected estimator ("bias-corrected"), and the model-based or naive estimator ("model"). As default, \code{varest} is set to "robust". See \link{vcov.glmgee}.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @return A matrix with three columns which contains the following:
#' \describe{
#' \item{\code{Chi}}{ The value of the statistic of the test.}
#' \item{\code{df}}{ The number of degrees of freedom.}
#' \item{\code{Pr(>Chi)}}{ The \emph{p}-value of the test computed using the Chi-square distribution.}
#' }
#' @references Rotnitzky A., Jewell P. (1990) Hypothesis Testing of Regression Parameters
#' in Semiparametric Generalized Linear Models for Cluster Correlated Data. \emph{Biometrika} 77:485-497.
#' @references Boos D.D. (1992) On Generalized Score Tests. \emph{The American Statistician} 46:327-333.
#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
#' @method anova glmgee
#' @export
#' @examples
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod <- size ~ poly(days,4)
#' fit1 <- glmgee(mod, id=tree, family=Gamma(log), data=spruces, corstr="AR-M-dependent")
#' fit2 <- update(fit1, . ~ . + treat)
#' fit3 <- update(fit2, . ~ . + poly(days,4):treat)
#' anova(fit1,fit2,fit3,test="wald")
#' anova(fit3,test="wald")
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod2 <- depressd ~ group
#' fit1 <- glmgee(mod2, id=subj, family=binomial(logit), corstr="AR-M-dependent", data=depression)
#' fit2 <- update(fit1, . ~ . + visit)
#' fit3 <- update(fit2, . ~ . + group:visit)
#' anova(fit1,fit2,fit3,test="score")
#' anova(fit3,test="score")
#'
#' @references Boos D. (1992) On Generalized Score Tests. \emph{American Statistician} 46:327–33.
#' @references Rotnitzky A., Jewell N.P. (1990). Hypothesis Testing of Regression Parameters in Semiparametric Generalized Linear Models for Cluster Correlated Data. \emph{Biometrika} 77:485-497.
anova.glmgee <- function(object,...,test=c("wald","score"),verbose=TRUE,varest=c("robust","df-adjusted","model","bias-corrected")){
  test <- match.arg(test)
  varest <- match.arg(varest)
  x <- list(object,...)
  if(any(unlist(lapply(x,function(xx) class(xx)[1])!="glmgee")))
    stop("Only glmgee-type objects are supported!!",call.=FALSE)
  if(any(unlist(lapply(x,function(xx) xx$linear!=TRUE))))
    stop("Objects generated by the function gnmgee() are not supported!!",call.=FALSE)
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
      vcovsids <- chol(vcov(x[[i]],type=varest)[ids,ids])
      if(is.matrix(vcovsids)) vcovsids <- chol2inv(vcovsids) else vcovsids <- solve(vcov(x[[i]],type=varest)[ids,ids])
      sc <- crossprod(coef(x[[i]])[ids],vcovsids)%*%coef(x[[i]])[ids]
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
      I0 <- try(chol(resume2[1:p,2:(p+1)]),silent=TRUE)
      if(is.matrix(I0)) I0 <- chol2inv(I0) else I0 <- solve(resume2[1:p,2:(p+1)])
      I1 <- resume2[1:p,(p+2):(2*p+1)]
      vcovs <- I0%*%I1%*%I0
      sc <- crossprod((I0%*%resume2[,1])[ids],chol2inv(chol(vcovs[ids,ids])))%*%((I0%*%resume2[,1])[ids])
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
#' @param ...	one or several objects of the class \emph{glmgee}.
#' @param k an (optional) non-negative value giving the magnitude of the penalty. As default, \code{k} is set to 2.
#' @param u an (optional) logical switch indicating if QIC should be replaced by QICu. As default, \code{u} is set to \code{FALSE}.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param digits an (optional) integer indicating the number of digits to print. As default, \code{digits} is set to \code{max(3, getOption("digits") - 2)}.
#' @return A \code{data.frame} with the values of -2*quasi-likelihood, the number of parameters in the linear predictor, and the value of QIC (or QICu if \code{u}=TRUE) for each \emph{glmgee} object in the input.
#' @seealso \link{CIC}, \link{GHYC}, \link{RJC}, \link{AGPC}, \link{SGPC}
#' @export QIC
#' @examples
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod1 <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod1, id=tree, family=Gamma(log), data=spruces)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Exchangeable")
#' QIC(fit1, fit2, fit3, fit4)
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod2 <- depressd ~ visit + group
#' fit1 <- glmgee(mod2, id=subj, family=binomial(logit), data=depression)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Exchangeable")
#' QIC(fit1, fit2, fit3, fit4)
#'
#' ###### Example 3: Treatment for severe postnatal depression (2)
#' mod3 <- dep ~ visit*group
#' fit1 <- glmgee(mod3, id=subj, family=gaussian(identity), data=depression)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Exchangeable")
#' QIC(fit1, fit2, fit3)
#'
#' @references Pan W. (2001) Akaike's information criterion in generalized estimating equations, \emph{Biometrics} 57:120-125.
#' @references Hin L.-Y., Carey V.J., Wang Y.-G. (2007) Criteria for Working–Correlation–Structure Selection in GEE:
#' Assessment via Simulation. \emph{The American Statistician} 61:360–364.
#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
QIC <- function(...,k=2,u=FALSE,verbose=TRUE,digits=max(3, getOption("digits") - 2)){
  .Theta <- function() return(.Theta)
  x <- list(...)
  if(any(lapply(x,function(xx) class(xx)[1])!="glmgee"))
    stop("Only glmgee-type objects are supported!!",call.=FALSE)
  results <- matrix(NA,length(x),3)
  results2 <- matrix(NA,length(x),5)
  rows <-  matrix(NA,length(x),1)
  call. <- match.call()
  for(i in 1:length(x)){
    results[i,1] <- round(-2*x[[i]]$logLik,digits=digits)
    results[i,2] <- length(coef(x[[i]]))
    results[i,3] <- round(-2*x[[i]]$logLik+ifelse(u,k*length(x[[i]]$coefficients),k*x[[i]]$CIC),digits=3)
    results2[i,1] <- as.character(call.[i+1])
    results2[i,2] <- x[[i]]$family$family
    results2[i,3] <- x[[i]]$family$link
    if(x[[i]]$linear) results2[i,4] <- paste(c(attr(x[[i]]$terms,"intercept"),attr(x[[i]]$terms,"term.labels")),collapse=" + ")
    else results2[i,4] <- paste(deparse(x[[i]]$formula[[3]]),collapse="")
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
    colnames(out_) <- c(c("Object","Variance function","Link function","Predictor","Correlation")[ids],"-2*quasi-likelihood"," Parameters",ifelse(u,"QICu","QIC"))
    if(verbose) print(out_,row.names=FALSE)
    return(invisible(out_))
  }else return(results[,3])
}

#' @title Correlation Information Criterion for Generalized Estimating Equations
#' @description Computes the Correlation Information Criterion (CIC) for one or more objects of the class glmgee.
#' @param ...	one or several objects of the class \emph{glmgee}.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param digits an (optional) integer indicating the number of digits to print. As default, \code{digits} is set to \code{max(3, getOption("digits") - 2)}.
#' @return A \code{data.frame} with the values of the CIC for each \emph{glmgee} object in the input.
#' @export CIC
#' @seealso \link{QIC}, \link{GHYC}, \link{RJC}, \link{AGPC}, \link{SGPC}
#' @examples
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod1 <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod1, id=tree, family=Gamma(log), data=spruces)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Exchangeable")
#' CIC(fit1, fit2, fit3, fit4)
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod2 <- depressd ~ visit + group
#' fit1 <- glmgee(mod2, id=subj, family=binomial(logit), data=depression)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Exchangeable")
#' CIC(fit1, fit2, fit3, fit4)
#'
#' ###### Example 3: Treatment for severe postnatal depression (2)
#' mod3 <- dep ~ visit*group
#' fit1 <- glmgee(mod3, id=subj, family=gaussian(identity), data=depression)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Exchangeable")
#' CIC(fit1, fit2, fit3)
#'
#' @references Hin L.-Y., Wang Y.-G. (2009) Working-Correlation-Structure Identification in Generalized Estimating Equations. \emph{Statistics in Medicine}, 28:642-658.
#' @references Hin L.-Y., Carey V.J., Wang Y.-G. (2007) Criteria for Working–Correlation–Structure Selection in GEE:
#' Assessment via Simulation. \emph{The American Statistician} 61:360–364.
#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
CIC <- function(...,verbose=TRUE,digits=max(3, getOption("digits") - 2)){
  .Theta <- function() return(.Theta)
  x <- list(...)
  if(any(lapply(x,function(xx) class(xx)[1])!="glmgee"))
    stop("Only glmgee-type objects are supported!!",call.=FALSE)
  results <- matrix(NA,length(x),1)
  results2 <- matrix(NA,length(x),5)
  rows <-  matrix(NA,length(x),1)
  call. <- match.call()
  for(i in 1:length(x)){
    results[i,1] <- round(x[[i]]$CIC,digits=digits)
    results2[i,1] <- as.character(call.[i+1])
    results2[i,2] <- x[[i]]$family$family
    results2[i,3] <- x[[i]]$family$link
    if(x[[i]]$linear) results2[i,4] <- paste(c(attr(x[[i]]$terms,"intercept"),attr(x[[i]]$terms,"term.labels")),collapse=" + ")
    else results2[i,4] <- paste(deparse(x[[i]]$formula[[3]]),collapse="")
    results2[i,5] <- ifelse(grepl("M-dependent",x[[i]]$corstr),paste(x[[i]]$corstr,"(",attr(x[[i]]$corstr,"M"),")",sep=""),x[[i]]$corstr)
  }
  if(nrow(results) > 1){
    if(all(results2[,2]==results2[1,2]))
      if(verbose) cat("\n        Variance function: ",results2[1,2],"\n")
    if(all(results2[,3]==results2[1,3]))
      if(verbose) cat("            Link function: ",results2[1,3],"\n")
    if(all(results2[,4]==results2[1,4]))
      if(verbose) cat("                Predictor: ",results2[1,4],"\n")
    if(all(results2[,5]==results2[1,5]))
      if(verbose) cat("    Correlation structure: ",results2[1,5],"\n")
    if(verbose) cat("\n")
    ids <- c(TRUE,!all(results2[,2]==results2[1,2]),!all(results2[,3]==results2[1,3]),!all(results2[,4]==results2[1,4]),!all(results2[,5]==results2[1,5]))
    temp <- as.matrix(results2[,ids])
    out_ <- data.frame(temp,results)
    colnames(out_) <- c(c("Object","Variance function","Link function","Predictor","Correlation")[ids],"CIC")
    if(verbose) print(out_,row.names=FALSE)
    return(invisible(out_))
  }else return(results[,1])
}
#' @title Pardo-Alonso's Criterion for Generalized Estimating Equations
#' @description Computes the Pardo-Alonso's criterion (PAC) for one or more objects of the class glmgee.
#' @param ...	one or several objects of the class \emph{glmgee}.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param digits an (optional) integer indicating the number of digits to print. As default, \code{digits} is set to \code{max(3, getOption("digits") - 2)}.
#' @return A \code{data.frame} with the values of the PAC for each \emph{glmgee} object in the input.
#' @export PAC
#' @references Pardo M.C., Alonso R. (2019) Working correlation structure selection in GEE analysis.
#' \emph{Statistical Papers} 60:1447–1467.
#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
#' @seealso \link{QIC}, \link{CIC}, \link{RJC}, \link{AGPC}, \link{SGPC}, \link{GHYC}
#' @examples
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod1 <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod1, id=tree, family=Gamma(log), data=spruces)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Exchangeable")
#' PAC(fit1, fit2, fit3, fit4)
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod2 <- depressd ~ visit + group
#' fit1 <- glmgee(mod2, id=subj, family=binomial(logit), data=depression)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Exchangeable")
#' PAC(fit1, fit2, fit3, fit4)
#'
#' ###### Example 3: Treatment for severe postnatal depression (2)
#' mod3 <- dep ~ visit*group
#' fit1 <- glmgee(mod3, id=subj, family=gaussian(identity), data=depression)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Exchangeable")
#' PAC(fit1, fit2, fit3)
#'
PAC <- function(...,verbose=TRUE,digits=max(3, getOption("digits") - 2)){
  .Theta <- function() return(.Theta)
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
      yi <- D[,p+1]
      ni <- length(yi)
      places <- model$ids[D[1,p+5]]==model$id
      etai <- model$linear.predictors[places]
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
    cr <- det(resume[,(maxsize+1):(2*maxsize)]/resume[,(2*maxsize+1):(3*maxsize)])/det(resume[,1:maxsize]/resume[,(2*maxsize+1):(3*maxsize)])
    cr <- abs(cr - 1)
    return(cr)
  }
  for(i in 1:length(x)){
    results[i,1] <- round(Gosho(x[[i]]),digits=digits)
    results2[i,1] <- as.character(call.[i+1])
    results2[i,2] <- x[[i]]$family$family
    results2[i,3] <- x[[i]]$family$link
    if(x[[i]]$linear) results2[i,4] <- paste(c(attr(x[[i]]$terms,"intercept"),attr(x[[i]]$terms,"term.labels")),collapse=" + ")
    else results2[i,4] <- paste(deparse(x[[i]]$formula[[3]]),collapse="")
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
    colnames(out_) <- c(c("Object","Variance function","Link function","Predictor","Correlation")[ids],"PAC")
    if(verbose) print(out_,row.names=FALSE)
    return(invisible(out_))
  }else return(results[,1])
}



#' @title Gosho-Hamada-Yoshimura's Criterion for Generalized Estimating Equations
#' @description Computes the Gosho-Hamada-Yoshimura's criterion (GHYC) for one or more objects of the class glmgee.
#' @param ...	one or several objects of the class \emph{glmgee}.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param digits an (optional) integer indicating the number of digits to print. As default, \code{digits} is set to \code{max(3, getOption("digits") - 2)}.
#' @return A \code{data.frame} with the values of the GHYC for each \emph{glmgee} object in the input.
#' @export GHYC
#' @references Gosho M., Hamada C., Yoshimura I. (2011) Criterion for the Selection
#' of a Working Correlation Structure in the Generalized Estimating Equation Approach for
#' Longitudinal Balanced Data. \emph{Communications in Statistics — Theory and Methods} 40:3839-3856.
#' @references Gosho M. (2014) Criteria to Select a Working Correlation Structure in SAS.
#' \emph{Journal of Statistical Software, Code Snippets} 57:1548-7660.#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
#' @seealso \link{QIC}, \link{CIC}, \link{RJC}, \link{AGPC}, \link{SGPC}
#' @examples
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod1 <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod1, id=tree, family=Gamma(log), data=spruces)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Exchangeable")
#' GHYC(fit1, fit2, fit3, fit4)
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod2 <- depressd ~ visit + group
#' fit1 <- glmgee(mod2, id=subj, family=binomial(logit), data=depression)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Exchangeable")
#' GHYC(fit1, fit2, fit3, fit4)
#'
#' ###### Example 3: Treatment for severe postnatal depression (2)
#' mod3 <- dep ~ visit*group
#' fit1 <- glmgee(mod3, id=subj, family=gaussian(identity), data=depression)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Exchangeable")
#' GHYC(fit1, fit2, fit3)
#'
GHYC <- function(...,verbose=TRUE,digits=max(3, getOption("digits") - 2)){
  .Theta <- function() return(.Theta)
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
      yi <- D[,p+1]
      ni <- length(yi)
      places <- model$ids[D[1,p+5]]==model$id
      etai <- model$linear.predictors[places]
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
    results[i,1] <- round(Gosho(x[[i]]),digits=digits)
    results2[i,1] <- as.character(call.[i+1])
    results2[i,2] <- x[[i]]$family$family
    results2[i,3] <- x[[i]]$family$link
    if(x[[i]]$linear) results2[i,4] <- paste(c(attr(x[[i]]$terms,"intercept"),attr(x[[i]]$terms,"term.labels")),collapse=" + ")
    else results2[i,4] <- paste(deparse(x[[i]]$formula[[3]]),collapse="")
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
    colnames(out_) <- c(c("Object","Variance function","Link function","Predictor","Correlation")[ids],"GHYC")
    if(verbose) print(out_,row.names=FALSE)
    return(invisible(out_))
  }else return(results[,1])
}

#' @title Rotnitzky–Jewell's Criterion for Generalized Estimating Equations
#' @description Computes the Rotnitzky–Jewell's criterion (RJC) for one or more objects of the class glmgee.
#' @param ...	one or several objects of the class \emph{glmgee}.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param digits an (optional) integer indicating the number of digits to print. As default, \code{digits} is set to \code{max(3, getOption("digits") - 2)}.
#' @return A \code{data.frame} with the values of the RJC for each \emph{glmgee} object in the input.
#' @export RJC
#' @seealso \link{QIC}, \link{CIC}, \link{GHYC}, \link{AGPC}, \link{SGPC}
#' @references Hin L.-Y., Carey V.J., Wang Y.-G. (2007) Criteria for Working–Correlation–Structure
#' Selection in GEE: Assessment via Simulation. \emph{The American Statistician} 61:360-364.
#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
#' @examples
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod1 <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod1, id=tree, family=Gamma(log), data=spruces)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Exchangeable")
#' RJC(fit1, fit2, fit3, fit4)
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod2 <- depressd ~ visit + group
#' fit1 <- glmgee(mod2, id=subj, family=binomial(logit), data=depression)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Exchangeable")
#' RJC(fit1, fit2, fit3, fit4)
#'
#' ###### Example 3: Treatment for severe postnatal depression (2)
#' mod3 <- dep ~ visit*group
#' fit1 <- glmgee(mod3, id=subj, family=gaussian(identity), data=depression)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Exchangeable")
#' RJC(fit1, fit2, fit3)
#'
RJC <- function(...,verbose=TRUE,digits=max(3, getOption("digits") - 2)){
  .Theta <- function() return(.Theta)
  x <- list(...)
  if(any(lapply(x,function(xx) class(xx)[1])!="glmgee"))
    stop("Only glmgee-type objects are supported!!",call.=FALSE)
  results <- matrix(NA,length(x),1)
  results2 <- matrix(NA,length(x),5)
  rows <-  matrix(NA,length(x),1)
  call. <- match.call()
  for(i in 1:length(x)){
    results[i,1] <- round(x[[i]]$RJC,digits=digits)
    results2[i,1] <- as.character(call.[i+1])
    results2[i,2] <- x[[i]]$family$family
    results2[i,3] <- x[[i]]$family$link
    if(x[[i]]$linear) results2[i,4] <- paste(c(attr(x[[i]]$terms,"intercept"),attr(x[[i]]$terms,"term.labels")),collapse=" + ")
    else results2[i,4] <- paste(deparse(x[[i]]$formula[[3]]),collapse="")
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
    colnames(out_) <- c(c("Object","Variance function","Link function","Predictor","Correlation")[ids],"RJC")
    if(verbose) print(out_,row.names=FALSE)
    return(invisible(out_))
  }else return(results[,1])
}


#' @title Variable selection in Generalized Estimating Equations
#' @description Performs variable selection in generalized estimating equations using hybrid versions of forward stepwise
#' and backward stepwise.
#' @param model an object of the class \code{glmgee} which is obtained from the fit of a generalized estimating equation.
#' @param direction an (optional) character string indicating the type of procedure which should be used. The available
#' options are: hybrid backward stepwise ("backward") and hybrid forward stepwise ("forward"). As default, \code{direction}
#' is set to "forward".
#' @param levels an (optional) two-dimensional vector of values in the interval \eqn{(0,1)} indicating the levels at which
#' the variables should in and out from the model. This is only appropiate if \code{criterion}="p-value". By default,
#' \code{levels} is set to \code{c(0.05,0.05)}.
#' @param test an (optional) character string indicating the statistical test which should be used to compare nested
#' models. The available options are: Wald ("wald") and generalized score ("score") tests. As default, \code{test} is
#' set to "wald".
#' @param criterion an (optional) character string indicating the criterion which should be used to compare the candidate
#' models. The available options are: QIC ("qic"), QICu ("qicu"), Akaike-type penalized gaussian pseudo-likelihood criterion ("agpc"),
#' Schwarz-type penalized gaussian pseudo-likelihood criterion ("sgpc") and \emph{p}-value of the \code{test} test ("p-value"). By
#' default, \code{criterion} is set to "p-value".
#' @param ...	further arguments passed to or from other methods. For example, \code{k}, that is, the magnitude of the
#' penalty in the AGPC, which by default is set to 2.
#' @param trace an (optional) logical switch indicating if should the stepwise reports be printed. By default,
#' \code{trace} is set to TRUE.
#' @param digits an (optional) integer indicating the number of digits which should be used to print the most of the
#' criteria to compare the candidate models. As default, \code{digits} is set to \code{max(3, getOption("digits") - 2)}.
#' @param varest an (optional) character string indicating the type of estimator which should be used to the variance-covariance matrix of the interest parameters in the Wald-type test. The available options are: robust sandwich-type estimator ("robust"), degrees-of-freedom-adjusted estimator ("df-adjusted"), bias-corrected estimator ("bias-corrected"), and the model-based or naive estimator ("model"). As default, \code{varest} is set to "robust".
#' @param scope an (optional) list, containing components \code{lower} and \code{upper}, both formula-type objects,
#' indicating the range of models which should be examined in the stepwise search. As default, \code{lower} is a model
#' with no predictors and \code{upper} is the linear predictor of the model in \code{model}.
#' @param force.in an (optional) formula-type object indicating the effects that should be in all models
#' @param force.out an (optional) formula-type object indicating the effects that should be in no models
#' @return A list which contains the following objects:
#' \describe{
#' \item{\code{initial}}{ a character string indicating the linear predictor of the "initial model".}
#' \item{\code{direction}}{ a character string indicating the type of procedure which was used.}
#' \item{\code{criterion}}{ a character string indicating the criterion used to compare the candidate models.}
#' \item{\code{final}}{ a character string indicating the linear predictor of the "final model".}
#' \item{\code{final.fit}}{ an object of class \code{glmgee} with the results of the fit to the data of the "final model".}'
#' }
#' @seealso \link{stepCriterion.lm}, \link{stepCriterion.glm}, \link{stepCriterion.overglm}
#' @examples
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod <- size ~ poly(days,4)*treat
#' fit1 <- glmgee(mod, id=tree, family=Gamma(log), data=spruces, corstr="AR-M-dependent")
#' stepCriterion(fit1, criterion="p-value", direction="forward", scope=list(upper=mod),force.in=~treat)
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod <- depressd ~ visit*group
#' fit2 <- glmgee(mod, id=subj, family=binomial(probit), corstr="AR-M-dependent", data=depression)
#' stepCriterion(fit2, criterion="agpc", direction="forward", scope=list(upper=mod),force.in=~group)
#'
#' ###### Example 3: Treatment for severe postnatal depression (2)
#' mod <- dep ~ visit*group
#' fit2 <- glmgee(mod, id=subj, family=gaussian(identity), corstr="AR-M-dependent", data=depression)
#' stepCriterion(fit2, criterion="sgpc", direction="forward", scope=list(upper=mod),force.in=~group)
#'
#' @method stepCriterion glmgee
#' @export
#' @references James G., Witten D., Hastie T., Tibshirani R. (2013, page 210) \emph{An Introduction to Statistical Learning
#' with Applications in R}. Springer, New York.
#' @references Jianwen X., Jiamao Z., Liya F. (2019) Variable selection in generalized estimating equations via empirical
#' likelihood and Gaussian pseudo-likelihood. \emph{Communications in Statistics - Simulation and Computation} 48:1239-1250.
#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
stepCriterion.glmgee <- function(model, criterion=c("p-value","qic","qicu","agpc","sgpc"), test=c("wald","score"), direction=c("forward","backward"), levels=c(0.05,0.05), trace=TRUE, scope, digits=max(3, getOption("digits") - 2),varest=c("robust","df-adjusted","model","bias-corrected"), force.in, force.out,...){
  .Theta <- function() return(.Theta)
  if(!model$linear)
    stop("Objects generated by the function gnmgee() are not supported!!",call.=FALSE)
  xxx <- list(...)
  if(is.null(xxx$k)) k <- 2 else k <- xxx$k
  if(!missingArg(criterion)) criterion <- tolower(criterion)
  criterion <- match.arg(criterion)
  direction <- match.arg(direction)
  test <- match.arg(test)
  if(test=="wald") test2 <- "Wald test"
  if(test=="score") test2 <- "generalized score test"

  glmgeestats <- function(fitnow){
    qic <- -2*fitnow$logLik + 2*fitnow$CIC
    qicu <- -2*fitnow$logLik + 2*length(fitnow$coefficients)
    return(c(qic,qicu,AGPC(fitnow,k=k),SGPC(fitnow)))
  }

  criters <- c("qic","qicu","agpc","sgpc","p-value")
  criters2 <- c("QIC","QICu","AGPC","SGPC","P(Chisq>)(*)")
  sentido <- c(1,1,1,1,1)
  ids <- criters == criterion
  corstr <- ifelse(grepl("M-dependent",model$corstr),paste(model$corstr,"(",attr(model$corstr,"M"),")",sep=""),model$corstr)
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
  if(trace){
    cat("\n    Variance function: ",model$family$family)
    cat("\n        Link function: ",model$family$link,"\n")
    cat("Correlation structure: ",corstr,"\n")
    cat(" Comparison criterion: ",criters2[ids])
  }

  if(direction=="forward"){
    oldformula <- lower
    if(trace){
      cat("\n\nInitial model:\n")
      cat(paste("~",as.character(oldformula)[length(oldformula)],sep=" "),"\n\n")
      cat("\nStep",0,":\n")
    }
    out_ <- list(initial=paste("~",as.character(oldformula)[length(oldformula)],sep=" "),direction=direction,criterion=criters2[criters==criterion])
    while(tol){
      oldformula <-  update(oldformula,paste(as.character(eval(model$call$formula))[2],"~ ."))
      fit.x <- update(model,formula=oldformula,adjr2=FALSE,start=NULL)
      none <- c(NA,glmgeestats(fit.x),NA)
      S <- unlist(lapply(strsplit(attr(terms(oldformula),"term.labels"),":"),function(x) paste(sort(x),collapse =":")))
      entran <- seq(1:length(U))[is.na(match(U,S))]
      salen <- seq(1:length(U))[!is.na(match(U,S))]
      mas <- TRUE

      fsalen <- matrix(NA,length(salen),6)
      if(length(salen) > 0){
        nombres <- matrix("",length(salen),1)
        for(i in 1:length(salen)){
          salida <- apply(as.matrix(fs[,salen[i]]*fs[,-c(entran,salen[i])]),2,sum)
          if(all(salida < sum(fs[,salen[i]])) & U[salen[i]]!=cambio & !(U[salen[i]] %in% force.in)){
            newformula <- update(oldformula, paste("~ . -",U[salen[i]]))
            fit.0 <- update(model,formula=newformula,start=NULL)
            fsalen[i,1] <- fit.0$df.residual - fit.x$df.residual
            fsalen[i,2:5] <- glmgeestats(fit.0)
            fsalen[i,6] <- anova(fit.0,fit.x,test=test,verbose=FALSE,varest=varest)[1,1]
            fsalen[i,6] <- sqrt(9*fsalen[i,1]/2)*((fsalen[i,6]/fsalen[i,1])^(1/3) - 1 + 2/(9*fsalen[i,1]))
            nombres[i] <- U[salen[i]]
          }
        }
        rownames(fsalen) <- paste("-",nombres)
        if(criterion=="p-value" & any(!is.na(fsalen))){
          colnames(fsalen) <- c("df",criters2)
          if(nrow(fsalen) > 1){
            fsalen <- fsalen[order(fsalen[,6]),]
            fsalen <- na.omit(fsalen)
            attr(fsalen,"na.action")	<- attr(fsalen,"class") <- NULL
          }
          fsalen[,6] <- 1-pchisq((sqrt(2/(9*fsalen[,1]))*fsalen[,6] + 1 - 2/(9*fsalen[,1]))^3*fsalen[,1],fsalen[,1])
          if(fsalen[1,6] > levels[2]){
            fsalen <- rbind(fsalen,none)
            rownames(fsalen)[nrow(fsalen)] <- nonename
            if(trace){
              printCoefmat(fsalen,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=c(2,3,4,5),
                           signif.stars=FALSE,tst.ind=4,dig.tst=4,digits=digits)
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
        fentran <- matrix(NA,length(entran),6)
        nombres <- matrix("",length(entran),1)
        for(i in 1:length(entran)){
          salida <- apply(as.matrix(fs[,-c(salen,entran[i])]),2,function(x) sum(fs[,entran[i]]*x)!=sum(x))
          if(all(salida) & U[entran[i]]!=cambio & !(U[entran[i]] %in% force.out)){
            newformula <- update(oldformula, paste("~ . +",U[entran[i]]))
            fit.0 <- update(model,formula=newformula,start=NULL)
            fentran[i,1] <- fit.x$df.residual - fit.0$df.residual
            fentran[i,2:5] <- glmgeestats(fit.0)
            fentran[i,6] <- anova(fit.x,fit.0,test=test,verbose=FALSE,varest=varest)[1,1]
            fentran[i,6] <- sqrt(9*fentran[i,1]/2)*((fentran[i,6]/fentran[i,1])^(1/3) - 1 + 2/(9*fentran[i,1]))
            nombres[i] <- U[entran[i]]
          }
        }
        rownames(fentran) <- paste("+",nombres)
        if(criterion=="p-value"){
          colnames(fentran) <- c("df",criters2)
          if(nrow(fentran) > 1){
            fentran <- fentran[order(-fentran[,6]),]
            fentran <- na.omit(fentran)
            attr(fentran,"na.action")	<- attr(fentran,"class") <- NULL
          }
          fentran[,6] <- 1-pchisq((sqrt(2/(9*fentran[,1]))*fentran[,6] + 1 - 2/(9*fentran[,1]))^3*fentran[,1],fentran[,1])
          fentran <- rbind(fentran,none)
          rownames(fentran)[nrow(fentran)] <- nonename
          if(trace) printCoefmat(fentran,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=c(2,3,4,5),
                                 signif.stars=FALSE,tst.ind=4,dig.tst=4,digits=digits)
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
        fentran[,6] <- 1-pchisq((sqrt(2/(9*fentran[,1]))*fentran[,6] + 1 - 2/(9*fentran[,1]))^3*fentran[,1],fentran[,1])
        fentran <- rbind(fentran,c(0,none[-c(1,6)],0))
        rownames(fentran)[nrow(fentran)] <- nonename
        colnames(fentran) <- c("df",criters2)
        fentran <- na.omit(fentran)
        attr(fentran,"na.action")	<- attr(fentran,"class") <- NULL
        fentran[nrow(fentran),c(1,6)] <- NA
        fentran <- fentran[order(sentido[ids]*fentran[,c(FALSE,ids)]),]
        if(rownames(fentran)[1]!=nonename){
          if(trace){
            printCoefmat(fentran,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=c(2,3,4,5),
                         signif.stars=FALSE,tst.ind=4,dig.tst=4,digits=digits)
            cat("\nStep",paso,":",rownames(fentran)[1],"\n\n")
          }
          paso <- paso + 1
          cambio <- substring(rownames(fentran)[1],3)
          oldformula <- update(oldformula, paste("~ .",rownames(fentran)[1]))
        }else{
          tol <- FALSE
          if(trace) printCoefmat(fentran,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=c(2,3,4,5),
                                 signif.stars=FALSE,tst.ind=4,dig.tst=4,digits=digits)
        }
      }
      if(length(entran) == 0 & mas) tol <- FALSE
    }
  }
  if(direction=="backward"){
    oldformula <- upper
    if(trace){
      cat("\n\nInitial model:\n")
      cat(paste("~",as.character(oldformula)[length(oldformula)],sep=" "),"\n\n")
      cat("\nStep",0,":\n")
    }
    out_ <- list(initial=paste("~",as.character(oldformula)[length(oldformula)],sep=" "),direction=direction,criterion=criters2[criters==criterion])
    while(tol){
      oldformula <-  update(oldformula,paste(as.character(eval(model$call$formula))[2],"~ ."))
      fit.x <- update(model,formula=oldformula,adjr2=FALSE,start=NULL)
      none <- c(NA,glmgeestats(fit.x),NA)
      S <- unlist(lapply(strsplit(attr(terms(oldformula),"term.labels"),":"),function(x) paste(sort(x),collapse =":")))
      entran <- seq(1:length(U))[is.na(match(U,S))]
      salen <- seq(1:length(U))[!is.na(match(U,S))]
      menos <- TRUE

      fentran <- matrix(NA,length(entran),6)
      if(length(entran) > 0){
        nombres <- matrix("",length(entran),1)
        for(i in 1:length(entran)){
          salida <- apply(as.matrix(fs[,-c(salen,entran[i])]),2,function(x) sum(fs[,entran[i]]*x)!=sum(x))
          if(all(salida) & U[entran[i]]!=cambio & !(U[entran[i]] %in% force.out)){
            newformula <- update(oldformula, paste("~ . +",U[entran[i]]))
            fit.0 <- update(model,formula=newformula,start=NULL)
            fentran[i,1] <- fit.x$df.residual - fit.0$df.residual
            fentran[i,2:5] <- glmgeestats(fit.0)
            fentran[i,6] <- anova(fit.x,fit.0,test=test,verbose=FALSE,varest=varest)[1,1]
            fentran[i,6] <- sqrt(9*fentran[i,1]/2)*((fentran[i,6]/fentran[i,1])^(1/3) - 1 + 2/(9*fentran[i,1]))
            nombres[i] <- U[entran[i]]
          }
        }
        rownames(fentran) <- paste("+",nombres)
        if(criterion=="p-value" & any(!is.na(fentran))){
          colnames(fentran) <- c("df",criters2)
          if(nrow(fentran) > 1){
            fentran <- fentran[order(-fentran[,6]),]
            fentran <- na.omit(fentran)
            attr(fentran,"na.action")	<- attr(fentran,"class") <- NULL
          }
          fentran[,6] <- 1-pchisq((sqrt(2/(9*fentran[,1]))*fentran[,6] + 1 - 2/(9*fentran[,1]))^3*fentran[,1],fentran[,1])
          if(fentran[1,6] < levels[1]){
            fentran <- rbind(fentran,none)
            rownames(fentran)[nrow(fentran)] <- nonename
            if(trace){
              printCoefmat(fentran,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=c(2,3,4,5),
                           signif.stars=FALSE,tst.ind=4,dig.tst=4,digits=digits)
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
        fsalen <- matrix(NA,length(salen),6)
        nombres <- matrix("",length(salen),1)
        for(i in 1:length(salen)){
          salida <- apply(as.matrix(fs[,salen[i]]*fs[,-c(entran,salen[i])]),2,sum)
          if(all(salida < sum(fs[,salen[i]])) & !(U[salen[i]] %in% force.in)){
            newformula <- update(oldformula, paste("~ . -",U[salen[i]]))
            fit.0 <- update(model,formula=newformula,start=NULL)
            fsalen[i,1] <- fit.0$df.residual - fit.x$df.residual
            fsalen[i,2:5] <- glmgeestats(fit.0)
            fsalen[i,6] <- anova(fit.0,fit.x,test=test,verbose=FALSE,varest=varest)[1,1]
            fsalen[i,6] <- sqrt(9*fsalen[i,1]/2)*((fsalen[i,6]/fsalen[i,1])^(1/3) - 1 + 2/(9*fsalen[i,1]))
            nombres[i] <- U[salen[i]]
          }
        }
        rownames(fsalen) <- paste("-",nombres)
        if(criterion=="p-value"){
          colnames(fsalen) <- c("df",criters2)
          if(nrow(fsalen) > 1){
            fsalen <- fsalen[order(fsalen[,6]),]
            fsalen <- na.omit(fsalen)
            attr(fsalen,"na.action")	<- attr(fsalen,"class") <- NULL
          }
          fsalen[,6] <- 1-pchisq((sqrt(2/(9*fsalen[,1]))*fsalen[,6] + 1 - 2/(9*fsalen[,1]))^3*fsalen[,1],fsalen[,1])
          fsalen <- rbind(fsalen,none)
          rownames(fsalen)[nrow(fsalen)] <- nonename
          if(trace) printCoefmat(fsalen,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=c(2,3,4,5),
                                 signif.stars=FALSE,tst.ind=4,dig.tst=4,digits=digits)
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
        fsalen[,6] <- 1-pchisq((sqrt(2/(9*fsalen[,1]))*fsalen[,6] + 1 - 2/(9*fsalen[,1]))^3*fsalen[,1],fsalen[,1])
        fsalen <- rbind(fsalen,c(0,none[-c(1,6)],0))
        rownames(fsalen)[nrow(fsalen)] <- nonename
        colnames(fsalen) <- c("df",criters2)
        fsalen <- na.omit(fsalen)
        attr(fsalen,"na.action")	<- attr(fsalen,"class") <- NULL
        fsalen[nrow(fsalen),c(1,6)] <- NA
        fsalen <- fsalen[order(sentido[ids]*fsalen[,c(FALSE,ids)]),]
        if(rownames(fsalen)[1]!=nonename){
          if(trace){
            printCoefmat(fsalen,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=c(2,3,4,5),
                         signif.stars=FALSE,tst.ind=4,dig.tst=4,digits=digits)
            cat("\nStep",paso,":",rownames(fsalen)[1],"\n\n")
          }
          paso <- paso + 1
          cambio <- substring(rownames(fsalen)[1],3)
          oldformula <- update(oldformula, paste("~ .",rownames(fsalen)[1]))
        }else{
          tol <- FALSE
          if(trace) printCoefmat(fsalen,P.values=TRUE,has.Pvalue=TRUE,na.print="",cs.ind=c(2,3,4,5),
                                 signif.stars=FALSE,tst.ind=4,dig.tst=4,digits=digits)
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
      cat("\n Effects are added when their p-values are lower than",levels[1])
      cat("\n Effects are excluded when their p-values are higher than",levels[2])
    }
    if(!is.null(xxx$k)) cat("The magnitude of the penalty in the AGPC was set to ",xxx$k)
    cat("\n")
  }
  out_$final <- paste("~",as.character(oldformula)[length(oldformula)],sep=" ")
  if(!trace){
    final.fit <- update(model,formula=oldformula,adjr2=FALSE,start=NULL)
    out_$final.fit <- final.fit
  }
  return(invisible(out_))
}



#' @title AGPC for Generalized Estimating Equations
#' @description Computes the Akaike-type penalized Gaussian pseudo-likelihood criterion (AGPC) for one or more objects of the class glmgee.
#' @param ...	one or several objects of the class \emph{glmgee}.
#' @param k an (optional) non-negative value giving the magnitude of the penalty. As default, \code{k} is set to 2.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param digits an (optional) integer indicating the number of digits to print. As default, \code{digits} is set to \code{max(3, getOption("digits") - 2)}.
#' @return A \code{data.frame} with the values of the gaussian pseudo-likelihood, the number of parameters in the linear predictor plus the number of parameters in the correlation matrix, and the value of AGPC for each \emph{glmgee} object in the input.
#' @details If \code{k} is set to 0 then the AGPC reduces to the Gaussian pseudo-likelihood criterion (GPC), proposed by Carey and Wang (2011), which corresponds to the logarithm of the multivariate normal density function.
#' @export AGPC
#' @seealso \link{QIC}, \link{CIC}, \link{RJC}, \link{GHYC}, \link{SGPC}
#' @references Carey V.J., Wang Y.-G. (2011) Working covariance model selection for
#' generalized estimating equations. \emph{Statistics in Medicine} 30:3117-3124.
#' @references Zhu X., Zhu Z. (2013) Comparison of Criteria to Select Working Correlation
#' Matrix in Generalized Estimating Equations. \emph{Chinese Journal of Applied Probability
#' and Statistics} 29:515-530.
#' @references Fu L., Hao Y., Wang Y.-G. (2018) Working correlation structure
#' selection in generalized estimating equations. \emph{Computational Statistics} 33:983-996.
#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
#' @examples
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod1 <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod1, id=tree, family=Gamma(log), data=spruces)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Exchangeable")
#' AGPC(fit1, fit2, fit3, fit4)
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod2 <- depressd ~ visit + group
#' fit1 <- glmgee(mod2, id=subj, family=binomial(logit), data=depression)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Exchangeable")
#' AGPC(fit1, fit2, fit3, fit4)
#'
#' ###### Example 3: Treatment for severe postnatal depression (2)
#' mod3 <- dep ~ visit*group
#' fit1 <- glmgee(mod3, id=subj, family=gaussian(identity), data=depression)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Exchangeable")
#' AGPC(fit1, fit2, fit3)
#'
AGPC <- function(...,k=2,verbose=TRUE,digits=max(3, getOption("digits") - 2)){
  .Theta <- function() return(.Theta)
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
      Xi <- matrix(D[,1:p],ncol=p)
      yi <- D[,p+1]
      ni <- nrow(Xi)
      places <- model$ids[D[1,p+5]]==model$id
      etai <- model$linear.predictors[places]
      mui <- family$linkinv(etai)
      wi <- sqrt(family$variance(mui)/D[,p+3])
      Vi <- phi*t(model$corr[D[,p+4],D[,p+4]]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
      Vi2 <- try(chol(Vi),silent=TRUE)
      if(is.matrix(Vi2)) Vi2 <- chol2inv(Vi2) else Vi2 <- solve(Vi)
      return(ni*log(2*pi) + t(yi - mui)%*%Vi2%*%(yi - mui) + log(det(Vi)))
    }
    resume <- Reduce('+',lapply(model$arrangedata,VSi))
    q <- switch (model$corstr,
                 "AR-M-dependent" = attr(model$corstr,"M"),
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
    results[i,1] <- round(temporal$cr0,digits=digits)
    results[i,2] <- temporal$cr2
    results[i,3] <- round(temporal$cr1,digits=digits)
    results2[i,1] <- as.character(call.[i+1])
    results2[i,2] <- x[[i]]$family$family
    results2[i,3] <- x[[i]]$family$link
    if(x[[i]]$linear) results2[i,4] <- paste(c(attr(x[[i]]$terms,"intercept"),attr(x[[i]]$terms,"term.labels")),collapse=" + ")
    else results2[i,4] <- paste(deparse(x[[i]]$formula[[3]]),collapse="")
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
    colnames(out_) <- c(c("Object","Variance function","Link function","Predictor","Correlation")[ids],"GPL","Parameters","AGPC")
    if(verbose) print(out_,row.names=FALSE)
    return(invisible(out_))
  }else return(results[,3])
}

#' @title SGPC for Generalized Estimating Equations
#' @description Computes the Schwarz-type penalized Gaussian pseudo-likelihood criterion (SGPC) for one or more objects of the class glmgee.
#' @param ...	one or several objects of the class \emph{glmgee}.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param digits an (optional) integer indicating the number of digits to print. As default, \code{digits} is set to \code{max(3, getOption("digits") - 2)}.
#' @return A \code{data.frame} with the values of the gaussian pseudo-likelihood, the number of parameters in the linear predictor plus the number of parameters in the correlation matrix, and the value of SGPC for each \emph{glmgee} object in the input.
#' @export SGPC
#' @seealso \link{QIC}, \link{CIC}, \link{RJC}, \link{GHYC}, \link{AGPC}
#' @references Carey V.J., Wang Y.-G. (2011) Working covariance model selection for
#' generalized estimating equations. \emph{Statistics in Medicine} 30:3117-3124.
#' @references Zhu X., Zhu Z. (2013) Comparison of Criteria to Select Working Correlation
#' Matrix in Generalized Estimating Equations. \emph{Chinese Journal of Applied Probability
#' and Statistics} 29:515-530.
#' @references Fu L., Hao Y., Wang Y.-G. (2018) Working correlation structure
#' selection in generalized estimating equations. \emph{Computational Statistics} 33:983-996.
#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
#' @examples
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod1 <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod1, id=tree, family=Gamma(log), data=spruces)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Exchangeable")
#' SGPC(fit1, fit2, fit3, fit4)
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod2 <- depressd ~ visit + group
#' fit1 <- glmgee(mod2, id=subj, family=binomial(logit), data=depression)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Stationary-M-dependent(2)")
#' fit4 <- update(fit1, corstr="Exchangeable")
#' SGPC(fit1, fit2, fit3, fit4)
#'
#' ###### Example 3: Treatment for severe postnatal depression (2)
#' mod3 <- dep ~ visit*group
#' fit1 <- glmgee(mod3, id=subj, family=gaussian(identity), data=depression)
#' fit2 <- update(fit1, corstr="AR-M-dependent")
#' fit3 <- update(fit1, corstr="Exchangeable")
#' SGPC(fit1, fit2, fit3)
#'
SGPC <- function(...,verbose=TRUE,digits=max(3, getOption("digits") - 2)){
  .Theta <- function() return(.Theta)
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
      Xi <- matrix(D[,1:p],ncol=p)
      yi <- D[,p+1]
      ni <- nrow(Xi)
      places <- model$ids[D[1,p+5]]==model$id
      etai <- model$linear.predictors[places]
      mui <- family$linkinv(etai)
      wi <- sqrt(family$variance(mui)/D[,p+3])
      Vi <- phi*t(model$corr[D[,p+4],D[,p+4]]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
      Vi2 <- try(chol(Vi),silent=TRUE)
      if(is.matrix(Vi2)) Vi2 <- chol2inv(Vi2) else Vi2 <- solve(Vi)
      return(ni*log(2*pi) + t(yi - mui)%*%Vi2%*%(yi - mui) + log(det(Vi)))
    }
    resume <- Reduce('+',lapply(model$arrangedata,VSi))
    q <- switch (model$corstr,
                 "AR-M-dependent" = attr(model$corstr,"M"),
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
    results[i,1] <- round(temporal$cr0,digits=digits)
    results[i,2] <- temporal$cr2
    results[i,3] <- round(temporal$cr1,digits=digits)
    results2[i,1] <- as.character(call.[i+1])
    results2[i,2] <- x[[i]]$family$family
    results2[i,3] <- x[[i]]$family$link
    if(x[[i]]$linear) results2[i,4] <- paste(c(attr(x[[i]]$terms,"intercept"),attr(x[[i]]$terms,"term.labels")),collapse=" + ")
    else results2[i,4] <- paste(deparse(x[[i]]$formula[[3]]),collapse="")
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
    colnames(out_) <- c(c("Object","Variance function","Link function","Predictor","Correlation")[ids],"GPL","Parameters","SGPC")
    if(verbose) print(out_,row.names=FALSE)
    return(invisible(out_))
  }else return(results[,3])
}

#' @title Estimate of the variance-covariance matrix in GEEs
#' @description Computes the type-\code{type} estimate of the variance-covariance matrix from an object of the class glmgee.
#' @param object An object of the class \emph{glmgee}.
#' @param type an (optional) character string indicating the type of estimator which should be used. The available options are: robust sandwich-type estimator ("robust"), degrees-of-freedom-adjusted estimator ("df-adjusted"), bias-corrected estimator ("bias-corrected"), and the model-based or naive estimator ("model"). As default, \code{type} is set to "robust".
#' @param ...	further arguments passed to or from other methods.
#' @return A \code{matrix} with the type-\code{type} estimate of the variance-covariance matrix.
#' @method vcov glmgee
#' @export
#' @examples
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod, id=tree, family=Gamma(log), data=spruces, corstr="Exchangeable")
#' vcov(fit1)
#' vcov(fit1,type="bias-corrected")
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod <- depressd ~ visit + group
#' fit3 <- glmgee(mod, id=subj, family=binomial(logit), corstr="AR-M-dependent", data=depression)
#' vcov(fit3)
#' vcov(fit3,type="bias-corrected")
#'
#' ###### Example 3: Treatment for severe postnatal depression (2)
#' mod <- dep ~ visit*group
#' fit2 <- glmgee(mod, id=subj, family=gaussian(identity), corstr="AR-M-dependent", data=depression)
#' vcov(fit2)
#' vcov(fit2,type="bias-corrected")
#'
#' @references Mancl L.A., DeRouen T.A. (2001) A Covariance Estimator for GEE with Improved Small-Sample Properties. \emph{Biometrics} 57:126-134.
#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
vcov.glmgee <- function(object,...,type=c("robust","df-adjusted","model","bias-corrected","jackknife")){
  type <- match.arg(type)
  if(type=="robust") out_ <- object$R
  if(type=="df-adjusted") out_ <- (length(object$ids)/(length(object$ids)-length(coef(object))))*object$R
  if(type=="model") out_ <- object$naive*object$phi
  if(type=="bias-corrected") out_ <- crossprod(dfbeta(object,type="clusters",method="Preisser-Qaqish"))
  if(type=="jackknife"){
    out_ <- matrix(object$coefficients,length(object$sizes),length(object$coefficients),byrow=TRUE) - dfbeta(object,type="clusters",method="Preisser-Qaqish")
    out_ <- crossprod(out_ - matrix(apply(out_,2,mean),nrow(out_),ncol(out_),byrow=TRUE))
  }
  rownames(out_) <- colnames(out_) <- colnames(object$R)
  return(out_)
}
#' @title Leverage for Generalized Estimating Equations
#' @description Computes and, optionally, displays a graph of the leverage measures at the cluster- and observation-level.
#' @param object an object of class \emph{glmgee}.
#' @param level an (optional) character string indicating the level for which the leverage measures are required. The options are: cluster-level ("clusters") and observation-level ("observations"). As default, \code{level} is set to "clusters".
#' @param plot.it an (optional) logical indicating if the plot of the measures of leverage are required or just the data matrix in which that plot is based. As default, \code{plot.it} is set to \code{FALSE}.
#' @param identify an (optional) integer indicating the number of (\code{level=``clusters''}) or observations (\code{level=``observations''}) to identify on the plot of the leverage measures. This is only appropriate if \code{plot.it} is specified to be \code{TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it} is specified to be \code{TRUE} then \code{...} may be used to include graphical parameters to customize the plot. For example,  \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A vector with the values of the leverage measures with so many rows as clusters (\code{level=``clusters''}) or observations (\code{level=``observations''}) in the sample.
#' @references Preisser J.S., Qaqish B.F. (1996). Deletion diagnostics for generalised estimating equations. \emph{Biometrika}, 83:551-562.
#' @references Hammill B.G., Preisser J.S. (2006). A SAS/IML software program for GEE and regression diagnostics. \emph{Computational Statistics & Data Analysis}, 51:1197-1212.
#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
#' @method leverage glmgee
#' @export
#' @examples
#'
#' ###### Example 1: Tests of Auditory Perception in Children with OME
#' OME <- MASS::OME
#' mod <- cbind(Correct, Trials-Correct) ~ Loud + Age + OME
#' fit1 <- glmgee(mod, family = binomial(cloglog), id = ID, corstr = "Exchangeable", data = OME)
#' leverage(fit1,level="clusters",plot.it=TRUE)
#'
#' ###### Example 2: Guidelines for Urinary Incontinence Discussion and Evaluation
#' data(GUIDE)
#' mod <- bothered ~ gender + age + dayacc + severe + toilet
#' fit2 <- glmgee(mod, family=binomial(logit), id=practice, corstr="Exchangeable", data=GUIDE)
#' leverage(fit2,level="clusters",plot.it=TRUE)
#' leverage(fit2,level="observations",plot.it=TRUE)
#'
leverage.glmgee <- function(object,level=c("clusters","observations"),plot.it=FALSE,identify,...){
  level <- match.arg(level)
  Hs <- function(D){
    beta <- object$coefficients
    p <- length(beta)
    Xi <- matrix(D[,1:p],ncol=p)
    yi <- D[,p+1]
    ni <- nrow(Xi)
    etai <- Xi%*%beta + D[,p+2]
    mui <- object$family$linkinv(etai)
    wi <- sqrt(object$family$variance(mui)/D[,p+3])
    Vi <- t(object$corr[D[,p+4],D[,p+4]]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
    Xiw <- Xi*matrix(object$family$mu.eta(etai),nrow(Xi),p)
    if(object$corstr=="Independence") Vi2 <- diag(as.vector(1/wi[D[,p+4]]^2),ni,ni)
    else{Vi2 <- try(chol(Vi),silent=TRUE)
         if(is.matrix(Vi2)) Vi2 <- chol2inv(Vi2) else Vi2 <- solve(Vi)}
    hi <- diag(Xiw%*%tcrossprod(object$naive,Xiw)%*%Vi2)
    return(hi)
  }
  p <- length(object$coefficients)
  rowsn <- ""
  out_ <- 0
  for(i in 1:length(object$arrangedata)){
    if(level=="observations"){
      rowsn <- c(rowsn,paste(object$ids[i],"(",object$arrangedata[[i]][,p+4],")",sep=""))
      out_ <- c(out_,Hs(object$arrangedata[[i]]))
    }else out_ <- rbind(out_,sum(Hs(object$arrangedata[[i]])))
  }
  if(level=="clusters") rowsn <- object$ids else rowsn <- rowsn[-1]
  out_ <- matrix(out_[-1],nrow=length(out_[-1]))
  rownames(out_) <- rowsn
  colnames(out_) <- level
  if(plot.it){
    nano <- list(...)
    nano$x <- 1:length(out_)
    nano$y <- out_
    if(is.null(nano$xlab)) nano$xlab <- ifelse(level=="clusters","Cluster Index","Observation Index")
    if(is.null(nano$type)) nano$type <- "h"
    if(is.null(nano$ylab)) nano$ylab <- ifelse(level=="clusters",expression(mean(h[ij])),expression(h[ij]))
    if(is.null(nano$labels)) labels <- rowsn
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    do.call("plot",nano)
    abline(h=3*mean(out_),lty=3)
    if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=labels)
  }
  return(out_)
}

#' @title Local Influence for Generalized Estimating Equations
#' @description Computes some measures and, optionally, display	graphs of them to perform influence analysis based on the approaches described in Cook (1986) and Jung (2008).
#' @param object an object of class \emph{glmgee}.
#' @param type an (optional) character string indicating the type of approach to study the local influence. The options are: the absolute value of the elements of the eigenvector which corresponds to the maximum absolute eigenvalue ("local"); and the elements of the main diagonal ("total"). As default, \code{type} is set to "total".
#' @param perturbation an (optional) character string indicating the perturbation scheme to apply. The options are: case weight perturbation of clusters ("cw-clusters"); Case weight perturbation of observations ("cw-observations"); and perturbation of response ("response"). As default, \code{perturbation} is set to "cw-clusters".
#' @param plot.it an (optional) logical indicating if the plot of the measures of local influence is required or just the data matrix in which that plot is based. As default, \code{plot.it} is set to \code{FALSE}.
#' @param coefs	an (optional) character string which (partially) match with the names of some of the parameters in the linear predictor.
#' @param identify an (optional) integer indicating the number of clusters/observations to identify on the plot of the measures of local influence. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix as many rows as clusters/observations in the sample and one column with the values of the measures of local influence.
#' @method localInfluence glmgee
#' @export
#' @references Cook D. (1986) Assessment of Local Influence. \emph{Journal of the Royal Statistical Society: Series B (Methodological)} 48:133-155.
#' @references Jung K.-M. (2008) Local Influence in Generalized Estimating Equations. \emph{Scandinavian Journal of Statistics} 35:286-294.
#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
#' @examples
#' ###### Example 1: Effect of ozone-enriched atmosphere on growth of sitka spruces
#' data(spruces)
#' mod1 <- size ~ poly(days,4) + treat
#' fit1 <- glmgee(mod1, id=tree, family=Gamma(log), corstr="AR-M-dependent", data=spruces)
#' localInfluence(fit1,type="total",perturbation="cw-clusters",coefs="treat",plot.it=TRUE)
#'
#' ###### Example 2: Treatment for severe postnatal depression
#' data(depression)
#' mod2 <- depressd ~ visit + group
#' fit2 <- glmgee(mod2, id=subj, family=binomial(logit), corstr="AR-M-dependent", data=depression)
#' localInfluence(fit2,type="total",perturbation="cw-clusters",coefs="group",plot.it=TRUE)
#'
#' ###### Example 3: Treatment for severe postnatal depression (2)
#' mod3 <- dep ~ visit*group
#' fit3 <- glmgee(mod3, id=subj, family=gaussian(identity), corstr="AR-M-dependent", data=depression)
#' localInfluence(fit3,type="total",perturbation="cw-clusters",coefs="visit:group",plot.it=TRUE)
#'
localInfluence.glmgee <- function(object,type=c("total","local"),perturbation=c("cw-clusters","cw-observations",
                                                                                "response"),coefs,plot.it=FALSE,identify,...){
  if(!object$linear)
    stop("Objects generated by the function gnmgee() are not supported!!",call.=FALSE)
  type <- match.arg(type)
  perturbation <- match.arg(perturbation)
  subst <- NULL
  if(!missingArg(coefs)){
    ids <- grepl(coefs,rownames(object$coefficients),ignore.case=TRUE)
    if(sum(ids) > 0) subst <- rownames(object$coefficients)[ids]
    else stop(paste("There are no variables with the name",coefs,collapse=""),call.=FALSE)
  }
  Qbb <- function(D){
    beta <- object$coefficients
    p <- length(beta)
    Xi <- matrix(D[,1:p],ncol=p)
    yi <- D[,p+1]
    ni <- nrow(Xi)
    etai <- Xi%*%beta + D[,p+2]
    mui <- object$family$linkinv(etai)
    vmui <- object$family$variance(mui)
    wi <- sqrt(vmui/D[,p+3])
    #Vi <- object$phi*t(object$corr[D[,p+4],D[,p+4]]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
    Vi <- t(object$corr[D[,p+4],D[,p+4]]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
    kpi <- as.vector(object$family$mu.eta(etai))
    kpii <- grad(object$family$mu.eta,etai)
    vpi <- grad(object$family$variance,mui)
    Xiw <- Xi*matrix(kpi,nrow(Xi),p)
    if(object$corstr=="Independence") Vi2 <- diag(as.vector(1/wi[D[,p+4]]^2),ni,ni)
    else{Vi2 <- try(chol(Vi),silent=TRUE)
    if(is.matrix(Vi2)) Vi2 <- chol2inv(Vi2) else Vi2 <- solve(Vi)}
    zi <- crossprod(Vi2,yi-mui)
    a <- kpii - (1/2)*kpi^2*vpi/vmui
    b <- as.vector(-kpi*(1 + (1/2)*(yi-mui)*vpi/vmui))
    Qpp <- -crossprod(Xi,diag(as.vector(a*zi),ni,ni) + diag(kpi,ni,ni)%*%Vi2%*%diag(b,ni,ni))%*%Xi
    if(perturbation=="cw-clusters") Delta <- matrix(apply(matrix(kpi*zi,ni,p)*Xi,2,sum),ncol=p,nrow=1)
    if(perturbation=="cw-observations") Delta <- matrix(kpi*zi,ni,p)*Xi
    if(perturbation=="response") Delta <- diag(sqrt(diag(Vi)))%*%Vi2%*%diag(kpi)%*%Xi
    return(list(Qpp=Qpp,Delta=Delta))
  }
  p <- length(coef(object))
  Qpp <- 0; Delta <- 0; rowsn <- ""
  for(i in 1:length(object$arrangedata)){
    resu <- Qbb(object$arrangedata[[i]])
    Qpp <- Qpp + resu$Qpp
    Delta <- rbind(Delta,resu$Delta)
    if(perturbation!="cw-clusters") rowsn <- c(rowsn,paste(object$ids[i],"(",object$arrangedata[[i]][,p+4],")",sep=""))
  }
  Delta <- matrix(Delta[-1,],nrow(Delta)-1,ncol(Delta))
  Qpp2 <- try(chol(Qpp),silent=TRUE)
  if(is.matrix(Qpp2)) Qpp2 <- chol2inv(Qpp2) else Qpp2 <- solve(Qpp)
  if(!is.null(subst)) Qpp2[-ids,-ids] <- Qpp2[-ids,-ids] - solve(Qpp[-ids,-ids])
  li <- Delta%*%Qpp2
  if(type=="local"){
    tol <- 1
    bnew <- matrix(rnorm(nrow(li)),nrow(li),1)
    while(tol > 0.000001){
      bold <- bnew
      bnew <- li%*%crossprod(Delta,bold)
      bnew <- bnew/sqrt(sum(bnew^2))
      tol <- max(abs((bnew - bold)/bold))
    }
    out_ <- abs(bnew/sqrt(sum(bnew^2)))
  }else out_ <- apply(li*Delta,1,sum)
  if(perturbation=="cw-clusters") rowsn <- object$ids else rowsn <- rowsn[-1]
  out_ <- matrix(out_,nrow=length(out_))
  rownames(out_) <- rowsn
  colnames(out_) <- type
  if(plot.it){
    nano <- list(...)
    nano$x <- 1:length(out_)
    nano$y <- out_
    if(is.null(nano$xlab)) nano$xlab <- ifelse(perturbation=="cw-clusters","Cluster Index","Observation Index")
    if(is.null(nano$type)) nano$type <- "h"
    if(is.null(nano$ylab)) nano$ylab <- ifelse(type=="local",expression(d[max]),ifelse(perturbation=="cw-clusters",expression(diag[i]),expression(diag[ij])))
    if(is.null(nano$main)) nano$main <- ""
    if(is.null(nano$labels)) labels <- rowsn
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    do.call("plot",nano)
    if(any(out_>0)) abline(h=3*mean(out_[out_>0]),lty=3)
    if(any(out_<0)) abline(h=3*mean(out_[out_<0]),lty=3)
    if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=labels)
  }
  if(!is.null(subst)){
    message("The coefficients included in the measures of local influence are: ",paste(subst,sep=""),"\n")
  }
  return(out_)
}


#' @title Fit Nonlinear Generalized Estimating Equations
#' @description Produces an object of the class \code{glmgee} in which the main results of a Nonlinear Generalized Estimating Equation (GEE) fitted to the data are stored.
#' @param formula a nonlinear model \code{formula} including variables and parameters, which is a symbolic description of the nonlinear predictor of the model to be fitted to the data.
#' @param family an (optional) \code{family} object, that is, a list of functions and expressions for defining link and variance functions. Families (and links) supported are the same supported by \link{glm} using its \link{family} argument, that is,
#' \code{gaussian}, \code{binomial}, \code{poisson}, \code{Gamma}, \code{inverse.gaussian}, and \code{quasi}. The family \code{negative.binomial} in the library \pkg{MASS} are also available. As default, the argument \code{family} is set to \code{gaussian(identity)}.
#' @param id a vector which identifies the subjects or clusters. The length of \code{id} should be the same as the number of observations.
#' @param offset an (optional) numeric vector of length equal to the number of cases, which can be used to specify an a priori known component to be included in the linear predictor during fitting.
#' @param weights an (optional) vector of positive "prior weights" to be used in the fitting process. The length of \code{weights} should be the same as the total number of observations.
#' @param data an (optional) \code{data frame} in which to look for variables involved in the \code{formula} expression, as well as for variables specified in the arguments \code{id} and \code{weights}. The data are assumed to be sorted by \code{id} and time.
#' @param subset an (optional) vector specifying a subset of observations to be used in the fitting process.
#' @param corstr an (optional) character string which allows to specify the working-correlation structure. The available options are: "Independence", "Unstructured", "Stationary-M-dependent(\emph{m})", "Non-Stationary-M-dependent(\emph{m})", "AR-M-dependent(\emph{m})", "Exchangeable" and "User-defined", where \emph{m} represents the lag of the dependence. As default, \code{corstr} is set to "Independence".
#' @param corr an (optional) square matrix of the same dimension of the maximum cluster size containing the user specified correlation. This is only appropriate if \code{corstr} is specified to be "User-defined".
#' @param start an (optional) vector of starting values for the parameters in the nonlinear predictor. When \code{start} is
#' missing (and \code{formula} is not a self-starting model, see \link{nls} and \link{selfStart}), a very cheap guess for \code{start} is tried.
#' @param maxit an (optional) integer value which represents the maximum number of iterations allowed for the fitting algorithm. As default, \code{maxit} is set to 50.
#' @param toler an (optional) positive value which represents the \emph{convergence tolerance}. The convergence is reached when the maximum of the absolute relative differences between the values of the parameters in the nonlinear predictor in consecutive iterations of the fitting algorithm is lower than \code{toler}. As default, \code{toler} is set to 0.00001.
#' @param trace an (optional) logical variable. If TRUE, output is produced for each iteration of the estimating algorithm.
#' @param scale.fix an (optional) logical variable. If TRUE, the scale parameter is fixed at the value of \code{scale.value}. As default, \code{scale.fix} is set to \code{FALSE}.
#' @param scale.value an (optional) numeric value at which the scale parameter should be fixed. This is only appropriate if \code{scale.fix=TRUE}. As default, \code{scale.value} is set to 1.
#' @param waves an (optional) positive integer-valued variable that is used to identify the order and spacing of observations within clusters. This argument is crucial when there are missing values and gaps in the data. As default, \code{waves} is equal to the integers from 1 to the size of each cluster.
#' @param ...	further arguments passed to or from other methods.
#' @details The values of the multivariate response variable measured on \eqn{n} subjects or clusters,
#' denoted by \eqn{y_{i}=(y_{i1},\ldots,y_{in_i})^{\top}} for \eqn{i=1,\ldots,n}, are assumed to be
#' realizations of independent random vectors denoted by \eqn{Y_{i}=(Y_{i1},\ldots,Y_{in_i})^{\top}}
#' for \eqn{i=1,\ldots,n}. The random variables associated to the \eqn{i}-th subject or
#' cluster, \eqn{Y_{ij}} for \eqn{j=1,\ldots,n_i}, are assumed to satisfy
#' \eqn{\mu_{ij}=} E\eqn{(Y_{ij})},Var\eqn{(Y_{ij})=\frac{\phi}{\omega_{ij}}}V\eqn{(\mu_{ij})}
#' and Corr\eqn{(Y_{ij},Y_{ik})=r_{jk}(\rho)},
#' where \eqn{\phi>0} is the dispersion parameter,
#' V\eqn{(\mu_{ij})} is the variance function, \eqn{\omega_{ij}>0} is a known weight, and
#' \eqn{\rho=(\rho_1,\ldots,\rho_q)^{\top}} is a parameter vector.
#' In addition, \eqn{\mu_{ij}} is assumed to be dependent on the regressors vector \eqn{x_{ij}}
#' by \eqn{g(\mu_{ij})=z_{ij} + m(x_{ij},\beta)}, where \eqn{g(\cdot)} is the link function,
#' \eqn{z_{ij}} is a known \emph{offset}, \eqn{\beta=(\beta_1,\ldots,\beta_p)^{\top}} is
#' a vector of regression parameters and \eqn{m(x_{ij},\beta)} is a known nonlinear function of \eqn{\beta}.
#' The parameter estimates are obtained by iteratively
#' solving the estimating equations described by Liang and Zeger (1986).
#'
#' If the maximum cluster size is 6 and for a cluster of size 4 the value
#' of \code{waves} is set to 2, 4, 5, 6, then it means that the data at
#' times 1 and 3 are missing, which should be taken into account by
#' \code{gnmgee} when the structure of the correlation matrix is assumed
#' to be "Unstructured", "Stationary-M-dependent", "Non-Stationary-M-dependent"
#' or "AR-M-dependent". If in this scenario \code{waves} is not specified
#' then \code{gnmgee} assumes that the available data for this cluster
#' were taken at times 1, 2, 3 and 4.
#'
#' A set of standard extractor functions for fitted model objects is
#' available for objects of class \emph{glmgee}, including methods to generic functions such as   \code{print}, \code{summary},	\code{model.matrix}, \code{estequa},
#' \code{coef}, \code{vcov}, \code{logLik}, \code{fitted}, \code{confint} and \code{predict}.
#' In addition, the model may be assessed using functions such as \link{anova.glmgee},
#' \link{residuals.glmgee}, \link{dfbeta.glmgee}, \link{cooks.distance.glmgee}, \link{tidy.glmgee} and \link{glance.glmgee}.
#'
#' @return an object of class \emph{glmgee} in which the main results of the GEE model fitted to the data are stored, i.e., a
#' list with components including
#' \tabular{ll}{
#' \code{coefficients} \tab a vector with the estimates of \eqn{\beta_1,\ldots,\beta_p},\cr
#' \tab \cr
#' \code{fitted.values}\tab a vector with the estimates of \eqn{\mu_{ij}} for \eqn{i=1,\ldots,n} and \eqn{j=1,\ldots,n_i},\cr
#' \tab \cr
#' \code{start}        \tab a vector with the starting values used,\cr
#' \tab \cr
#' \code{iter}         \tab a numeric constant with the number of iterations,\cr
#' \tab \cr
#' \code{prior.weights}\tab a vector with the values of \eqn{\omega_{ij}} for \eqn{i=1,\ldots,n} and \eqn{j=1,\ldots,n_i},\cr
#' \tab \cr
#' \code{offset}       \tab a vector with the values of \eqn{z_{ij}} for \eqn{i=1,\ldots,n} and \eqn{j=1,\ldots,n_i},\cr
#' \tab \cr
#' \code{terms}        \tab an object containing the terms objects,\cr
#' \tab \cr
#' \code{loglik}       \tab the value of the quasi-log-likelihood function evaluated at the parameter\cr
#'                     \tab estimates and the observed data,\cr
#' \tab \cr
#' \code{estfun}       \tab a vector with the estimating equations evaluated at the parameter\cr
#'                     \tab estimates and the observed data,\cr
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
#' \code{y}            \tab a vector with the values of \eqn{y_{ij}} for \eqn{i=1,\ldots,n} and \eqn{j=1,\ldots,n_i},\cr
#' \tab \cr
#' \code{family}       \tab an object containing the \link{family} object used,\cr
#' \tab \cr
#' \code{linear.predictors} \tab a vector with the estimates of \eqn{g(\mu_{ij})} for \eqn{i=1,\ldots,n} and \eqn{j=1,\ldots,n_i},\cr
#' \tab \cr
#' \code{R}            \tab a matrix with the (robust) estimate of the variance-covariance,\cr
#' \tab \cr
#' \code{corr}         \tab a matrix with the estimate of the working-correlation,\cr
#' \tab \cr
#' \code{corstr}       \tab a character string specifying the working-correlation structure,\cr
#' \tab \cr
#' \code{id}           \tab a vector which identifies the subjects or clusters,\cr
#' \tab \cr
#' \code{sizes}        \tab a vector with the values of \eqn{n_i} for \eqn{i=1,\ldots,n},\cr
#' \tab \cr
#' \code{call}         \tab the original function call,\cr
#' }
#' @seealso \link{glmgee}, \link{wglmgee}
#' @export gnmgee
#' @examples
#' ###### Example 1: Orange trees grown at Riverside, California
#' data(Oranges)
#' mod <- Trunk ~ b1/(1 + exp((b2-Days)/b3))
#' start <- c(b1=200,b2=760,b3=375)
#' fit1 <- gnmgee(mod, start=start, id=Tree, family=Gamma(identity), corstr="Exchangeable",
#'                data=Oranges)
#' summary(fit1, corr.digits=2)
#'
#' mod <- Trunk ~ SSlogis(Days,b1,b2,b3)
#' fit2 <- gnmgee(mod, id=Tree, family=Gamma(identity), corstr="Exchangeable", data=Oranges)
#' summary(fit2, corr.digits=2)
#'
#' ###### Example 2: Growth of Paramecium aurelium
#' data(paramecium)
#' fit2 <- gnmgee(Number ~ exp(alpha - exp(beta - gamma*Days)), id=Colony, family=poisson(log),
#'          start=c(alpha=1.85,beta=0.7,gamma=0.35), corstr="AR-M-dependent(1)", data=paramecium)
#' summary(fit2, corr.digits=2)
#'
#' @references Liang K.Y., Zeger S.L. (1986) Longitudinal data analysis using generalized linear models.
#' \emph{Biometrika} 73:13-22.
#' @references Zeger S.L., Liang K.Y. (1986) Longitudinal data analysis for discrete and continuous outcomes.
#' \emph{Biometrics} 42:121-130.
#' @references Hardin J.W., Hilbe J.M. (2013) \emph{Generalized Estimating Equations}. Chapman & Hall, London.
#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
gnmgee <- function(formula,family=gaussian(),offset=NULL,weights=NULL,id,waves,data,subset=NULL,corstr,corr,start=NULL,scale.fix=FALSE,scale.value=1,toler=0.00001,maxit=50,trace=FALSE,...){
  if(missingArg(data)) data <- environment(eval(formula))
  if(missingArg(corstr)) corstr <- "Independence"
  if(is(family,"function")) family <- family()
  if(is.null(start)){
    defaultW <- getOption("warn")
    options(warn = -1)
    form <- paste0("y2 <- family$linkfun(",deparse(formula[[2]]),")")
    if(family$family=="binomial"){
      form <- paste0("ifelse(",deparse(formula[[2]]),"*(1-",deparse(formula[[2]]),")==0,abs(",deparse(formula[[2]]),"-0.01),",deparse(formula[[2]]),")")
      form <- paste0("y2 <- family$linkfun(",form,")")
    }
    if(family$family=="poisson") form <- paste0("y2 <- family$linkfun(ifelse(",deparse(formula[[2]]),"==0,0.01,",deparse(formula[[2]]),"))")
    eval(parse(text=paste0("data <- within(data,",form,")")))
    mmf <- match.call(expand.dots = FALSE)
    m <- match(c("subset", "weights", "offset"), names(mmf), 0)
    mmf <- mmf[c(1L,m)]
    if(!is.null(mmf$offset)) eval(parse(text=paste0("data <- within(data,y2 <- y2 - ",deparse(mmf$offset),")") ))
    conls <- try(do.call(nls,list(formula=as.formula(paste0("y2 ~ ",deparse(formula[[3]]))),data=data,subset=mmf$subset,weights=mmf$weights)),silent=TRUE)
    options(warn = defaultW)
    if(is.list(conls)) start <- coef(conls)
  }
  if(is.null(start)) stop("Starting values are required. Please specify some!!",call.=FALSE)
  formula2 <- formula
  avs <- all.vars(formula)[-1]
  if(is.null(names(start))) pars <- rownames(start) else pars <- names(start)
  vnames <- avs[is.na(match(avs,pars))]
  formula <- paste(formula[2],"~ -1 + ",paste(vnames,collapse="+"))
  formula <- as.formula(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "offset", "weights", "waves", "data", "subset", "id"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- na.omit
  mf[[1L]] <- as.name("model.frame")
  mf$formula <- formula
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- as.matrix(model.response(mf, "any"))
  weights <- as.vector(model.weights(mf))

  if(family$family %in% c("quasi","quasibinomial","quasipoisson")){
    if(family$family=="quasi"){
      family$family <- switch(family$varfun,"constant"="gaussian","mu(1-mu)"="binomial",
                              "mu"="poisson","mu^2"="Gamma","mu^3"="inverse.gaussian")
    }
    else{
      family$family <- switch(family$family,"quasibinomial"="binomial","quasipoisson"="poisson")
    }
    family <- do.call(family$family,list(link=family$link))
  }

  if(ncol(y)==2 & family$family=="binomial"){
    weights <- as.matrix(y[,1] + y[,2])
    y <- as.matrix(y[,1]/weights)
  }
  y <- as.matrix(as.numeric(y))
  offset <- as.vector(model.offset(mf))
  X <- model.matrix(mt, mf)
  p <- length(start)
  n <- nrow(X)
  if(is.null(offset)) offs <- matrix(0,n,1) else offs <- as.matrix(offset)
  if(is.null(weights)) weights <- matrix(1,n,1) else weights <- as.matrix(weights)
  if(any(weights <= 0)) stop("Only positive weights are allowed!!",call.=FALSE)
  waves <- as.vector(model.extract(mf,waves))
  if(!is.null(waves)) if(any(waves!=floor(waves)) | any(waves<=0)) stop("Only positive integers are allowed in waves!!",call.=FALSE)
  id <- model.extract(mf,id)
  if(is.null(id)) id <- matrix(1:n,n,1)
  id2 <- as.matrix(unique(id))
  nclus <- length(id2)
  sizes <- matrix(0,nclus,1)
  datas <- list()
  for(i in 1:nclus){
    places <- id==id2[i]
    sizes[i] <- sum(places)
    if(is.null(waves))
      datas[[i]] <- cbind(matrix(0,sizes[i],p),y[places,],offs[places],weights[places],1:sizes[i],i)
    else
      datas[[i]] <- cbind(matrix(0,sizes[i],p),y[places,],offs[places],weights[places],waves[places],i)
  }
  maxsize <- ifelse(is.null(waves),max(sizes),max(waves))
  if(maxsize==1) corstr <- "Independence"
  Rhat <- function(D,beta){
    places <- id==id2[D[1,p+5]]
    etai <- etas[places] + D[,p+2]
    mui <- family$linkinv(etai)
    wi <- sqrt(family$variance(mui)/D[,p+3])
    es <- rep(0,maxsize)
    es2 <- es
    es[D[,p+4]] <- (D[,p+1]-mui)/wi
    es2[D[,p+4]] <- 1
    nums <- tcrossprod(es)
    nums2 <- tcrossprod(es2)
    cbind(nums,nums2)
  }
  temp <- strsplit(gsub("[[:space:]]","",corstr),"[()]")[[1]]
  args <- c("Independence","Unstructured","Stationary-M-dependent",
            "Non-Stationary-M-dependent","AR-M-dependent","Exchangeable","User-defined")
  temp2 <- grep(tolower(temp[1]),tolower(args))
  if(length(temp)>0){
    corstr <- args[temp2[1]]
    if(corstr=="Stationary-M-dependent" | corstr=="Non-Stationary-M-dependent" | corstr=="AR-M-dependent"){
      M <- min(max(1,floor(abs(as.numeric(temp[2])))),maxsize-1)
      if(is.na(M)) M <- 1
      attr(corstr,"M") <- M
    }
  }else stop("Correlation structure should be one of Independence, Unstructured, Stationary-M-dependent,
 		            Non-Stationary-M-dependent, AR-M-dependent, Exchangeable and User-defined!!",call.=FALSE)
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
    if(corstr=="AR-M-dependent"){
      alphas <- matrix(0,M,1)
      R <- matrix(0,maxsize,maxsize)
      for(i in 1:M){
        inds <- cbind(1:(maxsize-i),(1+i):maxsize)
        inds2 <- cbind(inds[,1],maxsize+inds[,2])
        alphas[i] <- sum(resume[inds])/(sum(resume[inds2])-p)
      }
      alphas <- alphas/phi
      alphasf <- function(x) if(x==0) return(1) else return(alphas[x])
      phises <- crossprod(chol2inv(chol(apply(toeplitz(0:(M-1)),c(1,2),alphasf))),alphas)
      rhos <- matrix(0,maxsize-1,1)
      rhos[1:M] <- alphas
      for(i in (M+1):(maxsize-1)) rhos[i] <- sum(phises*rhos[i-c(1:M)])
      R <- toeplitz(c(1,rhos))
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
  etaf <- function(beta){
    temp <- deparse(formula2[[3]])
    for(i in 1:p) temp <- gsub(pars[i],beta[i],temp)
    matrix(eval(parse(text=temp),data.frame(X)),ncol=1)
  }
  Der <- function(beta) matrix(jacobian(etaf,beta),ncol=p)
  score <- function(D,beta,out){
    places <- id==id2[D[1,p+5]]
    yi <- D[,p+1]
    ni <- length(yi)
    etai <- etas[places] + D[,p+2]
    Xi <- matrix(Ders[places,],ncol=p)
    mui <- family$linkinv(etai)
    wi <- as.vector(sqrt(family$variance(mui)/D[,p+3]))
    Xiw <- Xi*matrix(family$mu.eta(etai),nrow(Xi),p)
    Vi <- t(R[D[,p+4],D[,p+4]]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
    if(corstr=="Independence") Vi2 <- diag(as.vector(1/wi[D[,p+4]]^2),ni,ni)
    else{Vi2 <- try(chol(Vi),silent=TRUE)
    if(is.matrix(Vi2)) Vi2 <- chol2inv(Vi2) else Vi2 <- solve(Vi)}
    Xiw2 <- crossprod(Vi2,Xiw)
    if(out) cbind(crossprod(Xiw2,(yi-mui)),crossprod(Xiw2,Xiw))
    else cbind(crossprod(Xiw2,(yi-mui)),crossprod(Xiw2,Xiw),crossprod(Xiw2,(tcrossprod(yi-mui))%*%Xiw2))
  }
  beta_new <- start
  tol <- 1
  niter <- 0
  if(missingArg(corr)) corr <- diag(maxsize)
  if(trace) message("\nIteration  Convergence Criterion")
  while(tol > toler & niter < maxit){
    beta_old <- beta_new
    etas <- etaf(beta_old)
    Ders <- Der(beta_old)
    resume <- 0
    for(i in 1:nclus) resume <- resume + Rhat(datas[[i]],beta=beta_old)
    phi <- sum(diag(resume[1:maxsize,1:maxsize]))/(sum(sizes)-p)
    if(corstr!="User-defined" & corstr!="Independence") R <- Rout(resume,corstr)
    else R <- as.matrix(corr)
    resume2 <- 0
    for(i in 1:nclus) resume2 <- resume2 + score(datas[[i]],beta=beta_old,out=FALSE)
    kchol <- try(chol(resume2[1:p,2:(p+1)]),silent=TRUE)
    if(is.matrix(kchol)) kchol <- chol2inv(kchol) else kchol <- solve(resume2[1:p,2:(p+1)])
    beta_new <- beta_old + crossprod(kchol,resume2[,1])
    tol <- max(abs((beta_new-beta_old)/beta_old))
    niter <- niter + 1
    if(trace) message("    ",niter,"            ",signif(tol,digits=5))
  }
  rownames(beta_new) <- pars
  colnames(beta_new) <- ""
  if(niter==maxit) warning("Iteration limit exceeded!!\n",call.=FALSE)
  for(i in 1:nclus){
    places <- id==id2[i]
    datas[[i]][,1:p] <- matrix(Ders[places,],ncol=p)
  }
  eta <- etas
  X <- Ders
  mu <- family$linkinv(eta)
  phi <- sum(diag(matrix(resume[1:maxsize,1:maxsize],maxsize,maxsize)))/(sum(sizes)-p)
  sera <- try(chol(R),silent=TRUE)
  if(!is.matrix(sera)) warning("Estimate of correlation matrix is not positive definite",call.=FALSE)
  I0 <- try(chol(resume2[1:p,2:(p+1)]),silent=TRUE)
  if(is.matrix(I0)) I0 <- chol2inv(I0) else I0 <- solve(resume2[1:p,2:(p+1)])
  I1 <- resume2[1:p,(p+2):(2*p+1)]
  RJC <- I0%*%I1
  vcovs <- RJC%*%I0
  rownames(vcovs) <- pars
  colnames(vcovs) <- pars
  qll <- NULL
  if(family$family=="gaussian") qll <- -weights*(y - mu)^2/2
  if(family$family=="binomial") qll <- weights*(y*log(mu) + (1-y)*log(1-mu))
  if(family$family=="poisson") qll <- weights*(y*log(mu) - mu)
  if(family$family=="Gamma") qll <- -weights*(y/mu + log(mu))
  if(family$family=="inverse.gaussian") qll <- weights*(mu - y/2)/mu^2
  if(family$family=="Negative Binomial"){
    .Theta <- function() return(.Theta)
    environment(.Theta) <- environment(family$variance)
    .Theta <- .Theta()
    qll <- weights*(y*log(mu/(mu + .Theta)) + .Theta*log(.Theta/(.Theta + mu)))
  }
  if(family$family=="Tweedie"){
    .Theta <- function() return(p)
    environment(.Theta) <- environment(family$variance)
    .Theta <- .Theta()
    if(.Theta %in% c(0,1,2)){
      if(.Theta==0) qll <- -weights*(y - mu)^2/2
      if(.Theta==1) qll <- weights*(y*log(mu) - mu)
      if(.Theta==2) qll <- -weights*(y/mu + log(mu))
    }else qll <- mu^(-.Theta)*(mu*y/(1 - .Theta) - mu^2/(2 - .Theta))
  }
  if(is.null(qll)) qll <- family$dev.resids(y,mu,weights)
  w <- sqrt(weights*family$mu.eta(eta)^2/family$variance(mu))
  Xw <- matrix(w,nrow(X),ncol(X))*X
  CIC <- sum(diag((crossprod(Xw,Xw)/phi)%*%vcovs))
  phi <- ifelse(scale.fix,scale.value,phi)
  RJC <- sqrt((1 - sum(diag(RJC))/(p*phi))^2 + (1 - sum(diag(RJC%*%RJC))/(p*phi^2))^2)
  logLik <- sum(qll)/phi
  rownames(R) <- paste("[",1:maxsize,"]",sep="")
  colnames(R) <- paste("[",1:maxsize,"] ",sep="")
  estfun <- as.matrix(resume2[,1])
  rownames(estfun) <- pars
  colnames(estfun) <- ""
  out_ <- list(coefficients=beta_new,fitted.values=mu,linear.predictors=eta,sizes=sizes,arrangedata=datas,
               prior.weights=weights,y=y,formula=formula2,call=match.call(),offset=offs,waves=waves,model=mf,data=data,score=score,ids=id2,
               converged=ifelse(niter<maxit,TRUE,FALSE),estfun=estfun,R=vcovs,naive=I0,terms=mt,family=family,Rout=Rout,Rhat=Rhat,id=id,
               phi=phi,CIC=CIC,RJC=RJC,logLik=logLik,corr=R,clusters=c(length(sizes),max(sizes),min(sizes)),corstr=corstr,
               deviance=sum(family$dev.resids(y,mu,weights)),df.residual=length(y)-length(beta_new),levels=.getXlevels(attr(mf,"terms"),mf),
               contrasts=attr(X,"contrasts"),start=start,iter=niter,etaf=etaf,linear=FALSE)
  class(out_) <- "glmgee"
  return(out_)
}
#' @title Fit Weighted Generalized Estimating Equations
#' @description Produces an object of the class \code{wglmgee} in which the main results of a Weighted Generalized Estimating Equation (WGEE) fitted to the data are stored.
#' @param formula an \code{Formula} expression of the form \code{response ~ x1 + x2 + ... | z1 + z2 + ...}, whose first part is a symbolic description of the linear predictor of the GEE model to be fitted to the data, whereas the second part is a symbolic description of the linear predictor of the logistic model to be used to calculate the missingness probabilities under the MAR assumption. Then, those probabilities are used to computed the weights to be included in the parameter estimation algorithm.
#' @param family an (optional) \code{family} object, that is, a list of functions and expressions for defining link and variance functions. Families (and links) supported are the same supported by \link{glm} using its \link{family} argument, that is,
#' \code{gaussian}, \code{binomial}, \code{poisson}, \code{Gamma}, \code{inverse.gaussian}, and \code{quasi}. The family \code{negative.binomial} in the library \pkg{MASS} are also available. As default, the argument \code{family} is set to \code{gaussian(identity)}.
#' @param id a vector which identifies the subjects or clusters. The length of \code{id} should be the same as the number of observations.
#' @param level an (optional) character string which allows to specify the weighted GEE method. The available options are: "observations" and "clusters" for Observation- and Cluster-specified Weighted GEE, respectively. As default, \emph{level} is set to "observations".
#' @param weights an (optional) vector of positive "prior weights" to be used in the fitting process. The length of \code{weights} should be the same as the total number of observations.
#' @param data an (optional) \code{data frame} in which to look for variables involved in the \code{formula} expression, as well as for variables specified in the arguments \code{id} and \code{weights}. The data are assumed to be sorted by \code{id} and time.
#' @param subset an (optional) vector specifying a subset of observations to be used in the fitting process.
#' @param corstr an (optional) character string which allows to specify the working-correlation structure. The available options are: "Independence", "Unstructured", "Stationary-M-dependent(\emph{m})", "Non-Stationary-M-dependent(\emph{m})", "AR-M-dependent(\emph{m})", "Exchangeable" and "User-defined", where \emph{m} represents the lag of the dependence. As default, \code{corstr} is set to "Independence".
#' @param corr an (optional) square matrix of the same dimension of the maximum cluster size containing the user specified correlation. This is only appropriate if \code{corstr} is specified to be "User-defined".
#' @param start an (optional) vector of starting values for the parameters in the linear predictor.
#' @param maxit an (optional) integer value which represents the maximum number of iterations allowed for the fitting algorithm. As default, \code{maxit} is set to 50.
#' @param toler an (optional) positive value which represents the \emph{convergence tolerance}. The convergence is reached when the maximum of the absolute relative differences between the values of the parameters in the linear predictor in consecutive iterations of the fitting algorithm is lower than \code{toler}. As default, \code{toler} is set to 0.00001.
#' @param trace an (optional) logical variable. If TRUE, output is produced for each iteration of the estimating algorithm.
#' @param scale.fix an (optional) logical variable. If TRUE, the scale parameter is fixed at the value of \code{scale.value}. As default, \code{scale.fix} is set to \code{FALSE}.
#' @param scale.value an (optional) numeric value at which the scale parameter should be fixed. This is only appropriate if \code{scale.fix=TRUE}. As default, \code{scale.value} is set to 1.
#' @param ...	further arguments passed to or from other methods.
#' @details The values of the multivariate response variable measured on \eqn{n} subjects or clusters,
#' denoted by \eqn{y_{i}=(y_{i1},\ldots,y_{in_i})^{\top}} for \eqn{i=1,\ldots,n}, are assumed to be
#' realizations of independent random vectors denoted by \eqn{Y_{i}=(Y_{i1},\ldots,Y_{in_i})^{\top}}
#' for \eqn{i=1,\ldots,n}. The random variables associated to the \eqn{i}-th subject or
#' cluster, \eqn{Y_{ij}} for \eqn{j=1,\ldots,n_i}, are assumed to satisfy
#' \eqn{\mu_{ij}=} E\eqn{(Y_{ij})},Var\eqn{(Y_{ij})=\frac{\phi}{\omega_{ij}}}V\eqn{(\mu_{ij})}
#' and Corr\eqn{(Y_{ij},Y_{ik})=r_{jk}(\rho)},
#' where \eqn{\phi>0} is the dispersion parameter,
#' V\eqn{(\mu_{ij})} is the variance function, \eqn{\omega_{ij}>0} is a known weight, and
#' \eqn{\rho=(\rho_1,\ldots,\rho_q)^{\top}} is a parameter vector.
#' In addition, \eqn{\mu_{ij}} is assumed to be dependent on the regressors vector \eqn{x_{ij}}
#' by \eqn{g(\mu_{ij})=z_{ij} + x_{ij}^{\top}\beta}, where \eqn{g(\cdot)} is the link function,
#' \eqn{z_{ij}} is a known \emph{offset} and \eqn{\beta=(\beta_1,\ldots,\beta_p)^{\top}} is
#' a vector of regression parameters. The probabilities \eqn{Pr[T_{ij}=1|T_{i,j-1}=1,x_{i1},\ldots,x_{ij},Y_{i1},\ldots,Y_{i,j-1}]}
#' are estimated by using a logistic model whose covariates are given by \eqn{z_{1},\ldots,z_{r}}. Then, those
#' probabilities are used to computed the weights to be included in the parameter estimation algorithm.
#'
#' A set of standard extractor functions for fitted model objects is available for objects of class  \emph{glmgee},
#' including methods to the generic functions such as \code{print}, \code{summary},	\code{model.matrix}, \code{estequa},
#' \code{coef}, \code{vcov}, \code{fitted}, \code{confint} and \code{predict}. The input data are assumed to be ordered
#' in time within each cluster.
#'
#' @return an object of class \emph{wglmgee} in which the main results of the weighted GEE model fitted to the data are stored,
#' i.e., a list with components including
#' \tabular{ll}{
#' \code{coefficients} \tab a vector with the estimates of \eqn{\beta_1,\ldots,\beta_p},\cr
#' \tab \cr
#' \code{fitted.values}\tab a vector with the estimates of \eqn{\mu_{ij}} for \eqn{i=1,\ldots,n} and \eqn{j=1,\ldots,n_i},\cr
#' \tab \cr
#' \code{start}        \tab a vector with the starting values used,\cr
#' \tab \cr
#' \code{iter}         \tab a numeric constant with the number of iterations,\cr
#' \tab \cr
#' \code{prior.weights}\tab a vector with the values of \eqn{\omega_{ij}} for \eqn{i=1,\ldots,n} and \eqn{j=1,\ldots,n_i},\cr
#' \tab \cr
#' \code{offset}       \tab a vector with the values of \eqn{z_{ij}} for \eqn{i=1,\ldots,n} and \eqn{j=1,\ldots,n_i},\cr
#' \tab \cr
#' \code{terms}        \tab an object containing the terms objects,\cr
#' \tab \cr
#' \code{estfun}       \tab a vector with the estimating equations evaluated at the parameter\cr
#'                     \tab estimates and the observed data,\cr
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
#' \code{y}            \tab a vector with the values of \eqn{y_{ij}} for \eqn{i=1,\ldots,n} and \eqn{j=1,\ldots,n_i},\cr
#' \tab \cr
#' \code{family}       \tab an object containing the \link{family} object used,\cr
#' \tab \cr
#' \code{linear.predictors} \tab a vector with the estimates of \eqn{g(\mu_{ij})} for \eqn{i=1,\ldots,n} and \eqn{j=1,\ldots,n_i},\cr
#' \tab \cr
#' \code{R}            \tab a matrix with the (robust) estimate of the variance-covariance,\cr
#' \tab \cr
#' \code{corr}         \tab a matrix with the estimate of the working-correlation,\cr
#' \tab \cr
#' \code{corstr}       \tab a character string specifying the working-correlation structure,\cr
#' \tab \cr
#' \code{level}        \tab a character string specifying the weighted GEE method,\cr
#' \tab \cr
#' \code{id}           \tab a vector which identifies the subjects or clusters,\cr
#' \tab \cr
#' \code{sizes}        \tab a vector with the values of \eqn{n_i} for \eqn{i=1,\ldots,n},\cr
#' \tab \cr
#' \code{call}         \tab the original function call,\cr
#' }
#' @seealso \link{glmgee}, \link{gnmgee}
#' @export wglmgee
#' @references Fitzmaurice G.M., Laird N.M., Ware J.H. (2011). \emph{Applied Longitudinal Analysis. 2nd ed.} John Wiley & Sons.
#' @references Preisser J.S., Lohman K.K., Rathouz P.J. (2002). Performance of Weighted Estimating Equations for
#' Longitudinal Binary Data with Drop-Outs Missing at Random. \emph{Statistics in Medicine} 21:3035–3054.
#' @references Robins J.M., Rotnitzky A., Zhao L.P. (1995) Analysis of Semiparametric Regression Models for Repeated
#' Outcomes in the Presence of Missing Data. \emph{Journal of the American Statistical Association} 90:122–129.
#' @references \href{https://journal.r-project.org/articles/RJ-2023-056/}{Vanegas L.H., Rondon L.M., Paula G.A. (2023) Generalized Estimating Equations using the new R package glmtoolbox.
#' \emph{The R Journal} 15:105-133.}
#' @examples
#' ###### Example: Amenorrhea rates over time
#' data(amenorrhea)
#' amenorrhea2 <- within(amenorrhea,{
#'								 Ctime <- factor(Time)
#'								 Ctime <- relevel(Ctime,ref="1")
#'								 ylag1 <- c(0,amenorrhea[-length(ID)])
#'								 ylag1 <- ifelse(Time==0,0,ylag1)})
#'
#' mod <- amenorrhea ~ poly(Time,2) + Dose | Ctime + Dose + ylag1
#'
#' ### Observation-specified Weighted GEE
#' fit1 <- wglmgee(mod, family=binomial, data=amenorrhea2, id=ID,
#'                 corstr="AR-M-dependent(1)", level="observations")
#' summary(fit1)
#'
#' ### Cluster-specified Weighted GEE
#' fit2 <- wglmgee(mod, family=binomial, data=amenorrhea2, id=ID,
#'                 corstr="Exchangeable", level="clusters")
#' summary(fit2)
#'
wglmgee <- function(formula,level=c("observations","clusters"),family=gaussian(),weights,id,data,subset,corstr,corr,start=NULL,scale.fix=FALSE,scale.value=1,toler=0.00001,maxit=50,trace=FALSE,...){
  if(missingArg(data)) data <- environment(eval(formula))
  if(missingArg(corstr)) corstr <- "Independence"
  level <- match.arg(level)
  if(missing(data)) data <- environment(formula)
  mmf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "id", "weights"), names(mmf), 0)
  mmf <- mmf[c(1,m)]
  mmf$drop.unused.levels <- TRUE
  mmf[[1]] <- as.name("model.frame")
  mmf$formula <- Formula(formula)
  mmf$na.action <- na.pass
  mmf <- eval(mmf, parent.frame())
  y <- as.matrix(model.part(Formula(formula), data = mmf, lhs=1))
  if(all(!is.na(y))) stop("There is not missing values in the response variable!!",call.=FALSE)
  mx <- model.part(Formula(formula), data = mmf, rhs = 1, terms = TRUE)
  X <- model.matrix(mx, data = mmf); p <- ncol(X)
  offs <- model.offset(mx)
  if(is.null(offs)) offs <- matrix(0,nrow(X),1)
  if(suppressWarnings(formula(Formula(formula),lhs=0,rhs=2)=="~0")) mz <- mx
  else mz <- model.part(Formula(formula), data = mmf, rhs = 2, terms = TRUE)
  Z <- model.matrix(mz, data = mmf); q <- ncol(Z)
  weights <- model.weights(mmf)
  if(is.null(weights)) weights <- matrix(1,nrow(X),1)
  if(any(weights <= 0)) stop("Only positive weights are allowed!!",call.=FALSE)
  id <- model.extract(mmf,id)
  if(is.null(id)) stop("The argument id is required!!",call.=FALSE)
  id2 <- as.matrix(unique(id))
  nclus <- length(id2)
  sizes <- matrix(0,nclus,1)
  sizes2 <- matrix(0,nclus,1)
  if(is(family,"function")) family <- family()
  if(family$family %in% c("quasi","quasibinomial","quasipoisson")){
    if(family$family=="quasi"){
      family$family <- switch(family$varfun,"constant"="gaussian","mu(1-mu)"="binomial",
                              "mu"="poisson","mu^2"="Gamma","mu^3"="inverse.gaussian")
    }
    else{
      family$family <- switch(family$family,"quasibinomial"="binomial","quasipoisson"="poisson")
    }
    family <- do.call(family$family,list(link=family$link))
    family2 <- family
  }
  else{
    if(family$family %in% c("gaussian","binomial","poisson","Gamma","inverse.gaussian")){
      varfun <- switch(family$family,"gaussian"="constant","binomial"="mu(1-mu)",
                       "poisson"="mu","Gamma"="mu^2","inverse.gaussian"="mu^3")
      family2 <- do.call("quasi",list(variance=varfun,link=family$link))
      if(family$family=="binomial") family2 <- do.call("quasibinomial",list(link=family$link))
      if(family$family=="poisson") family2 <- do.call("quasipoisson",list(link=family$link))
    }else family2 <- family
  }
  datas <- list()
  for(i in 1:nclus){
    places <- id==id2[i]
    sizes[i] <- sum(!is.na(y[places]))
    sizes2[i] <- sum(places)
    datas[[i]] <- cbind(matrix(Z[places,],ncol=q),!is.na(y[places]),matrix(X[places,],ncol=p),y[places],
                        offs[places],weights[places],1:sum(places),c(1:sum(places)) <= sizes[i]+1)
  }
  if(level=="observations" & var(sizes2)>0) stop("Observation-specific weighted GEE requires the same number of observations per subject/cluster!!",call.=FALSE)
  dropout <- do.call(rbind,lapply(datas,function(x) c(sum(x[,q+1]),sum(x[,q+1]*x[,p+q+5]))))
  dropout <- dropout[,1]*(dropout[,1]+1)/2 != dropout[,2]
  if(any(dropout)) stop(paste("There are",sum(dropout),"subjects/clusters with intermixed missing
                               values. Intermixed missing values are not supported!!"),call.=FALSE)
  Zym <- do.call(rbind,datas)
  ones <- Zym[,p+q+6]==1
  if(any(is.na(Zym[,(q+2):(q+p)])) & level=="observations")
    stop(paste("There are",sum((is.na(Zym[,(q+2):(q+p)]))),"missing values in the covariates of the GEE model."),call.=FALSE)
  if(any(is.na(Zym[ones,(q+2):(q+p)])) & level=="clusters")
    stop(paste("There are",sum((is.na(Zym[ones,(q+2):(q+p)]))),"missing values in the covariates of the GEE model."),call.=FALSE)
  colnamesZ <- colnames(Z)
  Z <- matrix(Zym[ones,1:q],ncol=q)
  ym <- Zym[ones,q+1]
  if(any(is.na(Z))) stop(paste("There are",sum(is.na(Z)),"missing values in the covariates of the missingness model."),call.=FALSE)
  fitmiss <- glm(ym ~ 0 + Z, family=binomial, subset={Zym[ones,p+q+5]>1})
  vl <- !is.na(coef(fitmiss))
  vl2 <- c(!is.na(coef(fitmiss)),rep(FALSE,ncol(datas[[1]])-q))
  fsm <- function(x){
    x <- x[1:sum(x[,p+q+6]),]
    return(tcrossprod(crossprod(x[,vl2],x[,q+1]-fitmiss$family$linkinv(x[,vl2]%*%coef(fitmiss)[vl]))))
  }
  B <- Reduce('+',lapply(datas,fsm))
  for(i in 1:length(id2)){
    probsi <- fitmiss$family$linkinv(datas[[i]][,vl2]%*%coef(fitmiss)[vl])
    probsi[1] <- 1
    if(level=="clusters"){
      ones <- datas[[i]][,p+q+6]
      if(sum(ones) > sizes[i]){
        probsi2 <- probsi
        probsi2[sum(ones)] <- 1 - probsi2[sum(ones)]
        datas[[i]] <- cbind(datas[[i]],probsi,1/prod(probsi2[1:sum(ones)]))
      }else datas[[i]] <- cbind(datas[[i]],probsi,1/prod(probsi))
    }
    else{
      probsi <- ifelse(is.na(probsi),0,probsi)
      datas[[i]] <- cbind(datas[[i]],probsi,ifelse(is.na(datas[[i]][,p+q+2]),0,1/cumprod(probsi)))
    }
  }
  maxsize <- max(sizes)
  if(maxsize==1) corstr <- "Independence"
  Rhat <- function(D2,beta){
    D <- matrix(na.omit(D2[,-c(1:(q+1))]),ncol=p+7)
    etai <- D[,1:p]%*%beta + D[,p+2]
    mui <- family$linkinv(etai)
    if(level=="observations") wi <- sqrt(family$variance(mui)/(D[,p+3]))
    else wi <- sqrt(family$variance(mui)/(D[,p+3]*D[,p+7]))
    es <- rep(0,maxsize)
    es2 <- es
    es[D[,p+4]] <- (D[,p+1]-mui)/wi
    es2[D[,p+4]] <- 1
    nums <- tcrossprod(es)
    nums2 <- tcrossprod(es2)
    cbind(nums,nums2)
  }
  temp <- strsplit(gsub("[[:space:]]","",corstr),"[()]")[[1]]
  args <- c("Independence","Unstructured","Stationary-M-dependent",
            "Non-Stationary-M-dependent","AR-M-dependent","Exchangeable","User-defined")
  temp2 <- grep(tolower(temp[1]),tolower(args))
  if(length(temp)>0){
    corstr <- args[temp2[1]]
    if(corstr=="Stationary-M-dependent" | corstr=="Non-Stationary-M-dependent" | corstr=="AR-M-dependent"){
      M <- min(max(1,floor(abs(as.numeric(temp[2])))),maxsize-1)
      if(is.na(M)) M <- 1
      attr(corstr,"M") <- M
    }
  }else stop("Correlation structure should be one of Independence, Unstructured, Stationary-M-dependent,
 		            Non-Stationary-M-dependent, AR-M-dependent, Exchangeable and User-defined!!",call.=FALSE)
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
    if(corstr=="AR-M-dependent"){
      alphas <- matrix(0,M,1)
      R <- matrix(0,maxsize,maxsize)
      for(i in 1:M){
        inds <- cbind(1:(maxsize-i),(1+i):maxsize)
        inds2 <- cbind(inds[,1],maxsize+inds[,2])
        alphas[i] <- sum(resume[inds])/(sum(resume[inds2])-p)
      }
      alphas <- alphas/phi
      alphasf <- function(x) if(x==0) return(1) else return(alphas[x])
      phises <- crossprod(chol2inv(chol(apply(toeplitz(0:(M-1)),c(1,2),alphasf))),alphas)
      rhos <- matrix(0,maxsize-1,1)
      rhos[1:M] <- alphas
      for(i in (M+1):(maxsize-1)) rhos[i] <- sum(phises*rhos[i-c(1:M)])
      R <- toeplitz(c(1,rhos))
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
  score <- function(D2,beta,out){
    if(level=="observations"){
      D <- matrix(D2[,-c(1:(q+1))],ncol=p+7)
      W <- D[,p+7]
      W2 <- D[,p+3]
    }
    else{
      D <- matrix(na.omit(D2[,-c(1:(q+1))]),ncol=p+7)
      W <- rep(1,length(D[,p+7]))
      W2 <- D[,p+3]*D[,p+7]
    }
    Xi <- matrix(D[,1:p],ncol=p)
    yi <- ifelse(is.na(D[,p+1]),0,D[,p+1])
    ni <- nrow(Xi)
    etai <- Xi%*%beta + D[,p+2]
    mui <- family$linkinv(etai)
    wi <- sqrt(family$variance(mui)/W2)
    Xiw <- Xi*matrix(family$mu.eta(etai),nrow(Xi),p)
    Vi <- t(R[D[,p+4],D[,p+4]]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
    if(corstr=="Independence") Vi2 <- diag(as.vector(1/wi[D[,p+4]]^2),ni,ni)
    else{
      Vi2 <- try(chol(Vi),silent=TRUE)
      if(is.matrix(Vi2)) Vi2 <- chol2inv(Vi2) else Vi2 <- solve(Vi)
    }
    Xiw2 <- crossprod(Vi2,Xiw)
    ones <- D2[,p+q+6]
    Si <- crossprod(D2[1:sum(ones),vl2],D2[1:sum(ones),q+1]-D2[1:sum(ones),p+q+7])
    UiSi <- tcrossprod(crossprod(Xiw2,(yi-mui)*W),Si)
    cbind(crossprod(Xiw2,(yi-mui)*W),crossprod(Xiw2,Xiw),UiSi)
  }
  options(warn=-1)
  if(is.null(start)){
    beta_new <- try(glm.fit(y=y[!is.na(y)],x=X[!is.na(y),],family=family2,weights=weights[!is.na(y)],offset=offs[!is.na(y)]),silent=TRUE)
    if(is.list(beta_new)){
      if(beta_new$converged) beta_new <- beta_new$coefficients
      else{beta_new <- try(glm.fit(y=y[!is.na(y)],x=X[!is.na(y),],family=family,weights=weights[!is.na(y)],offset=offs[!is.na(y)]),silent=TRUE)
      if(is.list(beta_new)){
        if(beta_new$converged) beta_new <- beta_new$coefficients
        else stop("cannot find valid starting values: please specify some!!",call.=FALSE)
      }
      }
    }else{beta_new <- try(glm.fit(y=y[!is.na(y)],x=X[!is.na(y),],family=family,weights=weights[!is.na(y)],offset=offs[!is.na(y)]),silent=TRUE)
    if(is.list(beta_new)){
      if(beta_new$converged) beta_new <- beta_new$coefficients
      else stop("cannot find valid starting values: please specify some!!",call.=FALSE)
    }
    }
  }else beta_new <- start
  options(warn=0)
  start <- beta_new
  tol <- 1
  niter <- 0
  if(missingArg(corr)) corr <- diag(maxsize)
  if(trace) message("\nIteration  Convergence Criterion")
  vl2 <- c(!is.na(coef(fitmiss)),rep(FALSE,ncol(datas[[1]])-q))
  while(tol > toler & niter < maxit){
    beta_old <- beta_new
    resume <- 0
    for(i in 1:nclus) resume <- resume + Rhat(datas[[i]],beta=beta_old)
    phi <- sum(diag(resume[1:maxsize,1:maxsize]))/(sum(sizes)-p)
    if(corstr!="User-defined" & corstr!="Independence") R <- Rout(resume,corstr)
    else R <- as.matrix(corr)
    resume2 <- 0
    for(i in 1:nclus) resume2 <- resume2 + score(datas[[i]],beta=beta_old,out=FALSE)
    kchol <- try(chol(resume2[1:p,2:(p+1)]),silent=TRUE)
    if(is.matrix(kchol)) kchol <- chol2inv(kchol) else kchol <- solve(resume2[1:p,2:(p+1)])
    beta_new <- beta_old + crossprod(kchol,resume2[,1])
    tol <- max(abs((beta_new-beta_old)/beta_old))
    niter <- niter + 1
    if(trace) message("    ",niter,"            ",signif(tol,digits=5))
  }
  rownames(beta_new) <- colnames(X)
  colnames(beta_new) <- ""
  estfun <- as.matrix(resume2[,1])
  rownames(estfun) <- colnames(X)
  colnames(estfun) <- ""
  if(niter==maxit) warning("Iteration limit exceeded!!\n",call.=FALSE)
  eta <- X%*%beta_new + offs
  mu <- family$linkinv(eta)
  phi <- sum(diag(resume[1:maxsize,1:maxsize]))/(sum(sizes)-p)
  phi <- ifelse(scale.fix,scale.value,phi)
  sera <- try(chol(R),silent=TRUE)
  if(!is.matrix(sera)) warning("Estimate of correlation matrix is not positive definite",call.=FALSE)
  rownames(R) <- paste("[",1:maxsize,"]",sep="")
  colnames(R) <- paste("[",1:maxsize,"] ",sep="")
  A <- resume2[,-c(1:(p+1))]
  B2 <- try(chol(B),silent=TRUE)
  if(is.matrix(B2)) B2 <- chol2inv(B2) else B2 <- solve(B)
  AB <- tcrossprod(A,B2)
  score2 <- function(D2,beta,out){
    if(level=="observations"){
      D <- matrix(D2[,-c(1:(q+1))],ncol=p+7)
      W <- D[,p+7]
      W2 <- D[,p+3]
    }
    else{
      D <- matrix(na.omit(D2[,-c(1:(q+1))]),ncol=p+7)
      W <- rep(1,length(D[,p+7]))
      W2 <- D[,p+3]*D[,p+7]
    }
    Xi <- matrix(D[,1:p],ncol=p)
    yi <- ifelse(is.na(D[,p+1]),0,D[,p+1])
    ni <- nrow(Xi)
    etai <- Xi%*%beta + D[,p+2]
    mui <- family$linkinv(etai)
    wi <- sqrt(family$variance(mui)/W2)
    Xiw <- Xi*matrix(family$mu.eta(etai),nrow(Xi),p)
    Vi <- t(R[D[,p+4],D[,p+4]]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
    if(corstr=="Independence") Vi2 <- diag(as.vector(1/wi[D[,p+4]]^2),ni,ni)
    else{
      Vi2 <- try(chol(Vi),silent=TRUE)
      if(is.matrix(Vi2)) Vi2 <- chol2inv(Vi2) else Vi2 <- solve(Vi)
    }
    Xiw2 <- crossprod(Vi2,Xiw)
    ones <- D2[,p+q+6]
    Si <- crossprod(D2[1:sum(ones),vl2],D2[1:sum(ones),q+1]-D2[1:sum(ones),p+q+7])
    Ei <- crossprod(Xiw2,(yi-mui)*W) - AB%*%Si
    cbind(crossprod(Xiw2,Xiw),tcrossprod(Ei))
  }
  EE <- 0
  for(i in 1:nclus) EE <- EE + score2(datas[[i]],beta=beta_new,out=FALSE)
  EE2 <- try(chol(EE[,1:p]),silent=TRUE)
  if(is.matrix(EE2)) EE2 <- chol2inv(EE2) else EE2 <- solve(EE[,1:p])
  vcovs <- tcrossprod(crossprod(EE2,EE[,(p+1):(2*p)]),EE2)
  missingm <- cbind(coef(fitmiss)[vl],sqrt(diag(vcov(fitmiss)[vl,vl])))
  missingm <- cbind(missingm,missingm[,1]/missingm[,2],1-pchisq((missingm[,1]/missingm[,2])^2,1))
  rownames(missingm) <- colnamesZ[vl]
  colnames(missingm) <- c("Estimate", "Std.Error", "z-value", "Pr(>|z|)")
  out_ <- list(coefficients=beta_new,fitted.values=mu,linear.predictors=eta,sizes=sizes,arrangedata=datas,
               prior.weights=weights,y=y,formula=formula,call=match.call(),offset=offs,model=mmf,data=data,score=score,ids=id2,
               converged=ifelse(niter<maxit,TRUE,FALSE),estfun=estfun,R=vcovs,terms=terms(mx),family=family,Rout=Rout,Rhat=Rhat,id=id,
               phi=phi,corr=R,clusters=c(length(sizes),max(sizes),min(sizes)),corstr=corstr,missingm=missingm,level=level,
               levels=.getXlevels(attr(mmf,"terms"),mmf),contrasts=attr(X,"contrasts"),start=start,iter=niter,dmm=q)
  class(out_) <- "wglmgee"
  return(out_)
}

#' @method coef wglmgee
#' @export
coef.wglmgee <- function(object,...){
  out_ <- object$coefficients
  colnames(out_) <- "Estimates"
  return(out_)
}

#' @method model.matrix wglmgee
#' @export
model.matrix.wglmgee <-	function(object,...){
  out_ <- model.matrix(object$terms, object$model, contrasts=object$contrasts)
  return(out_)
}

#' @method fitted wglmgee
#' @export
fitted.wglmgee <- function(object,...) return(object$fitted.values)

#' @method print wglmgee
#' @export
print.wglmgee <- function(x, ...){
  .Theta <- function() return(.Theta)
  environment(.Theta) <- environment(x$family$variance)
  maxsize <- ncol(x$corr)
  ms <- matrix(".",maxsize+2,maxsize+3)
  for(i in 1:maxsize){
    ms[i,1:i] <- "X"
    ms[i,maxsize+2] <- sum(x$sizes==i)
    ms[i,maxsize+3] <- round(100*sum(x$sizes==i)/length(x$sizes),1)
  }
  ms[,maxsize+1] <- "|"
  ms[maxsize+1,-(maxsize+1)] <- "----"
  ms[maxsize+2,maxsize+2] <- length(x$sizes)
  ms[maxsize+2,maxsize+3] <- 100
  ms[maxsize+2,1:maxsize] <- ""
  ms <- as.data.frame(ms)
  colnames(ms) <- c("Time 1",2:maxsize,"","Freq","% ")
  ms <- rbind(ms[maxsize+1,],ms)
  cat("\nClusters by dropout pattern\n")
  print(ms[ms[,maxsize+2]!=0,], row.names=FALSE)
  cat("*************************************************************")
  if(x$level=="observations") cat("\nObservation-specific Weighted GEE\n")
  else cat("\nCluster-specific Weighted GEE\n")
  cat("\n        Variance function: ",x$family$family)
  cat("\n            Link function: ",x$family$link)
  cat("\n    Correlation structure: ",ifelse(grepl("M-dependent",x$corstr),paste(x$corstr,"(",attr(x$corstr,"M"),")",sep=""),x$corstr),"\n")
}
#' @method predict wglmgee
#' @export
predict.wglmgee <- function(object, ...,newdata, se.fit=FALSE, type=c("link","response")){
  type <- match.arg(type)
  if(missingArg(newdata)){
    predicts <- object$linear.predictors
  }
  else{
    newdata <- data.frame(newdata)
    mf <- model.frame(delete.response(object$terms),newdata,xlev=object$levels)
    X <- model.matrix(delete.response(object$terms),mf,contrasts=object$contrasts)
    predicts <- X%*%object$coefficients
    offs <- model.offset(mf)
    if(!is.null(offs)) predicts <- predicts + offs
  }
  if(type=="response") predicts <- object$family$linkinv(predicts)
  if(se.fit){
    varhat <- vcov(object)
    se <- matrix(sqrt(apply(tcrossprod(X,varhat)*X,1,sum)),ncol=1)
    if(type=="response") se <- se*abs(object$family$mu.eta(object$family$linkfun(predicts)))
    predicts <- cbind(predicts,se)
    colnames(predicts) <- c("fit","se.fit")
  }else colnames(predicts) <- c("fit")
  return(predicts)
}

#' @method estequa wglmgee
#' @export
estequa.wglmgee <- function(object,...){
  out_ <- object$estfun/object$phi
  colnames(out_) <- " "
  return(out_)
}

#' @method vcov wglmgee
#' @export
vcov.wglmgee <- function(object,...){
  out_ <- object$R
  rownames(out_) <- colnames(out_) <- rownames(object$coefficients)
  return(out_)
}

#' @method confint wglmgee
#' @export
confint.wglmgee <- function(object,parm,level=0.95,contrast,digits=max(3, getOption("digits") - 2),verbose=TRUE,...){
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
  return(invisible(round(results,digits=digits)))
}

#' @method summary wglmgee
#' @export
summary.wglmgee <- function(object,...,digits=max(3, getOption("digits") - 2),corr.digits=3,corr=TRUE){
  .Theta <- function() return(.Theta)
  environment(.Theta) <- environment(object$family$variance)
  maxsize <- ncol(object$corr)
  ms <- matrix(".",maxsize+2,maxsize+3)
  for(i in 1:maxsize){
    ms[i,1:i] <- "X"
    ms[i,maxsize+2] <- sum(object$sizes==i)
    ms[i,maxsize+3] <- round(100*sum(object$sizes==i)/length(object$sizes),1)
  }
  ms[,maxsize+1] <- "|"
  ms[maxsize+1,-(maxsize+1)] <- "----"
  ms[maxsize+2,maxsize+2] <- length(object$sizes)
  ms[maxsize+2,maxsize+3] <- 100
  ms[maxsize+2,1:maxsize] <- ""
  ms <- as.data.frame(ms)
  colnames(ms) <- c("Time 1",2:maxsize,"","Freq","% ")
  ms <- rbind(ms[maxsize+1,],ms)
  cat("\nClusters by dropout time\n")
  print(ms[ms[,maxsize+2]!=0,], row.names=FALSE)
  cat("*************************************************************")
  cat("\nMissingness model\n")
  cat("Coefficients\n")
  printCoefmat(object$missingm, P.values=TRUE, signif.stars=TRUE, has.Pvalue=TRUE, digits=digits, dig.tst=digits, signif.legend=FALSE, tst.ind=c(1,2,3), na.print="")
  cat("*************************************************************")
  if(object$level=="observations"){
    cat("\nObservation-specific Weighted GEE\n")
    ws <- do.call(rbind,object$arrangedata)
    ws <- ws[ws[,object$dmm+1]==1,ncol(ws)]
  }else{
    cat("\nCluster-specific Weighted GEE\n")
    ws <- do.call(rbind,lapply(object$arrangedata,function(x)min(x[,ncol(x)])))
  }
  out_ <- matrix(quantile(ws,probs=c(0,0.05,0.1,0.25,0.5,0.75,0.9,0.95,1)),1,9)
  rownames(out_) <- " ";colnames(out_) <- c(" Min"," 5%"," 10%"," 25%"," 50%"," 75%"," 90%"," 95%"," Max")
  cat("Weights\n")
  print(out_,digits=3)
  cat("\n        Variance function: ",object$family$family)
  cat("\n            Link function: ",object$family$link)
  cat("\n    Correlation structure: ",ifelse(grepl("M-dependent",object$corstr),paste(object$corstr,"(",attr(object$corstr,"M"),")",sep=""),object$corstr))
  cat("\n*************************************************\n")
  cat("Coefficients\n")
  TAB	<- rbind(cbind(Estimate <- object$coefficients,
                     StdErr <- sqrt(diag(object$R)),
                     tval <- Estimate/StdErr,
                     p.value <- 2*pnorm(-abs(tval))),
               rep(NA,4),
               c(object$phi,NA,NA,NA))
  colnames(TAB) <- c("Estimate", "Std.Error", "z-value", "Pr(>|z|)")
  rownames(TAB) <- c(rownames(object$coefficients),"","Dispersion")
  printCoefmat(TAB, P.values=TRUE, signif.stars=TRUE, has.Pvalue=TRUE, digits=digits, dig.tst=digits, signif.legend=FALSE, tst.ind=c(1,2,3), na.print="")
  cat("*************************************************\n")
  if(corr){
    cat("Working correlation\n")
    print(round(object$corr,digits=corr.digits))
  }
  return(invisible(list(coefficients=round(TAB,digits=digits),dispersion=object$phi,corr=round(object$corr,digits=corr.digits))))
}


