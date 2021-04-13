geeglm <- function(formula,family=gaussian(),weights,id,data,subset,corstr,corr,start,maxit=50,adjr2=FALSE,...){
  if(missingArg(data)) data <- environment(formula)
  if(missingArg(corstr)) corstr <- "Independence"
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "weights", "data", "subset", "id"), names(mf), 0L)
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
  if(family$family=="binomial") family2$initialize <- expression({n <- rep.int(1, nobs)
  mustart <- pmax(0.1, pmin(0.9, y))})
  if(ncol(y)==2 & family$family=="binomial"){
    weights <- as.matrix(y[,1]+y[,2])
    y <- as.matrix(y[,1]/weights)
  }  
  offset <- as.vector(model.offset(mf))
  X <- model.matrix(mt, mf)
  p <- ncol(X)
  q <- p
  n <- nrow(X)
  if(is.null(offset)) offs <- matrix(0,n,1) else offs <- as.matrix(offset)
  if(is.null(weights)) weights <- matrix(1,n,1) else weights <- as.matrix(weights)
  if(any(weights <= 0)) stop("Only positive weights are allowed!!",call.=FALSE)
  id <- factor(model.extract(mf,id))
  id2 <- as.matrix(levels(id))
  nclus <- length(id2)
  sizes <- matrix(0,nclus,1)
  datas <- list()
  for(i in 1:nclus){
    places <- id==id2[i]
    datas[[i]] <- cbind(matrix(X[places,],ncol=p),y[places,],offs[places],weights[places])
    sizes[i] <- sum(places)
  }
  maxsize <- max(sizes)
  Rhat <- function(D,beta){
    etai <- tcrossprod(D[,1:p],t(beta)) + D[,q+2]
    mui <- family$linkinv(etai)
    wi <- sqrt(family$variance(mui)/D[,q+3])
    es <- (D[,q+1]-mui)/wi
    es2 <- rep(1,length(es)) 
    if(length(es) < maxsize){
      es <- c(es,rep(0,maxsize-length(es)))
      es2 <- c(es2,rep(0,maxsize-length(es2)))			 
    }
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
      for(i in 1:M) alphas[i] <- sum(resume[cbind(1:(maxsize-i),(i+1):maxsize)])/(sum(sizes-i)-p)
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
      alpha <- sum(resume[cbind(1:(maxsize-1),2:maxsize)])/(sum(sizes-1)-p)
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
    Vi <- t(R[1:ni,1:ni]*matrix(wi,ni,ni))*matrix(wi,ni,ni)
    Xiw <- Xi*matrix(family$mu.eta(etai),nrow(Xi),p)
    Vi2 <- solve(Vi)
    if(out) cbind(crossprod(Xiw,Vi2%*%(yi-mui)),crossprod(Xiw,Vi2%*%Xiw))
    else cbind(crossprod(Xiw,Vi2%*%(yi-mui)),crossprod(Xiw,Vi2%*%Xiw),crossprod(Xiw,Vi2%*%(tcrossprod(yi-mui))%*%Vi2%*%Xiw))
  }
  if(missingArg(start)){
    fit0 <- glm.fit(y=y, x=X, offset=offs, weights=weights, family=family2)
    beta_new <- coef(fit0)
  }else beta_new <- start  
  tol <- 1
  niter <- 0
  if(missingArg(corr)) corr <- diag(maxsize)
  while(tol > 0.0000001 & niter < maxit){
    beta_old <- beta_new
    resume <- Reduce('+',lapply(datas,Rhat,beta=beta_old))
    phi <- sum(diag(resume[1:maxsize,1:maxsize]))/(sum(sizes)-p)
    if(corstr!="User-defined" & corstr!="Independence") R <- Rout(resume,corstr)
    else R <- as.matrix(corr)
    resume2 <- Reduce('+',lapply(datas,score,beta=beta_old,out=TRUE))
    beta_new <- beta_old + solve(resume2[,-1])%*%resume2[,1]
    tol <- max(abs((beta_new-beta_old)/beta_old))
    niter <- niter + 1
  }
  rownames(beta_new) <- colnames(X)
  colnames(beta_new) <- ""		
  if(niter==maxit) cat("Iteration limit exceeded!!\n",call.=FALSE)
  eta <- tcrossprod(X,t(beta_new)) + offs
  mu <- family$linkinv(eta)
  nano <- list(...)
  if(is.null(nano$si.mu.la.tion)){
    resume2 <- Reduce('+',lapply(datas,score,beta=beta_new,out=FALSE))
    I0 <- solve(resume2[1:p,2:(p+1)])
    I1 <- resume2[1:p,(p+2):(2*p+1)]
    vcovs <- I0%*%I1%*%I0
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
    QIC <- -2*sum(qll)/phi + 2*sum(diag(solve(vcov0)%*%vcovs))
    CIC <- sum(diag(solve(vcov0)%*%vcovs))
    logLik <- sum(qll)/phi
    rownames(R) <- paste("[",1:max(sizes),"]",sep="")
    colnames(R) <- paste("[",1:max(sizes),"] ",sep="")
    estfun <- as.matrix(resume2[,1])/phi
    rownames(estfun) <- colnames(X)
    colnames(estfun) <- ""
    out_ <- list(coefficients=beta_new,fitted.values=mu,linear.predictors=eta,sizes=sizes,arrangedata=datas,
                 prior.weights=weights,y=y,formula=formula,call=match.call(),offset=offs,model=mf,data=data,score=score,ids=id2,
                 converged=ifelse(niter<maxit,TRUE,FALSE),estfun=estfun,R=vcovs,terms=mt,family=family,Rout=Rout,Rhat=Rhat,id=id,
                 phi=phi,QIC=QIC,CIC=CIC,logLik=logLik,corr=R,clusters=c(length(sizes),max(sizes),min(sizes)),corstr=corstr,
                 deviance=sum(family$dev.resids(y,mu,weights)),df.residual=length(y)-length(beta_new))
    class(out_) <- "geeglm"
    if(colnames(X)[1]=="(Intercept)" & adjr2==TRUE){
      p <- 1
      fit0 <- glm.fit(y=y, x=rep(1,length(out_$y)), offset=offs, weights=weights, family=family2)
      beta_new <- coef(fit0)
      tol <- 1
      niter <- 0
      while(tol > 0.0000001 & niter < maxit){
        beta_old <- beta_new
        resume <- Reduce('+',lapply(datas,Rhat,beta=beta_old))
        phi <- sum(diag(resume[1:maxsize,1:maxsize]))/(sum(sizes)-p)
        if(corstr!="User-defined") R <- Rout(resume,corstr)
        else R <- as.matrix(corr)
        resume2 <- Reduce('+',lapply(datas,score,beta=beta_old,out=TRUE))
        beta_new <- beta_old + solve(resume2[,-1])%*%resume2[,1]
        tol <- max(abs((beta_new-beta_old)/beta_old))
        niter <- niter + 1
      }
      if(niter < maxit){
        mu0 <- family$linkinv(rep(beta_new,length(out_$y)) + offs)
        out_$null.deviance <- sum(family$dev.resids(y,mu0,weights))
        out_$df.null <- length(y)-length(beta_new)			  
      }
    }
    return(out_)
  }else{out_ <- list(coefficients=beta_new,converged=TRUE,arrangedata=datas,phi=phi,family=family,fitted.values=mu,corr=R,ids=id2,y=y)
  class(out_) <- "geeglm"
  return(out_)
  }
}
