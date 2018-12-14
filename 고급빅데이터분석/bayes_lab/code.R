# rm(list=ls());gc()


#--- 2. 켤레사전분포를 이용한 사후분포 계산

# 이항분포-베타사전분포 예제
a=13; b=17
a/(a+b) # mean
a*b/((a+b)^2*(a+b+1)) # variance
c(qbeta(0.025,a,b),qbeta(0.975,a,b)) # credible interval

# likelihood, prior, posterior 분포 비교
x=seq(from=0,to=1,length.out=1000)
prior=dbeta(x,5,5)
post=dbeta(x,13,17)
loglik=dbeta(x,9,13)
plot(x,post,type="l",col=2,xlab="theta",ylab="density",lwd=3)
lines(x,prior,lwd=3,col=3)
lines(x,loglik,lwd=3,col=1)
legend("topright", c("posterior","prior", "likelihood"), 
      lwd=rep(3,4), col=c(2,3,1))



#--- 3. MCMC 알고리즘

#-- 3-1. 몬테카를로 방법
# 원주율 구하기
CalculatePi <- function(n)
{
  count <- 0
  
  par(mfrow=c(1,1))
  x=seq(-1,1, 0.01)
  y=sqrt(1-x^2)
  plot(x,y,type="l", xlim=c(-1,1), ylim=c(-1,1), axes = F, xlab="", ylab="")
  lines(x,-y,type="l", xlim=c(-1,1), ylim=c(-1,1))
  lines(x=seq(-1,1, 0.01), rep(1, 201))
  lines(x=seq(-1,1, 0.01), rep(-1, 201))
  lines(rep(-1, 201),seq(-1,1, 0.01))
  lines(rep(1, 201),seq(-1,1, 0.01))
  
  for (i in 1: n) {
    coord <- runif(2, min=-1, max=1)
    if (sqrt(coord[1]^2+coord[2]^2)<=1)
    {
      count <- count+1
      points(coord[1],coord[2], col="red", pch=20)
    }
    else
    {
      points(coord[1],coord[2], col="blue", pch=20)
    }
  }
  return(4*count/n)
}

set.seed(2017)
CalculatePi(100)

CalculatePi(1000)

CalculatePi(5000)


# 샘플링한 theta 의 분포는 표본 크기를 늘려감에 따라 사후분포로 수렴
set.seed(100)
x=seq(from=0,to=1,length.out=1000)
post=dbeta(x,13,17)
theta=rbeta(10000,13,17)
par(mfrow=c(1,4))
hist(theta[1:10],main="M=10",probability=T,xlab="theta",xlim=c(0,1),ylim=c(0,6))
lines(x,post,col=2,lwd=3)
hist(theta[1:100],10,main="M=100",probability=T,xlab="theta",xlim=c(0,1),ylim=c(0,6))
lines(x,post,col=2,lwd=3)
hist(theta[1:1000],50,main="M=1000",probability=T,xlab="theta",xlim=c(0,1),ylim=c(0,6))
lines(x,post,col=2,lwd=3)
hist(theta[1:10000],50,main="M=10000",probability=T,xlab="theta",xlim=c(0,1),ylim=c(0,6))
lines(x,post,col=2,lwd=3)


# 사후분포의 평균, 분산, Credible interval을 표본 이용해 근사
a=13; b=17
theta=rbeta(100000,a,b)
mean(theta) # mean, 13/(13+17)
var(theta) # variance, 13*17/((13+17)^2*(13+17+1))
quantile(theta,c(0.025,0.975)) # credible interval, qbeta(c(0.025,0.975),13,17)



#-- 3-2. MCMC 알고리즘

#(1) 깁스샘플러
# 정규분포-정규감마사전분포
set.seed(100)
X=rnorm(100,5,2) # Normal(5,4)
Gibbs=function(X, alpha, beta, m0, k0, iter, burnin, thinning){
  mus=numeric(iter)
  lambdas=numeric(iter)
  Xsum=sum(X)
  n=length(X)
  mu=1 # initial value
  for(i in 1:iter){
    lambdas[i]=lambda=rgamma(1,alpha+(n+1)/2,
                             beta+(k0*(mu-m0)^2+sum((X-mu)^2))/2)
    mus[i]=mu=rnorm(1,(k0*m0+Xsum)/(n+k0),1/sqrt(lambda*(n+k0)))
  }
  mus=mus[-(1:burnin)] # burn-in
  mus=mus[1:((iter-burnin)/thinning)*thinning] # thinning
  lambdas=lambdas[-(1:burnin)]
  lambdas=lambdas[1:((iter-burnin)/thinning)*thinning]
  list(mu=mus,lambda=lambdas)
}
res=Gibbs(X,5,1,0,1,20000,10000,10)
mu=res$mu
lambda=res$lambda


# trace plot 과 사후분포 추정
par(mfrow=c(2,2))
plot(mu,type="l",main="")
plot(lambda,type="l",main="")
hist(mu,20,main="",probability=F,xlab="mu")
abline(v=5,col=2,lwd=3)
legend("topright", c("true"), lty=1, lwd=3, col=2)
hist(lambda,20,main="",probability=T,xlab="lambda")
abline(v=1/4,col=2,lwd=3)
legend("topright", c("true"), lty=1, lwd=3, col=2)



#(2) Metropolis-Hastings 알고리즘
# 정규분포-코시사전분포
n=20; Xbar=5 # observations
pi=function(theta,n,Xbar){ # log posterior
  -n*(theta-Xbar)*(theta-Xbar)/2-log(1+theta*theta)
}
q=function(theta,u){ # log proposal
  dnorm(theta,u,log=T)
}
MH=function(theta1, n, Xbar, iter, burnin, thinning){
  tilde=rnorm(iter-1)
  theta=numeric(iter)
  theta[1]=theta1
  u=runif(iter-1)
  for(i in 1:(iter-1)){
    tilde[i]=tilde[i]+theta[i] # candidate
    alpha=exp(pi(tilde[i],n,Xbar)+q(theta[i],tilde[i])
              -pi(theta[i],n,Xbar)-q(tilde[i],theta[i]))
    if(u[i]<alpha) theta[i+1]=tilde[i]
    else theta[i+1]=theta[i]
  }
  theta=theta[-(1:burnin)] # burn-in
  theta=theta[1:((iter-burnin)/thinning)*thinning] # thinning
}
set.seed(100)
theta=MH(1,n,Xbar,20000,10000,10) # run M-H algorithm


# trace plot, M-H 알고리즘으로 추정한 사후분포
par(mfrow=c(1,2))
plot(theta,type="l",main="Trace plot")
C=integrate(function(x) exp(pi(x,n,Xbar)),-Inf,Inf)$value
x=seq(from=min(theta),to=max(theta),length.out=1000)
post=exp(pi(x,n,Xbar))/C
hist(theta,15,main="Histogram of MCMC samples",probability=T,xlab="theta")
lines(x,post,col=2,lwd=3)
legend("topright", c("true posterior"), lty=1, lwd=3, col=2)



#(3) 베이지안 회귀분석
data <- read.csv("asthma.csv")
X <- as.matrix(data[,3:6])
Y <- as.matrix(data[,2])
beta0=rep(0,ncol(X)); Sigma0inv=diag(rep(1,ncol(X))); u=1; v=1  # prior

Gibbs_REG=function(beta0, Sigma0inv, u, v, X, Y, iter, burnin, thinning){
  k=ncol(X)
  n=nrow(X)
  XtX=t(X)%*%X
  
  betas=matrix(0,nrow=iter,ncol=k)
  taus=numeric(iter)
  beta=coefficients(lm(Y~X-1)) # initial values 
  
  for(i in 1:iter){
    taus[i]=tau=rgamma(1,u+n/2,v+sum((Y-X%*%beta)*(Y-X%*%beta))/2)
    Sigma=solve(Sigma0inv+tau*XtX)
    betas[i,]=beta=Sigma%*%(Sigma0inv%*%beta0+tau*t(X)%*%Y)+
      matrix(rnorm(k)%*%chol(Sigma),ncol=1)
  }
  betas=betas[-(1:burnin),] # burn-in
  betas=betas[1:((iter-burnin)/thinning)*thinning,] # thinning
  taus=taus[-(1:burnin)]
  taus=taus[1:((iter-burnin)/thinning)*thinning]
  list(beta=betas,tau=taus)
}
res=Gibbs_REG(beta0,Sigma0inv,u,v,X,Y,20000,10000,10)
beta=res$beta
tau=res$tau


# trace plot, autocorrelation(자기상관) plot
par(mfrow=c(2,2))
plot(beta[,1],type="l",main="Trace plot",ylab=names(data)[3])
acf(beta[,1],main="Autocorrelation function")
plot(beta[,2],type="l",main="Trace plot",ylab=names(data)[4])
acf(beta[,2],main="Autocorrelation function")

par(mfrow=c(2,2))
plot(beta[,3],type="l",main="Trace plot",ylab=names(data)[5])
acf(beta[,3],main="Autocorrelation function")
plot(beta[,4],type="l",main="Trace plot",ylab=names(data)[6])
acf(beta[,4],main="Autocorrelation function")

par(mfrow=c(1,2))
plot(tau,type="l",main="Trace plot",ylab="tau")
acf(tau,main="Autocorrelation function")



#--- 4. 베이지안 혼합모형

#(1) 베이지안 정규분포 혼합모형
set.seed(10)
M=rnorm(500,173.5,3.8); W=rnorm(500,160.5,3.2);  # true
X=c(M,W) 
alpha=1; mu0=mean(X); lambda=1; u=5; v=5; #prior

Gibbs_MIX=function(alpha, mu0, lambda, u, v, X, iter, burnin, thinning){
  n=length(X); idx=list()
  pis=mus=sigmas=matrix(0,nrow=iter,ncol=2)
  pi=rep(0.5,2)
  mu=c(mean(X[X<median(X)]),mean(X[X>=median(X)]))
  sigma=c(var(X[X<median(X)]),var(X[X>=median(X)]))
  
  for(i in 1:iter){
    # Z
    u=runif(n)
    Z=rep(2,n)
    for(j in 1:n){
      p=log(pi)-log(sigma)/2-(X[j]-mu)^2/(2*sigma)
      p=exp(p-max(p))
      if(u[j]<p[1]/sum(p)) Z[j]=1
    }
    idx[[1]]=which(Z==1); idx[[2]]=which(Z==2)
    n_k=c(length(idx[[1]]),length(idx[[2]]))
    
    # pi
    pis[i,1]=pi[1]=rbeta(1,alpha+n_k[1],alpha+n_k[2])
    pis[i,2]=pi[2]=1-pi[1]

    # mu, sigma
    q=1/(n_k*lambda+1)
    m0=c(mean(X[idx[[1]]]),mean(X[idx[[2]]]))
    m=q*mu0+(1-q)*m0
    s=lambda*sigma/(n_k*lambda+1)
    mus[i,1]=mu[1]=rnorm(1,m[1],sqrt(s[1]))
    mus[i,2]=mu[2]=rnorm(1,m[2],sqrt(s[2]))
    sigmas[i,1]=sigma[1]=1/rgamma(1,(u+n_k[1]+1)/2,
                                  (lambda*v+(mu[1]-mu0)^2+lambda*sum((X[idx[[1]]]-mu[1])^2))/2)
    sigmas[i,2]=sigma[2]=1/rgamma(1,(u+n_k[2]+1)/2,
                                  (lambda*v+(mu[2]-mu0)^2+lambda*sum((X[idx[[2]]]-mu[2])^2))/2)
  }
  pis=pis[-(1:burnin),] # burn-in
  pis=pis[1:((iter-burnin)/thinning)*thinning,] # thinning
  mus=mus[-(1:burnin),]
  mus=mus[1:((iter-burnin)/thinning)*thinning,]
  sigmas=sigmas[-(1:burnin),]
  sigmas=sigmas[1:((iter-burnin)/thinning)*thinning,]
  list(pi=pis,mu=mus,sigma=sigmas)
}
res=Gibbs_MIX(alpha,mu0,lambda,u,v,X,10000,5000,10)
pi=res$pi
mu=res$mu
sigma=res$sigma

#샘플링 한 모수 기반으로 추정 density 그려봄
par(mfrow=c(1,1))
xx=seq(from=min(X),to=max(X),length.out=1000)
l=length(mu[,1])/5
y=matrix(0,nrow=l,ncol=1000)
for(i in 1:l)
  y[i,]=pi[i*5,1]*dnorm(xx,mu[i*5,1],sqrt(sigma[i*5,1]))+
  pi[i*5,2]*dnorm(xx,mu[i*5,2],sqrt(sigma[i*5,2]))
hist(X,20,probability=T,main="MCMC samples of Probability Distribution"
     ,xlab="",ylim=c(0,max(y)))
for(i in 1:l)
  lines(xx,y[i,],lty=3,lwd=0.1,col=4)



#(2) k-평균 vs 혼합모형 
library(MASS);library(mclust);set.seed(50)

# 4개의 군집을 갖는 2차원 자료를 임의로 생성
set.seed(100)
mu=matrix(rnorm(8,sd=4),ncol=2)
Sigma=array(0,dim=c(4,2,2))
x=c()
for(i in 1:4){
  diag(Sigma[i,,])=rgamma(2,3,4)
  Sigma[i,1,2]=Sigma[i,2,1]= runif(1,-0.5,0.5)*sqrt(Sigma[i,1,1]*Sigma[i,2,2])
  x=rbind(x,mvrnorm(50*i,mu[i,],Sigma[i,,]))
}
clust0=c(rep(1,50),rep(2,100),rep(3,150),rep(4,200))
plot(x,col=clust0,xlab="",ylab="")


#k-평균
k_means=kmeans(x,4)
clust1=c(3,1,2,4)[k_means$cluster] # 2->1, 3->2, 1->3, 4->4
plot(x,col=clust1,xlab="",ylab="")
points(x[which(clust0!=clust1),],col="red",cex=2,lwd=3)

#혼합모형
mix_EII=Mclust(x,4,"EII")
clust2=c(1,3,2,4)[apply(mix_EII$z,1,which.max)] # 1->1, 3->2, 2->3, 4->4
plot(x,col=clust2,xlab="",ylab="")
points(x[which(clust0!=clust2),],col="red",cex=2,lwd=3)

mix_VII=Mclust(x,4,"VII")
clust3=c(1,3,2,4)[apply(mix_VII$z,1,which.max)] # 1->1, 3->2, 2->3, 4->4
plot(x,col=clust3,xlab="",ylab="")
points(x[which(clust0!=clust3),],col="red",cex=2,lwd=3)

mix_VEI=Mclust(x,4,"VEI")
clust4=c(1,3,2,4)[apply(mix_VEI$z,1,which.max)]
plot(x,col=clust4,xlab="",ylab="")
points(x[which(clust0!=clust4),],col="red",cex=2,lwd=3)



#--- 5. 토픽모형
library(lda); library(ggplot2); library(reshape2); library(cowplot)

setwd("D:/Dropbox/조교자료/고용노동부_추천_201809/실습/bayes_lab/LDA")
shop=read.csv("쇼핑.csv")

#쇼핑.csv를 쇼핑.dat로 변환하는 함수
preproc = function(data,name){
  tab=table(data[,1])
  line=paste(data[,2]-1,data[,3],sep=":")
  line=tapply(line,data[,1],FUN=identity)
  line=Map(c, tab, line)
  line=lapply(line,FUN=function(x) paste(x,collapse=" "))
  line=paste(line,collapse="\n")
  
  loc=paste0(name,".dat")
  outfile=file(loc)
  writeLines(line,outfile)
  close(outfile)
}
preproc(shop,"쇼핑") #정리해서 "쇼핑.dat"로 저장됨.

# collapsed gibbs sampler
shop2=read.documents("쇼핑.dat"); item_name=read.vocab("품목.txt")
head(shop2,1)
set.seed(100)
n=length(shop2); K=11; W=70; alpha=1.0; beta=1.0 # K: 토픽의 개수
lda=lda.collapsed.gibbs.sampler(shop2, K, item_name, 500, alpha, beta) # 500: num.iterations

# theta, phi 를 추정
theta=t(lda$document_sums) # document_sums : 마지막으로 추출된 z로 얻어지는 N_{jk}
for(i in 1:n) theta[i,]=theta[i,]/sum(theta[i,])
round(theta[1:3,1:10],2)

phi=lda$topics # topics : 마지막으로 추출된 z로 얻어지는 N_{kw}
for(i in 1:K)  phi[i,]=phi[i,]/sum(phi[i,]) 
round(phi[1:3,1:10],2)


### 토픽 해석 : 대표하는 주요 상품으로 토픽의 이름을 붙임

# 전체 자료에서 상품의 비율
p=colSums(lda$topics)/sum(lda$topics)

# lift 계산
lift=matrix(0,nrow=K,ncol=W)
colnames(lift)=item_name
for(i in 1:K){
  lift[i,p!=0]=phi[i,p!=0]/p[p!=0]
  lift[i,p==0]=0
}

# 토픽마다 lift 상위 2개 상품으로 이름을 붙임
topic_name=c()
for(i in 1:K){
  sorted=sort(lift[i,],decreasing=T)[1:2]
  topic_name=c(topic_name,paste(names(sorted),collapse=" "))
}
topic_name


# 1,6,7 번째 토픽의 확률과 리프트
plot_topic=function(phi,idx,ncol,main=""){
  theme_set(theme_bw()) 
  phi.df <- melt(cbind(data.frame(phi[idx,]), char=factor(idx)), variable.name="item", id.vars = "char")
  ggplot(phi.df,aes(item,value,fill=item)) + geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none") +  
    coord_flip() +
    facet_wrap(~ char, ncol=ncol) +
    ggtitle(main) +
    theme(plot.title = element_text(hjust = 0.5))
}

par(mfrow=c(1,2))
colnames(phi)=item_name
plot_grid(plot_topic(phi,c(1,6,7),ncol=3,"Probability"),plot_topic(lift,c(1,6,7),ncol=3,"Lift"),align='h')


# 5,100,500 번째 고객의 토픽 비율
plot_client=function(theta,idx,ncol){
  theme_set(theme_bw()) 
  theta.df <- melt(cbind(data.frame(theta[idx,]), client=factor(idx)), variable.name="topic", id.vars = "client")
  ggplot(theta.df,aes(topic,value,fill=topic)) + geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +  
    coord_flip() +
    facet_wrap(~ client, ncol=ncol)
}
par(mfrow=c(1,1))
colnames(theta)=topic_name
plot_client(theta,c(5,100,500),ncol=3)

