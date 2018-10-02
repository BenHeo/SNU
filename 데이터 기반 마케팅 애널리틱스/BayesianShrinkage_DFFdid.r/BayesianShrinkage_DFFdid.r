
##################################################################
#
# store level analysis: Bayesian Estimation of Linear Hierarchical Model
#   brand = 5
##################################################################


br5= read.csv('br5.csv')
attach(br5)
storenum = aggregate(volume~store,FUN=mean)
storenum = storenum[,1]
ns = length(storenum) # number of stores
detach(br5)


# big regression with store data pooled together
bigb=lm(log(br5$volume)~log(br5$price)+br5$promo)

# store by store regression
b = matrix(0,nrow=ns,ncol=3)
bse =matrix(0,nrow=ns,ncol=3)
for (k in 1:ns){
  datak = br5[br5$store==storenum[k],]
  tempb = lm(log(datak$volume)~log(datak$price)+datak$promo)
  b[k,]=tempb$coefficients
  bse[k,]=coef(summary(tempb))[,2]
#  print(summary(tempb))
}
betaout0 = cbind(storenum,b[,2],bse[,2],b[,3],bse[,3])
colnames(betaout0)=c("store","b_price", "se b_price","b_romo","se b_promo")



# Bayesian Hierarchical Regression
library(bayesm)
# if(nchar(Sys.getenv("LONG_TEST")) != 0) {R=2000} else {R=10}
R=10000
burnin1=5000  # number of inital draws to be discarded
nobs=52; nvar=3
#Z=rep(1,ns) 
#nz=ncol(Z)
Delta=matrix(c(0,0,0),ncol=1)
Delta=t(Delta) # first row of Delta is means of betas

iota=c(rep(1,nobs))
regdata=NULL

for (k in 1:ns) {
  X = cbind(iota,log(br5$price[br5$store==storenum[k]]),br5$promo[br5$store==storenum[k]])
  y=log(br5$volume[br5$store==storenum[k]])
  regdata[[k]]=list(y=y,X=X) }

#Data1=list(regdata=regdata,Z=Z)
Data1=list(regdata=regdata)
Mcmc1=list(R=R,keep=1)

#Prior1=list(Deltabar=Delta,A=0.01*diag(1),nu.e=3,ssq=rep(0.06,ns),nu=6,V=6*0.1*diag(3)) 
#out=rhierLinearModel(Data=Data1,Prior=Prior1, Mcmc=Mcmc1)
out=rhierLinearModel(Data=Data1, Mcmc=Mcmc1)
cat("Summary of Delta draws",fill=TRUE)
summary(out$Deltadraw)
cat("Summary of Vbeta draws",fill=TRUE)
summary(out$Vbetadraw)


tempb1 = out$betadraw[,2,(burnin1+1):10000]
tempb2 = out$betadraw[,3,(burnin1+1):10000]
betaout = cbind(storenum,rowMeans(tempb1),apply(tempb1,1,sd),rowMeans(tempb2),apply(tempb2,1,sd))
colnames(betaout)=c("store","mean b_price", "sd b_price","mean b_romo","sd b_promo")
print(betaout)

#plot(out$betadraw, burnin=burnin1)
#plot(out$Deltadraw, burnin=burnin1)

boxplot(tempb1~c(1:47),horizontal=F,col="lightblue",main="Price Elasticity by Stores",ylab="Price Elasticity", xlab="Store",outline=F, ylim=c(-5,-1.5))
lines(x=c(1:47),y=b[,2],type="b",col="darkred",lwd=2,lty=1)  # add store by store regression results
abline(h=bigb$coefficients[2],col="black", lwd=2,lty=1) # add pooled regression result

boxplot(tempb2~c(1:47),horizontal=F,col="lightblue",main="Promotion Sensitivity by Stores",ylab="Promotion Sensitivity", xlab="Store",outline=F)
lines(x=c(1:47),y=b[,3],type="b",col="darkred",lwd=2,lty=1)  # add store by store regression results
abline(h=bigb$coefficients[3],col="black", lwd=2,lty=1) # add pooled regression result

tempdelta1 = out$Deltadraw[(burnin1+1):10000,2]
tempdelta2 = out$Deltadraw[(burnin1+1):10000,3]

tmp = hist(tempdelta1,breaks=30,plot=F)
tmp$density = tmp$counts/sum(tmp$counts)
plot(tmp,freq=F,main="Distribution of Mean Price Elasticity", xlab="Mean Price Elasticity", ylab="Probability",col="lightblue")

tmp = hist(tempdelta2,breaks=30,plot=F)
tmp$density = tmp$counts/sum(tmp$counts)
plot(tmp,freq=F,main="Distribution of Mean Promotion Sensitivity", xlab="Mean Promotion Sensitivity", ylab="Probability",col="lightblue")

