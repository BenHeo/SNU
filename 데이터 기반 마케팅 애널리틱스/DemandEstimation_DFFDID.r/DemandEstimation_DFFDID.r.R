

didmarket = read.csv('didmarket.csv')


# brand
#	1. Dove
#	2. Palmolive
#	3. Dawn
#	4. Joy
#	5. Cascade
#	6. Ivory
#	7. Dominicks


# check demand with log-log model
v = matrix(didmarket$volume,ncol=7)
pr = matrix(didmarket$price,ncol=7)
pm = matrix(didmarket$promo,ncol=7)
w = matrix(didmarket$wpr,ncol=7)

b= matrix(0, nrow=7, ncol=9)
for (k in 1:7){
	tempb = lm(log(v[,k])~log(pr)+pm[,k])
	b[k,]=tempb$coefficients
	print(summary(tempb))
}

# fit the model logS(k) = logPr(1,..,7)+logLaggedS(k) with 1 year data
b1= matrix(0, nrow=7, ncol=9)
for (k in 1:7){
	tempb = lm(log(v[11:62,k])~log(pr[11:62,])+log(v[10:61,k]))
	b1[k,]=tempb$coefficients
	print(summary(tempb))
}

# expected profit at average price and average wholesale price
avgpr = colMeans(pr[11:62,])
avgwp = colMeans(w[11:62,])
avgv = colMeans(v[11:62,])
avgvlag = colMeans(v[10:61,])

predictor = cbind(rep(1,7),t(matrix(rep(log(avgpr),7),nrow=7)),log(avgvlag))
predictedv = exp(rowSums(predictor*b1))
predictedprofit = sum(predictedv*(avgpr-avgwp))

weightedprice = sum(predictedv*avgpr)/sum(predictedv)

bound = weightedprice*0.05

profit = function(newprice){

	if (max(abs((newprice-avgpr)/avgpr))>0.05) npredictedprofit = max(abs((newprice-avgpr)/avgpr))*(-10000)
	else {
		npredictor = cbind(rep(1,7),t(matrix(rep(log(newprice),7),nrow=7)),log(avgvlag))
		npredictedv = exp(rowSums(npredictor*b1))
		
		nweightedprice = sum(npredictedv*newprice)/sum(npredictedv)

		if (abs(nweightedprice-weightedprice)>bound) npredictedprofit = abs(nweightedprice-weightedprice)*(-10000)
		else npredictedprofit = sum(npredictedv*(newprice-avgwp))
	}
	return(-npredictedprofit)
}


optprice = optim(avgpr,profit,control=list(maxit=5000)) # optprice의 convergence가 0이여야 수렴했다는 것

print(cbind(avgpr,optprice$par,optprice$par/avgpr-1))
