#--- 자료 전처리

## buytest data
buytest = read.table("buytest.txt", header = T)
dim(buytest)
summary(buytest)
head(buytest, 3)

# 전처리
buytest[buytest$SEX == "", 'SEX'] = NA
levels(buytest$SEX)[1] = NA
buytest[buytest$ORGSRC == "", 'ORGSRC'] = NA
levels(buytest$ORGSRC)[1] = NA
buydata = buytest[,-c(1, 10, 19:26)] # 사용되지 않는 변수 제거
buydata = buydata[complete.cases(buydata),] # 결측치 제거
buydata = model.matrix(~., buydata)[,-1] # 더미변수 생성 & Intercept 생략

head(buydata)


#--- 1. 로지스틱 회귀 분석

# 모형 적합
logit = glm(RESPOND~., data = as.data.frame(buydata), family = 'binomial')
logit

# 자료 분할(train, test set)
set.seed(1)
train_ind = sample(1:nrow(buydata), size = floor(nrow(buydata)*0.7), 
                   replace = F)
train = as.data.frame(buydata[train_ind,])
test = as.data.frame(buydata[-train_ind,])
X_train = buydata[train_ind, -1]
y_train = buydata[train_ind, 1]
X_test = buydata[-train_ind, -1]
y_test = buydata[-train_ind, 1]

dim(X_train)
dim(X_test)

# 학습자료와 검증자료의 반응변수 분포
par(mfrow = c(1,2))
barplot(table(y_train), 
        main = paste("The distribution of RESPOND in Training set
                     \n # of (Y=0): # of (Y=1)=",
                     paste(round(table(y_train)/sum(y_train == 1),2),
                           collapse = ":")))

barplot(table(y_test), 
        main = paste("The distribution of RESPOND in Testset 
                     \n # of (Y=0): # of (Y=1)=",
                     paste(round(table(y_test)/sum(y_test == 1),2),
                           collapse = ":")))

# 로지스틱 회귀모형 적합
logit = glm(RESPOND~., data = train, family = "binomial")
prob = predict(logit, test, type = 'response')
prob[1:10]


# 절단값
cutoff = 0.5
ifelse(prob[1:10] > cutoff,1,0)
cutoff = 1/6

classification = function(model, newdata, cutoff){
  prob = predict(model,newdata,'response')
  ifelse(prob > cutoff,1,0)
} #절단값에 따라 예측 class를 출력해주는 함수


#교차표(분류표)
table(test$RESPOND, classification(logit, test, 0.5))

crosstable = function(model, newdata, cutoff){
  table(test$RESPOND,classification(model,newdata,cutoff))
}
crosstable(logit, test, 1/4)


# 교차표 분석
cutoff_res = function(beta_hat = NULL, newx, response, cutoff, pred_prob = NULL){
  if (!is.null(beta_hat)) {
    X = cbind(1,as.matrix(newx))
    pred_prob = 1/(1+exp(-X%*%beta_hat))}
  pred = ifelse(pred_prob> cutoff, 1, 0)
  error_rate = mean(response != pred)
  sensitivity = sum(response == 1 & pred == 1)/sum(response == 1)
  specificity = sum(response == 0 & pred == 0)/sum(response == 0)
  precision = sum(response == 1 & pred == 1)/sum(pred == 1)
  recall = sensitivity
  if (sum(response == 1 & pred == 1) == 0) {f1 = 0}
  else {f1 = 2*(precision*recall)/(precision+recall)}
  cross_table = table(response, pred)
  return(list(res = c(cutoff, round(error_rate,4), 
                      round(sensitivity,4), round(specificity,4), round(f1, 4)),
              cross_table = cross_table))
} # res: 절단값, 오차율, 민감도, 특이도, f1값 그리고 교차표 출력
cutoff_res(logit$coefficients, X_test, y_test, 1/4)


# ROC 곡선
prob = predict(logit, train, type = 'response')
#### AUC
library(ROCR) # AUC, ROC curve 구하기 위함
####Training set
AUC = performance(prediction(prob , train[,'RESPOND']) , "auc")
AUC@y.values # area under the curve


ROC = performance(prediction(prob ,y_train) , "tpr","fpr")
plot(ROC , main = paste("ROC curve for Train data\n AUC:", 
                        round(as.numeric(AUC@y.values),4)), 
     col = "blue", lwd = 2.5)
abline(c(0,0), c(1,1), lty = 2, lwd = 2)


# 절단값의 선택
cutoff_can = seq(0.01, 0.99, by = 0.01)

cutoff_out = t(sapply(1:length(cutoff_can),
                      function(i) cutoff_res(logit$coefficients, X_train,
                                             y_train, cutoff_can[i])[[1]]))
colnames(cutoff_out) = c("cutoff","error rate","sensitivity","specificity","f1 score")

cutoff_out


# 학습자료의 오분류율을 최소화 하는 절단값 선택
cutoff_out[which.min(cutoff_out[,2]),] 
mean(y_train ==1)
cutoff_res(logit$coefficients, X_train, y_train, 
           cutoff_out[which.min(cutoff_out[,2]), 1])[[2]] #0.39로 절단했을때 교차표

# 학습자료에서 민감도를 0.5 이상으로 하는 절단값 선택.
cutoff_out[tail(which(cutoff_out[,3] >= 0.5), n = 1),]
cutoff_sel = cutoff_out[tail(which(cutoff_out[,3] >= 0.5), n = 1), 1]
cutoff_res(logit$coefficients, X_train, y_train, cutoff_sel)[[2]]

# 학습자료의 F1 스코어를 최대화하는 절단값 선택.
cutoff_out[which.max(cutoff_out[,5]),]
cutoff_res(logit$coefficients, X_train, y_train,
           cutoff_out[which.max(cutoff_out[,5]), 1])[[2]]

# 예측자료에서의 비교.
cutoff = c()
cutoff[1] = cutoff_out[which.min(cutoff_out[,2]), 1]
cutoff[2] = cutoff_out[tail(which(cutoff_out[,3] >= 0.5), n = 1), 1]
cutoff[3] = cutoff_out[which.max(cutoff_out[,5]), 1]
as.data.frame(sapply(cutoff, function(cut) cutoff_res(logit$coefficients, X_test, 
                                                      y_test, cut)[[1]]),
row.names = colnames(cutoff_out))





#--- 2. 변수선택 및 벌점화 기법

# Forward selection (AIC 기준)
null = glm(RESPOND~1, data = train); full = glm(RESPOND~., data = train)
forward = step(null, scope = list(lower = null, upper = full), 
               data = train, direction = "forward")

summary(forward)

#---- logistic + ridge
library(glmnet)
ridge.fit = glmnet(X_train, as.factor(y_train), alpha = 0, 
                   family="binomial" )
ridge.fit$lambda[c(1, 10, 100)]
ridge.fit$beta[,c(1, 10, 100)]


#---- logistic + ridge with 10-fold cv
set.seed(1)
cv.ridge = cv.glmnet(X_train, as.factor(y_train), alpha = 0, 
                     family = "binomial")
bestlam = cv.ridge$lambda.min
ridge.fit = glmnet(X_train,as.factor(y_train), alpha = 0, 
                   lambda = bestlam, family = "binomial")
ridge.fit$beta


#---- logistic + lasso
lasso.fit = glmnet(X_train, as.factor(y_train), alpha = 1, 
                     family="binomial")
lasso.fit$lambda[c(1, 5, 10)]
lasso.fit$beta[,c(1, 5, 10)]


#---- logistic + lasso with 10-fold cv
set.seed(1)
cv.lasso = cv.glmnet(X_train, as.factor(y_train), alpha = 1,
                     family="binomial")
bestlam = cv.lasso$lambda.min
lasso.fit = glmnet(X_train, as.factor(y_train), alpha = 1, 
                   lambda = bestlam, family="binomial")
lasso.fit$beta


#--- 모형별 회귀계수 저장
beta_hat = logit$coefficients


# forward selection
tmp = forward$coefficients
beta_forward = c()
for (i in 1:length(beta_hat)){
  if (names(beta_hat)[i] %in% names(tmp)) beta_forward[i] = tmp[names(beta_hat)[i]]
  else beta_forward[i] = 0
}
beta_hat = cbind(beta_hat, beta_forward)


# Ridge & LASSO
beta_hat = cbind(beta_hat, c(ridge.fit$a0, as.vector(ridge.fit$beta)),
                 c(lasso.fit$a0, as.vector(lasso.fit$beta)))
colnames(beta_hat)[3:4] <- c("beta_ridge","beta_lasso")
head(beta_hat)


# Model Evaluation (모형별 AUC 값 비교)
library(ROCR)

auc_res = function(beta = NULL, newx, newy, pred_prob = NULL){ # 각 모형에 대해 예측값 구해서 AUC 계산 함수
  if (!is.null(beta)){
    X = cbind(1,as.matrix(newx))
    pred_prob = 1/(1+exp(-X%*%beta))    }
  AUC = performance(prediction(pred_prob , newy) , "auc")
  return(AUC@y.values[[1]])
}
auc_table = rbind(sapply(1:4, function(i) auc_res(beta_hat[,i], X_train, y_train)),
                  sapply(1:4, function(i) auc_res(beta_hat[,i], X_test, y_test)))
rownames(auc_table) = c('Training set', 'Test set')
colnames(auc_table) = model_names = c('Logistic','Logistic+AIC', 'Ridge', 'LASSO')
auc_table


# 학습자료에서의 각 기준에 따른 절단값 찾기
cut_sel = matrix(0, nrow = 4, ncol = 3)
for (i in 1:4){
  cutoff_out =  t(sapply(1:length(cutoff_can),
                         function(j) cutoff_res(beta_hat[,i], X_train,
                                                y_train, cutoff_can[j])[[1]]))
  cut_sel[i, 1] = cutoff_out[which.min(cutoff_out[,2]), 1] #오분류율 최소화
  cut_sel[i, 2] = cutoff_out[tail(which(cutoff_out[,3] >= 0.5), n = 1), 1] #민감도 0.5 이상
  cut_sel[i, 3] = cutoff_out[which.max(cutoff_out[,5]), 1] #f1값 최대화
}


# 각 절단값에 따라 test set 의 결과 출력

# 오분류율 최소화
matrix(t(sapply(1:4, function(i) cutoff_res(beta_hat[,i], X_test, 
                                            y_test, cut_sel[i, 1])[[1]])),
       nrow = 4,
       dimnames =list(model_names, c("cutoff","error rate","sensitivity",
                                     "specificity","f1 score")))

# 민감도 0.5이상
matrix(t(sapply(1:4, function(i) cutoff_res(beta_hat[,i], X_test, 
                                              y_test, cut_sel[i, 2])[[1]])), nrow = 4,
         dimnames =list(model_names, c("cutoff","error rate","sensitivity","specificity","f1 score")))


# f1값 최대화
matrix(t(sapply(1:4, function(i) cutoff_res(beta_hat[,i], X_test, 
                                            y_test, cut_sel[i, 3])[[1]])), nrow = 4,
       dimnames =list(model_names, c("cutoff","error rate","sensitivity","specificity","f1 score")))

