# install.packages("arules")

#########################################
#Association Analysis                   #
#########################################

# Practice 1
# (1) data check 
tot=read.csv("tot.csv", header = TRUE) #for 37284 ppl, 387 products

# need to load 'ref_data_ver2' file
load("ref_data_ver2.RData")
#ls() => tot.data, unq.itm, unq.itm.name, unq.user, usr.profile

tot=tot[,-1]
colnames(tot)=unq.itm
rownames(tot)=unq.usr
head(tot)

# (2)
#model fitting

library(arules)
colnames(tot)=unq.itm.name
rules=apriori(as.matrix(tot), parameter=list(supp=0.01, conf=0.5))
#minlen=최소물품수(lhs+rhs), maxlen=최대물품수(lhs+rhs), smax=최대 지지도
print(rules)
rules.sorted=sort(rules, by="lift")
inspect(rules.sorted) #inspect()함수로 규칙을 살펴볼수있음

#관심있는 단어(크림)가 포함된 연관규칙 => subset
rules.sub = subset(rules, rhs %pin% c("크림"))  #관심있는 item 찾을때는 in, ain을 사용할수있음 
                                                # pin = partial matching
inspect(rules.sub)

#강의노트의 appearance 어떻게 쓰나
temp=apriori(as.matrix(tot), parameter=list(supp=0.01, conf=0.5), 
             appearance = list(lhs=c("기저귀/분유  분유  매일유업 "), default="rhs"))
inspect(temp)


# Practice 2: 직접 해보세요!!
shopping=read.csv("shopping.csv", header = TRUE) #for 10000 ppl, 70 products
colnames(shopping) <- c("customer", "goods", "times")
i <- shopping$customer
j <- shopping$goods
shopping.m <- sparseMatrix(i,j, dims = c(max(i), max(j)))
shopping.m

image(shopping.m[1:100,], xlab = "Items(Columns)", ylab = "Transactions(Rows)")
