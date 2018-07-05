# library(dplyr)
library(tidyverse)
# list.files()
list.files("data")
surveys <- read.csv("data/surveys.csv")
names(surveys)
surveys[, match(c("plot_id", "species_id", "weight"),  names(surveys))] # 이름으로 컬럼을 정해주는 것이 훨씬 명확하고 데이터의 변경에 영향 덜 받는다
# 사실 df는 그냥 열 이름 넣으면 된다
surveys[,c("plot_id", "species_id", "weight")]
# df는 df이기 이전에 list이기 때문에 체인을 직접 불러오는 방법 가능하다
surveys[c("plot_id", "species_id", "weight")] # *****

# year가 1995인 데이터 행만 추출
head(surveys[surveys$year == 1995,])
surveys[surveys$weight<5, c("plot_id", "species_id", "weight")]
surveys[which(surveys$weight<5), c("plot_id", "species_id", "weight")] # which활용하면 NA는 나오지 않는다

# weight를 kg으로 하는 col 만들기
surveys_ex <- surveys # 원본 손상 x 위해서
surveys_ex$weight_kg <- surveys_ex$weight/1000
surveys_ex <- surveys_ex[!is.na(surveys_ex$weight_kg),] # na 컬럼은 제거

# sex로 group 하기
u = unique(surveys$sex)
length(u) # levels로 보면 "", "F", "M" 이다
u
class(u); levels(u)
#sex별로 weight계산
mean(surveys$weight[surveys$sex == u[1]], na.rm = T)
mean(surveys$weight[surveys$sex == u[2]], na.rm = T)
mean(surveys$weight[surveys$sex == u[3]], na.rm = T)

# sum(!is.na(surveys$weight[surveys$sex == u[1]])) # 몸무게가 na인 사람들을 제외한 사람의 수
# by 로 group
a <- by(data = surveys$weight, INDICES = surveys$sex, FUN = mean, na.rm = TRUE)
unlist(a) # df로 만들어줌

# by에 INDICES 일일이 써주는 건 귀찮다
aggregate(formula = weight ~ sex, data = surveys, FUN = mean, na.rm = TRUE) # sex에 대해 weight를 FUN함
aggregate(formula = weight ~ sex+species_id, data = surveys, FUN = mean, na.rm = TRUE) # 복수 개의 그룹으로 묶기도 편함
# 표준편차 구하기
aggregate(formula = weight ~ sex+species_id, data = surveys, FUN = sd, na.rm = TRUE)

# aggregate 는 FUN을 하나만 하니까 여러 정보량을 동시에 구하기에 적합하지 않다
table(surveys$sex, surveys$plot_id) # 2차원 테이블
a = c(10, 5, 3, 7)
order(a) # 3 2 4 1 이 나왔는데 가장 작은 게 3번째 있다, 다음이 2번째 있다, 가장 큰 게 1번째 있다라는 뜻
a[order(a, decreasing = TRUE)]
# plot_id 오름차순으로 surveys 정렬
surveys[order(surveys$plot_id),]

#####################################################################################################################
# dplyr
surveys %>%
  select(plot_id, species_id, weight) %>%   # 열을 선택하는 함수 select
  head()

surveys %>%
  filter(year == 1995, weight > 60)

surveys %>%
  filter(!is.na(weight)) %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight) %>% 
  head()

surveys_ex <- surveys %>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight/1000)

surveys %>%
  group_by(sex) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))  # summarize == summarise
# summarize를 이용하면 여러 function을 한번에 쓸 수 있다
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight),
            var_weight = var(weight),
            min_weight = min(weight),
            max_weight = max(weight)) %>%
  print(n = 5)

# 갯수 세기
surveys %>%
  group_by(sex) %>%
  tally() # *****

# 정렬하기
surveys %>%
  arrange(month, plot_id) %>%
  head()
## 내림차순은 앞에 desc 붙이기
surveys %>% 
  arrange(desc(month), plot_id) %>%
  head()

################################################################################################################
# reshape2
library(reshape2)
# wide format & long format
# 함수에 따라 wide를 요구하기도 하고 long 을 요구하기도 한다
names(airquality) <- tolower(names(airquality)) # lowering
melt(airquality) %>% # long format
  head(3)

aql <- melt(data = airquality, id.vars= c("month","day"), # month와 day를 기준으로 melt한다
            variable.name = "climate_variable", # variable 이름들로 구성된 열의 이름
            value.name = "climate_value")  # value로 구성된 열의 이름
head(aql, n = 3)

# dcast는 long format을 wide format으로 
dcast(aql, month~climate_variable, fun.aggregate = mean, na.rm=TRUE, margins = TRUE) %>%
  head()
