# 네트워크 시각화 위한 패키지 설치
if(!require(networkD3)){install.packages("networkD3"); library(networkD3)}
if(!require(igraph)){install.packages("igraph"); library(igraph)}
if(!require(reshape2)){install.packages("reshape2"); library(reshape2)}

src <- c("A", "A", "A", "A",
         "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J",
            "E", "F", "G", "H", "I")
networkData <- data.frame(src, target)
head(networkData)
networkD3::simpleNetwork(networkData, fontSize = 15, zoom = T)

# 가중 네트워크
data(MisLinks)
head(MisLinks)

data(MisNodes)
head(MisNodes)

forceNetwork(Links = MisLinks, Nodes = MisNodes, NodeID = "name",
             Source = "source", Target = "target",
             Value = "value", arrows = T,
             Group = "group", opacity = 0.8, zoom = TRUE)
# Link 데이터와 Node데이터의 시작점, 도착점, 값 등을 입력
# arrows option을 이용하여 방향표현 가능
# source option : start node
# target option : end node
# group option : group별 색상을 다르게 줌
# value option : value에 따라 화살표의 두깨를 조절해줌

# SankeyNetwork    :  일반적으로 지저분해져서 잘 안 쓰는 경향이 있지만 최근에 월드컵 분석에서 사용한 걸 봤다
sankeyNetwork(Links = MisLinks, Nodes = MisNodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 18)



# =================================================================================================================

load('data/data_list.Rdata') # 네이쳐에 머신러닝으로 검색한 논문의 단어 정리
str(data_list) # data, n_gram, date, conf_data 로 구성
head(data_list$data)
head(data_list$data$word[data_list$data$date == 2014]) # 2014년도 데이터만
word.df = as.data.frame(data_list$data)
word.df = dcast(word.df, word~date, value.var = "count") # dcast 이용해 wide form으로 변환 (연도 변수를 개별 컬럼들로 만듦)
word.df = cbind(word.df, sum = rowSums(word.df[-1]))
word_order = order(word.df$sum, decreasing = TRUE)
head(word.df[word_order,]) # 합 높은 거 보기 위해

doc_list <- data_list$n_gram
uniq_words <- sort(unique(do.call('c', doc_list))) # c 통해서 vector화 해준 후, unique
count_doc <- function(word, year){
  return(
    sum(sapply(data_list$n_gram[data_list$date == year], function(x) word %in% x)) 
    # 입력 받은 연도에 해당하는 n_gram들 안에 해당하는 단어의 합
  )
}
word_count_2016 <- sapply(uniq_words, function(word) count_doc(word, 2016)) # 모든 unique 단어에 대해

# confidence :  사건 A와 사건 B가 동시에 발생할 확 대비 A 발생 확률 : A 교집합 B의 발생 확률 / A의 발생 확률
# conf_result 설명 : 2014~2016 confidence 평균으로 2017 년의 단어 confidence 비율 파악하려 함
conf_result <- data_list$conf_data
head(conf_result)
mean_conf <- apply(conf_result[,2:4], 1, mean) # conf_result[,2:4]의 열에 대해 평균
conf_result$increasing_rate <- conf_result[,5]/mean_conf

text_conf <- conf_result[c(which(is.finite(conf_result$increasing_rate) & conf_result$increasing_rate > 20),
                         which(mean_conf > 0.3)),] # 20 넘는 increasing_rate와 mean_conf 가 큰 값들에 text를 입력해주기 위해 미리 설정
loc_conf <- mean_conf[c(which(is.finite(conf_result$increasing_rate) & conf_result$increasing_rate > 20),
                        which(mean_conf > 0.3))]
sum(conf_result$increasing_rate == Inf) # 102개가 Inf로 나온다

plot(mean_conf, conf_result$increasing_rate, ylim = c(-0.5, 27), xlim = c(-0.01, 0.5))
text(loc_conf+0.03, text_conf$increasing_rate, labels = text_conf$word, cex = 1, pos = 3) # x좌표를 보면 위 과정 이해 될 듯
abline(h=1, col='red')

# x, y 모두 exponential하기 때문에 log scale을 취해준다
# log0은 없기 때문에 아주 작은 수를 더해준다
plot(mean_conf + 1e-4, conf_result$increasing_rate + 1e-2, log = "xy", ylim = c(1e-2, 27), xlim = c(1e-4, 2)) # log = "xy"를 해주면 xy축 모두 로그스케일
text(loc_conf*exp(0.03), text_conf$increasing_rate, labels = text_conf$word, cex = 1, pos = 3)
abline(h = 1, col = 'red')


# Term Document Matrix 만들기 : 특정 Document 컬럼에 Term이 몇 번 나왔는지 나타내는 df
doc_list = data_list$n_gram
uniq_words <- unique(do.call('c', doc_list))
occur_vec_list <- lapply(doc_list, function(x) uniq_words %in% x) 
dtm_mat = do.call('rbind', occur_vec_list) # list를 matrix 형으로 바꿔줬음
colnames(dtm_mat) <- uniq_words # uniq_words들의 
dtm_mat[1:3, 1:3]
length(dtm_mat) # == nrow * ncol
# nrow(dtm_mat)
# ncol(dtm_mat)
refined_dtm_mat <- dtm_mat[, colSums(dtm_mat) != 0] # 단어 중 문서 전체에서 하나도 안 나온 것은 제거
refined_dtm_mat <- refined_dtm_mat[rowSums(dtm_mat) != 0,]
co_occur_mat <- t(refined_dtm_mat) %*% refined_dtm_mat # 행렬 곱을 통해 특정 단어가 나온 문서에서 다른 특정 단어가 나온 빈도수 표현 (마코브 체인 활용)
# 나오는 결과는 단어 X 단어 matrix
co_occur_mat[1:4, 1:4]

# co_occur_mat의 숫자의 강도를 power로 주고 sankey 그래프를 그리자
# 우선 matrix 크기를 줄일 것이다
## diag의 수가 빈도를 의미하기 때문에 diag가 너무 작은 것은 제거한다
inv = (diag(co_occur_mat) >= 150)
co_occur_mat1 <- co_occur_mat[inv, inv]
## 앞서 구한 confidence 비율 증가가 큰 단어들을 가져온다
idx = which(conf_result$increasing_rate[which(is.finite(conf_result$increasing_rate))] >= 5) # increasing_rate가 5보다 크고 유한인 경우만 사용
co_occur_mat2 <- co_occur_mat[idx, idx]

g = graph.adjacency(co_occur_mat1, weighted = T, mode = 'undirected') # 인접행렬 형태에서 igraph 만들기 편하게 해주는 함수
g = simplify(g) # loop나 다중간선 없게
wc = cluster_walktrap(g) # communities(densely connected subgraphs) 찾기
members = membership(wc) 
network_list = igraph_to_networkD3(g, group = members) # igraph to d3 list
sankeyNetwork(Links = network_list$links, Nodes = network_list$nodes,
              Source = "source", Target = "target", 
              Value = "value", NodeID = "name",
              units = "TWh", fontSize = 18, nodeWidth = 30)

# 연결관계끼리 관계선을 연결성 느껴지게 그리기. 선후 관계가 없는 관계인데 그냥 먼저 나오는 것의 색을 유지
network_list = igraph_to_networkD3(g, group = as.character(members))
network_list$links$group = network_list$nodes$group[network_list$links$source+1]
sankeyNetwork(Links = network_list$links, Nodes = network_list$nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              NodeGroup = "group", LinkGroup = "group",
              units = "TWh", fontSize = 18, nodeWidth = 30)

# greedy way fast grouping
network_list = igraph_to_networkD3(cluster_fast_greedy(g))
network_list$links$group = network_list$nodes$group[network_list$links$source+1]
sankeyNetwork(Links = network_list$links, Nodes = network_list$nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              NodeGroup = "group", LinkGroup = "group",
              units = "TWh", fontSize = 18, nodeWidth = 30)


# circular plot
if(!require(circlize)){install.packages("circlize"); library(circlize)}
name=c(3,10,10,3,6,7,8,3,6,1,2,2,6,10,2,3,3,10,4,5,9,10)
feature=paste("feature ", c(1,1,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5),
              sep="")
dat <- data.frame(name,feature)
dat <- table(name, feature)
head(dat,4)
chordDiagram(as.data.frame(dat), transparency = 0.5)


# easier and fancy graph to understand
if(!require(chorddiag)){devtools::install_github("mattflor/chorddiag"); # should install devtools package
  library(chorddiag)}
if(!require(RColorBrewer)){install.packages("RColorBrewer")}
doc_list = data_list$n_gram
table_list = lapply(doc_list, table)[1:3]
table_name = unique(unlist(do.call("c", doc_list[1:3] )))
names(table_list) = paste0("doc_", 1:3)

table_list = lapply(table_list, function(x){ # 테이블 만들 때 빈 자료란은 0으로 채운다
  word_table = rep(0, length = length(uniq_words))
  word_table = ifelse(uniq_words %in% names(x), x, 0)  
}
)

table_list = do.call("rbind", table_list)

refined_table_list = t(table_list[, apply(table_list, 2, sum) != 0])
rownames(refined_table_list) = table_name
groupColors <- brewer.pal(3, "Set3")

chorddiag(refined_table_list
          , groupColors = groupColors,  type = "bipartite", tickInterval = 3
          ,groupnameFontsize = 15)
