if(!require(rvest)){install.packages('rvest') ; library(rvest)}
if(!require(httr)){install.packages("httr"); library(httr)}
library(tidyverse)


# API정보
## authorization
client_id = '****' # api 에서 받은 id
client_secret = '****' # api 에서 받은 secret
header = httr::add_headers(
  'X-Naver-Client-Id' = client_id,
  'X-Naver-Client-Secret' = client_secret)

### query 설정
query = '블랙핑크'
#  encoding 변화
query = iconv(query, to = 'UTF-8', toRaw = T) # ec 83 88 ec 9a b0 ea b9 a1
# iconv(query, to = "UTF-8", toRaw = F)
query = paste0('%', paste(unlist(query), collapse = '%')) # %ec%83%88%ec%9a%b0%ea%b9%a1
query = toupper(query) # %EC%83%88%EC%9A%B0%EA%B9%A1

end_num = 1000
display_num = 100
start_point = seq(1,end_num,display_num) # 이와 같이 해주는 이유는 display를 100개까지 밖에 한 번에 못 긁기 때문이다
i = 1
final_dat = NULL
for(i in 1:length(start_point))
{
  # request xml format
  url = paste0('https://openapi.naver.com/v1/search/blog.xml?query=',query,'&display=',display_num,'&start=',start_point[i],'&sort=sim') # 정렬은 유사도로
  # query를 치고 100개씩 하겠다고 알리며, 시작은 start_point[i]로 하겠다는 의미
  #option header
  url_body = read_xml(GET(url, header), encoding = "UTF-8")
  title = url_body %>% xml_nodes('item title') %>% xml_text()
  # bloggername 같은 것은 네이버에서[https://developers.naver.com/docs/search/blog/] 제공한다
  bloggername = url_body %>% xml_nodes('item bloggername') %>% xml_text()
  postdate = url_body %>% xml_nodes('postdate') %>% xml_text()
  link = url_body %>% xml_nodes('item link') %>% xml_text()
  description = url_body %>% xml_nodes('item description') %>% html_text()
  temp_dat = cbind(title, bloggername, postdate, link, description)
  final_dat = rbind(final_dat, temp_dat)
  cat(i, '\n')
}
nrow(final_dat) # 1000
ncol(final_dat) # 5
rand_row = as.integer(runif(20, 1, 1000)) # 난수 생성으로 1부터 1000 중 60개 정수 생성
final_dat[rand_row,] # 위에서 생성한 정수에 해당하는 row만 봄


final_dat = data.frame(final_dat, stringsAsFactors = F)
final_dat$description = gsub('\n|\t|<.*?>|&quot;',' ',final_dat$description)
final_dat$description = gsub('[^가-힣a-zA-Z]',' ',final_dat$description)
final_dat$description = gsub(' +',' ',final_dat$description)

library(KoNLP)
nouns=KoNLP::extractNoun(final_dat$description)
nouns[1:5]
newdic=data.frame(V1=c("블랙핑크", "블랙 핑크", "Blackpink", "Black pink", "지수", "로제", "리사", 
                       "제니", "블핑", "블로그", "포스팅", "DDUDU", "언니", "치명", "데프콘", "정형돈",
                       "발매본", "아이돌", "뚜두뚜두", "DDUDU DDUDU", "ddudu", "마지막처럼", "불장난", "휘파람", "붐바야"),"ncn")
KoNLP::mergeUserDic(newdic)

nouns=KoNLP::extractNoun(final_dat$description)
nouns[1:5]
# save(nouns, file = "BlackPinkNouns.RData")
load("BlackPinkNouns.RData")

# class(final_dat)
length(nouns)
nouns[[1]]
words <- c()
for (i in 1:1000){
  for (word in nouns[[i]]){
    words <- c(words, tolower(word))
  }
}
tw <- table(words)
tw <- tw[tw>=20]
tw
hist(tw, breaks = 100, xlim = c(0, 200))
tw[tw>150]
sort(tw, decreasing = TRUE)
dfBP2 <- data.frame(tw)

library(wordcloud2)
# wordcloud2(demoFreq, size = 1,shape = 'star')
# ======================================================================================
dfBP2 <- dfBP2 %>%
  filter(!(words %in% c("해", "후", "한", "의", "이", "장", "저", "적", '전', '제', '주', '중', '지',
                       '은', '을', '위', '월', '원', '세', '수', '로', '만', '명', '본', '분', '라', '데', '도', '두',
                       '들', '를', '기', '나', '날', '내', '대', '데', '개', '그', '때', '리', '화', '양', '들이',
                       '듯', '과', '드', '니', '바', '림', '얼', '거', '시', '호', "년", "것", "출처", "번", "속")))
dfBP2 <- dfBP2 %>%
  filter(!(words %in% c('amp', 'gt', 'k', 'lt', 'm',  'q', 's', 'x', 'v', 'u', 'a', 'b', 'r', 'www', 'com',
                        'ne', 'l', 'e', 'd', 'cm', 'https')))


wordcloud2(dfBP2, size = 2, shape = 'star')
letterCloud(dfBP2, word = "BLACKPINK", wordSize = 0.008, color="white", backgroundColor="pink")
# letterCloud(dfBP2, word = "BLACKPINK", wordSize = 0.1, color="white", backgroundColor="pink")

# save(dfBP2, file = "dfBP.RData")
#============================================================================================================
load("dfBP.RData")
# Term Document Matrix 만들기 : 특정 Document 컬럼에 Term이 몇 번 나왔는지 나타내는 df
noun_list = nouns
uniq_words <- unique(do.call('c', noun_list))
occur_vec_list <- lapply(noun_list, function(x) uniq_words %in% x) 
dtm_mat = do.call('rbind', occur_vec_list) # list를 matrix 형으로 바꿔줬음
colnames(dtm_mat) <- uniq_words # uniq_words들의 
dtm_mat[1:7, 1:7]
length(dtm_mat) # == nrow * ncol
# nrow(dtm_mat)
# ncol(dtm_mat)
refined_dtm_mat <- dtm_mat[, colSums(dtm_mat) != 0] # 단어 중 문서 전체에서 하나도 안 나온 것은 제거
refined_dtm_mat <- refined_dtm_mat[rowSums(dtm_mat) != 0,]
co_occur_mat <- t(refined_dtm_mat) %*% refined_dtm_mat # 행렬 곱을 통해 특정 단어가 나온 문서에서 다른 특정 단어가 나온 빈도수 표현 (마코브 체인 활용)
# 나오는 결과는 단어 X 단어 matrix
co_occur_mat[1:7, 1:7]

# co_occur_mat의 숫자의 강도를 power로 주고 sankey 그래프를 그리자
# 우선 matrix 크기를 줄일 것이다
## diag의 수가 빈도를 의미하기 때문에 diag가 너무 작은 것은 제거한다
inv = (diag(co_occur_mat) >= 40)
co_occur_mat1 <- co_occur_mat[inv, inv]
co_occur_mat1[1:7, 1:7]
noIdx <- which(colnames(co_occur_mat1) %in% c("해", "후", "한", "의", "이", "장", "저", "적", '전', '제', '주', '중', '지',
                                     '은', '을', '위', '월', '원', '세', '수', '로', '만', '명', '본', '분', '라', '데', '도', '두',
                                     '들', '를', '기', '나', '날', '내', '대', '데', '개', '그', '때', '리', '화', '양', '들이',
                                     '듯', '과', '드', '니', '바', '림', '얼', '거', '시', "호", "년", "것", "출처", "번", "속",
                                     'amp', 'cm', 'com', 'gt', 'https', 'k', 'lt', 'm', 
                                     'q', 's', 'x', 'v', 'www', 'u', 'a', 'b', 'r',
                                     'ne', 'l', 'e', 'd'))
co_occurrence <- co_occur_mat1[-noIdx, -noIdx]

which_are_related <- data.frame()
for (i in 1:nrow(co_occurrence)){
  for (j in 1:ncol(co_occurrence)){
    if (i < j){
      if (co_occurrence[i,j] > 30){
        row = i
        col = j
        row_word = rownames(co_occurrence)[i]
        col_word = colnames(co_occurrence)[j]
        dat <- data.frame("row" = row, "col" = col, "row_word" = row_word, "col_word" = col_word, "cnt" = co_occurrence[i,j])
        which_are_related <- rbind(which_are_related, dat)
      }
    }
  }
}
head(which_are_related)
which_are_related %>%
  arrange(desc(cnt)) %>%
  head(50)


if(!require(networkD3)){install.packages("networkD3"); library(networkD3)}
if(!require(igraph)){install.packages("igraph"); library(igraph)}
g = graph.adjacency(co_occurrence, weighted = T, mode = 'undirected') # 인접행렬 형태에서 igraph 만들기 편하게 해주는 함수
g = simplify(g) # loop나 다중간선 없게
wc = cluster_walktrap(g) # communities(densely connected subgraphs) 찾기
members = membership(wc) 
network_list = igraph_to_networkD3(g, group = members) # igraph to d3 list
sankeyNetwork(Links = network_list$links, Nodes = network_list$nodes,
              Source = "source", Target = "target", 
              Value = "value", NodeID = "name",
              units = "TWh", fontSize = 20, nodeWidth = 20)

# 지수 with 댄스
theIdx <- which(colnames(co_occur_mat1) %in% c("지수", "안무", "댄스", "매력", "노래", "음악"))
Jisumatrix <- co_occur_mat1[theIdx, theIdx]
Jisumatrix <- Jisumatrix[c(6, 5, 4, 1:3), c(6, 5, 4, 1:3)]
g = graph.adjacency(Jisumatrix, weighted = T, mode = 'undirected') # 인접행렬 형태에서 igraph 만들기 편하게 해주는 함수
wc = cluster_walktrap(g) # communities(densely connected subgraphs) 찾기
members = membership(wc) 
network_list = igraph_to_networkD3(g, group = as.character(members)) # igraph to d3 list
network_list$links$group = network_list$nodes$group[network_list$links$source+1]
sankeyNetwork(Links = network_list$links, Nodes = network_list$nodes,
              Source = "source", Target = "target", 
              Value = "value", NodeID = "name",
              units = "TWh", fontSize = 20, nodeWidth = 40)

forceNetwork(Links = network_list$links, Nodes = network_list$nodes, NodeID = "name",
             Source = "source", Target = "target",
             Value = "value", arrows = F, fontSize = 20,
             Group = "group", opacity = 0.8, zoom = TRUE)

# 멤버
memberIdx <- which(colnames(co_occur_mat1) %in% c("지수", "제니", "리사", "로제"))
membermatrix <- co_occur_mat1[memberIdx, memberIdx]
g = graph.adjacency(membermatrix, weighted = T, mode = 'undirected') # 인접행렬 형태에서 igraph 만들기 편하게 해주는 함수
wc = cluster_walktrap(g) # communities(densely connected subgraphs) 찾기
members = membership(wc) 
network_list = igraph_to_networkD3(g, group = as.character(members)) # igraph to d3 list
network_list$links$group = network_list$nodes$group[network_list$links$source+1]
sankeyNetwork(Links = network_list$links, Nodes = network_list$nodes,
              Source = "source", Target = "target", 
              Value = "value", NodeID = "name",
              units = "TWh", fontSize = 20, nodeWidth = 40)
