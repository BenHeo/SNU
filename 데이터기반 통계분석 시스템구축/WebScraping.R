if(!require(rvest)){install.packages('rvest') ; library(rvest)}
if(!require(httr)){install.packages("httr"); library(httr)}

# 청춘시대
url_tvcast = 'http://tvcast.naver.com/jtbc.youth'
html_tvcast = read_html(x = url_tvcast, encoding = 'UTF-8')

html_tvcast %>%
  html_nodes(".title a") %>% # title 안에 a가 있는 경우만 찾아내겠다
  head(n = 3)
# tag를 다 떼고 text만 불러오려면 html_text
html_tvcast %>%
  html_nodes(".title a") %>%
  html_text() %>%
  head(3)

html_tvcast %>%
  html_nodes("body.ch_home #u_skip a") %>%
  html_text() %>%
  head(3)



# 왕은 사랑한다
url_tvcast2 = 'http://tv.naver.com/mbc.kingloves'
html_tvcast2 = read_html(x = url_tvcast2, encoding = 'UTF-8')

html_tvcast2 %>%
  html_nodes(".title a") %>% # title이 클래스였기 때문에 .title로 했다 만약 id였다면 #title로 해야 한다  / 클래스는 반복 사용할 것, id는 한번만 사용할 것
  html_text() %>%
  data.frame() %>%
  head(3)



# t distribution
url_wiki = 'https://en.wikipedia.org/wiki/Student%27s_t-distribution'
html_wiki = read_html(x=url_wiki, encoding = 'UTF-8')
html_wiki %>%
  html_nodes('.wikitable') %>%
  html_table() %>%
  data.frame() %>%
  head(5)



# MLB
url <- "http://www.baseball-reference.com/leagues/MLB/2017.shtml"
webpage <- read_html(url)
webpage %>%
  html_nodes('div #div_teams_standard_batting table') %>%
  html_table() %>%
  data.frame() %>%
  head(5)

# 50년치를 모으자
mlb_base_url <- "http://www.baseball-reference.com/leagues/MLB/"
years <- 2008:2017
batting_table <- vector("list", length(years)) # 전에 리스트의 장점이 여러 연구를 연구별로 리스트에 넣을 수 있다는 것이었는데 지금 하는 것
names(batting_table) <- years
for (i in 1:length(years)){
  url <- paste0(mlb_base_url, years[i],".shtml") # 형식 유지하면서 years부분만 바꾸기 위해서
  webpage <- read_html(url)
  batting_table[[i]] <- webpage %>%
    html_nodes('div #div_teams_standard_batting table') %>%
    html_table() %>%
    data.frame()
  batting_table[[i]] <- batting_table[[i]][1:(nrow(batting_table[[i]])-3),] # 뒤에 3 row는 요약통게량이라 뺴준다
  batting_table[[i]][-1] <- Map(as.numeric, batting_table[[i]][-1]) # 첫 열은 string이기 때문에 빼고 numeric으로 바꾼다
  cat(i, "\n")
}



# 기상청 데이터
url = "http://www.weather.go.kr/weather/observation/currentweather.jsp?auto_man=m&type=t99&tm=2017.09.06.13%3A00&x=19&y=3"
webpage <- read_html(url, encoding = "EUC-KR")

Sys.setlocale("LC_ALL", "English")
webpage %>% html_nodes("table.table_develop3")
tmp <- webpage %>% html_nodes("table.table_develop3") %>% 
  html_table(header = FALSE, fill=TRUE)%>%
  data.frame()
head(tmp)

Sys.setlocale("LC_ALL", "Korean")
for(i in 1:ncol(tmp)){
  tmp[,i] = rvest::repair_encoding(tmp[,i])
}
head(tmp)



# 네이버 영화 평점
### gsub는 매칭되는 것 다 찾아서 원하는 것으로 바꿈
## 평가 글 크롤링
total_con = NULL
for (i in 1:10) {
  movie_url = paste0('https://movie.naver.com/movie/point/af/list.nhn?&page=', i)
  movie_html = read_html(GET(movie_url), encoding = 'CP949')
  contents = html_nodes(movie_html, '.title') %>%
    html_text() # 이대로 출력하면 \r\n\t 같은 것이 매우 많다
  contents = gsub('\n|\t|<.*?>|&quot;', '', contents) # \n \t <br> "" '' 같이 표현된 모든 것을 제거
  # 이렇게 하면 \r 이랑 신고 외에는 대부분 정상적이다
  
  part_con = data.frame(do.call(rbind, # do.call은 list에 있는 내용에 대해 특정 function을 수행해주는 것
                                lapply(strsplit(contents, '\r'), # \r 로 split한다
                                       function(x) {x[x != "" & x != "신고"]} ))) # 빈 것과 신고는 지운다
  total_con = rbind(total_con, part_con)
  cat(i, "\n")
}

## 글 + 평점 크롤링
total_star = NULL
for(i in 1:10){
  url = paste0("https://movie.naver.com/movie/point/af/list.nhn?&page=",i)
  mov_html = read_html(GET(url), encoding = "CP949") # encoding은 아직 원인 불명
  content = html_nodes(mov_html, '.title') %>% html_text()
  content = gsub('\n|\t|<.*?>|&quot;','',content)
  point = html_nodes(mov_html, '.point') %>% html_text()
  part_star = data.frame(do.call(rbind, 
                                lapply(strsplit(content, "\r"), function(x) {x[x != "" & x != "신고"]})), point = point)
  total_star = rbind(total_star, part_star)
  cat(i, "\n")
}
head(total_star)


# API정
## authorization
client_id = 'ZoVED2Kc25huNZGJFpev' # api 에서 받은 id
client_secret = 'JGw2VkcxyA' # api 에서 받은 secret
header = httr::add_headers(
  'X-Naver-Client-Id' = client_id,
  'X-Naver-Client-Secret' = client_secret)

### query 설ㅈ
query = '새우깡' # "새우깡"
# encoding 변화
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
rand_row = as.integer(runif(60, 1, 1000)) # 난수 생성으로 1부터 1000 중 60개 정수 생성
final_dat[rand_row,] # 위에서 생성한 정수에 해당하는 row만 봄
