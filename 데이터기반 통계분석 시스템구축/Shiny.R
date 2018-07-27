library(httr)
library(rvest)
if(!require(xml2)){install.packages("xml2"); library(xml2)}
if(!require(xml2)){install.packages("xml2"); library(xml2)}


service_key <- 'bq8VKx5vz8uAJmKyBCc4%2BnyKJDtYAy%2BOtSYPNSPC49wrgcxGGVlp6WxsXV%2FNKpNLm09ukA4DibxSeeIA%2BThJfA%3D%3D'
url = paste0("http://openapi.airkorea.or.kr/openapi/services/rest/",
             "ArpltnInforInqireSvc/getCtprvnMesureSidoLIst?", # 시군구별 실시간 평균정보 조회
             "sidoName=서울",
             "&searchCondition=DAILY",
             "&pageNo=",1,
             "&numOfRows=",25,
             "&ServiceKey=",service_key)

url_get <- GET(url)
url_xml = read_xml(url_get)
url_xml
item_list = xml_nodes(url_xml, 'items item')
item_list[[1]]
tmp_item = xml_children(item_list[[1]])
tmp_item
tmp_item = xml_text(tmp_item)
tmp_item
item_list = lapply(item_list, function(x) return(xml_text(xml_children(x))))
item_list[[1]]
item_dat = do.call('rbind',item_list)
item_dat = data.frame(item_dat, stringsAsFactors = F)
head(item_dat)
tmp = xml_nodes(url_xml, 'items item') 
colnames_dat = html_name(xml_children(tmp[[1]]))
colnames_dat
colnames(item_dat) = colnames_dat
head(item_dat)



library(shiny)
# shiny는 shinyApp() 함수를 통해 구동된다. shinyApp() 는 arguement 로 ui 객체와 server 함수를 기본적으로 요구한다.
ui = fluidPage(
  titlePanel("Welcome shiny!"),
  sidebarLayout(
    sidebarPanel(
      textInput("input_text", "텍스트를 입력하세요.")
    ),
    mainPanel(
      textOutput("output_text")
    )
  )
)
ui

server <- function(input, output) # 입력어가 input, output인 것은 정해져 있으니 바꾸지 마라
{
  return(NULL)
}

shinyApp(ui = ui, server = server)

server = function(input, output)
{
  output$output_text = renderText({
    paste(input$input_text, '만세')
  })
}
shinyApp(ui = ui, server = server)


ui = fluidPage(
  headerPanel('Iris k-means clustering'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(iris)),
    selectInput('ycol', 'Y Variable', names(iris),
                selected=names(iris)[[2]]),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9),
    checkboxInput('center_tf','Center points')
  ),
  mainPanel(
    plotOutput('plot1')
  )
)
server = function(input, output)
{
  selectedData = reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  clusters <- reactive({
    kmeans(selectedData(), input$clusters) # selectedData()로 되는 이유는 reactive를 통해 함수가 되었기 떄문이다
  })
  
  output$plot1 <- renderPlot({
    plot(selectedData(),
         col = clusters()$cluster, # clusters() 역시 함수
         pch = 20, cex = 3)
    if(input$center_tf) points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
}

shinyApp(ui = ui, server = server)




library(ggmap)
uniq_region = unique(item_dat$cityName)
geo_dat = geocode(paste("서울특별시", uniq_region))
geo_dat = cbind(cityName = uniq_region, geo_dat)
head(geo_dat)
item_dat = merge(item_dat, geo_dat, by = "cityName")
head(item_dat)
# write.csv(item_dat, 'air_quality.csv', row.names = F)
dat = read.csv('data/air_quality.csv', stringsAsFactors = F)
head(dat)
str(dat)

ui = fluidPage(
  titlePanel("Air quality data visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput('region', 'cityName', choices = sort(unique(dat$cityName))),
      selectInput('date', 'dataTime', choices = sort(unique(dat$dataTime))),
      selectInput('category', 'category', choices = colnames(dat)[3:8])
    ),
    mainPanel(
      plotOutput("hist1"),
      plotOutput("hist2")
    )
  )
)
server = function(input, output)
{
  selectedData1 = reactive({
    dat[dat$dataTime == input$date, c(input$category)]
  })
  selectedData2 = reactive({
    dat[dat$cityName == input$region, c(input$category)]
  })
  output$hist1 = renderPlot({
    hist(selectedData1(), main = "선택된 시간의 미세먼지", xlab = "", ylab = "", col = 1:10)
  })
  output$hist2 = renderPlot({
    hist(selectedData2(), main = "선택된 구의 미세먼지", xlab = "", ylab = "", col = 1:10)
  })
}
shinyApp(ui = ui, server = server)

ui = fluidPage(
  titlePanel("Air quality data visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput('date', 'dataTime', choices = sort(unique(dat$dataTime))),
      selectInput('category', 'category', choices = colnames(dat)[3:8]),
      sliderInput('bins', 'detalied density', min = 5, max = 30, value = 10)
    ),
    mainPanel(
      plotOutput("mapplot"),
      tableOutput("tt")
    )
  )
)
server = function(input, output)
{
  map_dat = reactive({
    tmp_dat = dat[dat$dataTime == input$date, c(input$category, "lon", "lat")]
    
    values = tmp_dat[,c(input$category)]
    min_value = min(values[values != 0])
    values = values / min_value  
    tmp_dat[,c(input$category)] = values
    with(tmp_dat, tmp_dat[rep(1:nrow(tmp_dat), tmp_dat[,c(input$category)]),])
  })
  map = ggmap(get_googlemap(center = c(lon = 127.02, lat = 37.53),
                            zoom = 11,
                            maptype = "roadmap",
                            color = "bw"))
  output$mapplot = renderPlot({
    map  + stat_density2d(aes(x = lon, y = lat, alpha = ..level..),
                          data = map_dat(),
                          size= 2,
                          bins= input$bins,
                          geom="polygon") +
      scale_alpha(range = c(0, 0.3))
  }, height = 1200, width = 1024)
}
shinyApp(ui = ui, server = server)
