library(httr)
library(rvest)

'bq8VKx5vz8uAJmKyBCc4%2BnyKJDtYAy%2BOtSYPNSPC49wrgcxGGVlp6WxsXV%2FNKpNLm09ukA4DibxSeeIA%2BThJfA%3D%3D'



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

server <- function(input, output)
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
