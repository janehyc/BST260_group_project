library(shiny)
library(tidyverse)
library(grid)
library(png)
forestfire<-read.csv("forestfires.csv")
map <- readPNG("map.png")

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("month","Month:",1,12,1,step = 1)
    ), #closing sidebarPanel
    mainPanel(
      plotOutput("map")
    )
  )# closing sidebarLayout
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$map <-renderPlot({
    list <- c("jan","feb","mar","apr","may","jun",
              "jul","aug","sep","oct","nov","dec")
    forestfire %>%
      filter(month==list[input$month]) %>%
      ggplot(aes(X, Y))+
      annotation_custom(rasterGrob(map))+
      geom_jitter()+
      scale_x_continuous(breaks = seq(0, 9),limits=c(1,9))+
      scale_y_reverse(breaks = seq(0, 9),limits=c(9,1))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
