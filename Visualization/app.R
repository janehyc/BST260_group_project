library(shiny)
library(tidyverse)
library(grid)
library(png)
library(Unicode)

forestfire<-read.csv("forestfires.csv")
map <- readPNG("map.png")

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("month","Month:",1,12,1,step = 1,ticks = FALSE)
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
      annotation_custom(rasterGrob(map,width = unit(1,"npc"), 
                                   height = unit(1,"npc")), 
                        xmin = 0.5, xmax = 9.2, ymin = -9.2, ymax = -0.5)+
      geom_jitter(width = 0.25,height = 0.25)+
      scale_x_continuous(breaks = seq(1, 9),limits=c(0.5,9.5))+
      scale_y_reverse(breaks = seq(1, 9),limits=c(9.5,0.5))+
      theme_void()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
