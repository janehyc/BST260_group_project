library(shiny)
library(tidyverse)
library(grid)
library(png)
library(Unicode)
library(colorspace)

forestfire<-read.csv("forestfires.csv")
map <- readPNG("map.png")

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("month","Month:",1,12,1,step = 1,ticks = FALSE),
      selectInput("var","Variables:",c("Temperature (°C)","Relative humidity (%)","Wind speed (km/h)","Rain (mm/m^2)"))
    ), 
    
    mainPanel(
      plotOutput("map")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$map <-renderPlot({
    list <- c("jan","feb","mar","apr","may","jun",
              "jul","aug","sep","oct","nov","dec")
    var2 <- c("Temperature (°C)","Relative humidity (%)","Wind speed (km/h)","Rain (mm/m^2)")
    col <- c("temp","RH","wind","rain")
    pal <- c("Heat","Teal","BrwnYl","Teal")
    lim <- list(c(0,35), c(15,100), c(0,10), c(0,7))
    
    forestfire %>%
      filter(month==list[input$month]) %>%
      ggplot(aes_string("X", "Y",size = "area",color= col[which(var2 %in% input$var)]))+
      annotation_custom(rasterGrob(map,width = unit(1,"npc"), 
                                   height = unit(1,"npc")), 
                        xmin = 0.5, xmax = 9.2, 
                        ymin = -9.2, ymax = -0.5)+
      geom_jitter(width = 0.35,
                  height = 0.3)+
      scale_x_continuous(breaks = seq(1, 9),limits=c(0.5,9.5))+
      scale_y_reverse(breaks = seq(1, 9),limits=c(9.5,0.5))+
      scale_size(name = "Area (Ha)", limits = c(0,1200),range = c(1.5,7))+
      scale_color_continuous_sequential(name = input$var,
                                        palette = pal[which(var2 %in% input$var)],begin = 0.15,
                                        limits = unlist(lim[which(var2 %in% input$var)]))+
      
      theme_void()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
