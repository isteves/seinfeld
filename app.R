#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

seinfeld <- read.csv("scripts.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Seinfeld"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    mainPanel(
      plotOutput("distPlot")),
    
    sidebarPanel(
      checkboxGroupInput("checkGroup", 
                   label = h3("Characters"),
                   choices = list("Jerry" = "JERRY", 
                                  "George" = "GEORGE", 
                                  "Elaine" = "ELAINE",
                                  "Kramer" = "KRAMER"), 
                   selected = "KRAMER")
      
      
      # Show a plot of the generated distribution
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      # x    <- faithful[, 2] 
      # output$value <- renderPrint({ input$radio })
      
      # PLOT HERE
     seinfeld %>% 
       as_tibble() %>% 
       group_by(Season, Character, EpisodeNo) %>% 
       summarize(NLines = n()) %>% 
       arrange(desc(NLines)) %>% 
       filter(NLines > 10,
              Character %in% input$checkGroup) %>% 
       ungroup() %>% 
       group_by(Season, Character) %>% 
       summarize(MeanNLine = mean(NLines)) %>% 
        ggplot(aes(x = Season, y = MeanNLine, color = Character)) +
        geom_point() +
        geom_line() +
       xlim(0,10) +
       ylim(10,150) +
       scale_x_continuous(breaks = 0:9)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

