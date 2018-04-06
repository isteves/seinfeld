library(shiny)
library(tidyverse)

seinfeld <- read.csv("scripts.csv")

# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("Seinfeld"),
  
  # Sidebar with a checkbox input for character
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
      
    )
  )
)

# Define server logic required to draw a plot
server <- function(input, output) {
   
   output$distPlot <- renderPlot({

     # filter & plot
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

