library(janitor)
library(shiny)
library(tidyverse)

ui <- fluidPage(
   
   titlePanel(
     textOutput("destTitle")
   ),
   
   sidebarLayout(position = "right",
      sidebarPanel(
         selectInput("dest",
                     "Destination Country:",
                     choices = unique(asylum_status$dest),
                     selected = "France")
      ),
      
      mainPanel(
         plotOutput("destPlot")
      )
   )
)

server <- function(input, output) {
   output$destTitle <- renderText({
     paste("Number of Asylum Applications Rejected by ", input$dest)
   }) 
  
   output$destPlot <- renderPlot({
      dest_status <- asylum_status %>% 
        filter(dest == input$dest)
      
      ggplot(dest_status) +
        geom_col(aes(x = year, y = rejected)) +
        labs(
          y = "Number of Rejections",
          x = ""
        )
   })
}

shinyApp(ui = ui, server = server)

