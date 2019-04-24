library(janitor)
library(shiny)
library(shinythemes)
library(tidyverse)

ui <- shinyUI(
  navbarPage("Asylum Applicants",
             theme = shinytheme("flatly"),
#--------------------------------------------
     tabPanel("Rejections",
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
     ),
#--------------------------------------------
      tabPanel("Acceptances")


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
        geom_col(aes(x = year, y = rejected, fill = year)) +
        labs(
          y = "Number of Rejections",
          x = ""
        )
   })
}

shinyApp(ui = ui, server = server)

