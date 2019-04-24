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
      tabPanel("Acceptances"),
navbarMenu("Countries",
           tabPanel("Origin",
                    titlePanel(
                      textOutput("originTitle")
                    ),
                    sidebarLayout(position = "right",
                                  sidebarPanel(
                                    selectInput("year_origin",
                                                "Year:",
                                                choices = 2000:2017,
                                                selected = 2014)
                                  ),
                                  mainPanel(
                                    
                                  ))),
           tabPanel("Destination"))
#--------------------------------------------

   )
   
)

server <- function(input, output) {
   output$destTitle <- renderText({
     paste("Number of Asylum Applications Rejected by ", input$dest)
   }) 
#--------------------------------------------
   output$destPlot <- renderPlot({
      dest_status <- asylum_status_gathered %>% 
        filter(dest == input$dest)
      
      ggplot(dest_status) +
        geom_col(aes(x = year, y = value, fill = factor(decision))) +
        labs(
          y = "Number of Rejections",
          x = ""
        )
   })
#--------------------------------------------
   output$originTitle <- renderText({
     paste("Volume of Asylum Applicants Leaving Countries in ", input$year_origin)
   })
}

shinyApp(ui = ui, server = server)

