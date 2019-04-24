library(janitor)
library(shiny)
library(shinythemes)
library(leaflet)
library(maps)
library(sf)
library(tidyverse)

# The asylum status data from the UNHCR's popstat database provides annual
# information on the status of asylum-seeker applications

asylum_status <- read_csv("http://popstats.unhcr.org/en/asylum_seekers.csv",
                          skip = 3,
                          col_types = cols(
                            Year = col_double(),
                            `Country / territory of asylum/residence` = col_character(),
                            Origin = col_character(),
                            `RSD procedure type / level` = col_character(),
                            `Tota pending start-year` = col_double(),
                            `of which UNHCR-assisted` = col_double(),
                            `Applied during year` = col_double(),
                            statistics.filter.decisions_recognized = col_double(),
                            statistics.filter.decisions_other = col_double(),
                            Rejected = col_double(),
                            `Otherwise closed` = col_double(),
                            `Total decisions` = col_double(),
                            `Total pending end-year` = col_double(),
                            `of which UNHCR-assisted_1` = col_double()
                          )) %>% 
  clean_names() %>% 
  rename("dest" = "country_territory_of_asylum_residence",
         "recognized" = "statistics_filter_decisions_recognized") %>% 
  filter(!is.na(origin)) %>% 
  filter(!is.na(dest),
         !origin %in% c("Curaçao", "Holy See (the)"))

asylum_status$origin  <- fct_recode(asylum_status$origin, 
                                    Bolivia = "Bolivia (Plurinational State of)",
                                    Brunei = "Brunei Darussalam",
                                    `Central African Republic` = "Central African Rep.",
                                    `Ivory Coast` = "Côte d'Ivoire",
                                    `Democratic Republic of the Congo` = "Dem. Rep. of the Congo",
                                    `Cape Verde` = "Cabo Verde",
                                    `Czech Republic` = "Czech Rep.",
                                    `Dominican Republic` = "Dominican Rep.",
                                    China = "China, Hong Kong SAR",
                                    Iran = "Iran (Islamic Rep. of)",
                                    `South Korea` = "Rep. of Korea",
                                    Laos = "Lao People's Dem. Rep.",
                                    China = "China, Macao SAR",
                                    Moldova = "Rep. of Moldova",
                                    Macedonia = "The former Yugoslav Republic of Macedonia",
                                    `North Korea` = "Dem. People's Rep. of Korea",
                                    Palestine = "Palestinian",
                                    Russia = "Russian Federation",
                                    `Serbia and Montenegro` = "Serbia and Kosovo (S/RES/1244 (1999))",
                                    Syria = "Syrian Arab Rep.",
                                    `East Timor` = "Timor-Leste",
                                    Tanzania = "United Rep. of Tanzania",
                                    Venezuela = "Venezuela (Bolivarian Republic of)",
                                    Vietnam = "Viet Nam",
                                    NULL = "Stateless",
                                    NULL = "Various/Unknown",
                                    `Democratic Republic of the Congo` = "Congo",
                                    Micronesia = "Micronesia (Federated States of)",
                                    `Serbia and Montenegro` = "Montenegro",
                                    `Saint Pierre and Miquelon` = "Saint-Pierre-et-Miquelon")

# Bar Plot Processing
#--------------------------------------------
asylum_status_gathered <- gather(asylum_status,
                                 "recognized",
                                 "rejected",
                                 key = "decision",
                                 value = "value"
)

asylum_status_gathered$decision <- factor(asylum_status_gathered$decision,
                                          levels = c("rejected",
                                                     "recognized"))
#--------------------------------------------




#--------------------------------------------
#--------------------------------------------
#-------------SHINY UI-----------------------
#--------------------------------------------
#--------------------------------------------

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
                      textOutput("originmapTitle")
                    ),
                    sidebarLayout(position = "right",
                                  sidebarPanel(
                                    selectInput("year_origin",
                                                "Year:",
                                                choices = 2000:2017,
                                                selected = 2014)
                                  ),
                                  mainPanel(
                                    leafletOutput("originMap")
                                  ))),
           tabPanel("Destination",
                    titlePanel(
                      textOutput("destmapTitle")
                    ),
                    sidebarLayout(position = "right",
                                  sidebarPanel(
                                    selectInput("year_dest",
                                                "Year:",
                                                choices = 2000:2017,
                                                selected = 2014)
                                  ),
                                  mainPanel(
                                    
                                  ))))
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
   output$originmapTitle <- renderText({
     paste("Volume of Asylum Applicants Leaving Countries in ", input$year_origin)
   })
#--------------------------------------------
   output$originMap <- renderLeaflet({
     leaflet() %>% 
       addProviderTiles(providers$Esri.WorldStreetMap) %>% 
       addCircles(data = geo_status_origin,
                  weight = log(geo_status_origin$total))
   })
#--------------------------------------------
   output$destmapTitle <- renderText({
     paste("Volume of Asylum Applicants Leaving Countries in ", input$year_dest)
   })
}

shinyApp(ui = ui, server = server)

