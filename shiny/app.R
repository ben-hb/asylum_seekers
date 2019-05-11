library(janitor)
library(shiny)
library(shinythemes)
library(leaflet)
library(maps)
library(sf)
library(fs)
library(lubridate)
library(shinyBS)
library(tidyverse)

# The asylum status data from the UNHCR's popstat database provides annual
# information on the status of asylum-seeker applications

# Asylum Status Reading
#--------------------------------------------

# CSV downloaded from "http://popstats.unhcr.org/en/asylum_seekers.csv" I opt to
# read from a local file to decrease render time, however for those wishing to
# rerun this locally you can simply replace the local directory with the url

asylum_status <- read_csv("asylum_seekers.csv",
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

asylum_status$dest  <- fct_recode(asylum_status$dest, 
                                    Bolivia = "Bolivia (Plurinational State of)",
                                    `Central African Republic` = "Central African Rep.",
                                    `Ivory Coast` = "Côte d'Ivoire",
                                    `Democratic Republic of the Congo` = "Dem. Rep. of the Congo",
                                    `Czech Republic` = "Czech Rep.",
                                    `Dominican Republic` = "Dominican Rep.",
                                    China = "China, Hong Kong SAR",
                                    Iran = "Iran (Islamic Rep. of)",
                                    `South Korea` = "Rep. of Korea",
                                    Laos = "Lao People's Dem. Rep.",
                                    China = "China, Macao SAR",
                                    Moldova = "Rep. of Moldova",
                                    Macedonia = "The former Yugoslav Republic of Macedonia",
                                    Russia = "Russian Federation",
                                    `Serbia and Montenegro` = "Serbia and Kosovo (S/RES/1244 (1999))",
                                    Syria = "Syrian Arab Rep.",
                                    `East Timor` = "Timor-Leste",
                                    Tanzania = "United Rep. of Tanzania",
                                    Venezuela = "Venezuela (Bolivarian Republic of)",
                                    NULL = "Stateless",
                                    NULL = "Various/Unknown",
                                    `Democratic Republic of the Congo` = "Congo",
                                    Micronesia = "Micronesia (Federated States of)",
                                    `Serbia and Montenegro` = "Montenegro")

# Asylum Monthly Reading
#--------------------------------------------

# CSV downloaded from http://popstats.unhcr.org/en/asylum_seekers_monthly.csv I
# opt to read from a local file to decrease render time, however for those
# wishing to rerun this locally you can simply replace the local directory with
# the url

asylum_monthly <- read_csv("asylum_seekers_monthly.csv",
                           skip = 3,
                           col_types = cols(
                             `Country / territory of asylum/residence` = col_character(),
                             Origin = col_character(),
                             Year = col_double(),
                             Month = col_character(),
                             Value = col_double()
                           )) %>% 
  clean_names() %>% 
  rename("dest" = "country_territory_of_asylum_residence") %>% 
  filter(!is.na(origin)) %>% 
  filter(!is.na(dest),
         !origin %in% c("Curaçao", "Holy See (the)"))

asylum_monthly <- asylum_monthly %>% 
  mutate(date = ymd(paste(asylum_monthly$year, asylum_monthly$month, "1")))

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
# Get Capital City Coordinates
#--------------------------------------------

data(world.cities)

world.cities$country.etc <- fct_recode(world.cities$country.etc,
                                       `United Kingdom` = "UK",
                                       `Democratic Republic of the Congo` = "Congo",
                                       `South Korea` = "Korea South",
                                       `North Korea` = "Korea North",
                                       `United States of America` = "USA",
                                       `Saint Vincent and the Grenadines` = "Saint Vincent and The Grenadines")

all_capitals <- world.cities %>% 
  filter(capital == 1 | name == "Suva" | name == "Ram Allah") %>% 
  rename("country" = "country.etc") %>% 
  select(country, long, lat)
#--------------------------------------------


#--------------------------------------------
#--------------------------------------------
#-------------SHINY UI-----------------------
#--------------------------------------------
#--------------------------------------------

ui <- shinyUI(
  navbarPage("Asylum Applicants",
             theme = shinytheme("flatly"),
             position = "static-top",
#--------------------------------------------
     tabPanel("Decisions",
              bsModal(
                id = "tutorialModal",
                title = "Welcome to the Asylum Applicant Visualization Tool",
                trigger = "",
                htmlOutput("tutorial")
              ),
              titlePanel(
       textOutput("destTitle")
              ),
              
              sidebarLayout(position = "right",
                            sidebarPanel(
                              selectInput("dest",
                                          "Destination Country:",
                                          choices = sort(unique(asylum_status$dest)),
                                          selected = "Argentina")
                            ),
                            
                            mainPanel(
                              plotOutput("destPlot")
                            )
              )
     ),
navbarMenu("Application Maps",
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
                                    leafletOutput("destMap")
                                  )))),
tabPanel("About")
#--------------------------------------------

   )
   
)

#--------------------------------------------
#--------------------------------------------
#---------------SERVER-----------------------
#--------------------------------------------
#--------------------------------------------


server <- function(input, output, session) {
  toggleModal(session, "tutorialModal", toggle = "open")
  
  output$tutorial <- renderText({
    HTML("<p><b>Introduction</b></br>
In theory, political asylum is designed to function as a way to seek refuge from oppressive governments by moving to more welcoming governments. 
In reality, applicants often are challenged to find a country willing to accept them.</br></p> 
<p>This tool is designed to shine light upon the sources of asylum applicants and the countries they choose to go to, and how easy that process is. 
You'll find graphics that show how likely governments are to accept asylum applicants, where asylum applicants tend to come from, and where they tend to go.</br></p> 
All of the data is sourced from the UN High Commissioner on Refugees, a big thanks to their team for curating this data and making it publicly accessible.</br> 
For the maps, size of dot correlates with number of applicants on a log scale. 
Labels with numbers of applicants for each country will be added shortly. 
    
    For those interested in the technical end of this program, you can check out the code here: https://github.com/ben-hb/asylum_seekers"
    )
  })
#--------------------------------------------
   output$destTitle <- renderText({
     paste("Acceptance and Rejection of Asylum Applicants by ", input$dest)
   }) 
#--------------------------------------------
   output$destPlot <- renderPlot({
      dest_status <- asylum_status_gathered %>% 
        filter(dest == input$dest)
      
      ggplot(dest_status) +
        geom_col(aes(x = year, y = value, fill = factor(decision))) +
        labs(
          y = "Number of Rejections",
          x = "Year",
          rejected = "Rejected"
        ) +
        guides(fill=guide_legend(title=NULL)) +
        scale_fill_discrete(name="Title",
                            breaks=c("rejected", "recognized"),
                            labels=c("Rejected", "Recognized"))
   })
#--------------------------------------------
  output$acceptancePlot <- renderPlot({
    asylum_monthly_accept <- asylum_monthly %>% 
      filter(origin == input$originaccept,
             dest == input$destaccept)
    
    ggplot(asylum_monthly_accept) +
      geom_col(aes(x = date, y = value))
  })
#--------------------------------------------
   output$originmapTitle <- renderText({
     paste("Asylum Applicants Applying to Leave from each Country in", input$year_origin)
   })
#--------------------------------------------
   output$originMap <- renderLeaflet({
     status_origin <- asylum_status %>% 
       filter(year == input$year_origin) %>% 
       group_by(origin) %>% 
       summarize(total = sum(applied_during_year, na.rm = TRUE))
     
     geo_status_origin <- status_origin %>% 
       left_join(all_capitals,
                 by = c("origin" = "country"))     
     
     leaflet() %>% 
       addProviderTiles(providers$Esri.WorldStreetMap) %>% 
       addCircleMarkers(data = geo_status_origin,
                  weight = sqrt(geo_status_origin$total)/10,
                  popup = paste0("<b>Country: </b>",
                                 geo_status_origin$origin,
                                 "</br>",
                                 "<b>Number of Asylum Seekers Emmigrating: </b>",
                                 formatC(geo_status_origin$total,
                                         format = "f",
                                         big.mark = ",",
                                         drop0trailing = TRUE
                                 )))
   })
  

#--------------------------------------------
   output$destmapTitle <- renderText({
     paste("Asylum Applicants Entering Trying to Enter each Country in ", input$year_dest)
   })
#--------------------------------------------
   output$destMap <- renderLeaflet({
     status_dest <- asylum_status %>% 
       filter(year == input$year_dest) %>% 
       group_by(dest) %>% 
       summarize(total = sum(applied_during_year, na.rm = TRUE))
     
     geo_status_dest <- status_dest %>% 
       left_join(all_capitals,
                 by = c("dest" = "country")) 
     
     leaflet() %>% 
       addProviderTiles(providers$Esri.WorldStreetMap) %>% 
       addCircleMarkers(data = geo_status_dest,
                  weight = sqrt(geo_status_dest$total)/10,
                  popup = paste0("<b>Country: </b>",
                                 geo_status_dest$dest,
                                 "</br>",
                                 "<b>Number of Asylum Seekers Immigrating: </b>",
                                 formatC(geo_status_dest$total,
                                         format = "f",
                                         big.mark = ",",
                                         drop0trailing = TRUE)))
   })
}

shinyApp(ui = ui, server = server)

