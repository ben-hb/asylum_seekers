# This file is intended to do the initial cleaning, processing, and
# visualizations outside of the Shiny file to allow for an easy transfer after
library(leaflet)
library(maps)
library(janitor)
library(sf)
library(shiny)
library(shinythemes)
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
  filter(!origin %in% c("Curaçao", 
                        "Holy See (the)", 
                        "Stateless",
                        "Various/Unknown"))

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
                                    `Democratic Republic of the Congo` = "Congo",
                                    Micronesia = "Micronesia (Federated States of)",
                                    `Serbia and Montenegro` = "Montenegro",
                                    `Saint Pierre and Miquelon` = "Saint-Pierre-et-Miquelon",
                                    China = "Tibetan",
                                    `Turks and Caicos` = "Turks and Caicos Islands",
                                    `Wallis and Futuna` = "Wallis and Futuna Islands")

asylum_status_gathered <- gather(asylum_status,
                                 "recognized",
                                 "rejected",
                                 key = "decision",
                                 value = "value"
)

asylum_status_gathered$decision <- factor(asylum_status_gathered$decision,
                                          levels = c("rejected",
                                                     "recognized"))

# Mapping

data(world.cities)

world.cities$country.etc <- fct_recode(world.cities$country.etc,
                                       `United Kingdom` = "UK",
                                       `Democratic Republic of the Congo` = "Congo",
                                       `South Korea` = "Korea South",
                                       `North Korea` = "Korea North",
                                       `United States of America` = "USA",
                                       `Saint Vincent and the Grenadines` = "Saint Vincent and The Grenadines")

# Because maps is outdated, it doesn't include South Sudan, the youngest country
# in the world. I find it particularly worthwhile to add South Sudan as the
# ongoing conflict in the country has been a substantial source of asylum
# seekers

south_sudan <- world.cities %>% 
  filter(name == "Juba") %>% 
  mutate(country.etc = "South Sudan",
         capital = 1)

world.cities <- rbind(world.cities, south_sudan)

# Starting by geocoding the capital cities of origin countries

asylum_status_origin <- asylum_status %>% 
  filter(year == 2014) %>% 
  group_by(origin) %>% 
  summarize(total = sum(total_decisions, na.rm = TRUE))

all_capitals <- world.cities %>% 
  filter(capital == 1 | name == "Suva" | name == "Ram Allah") %>% 
  rename("country" = "country.etc") %>% 
  select(country, long, lat)

geo_status_origin <- asylum_status_origin %>% 
  left_join(all_capitals,
            by = c("origin" = "country"))

#--------------------------------------------
#--------------------------------------------
#-----------------SHINY UI-------------------
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
navbarMenu("Maps",
           tabPanel("Origin",
                    sidebarLayout(position = "right",
                      sidebarPanel(
                        selectInput("origin_year",
                                    "Year:",
                                    choices = 2000:2017,
                                    selected = 2014)
                      ),
                      mainPanel(
                        leafletOutput("originPlot")
                      )
                    )),
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
   output$originPlot <- renderLeaflet({
     asylum_status_origin <- asylum_status %>% 
       filter(year == input$origin_year) %>% 
       group_by(origin) %>% 
       summarize(total = sum(total_decisions, na.rm = TRUE))
     
     geo_status_origin <- asylum_status_origin %>% 
       left_join(all_capitals,
                 by = c("origin" = "country"))
     
     leaflet(geo_status_origin) %>% 
       addProviderTiles(providers$Esri.WorldStreetMap) %>% 
       addCircles(color = "green", 
                  weight = log(geo_status_origin$total))
   })
}

shinyApp(ui = ui, server = server)

