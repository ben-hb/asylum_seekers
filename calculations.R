# This file is intended to do the initial cleaning, processing, and
# visualizations outside of the Shiny file to allow for an easy transfer after
library(leaflet)
library(maps)
library(janitor)
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
         "recognized" = "statistics_filter_decisions_recognized") 

asylum_status$origin  <- fct_recode(asylum_status$origin, 
                                    Bolivia = "Bolivia (Plurinational State of)",
                                    Brunei = "Brunei Darussalam",
                                    `Central African Republic` = "Central African Rep.",
                                    `Côte d'Ivoire` = "Ivory Coast",
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
                                    `Serbia and Montenegro` = "Serbia and Kosovo (S/RES/1244 (1999)",
                                    Syria = "Syrian Arab Rep.",
                                    `East Timor` = "Timor-Leste",
                                    Tanzania = "United Rep. of Tanzania",
                                    Venezuela = "Venezuela (Bolivarian Republic of)",
                                    Vietnam = "Viet Nam",
                                    NULL = "Stateless",
                                    NULL = "Various/Unknown"
                                    )


  
# The monthly asylum data, also from the UNHCR popstat database, offers a lower
# amount of information, only including the total number of asylum-seekers, but
# on a more regular (monthly) basis

asylum_monthly <- read_csv("http://popstats.unhcr.org/en/asylum_seekers_monthly.csv",
                           skip = 3,
                           col_types = cols(
                             `Country / territory of asylum/residence` = col_character(),
                             Origin = col_character(),
                             Year = col_double(),
                             Month = col_character(),
                             Value = col_double()
                           )) %>% 
  clean_names() %>% 
  rename("dest" = "country_territory_of_asylum_residence")

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

world.cities$country.etc <- fct_recode(`United Kingdom` = "UK",
                                       `Democratic Republic of the Congo` = "Congo",
                                       `South Korea` = "Korea South",
                                       `North Korea` = "Korea North",
                                       `United States of America` = "USA",
                                       `Saint Vincent and the Grenadines` = "Saint Vincent and The Grenadines")

# Starting by geocoding the capital cities of origin countries

all_capitals <- world.cities %>% 
  filter(capital == 1 | name == "Suva" | name == "Ram Allah") %>% 
  rename("country" = "country.etc") %>% 
  select(country, long, lat)

geo_status <- asylum_status %>% 
  anti_join(all_capitals,
            by = c("origin" = "country"))

sf_status <- st_as_sf(geo_status,
                      coords = c("long", "lat"),
                      crs = 4326)

