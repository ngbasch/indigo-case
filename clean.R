###############################################################################
###   AUTHOR: Nathan Basch
###   DATE: 5/6/2002
###
###   DESCRIPTION: LOAD AND CLEAN USDA QUICKSTATS DATA; LOAD TIGRIS SHAPEFILE
###############################################################################

### Set Environment -------------------------

# Clear the environment 
rm(list=ls())

# Set working directory (CHANGE based on where you download NASS data)
path <- "C:\\Users\\nated\\Documents\\Documents_NB\\Projects\\Indigo"
options(stringsAsFactors = F)
setwd(path)

# Load packages
library(tidyverse)
library(janitor)
library(readxl)
library(rjson)
library(plotly)
library(tigris)
library(maps)
library(leaflet)
library(rmapshaper)

#Import Data ---------------------------------------------------------------------
#View first few lines
x<-readLines("Input/qs.crops_20200429.txt", 10)
#Load raw data
raw<-read_delim(file = "Input/qs.crops_20200429.txt", delim = "\t",escape_double = FALSE)

#Load county shape files ------------------------------------------
#States, excluding alaska
states = state.fips%>%distinct(abb)%>%pull(abb)
counties_tigris<-counties(state = states, cb = T)


#Basic Cleaning -----------------------------------------------------------------------

clean <- raw %>% 
  clean_names() %>%
  filter(#Choose after 1990
         year >= 1990,
         #Per Nick's instructions, only focus on Survey data (not Census)
         source_desc == "SURVEY",
         #Domain doesn't appear to be very relevant for five crops of interest, so just take the total.
         domain_desc == "TOTAL",
         #Five crops of interest
         commodity_desc %in% c("CORN", "WHEAT", "SOYBEANS", "COTTON", "RICE"),
         #Per Nick's instructions, ignore Corn Silage
         util_practice_desc != "SILAGE",
         #Ignore monthly or "POINT IN TIME" data
         freq_desc == "ANNUAL",
         #Remove "OPERATIONS" unit from area harvested as it doesn't appear to be relevant
         unit_desc!= "OPERATIONS",
         #Area Planted, Area Harvested, Yield, and Production by Crop and Variety
         statisticcat_desc %in% c("AREA PLANTED", "AREA HARVESTED", "YIELD", "PRODUCTION")
         )%>%
  mutate(value_num = as.numeric(gsub(",","",value)),
         #Create a missing code variable to see why data is missing
         missing_code = ifelse(is.na(value_num), value, NA))%>%
  select(-value, -sector_desc)

#Further Clean Data ------------------------------------------------------------------------

#County Level by Year
county<-
  clean%>%
  filter(agg_level_desc == "COUNTY")%>%
  #For Yield, remove planted or net planted ACRE and focus just on Yield = Production / Area Harvested.
  filter(!(unit_desc %in% c("BU / NET PLANTED ACRE", "BU / PLANTED ACRE", "LB / NET PLANTED ACRE")))%>%
  select(source_desc, year, state_fips_code, state_alpha, asd_code, asd_desc, county_code, county_name, 
         commodity_desc, class_desc,statisticcat_desc, util_practice_desc, prodn_practice_desc, unit_desc, value_num)


#1. Calculate the total Production Practices for counties missing "ALL PRODUCTION PRACTICES"
county_missing_prod<-
  county%>%
  group_by(source_desc, year, commodity_desc, class_desc, statisticcat_desc, util_practice_desc, unit_desc, 
           state_fips_code, state_alpha, asd_code, asd_desc, county_code, county_name)%>%
  mutate(contains_all_prodn = sum(prodn_practice_desc == "ALL PRODUCTION PRACTICES"))%>%
  #Yield is not addititive, so remove and calculate in step 3
  filter(contains_all_prodn == 0, statisticcat_desc != "YIELD")%>%
  #For now, filter to just irrigated and non-irrigated. There are several other prodn categories (mostly for non-irrigated),
  #but some of them appear to be subtotals.
  filter(prodn_practice_desc %in% c("IRRIGATED", "NON-IRRIGATED"))%>%
  summarise(`ALL PRODUCTION PRACTICES` = sum(value_num, na.rm=T))%>%
  gather(prodn_practice_desc, value_num,`ALL PRODUCTION PRACTICES`)

#Bind data where "ALL PRODUCTION PRACTICES" were missing
county_all<-
  county%>%
  filter(prodn_practice_desc == "ALL PRODUCTION PRACTICES")%>%
  bind_rows(county_missing_prod)

#2. Calculate the Class subtotal for counties missing "ALL CLASSES"
county_missing_class<-
  county_all%>%
  #Corn appears to be OK in terms of class_desc. It is always equal to "ALL CLASSES" for silage and grain. No need to impute.
  filter(commodity_desc != "CORN")%>%
  group_by(source_desc, year, commodity_desc, prodn_practice_desc, statisticcat_desc, util_practice_desc, unit_desc, state_fips_code, state_alpha, asd_code, asd_desc, county_code, county_name)%>%
  mutate(contains_all_class = sum(class_desc == "ALL CLASSES"))%>%
  #Yield is not addititive, so remove and calculate later.
  filter(contains_all_class == 0, statisticcat_desc != "YIELD")%>%
  summarise(`ALL CLASSES` = sum(value_num, na.rm=T))%>%
  gather(class_desc, value_num,`ALL CLASSES`)

#Bind data where "ALL CLASSES" were missing
county_all<-
  county_all%>%
  bind_rows(county_missing_class)

#3. Recalculate Yield  = Production / Area Harvested, where Yield is missing
yield_missing<-
  county_all%>%
  #Group by every column except these four:
  group_by_at(setdiff(names(county_all), c("value_num", "missing_code", "statisticcat_desc", "unit_desc")))%>%
  mutate(missing_yield = sum(statisticcat_desc %in% "YIELD"))%>%
  #Remove combos that aren't missing yield
  filter(missing_yield == 0)%>%
  select(-unit_desc)%>%
  #Turn the metrics from rows into columns
  spread(statisticcat_desc, value_num)%>%
  clean_names()%>%
  mutate(#Calculate Yield for missing values
          value_num = production/ area_harvested,
         #Cotton: multiply by 480 since unit is 480 lb bales. Rice: multiply by 100 to convert CWT to lb.
         value_num = ifelse(commodity_desc == "COTTON", value_num  * 480,
                              ifelse(commodity_desc == "RICE", value_num * 100,
                                value_num)),
         #Change yield units to "BU / ACRE" for Corn, Soybeans and Wheat. and LB / ACRE for Cotton and Rice.
         unit_desc = ifelse(commodity_desc %in% c("CORN", "SOYBEANS", "WHEAT"), "BU / ACRE", "LB / ACRE"),
         statisticcat_desc = "YIELD")%>%
  select(-missing_yield, -area_harvested, - area_planted, -production)%>%
  #Remove values where production and/or area harvested were missing, and therefore yield could not be calculated
  filter(!is.na(value_num))
    
#Bind data where YIELD wAS missing
county_all<-
  county_all%>%
  bind_rows(yield_missing)%>%
  arrange(year,state_alpha, county_name)

  


# Clean National Data ---------------------------------------------------------------
#County classes:
county_classes<-county%>%tabyl(class_desc)

#County Level by Year
national<-
  clean%>%
  #Take annual data at national level
  filter(agg_level_desc == "NATIONAL", reference_period_desc == "YEAR", unit_desc!= "$", prodn_practice_desc == "ALL PRODUCTION PRACTICES")%>%
  #Take relevant products
  filter(class_desc %in% county_classes$class_desc)%>%
  select(source_desc, year, commodity_desc, class_desc,statisticcat_desc, util_practice_desc, 
         unit_desc, value_num )

    

#Save data --------------------------------------------------------------------------

#Write to R Data compressed file
save(county_all, file = "Intermediate/county_all.Rda")
save(national, file = "Intermediate/national.Rda")
save(counties_tigris, file = "Intermediate/sp_counties.Rda")

#Write two FDA tables to CSV
write_csv(national, "Intermediate/national.csv")
write_csv(county_all, "Intermediate/county_all.csv")


#Checks --------------------------------------------------------------------------------
#Print observations in each year, commodity, and metric:
for (i in 1990:2019){
  print(county_all%>%filter(year == i, class_desc == "ALL CLASSES")%>%tabyl(commodity_desc, statisticcat_desc)%>%
          mutate(year = i))
}

#Compare county annual totals to national total (will not be exactly same, but how far off is it?)
compare_county_national<-
  county_all%>%
  #Ignore Yield, since it's a function of production and area harvested
  filter(statisticcat_desc!= "YIELD")%>%
    group_by(year, commodity_desc, class_desc, statisticcat_desc, util_practice_desc)%>%
    summarise(value_num_county = sum(value_num, na.rm=T))%>%
    left_join(national)%>%
    mutate(difference = value_num_county - value_num,
           county_perc_total = abs(round(value_num_county / value_num, 3)))

ggplot(compare_county_national, aes(x = value_num_county, y = value_num, colour = commodity_desc))+
  geom_point()+
  scale_y_continuous(label = scales::comma)+
  labs(x = "County Data Aggregated", y = "National Data", title = "How far off are aggregated county estimates to national estimates?")+
  facet_wrap(~statisticcat_desc, scales = "free")+
  theme_bw()
