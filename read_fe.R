#### Risk of being killed by police in the U.S. by age, race/ethnicity, and sex
#### Frank Edwards, Hedwig Lee, Michael Esposito
#### read in FE
library(tidyverse)
library(lubridate)
library(mice)

set.seed(1)
######## read in police mort data
setwd("~/Dropbox/data_analysis/police-mort-age-spec")
# ... attach and configure mortality file
fe <- read_csv("./data/fe_1_25_19.csv")

# .... make names more user-friendly
names(fe)<-c("id", "name", "age", "sex", "race", "URL", "death_date", 
                 "loc_address", "loc_city", "loc_state", "loc_zip", "loc_county", 
                 "loc_full_address", "Latitude", "Longitude", "agency", 
                 "cause_of_death","cause_description", "official_disposition", 
                 "news_url", "mental_illness", "video", "dateanddesc", 
                 "id_formula", "id2", "year")

### work on age unique(fe$age) is character
### when a range, set in middle, round down
fe<-fe%>%
  mutate(age = ifelse(grepl("18 months", age), "1", age),
         age = ifelse(grepl("mon", age), "0", age),
         age = ifelse(grepl("days", age), "0", age),
         age = ifelse(age=="18-25", 21, age),
         age = ifelse(age=="46/53", 49, age),
         age = ifelse(age=="20s", 25, age),
         age = ifelse(age=="30s", 35, age),
         age = ifelse(age=="40s", 45, age),
         age = ifelse(age=="50s", 55, age),
         age = ifelse(age=="60s", 65, age),
         age = ifelse(age=="70s", 75, age),
         age = ifelse(age=="55.", 55, age),
         age = ifelse(age=="20s-30s", 30, age),
         age = ifelse(age=="40-50", 45, age),
         age = ifelse(age=="25-30", 27, age),
         age = ifelse(age=="24-25", 24, age),
         age = ifelse(age=="45 or 49", 47, age),
         age = ifelse(age=="25`", 25, age))%>%
  mutate(age = as.numeric(age)) ## if no coerced NAs, then we've got all the strings

### 819 missing age pre and post re-code

#### make date from date of death
fe$death_date<-mdy(fe$death_date)
fe$year<-year(fe$death_date)

# .... re-label race variable for harmony with census
fe <- fe %>%
  select(id, name, age, sex, race, death_date, 
         loc_state, loc_county, loc_zip, loc_city, agency,
         Latitude, Longitude, year,
         agency, cause_of_death, official_disposition) %>%
  mutate(race = ifelse(race == "African-American/Black", "black", race),
         race = ifelse(race == "Asian/Pacific Islander", "asian", race),
         race = ifelse(race == "European-American/White", "white", race),
         race = ifelse(race == "Hispanic/Latino", "latino", race),
         race = ifelse(race == "Hispanic/Latinio", "latino", race),
         race = ifelse(race == "Middle Eastern", "white", race),
         race = ifelse(race == "Native American/Alaskan", "amind", race),
         race = ifelse(race == "Race unspecified", NA, race))

### 8690 missing race post re-code (of 25468), 8690 as Race unspecified in initial data
## by year table: fe%>%group_by(year)%>%summarise(sum(is.na(race))/n())

## by year table: 
missing_yr<-fe%>%group_by(year)%>%summarise(pct_missing = sum(is.na(race))/n())
ggplot(missing_yr, aes(x=year, y = pct_missing)) + geom_line()
## super clear pattern - missingness is function of time - 
## very high % missing (>0.5 until 2008)
## missing dips below 0.2 between 2012 and 2017
## Is only below 0.1 in 14 and 15

### Cause of death. Separate use-of-force, suicide, other officer-involved

fe<-fe%>%
  mutate(official_disposition=tolower(official_disposition))

#fe<-fe[-grep("suicide", fe$official_disposition),]
# filter non-use-of-force causes of death, dropping abt 2.5% of cases (excluding vehicles)
# fe<-fe%>%
#   filter(cause_of_death %in%
#            c('Asphyxiated/Restrained','Beaten/Bludgeoned with instrument',
#              'Chemical agent/Pepper spray', 'Medical emergency', 'Tasered',
#              'Gunshot'))

fe<-fe%>%
  mutate(
    fe_cause_of_death = 
      case_when(grepl("suicide", official_disposition) ~ "suicide",
                cause_of_death %in% c('Asphyxiated/Restrained',
                                      'Beaten/Bludgeoned with instrument',
                                      'Chemical agent/Pepper spray', 'Medical emergency', 
                                      'Tasered',
                                      'Gunshot') ~
                  "officer_force",
                cause_of_death == "Vehicle" ~ "vehicle",
                cause_of_death %in% c("Burned/Smoke inhalation", "Drowned",
                                      "Drug overdose", "Fell from a height",
                                      "Other", "Stabbed", "Undetermined") ~
                  "other"))

fe<-fe%>%
  filter(year<2019)