rm(list=ls())

library(tidyverse)
library(lubridate)
library(mice)

######## read in police mort data

# ... attach and configure mortality file
fe <- read_csv("./data/fe_data_7_18_18.csv")
# .... make names more user-friendly
names(fe)<-c("id", "name", "age", "gender", "race", "URL", "death_date", 
                 "loc_address", "loc_city", "loc_state", "loc_zip", "loc_county", 
                 "loc_full_address", "Latitude", "Longitude", "agency", 
                 "cause_of_death","cause_description", "official_disposition", 
                 "news_url", "mental_illness", "video", "null1", "dateanddesc", 
                 "null2", "id2", "year", "null3")

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
         age = ifelse(age=="60s", 55, age),
         age = ifelse(age=="70s", 55, age),
         age = ifelse(age=="55.", 55, age),
         age = ifelse(age=="20s-30s", 30, age),
         age = ifelse(age=="40-50", 45, age),
         age = ifelse(age=="25-30", 27, age),
         age = ifelse(age=="24-25", 24, age),
         age = ifelse(age=="45 or 49", 47, age),
         age = ifelse(age=="25`", 25, age))


# .... re-label race variable for harmony with census
fe <- fe %>%
  select(id, name, age, gender, race, death_date, 
         loc_state, loc_county, loc_zip, loc_city, agency,
         Latitude, Longitude, 
         agency, cause_of_death, official_disposition) %>%
  mutate(race = ifelse(race == "African-American/Black", "black", race),
         race = ifelse(race == "Asian/Pacific Islander", "asian", race),
         race = ifelse(race == "European-American/White", "white", race),
         race = ifelse(race == "Hispanic/Latino", "latino", race),
         race = ifelse(race == "Hispanic/Latinio", "latino", race),
         race = ifelse(race == "Middle Eastern", "white", race),
         race = ifelse(race == "Native American/Alaskan", "amind", race),
         race = ifelse(race == "Race unspecified", NA, race))
#### make date from date of death
fe$death_date<-mdy(fe$death_date)
fe$year<-year(fe$death_date)
### 
### filter ruled suicides
fe<-fe%>%
  mutate(official_disposition=tolower(official_disposition))

fe<-fe[-grep("suicide", fe$official_disposition),]
# filter non-use-of-force causes of death, dropping abt 2.5% of cases
fe<-fe%>%
  filter(cause_of_death %in%
           c('Asphyxiated/Restrained','Beaten/Bludgeoned with instrument',
             'Chemical agent/Pepper spray', 'Medical emergency', 'Tasered',
             'Gunshot', 'Vehicle'))

### filter transgender, too few for imputation model
fe<-fe%>%
  filter(gender != "Transgender")

#### impute missing age, gender, race_ethn data
fe<-fe%>%
  filter(year>2005)%>%
  select(age, gender, race, loc_state, year)%>%
  mutate(age = as.numeric(age),
         gender = factor(gender),
         race = factor(race),
         loc_state = factor(loc_state),
         year = as.numeric(year))


fe_imp<-mice(fe,
             maxit = 10,
             m=10)

fe_complete<-complete(fe_imp, action = "long")

### read in seer population data, make national data by year, age, race, sex, ethnicity
### count latino as latino only if white latino

pop<-read_fwf("./data/us.1990_2016.19ages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
              c("year", "state", "st_fips", "cnty_fips", "reg", "race", 
                "hisp", "sex", "age", "pop")))

pop_nat<-pop%>%
  mutate(pop = as.integer(pop))%>%
  mutate(race_ethn = ifelse(race==1 & hisp ==0, "white",
                            ifelse(race==2, "black",
                                   ifelse(race==3, "amind",
                                          ifelse(race==4, "asian",
                                                 ifelse(hisp==1, "latino",
                                                        "other"))))))%>%
  group_by(year, race_ethn, sex, age)%>%
  summarise(pop = sum(pop))%>%
  ungroup()

### recode variables

pop_nat<-pop_nat%>%
  mutate(gender = ifelse(sex=="1", "Male", "Female"),
         age = ifelse(age=="00", "0", 
                  ifelse(age=="01", "1-4",
                     ifelse(age=="02", "5-9",
                        ifelse(age=="03", "10-14",
                           ifelse(age=="04", "15-19",
                              ifelse(age=="05", "20-24",
                                 ifelse(age=="06", "25-29",
                                    ifelse(age=="07", "30-34",
                                       ifelse(age=="08", "35-39",
                                          ifelse(age=="09", "40-44",
                                             ifelse(age=="10", "45-49",
                                                ifelse(age=="11", "50-54",
                                                   ifelse(age=="12", "55-59",
                                                      ifelse(age=="13", "60-64",
                                                         ifelse(age=="14", "65-69",
                                                            ifelse(age=="15", "70-74",
                                                               ifelse(age=="16", "75-79",
                                                                  ifelse(age=="17", "80-84",
                                                                     "85+")))))))))))))))))))%>%
  filter(year>2005)%>%
  rename(race = race_ethn)%>%
  select(-sex)
  
### check time series on FE to start where it appears to stabilize
### looks like it levels out after 2005 - going to do 2006 - 2017

### make fe national data by year, age, race, sex, ethnicity
fe_nat<-fe_complete%>%
  mutate(age = as.integer(age))%>%
  mutate(age = ifelse(age==0, "0", 
                  ifelse(age<5, "1-4",
                     ifelse(age<10, "5-9",
                        ifelse(age<15, "10-14",
                           ifelse(age<20, "15-19",
                              ifelse(age<25, "20-24",
                                 ifelse(age<30, "25-29",
                                    ifelse(age<35, "30-34",
                                       ifelse(age<40, "35-39",
                                          ifelse(age<45, "40-44",
                                             ifelse(age<50, "45-49",
                                                ifelse(age<55, "50-54",
                                                   ifelse(age<60, "55-59",
                                                      ifelse(age<65, "60-64",
                                                         ifelse(age<70, "65-69",
                                                            ifelse(age<75, "70-74",
                                                               ifelse(age<80, "75-79",
                                                                  ifelse(age<85, "80-84",
                                                                     "85+")))))))))))))))))))%>%
  group_by(year, race, gender, age, .imp)%>%
  summarise(deaths = n())%>%
  ungroup()

### merge data, create appropriate zeroes
dat<-left_join(pop_nat, fe_nat%>%
                 tidyr::complete(year, race, gender, age, .imp)%>%
                 mutate(deaths = ifelse(is.na(deaths), 0, deaths)))%>%
  filter(!(is.na(.imp)))


write_csv(dat, "./data/fe_pop_imputed.csv")
