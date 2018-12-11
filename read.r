rm(list=ls())

library(tidyverse)
library(lubridate)
library(mice)
set.seed(1)
######## read in police mort data

# ... attach and configure mortality file
fe <- read_csv("./data/fe_9_21_18.csv")
# .... make names more user-friendly
names(fe)<-c("id", "name", "age", "sex", "race", "URL", "death_date", 
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
         age = ifelse(age=="25`", 25, age))%>%
  mutate(age = as.numeric(age)) ## if no coerced NAs, then we've got all the strings

# .... re-label race variable for harmony with census
fe <- fe %>%
  select(id, name, age, sex, race, death_date, 
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
# filter non-use-of-force causes of death, dropping abt 2.5% of cases (excluding vehicles)
fe<-fe%>%
  filter(cause_of_death %in%
           c('Asphyxiated/Restrained','Beaten/Bludgeoned with instrument',
             'Chemical agent/Pepper spray', 'Medical emergency', 'Tasered',
             'Gunshot'))

# write_csv(fe, "fe_filtered.csv")

### filter transgender, too few for imputation model
fe<-fe%>%
  filter(sex != "Transgender")

### read in seer population data, make national data by year, age, race, sex, ethnicity
### count latino as latino only if white latino

pop<-read_fwf("./data/us.1990_2016.19ages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips", "cnty_fips", "reg", "race", 
                           "hisp", "sex", "age", "pop")))

pop<-pop%>%
  mutate(pop = as.integer(pop))%>%
  mutate(race_ethn = ifelse(race==1 & hisp ==0, "white",
                            ifelse(race==2, "black",
                                   ifelse(race==3, "amind",
                                          ifelse(race==4, "asian",
                                                 ifelse(hisp==1, "latino",
                                                        "other"))))))

pop_nat<-pop%>%
  group_by(year, race_ethn, sex, age)%>%
  summarise(pop = sum(pop))%>%
  ungroup()

### recode variables

pop_nat<-pop_nat%>%
  mutate(sex = ifelse(sex=="1", "Male", "Female"),
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
  filter(year>=2010)%>%
  rename(race = race_ethn)


#### impute missing age, gender, race_ethn data
#### bind state population data (by year, 2017/18 not in SEER or mortality files)
fe<-fe%>%
  filter(year>=2010, year<=2016)%>%
  select(age, sex, race, loc_state, year)%>%
  mutate(age = as.numeric(age),
         sex = factor(sex),
         race = factor(race),
         loc_state = factor(loc_state),
         year = as.numeric(year))%>%
  left_join(pop%>% ### join to percent population by race-state-year
              group_by(year, race_ethn, state)%>%
              summarise(pop = sum(pop))%>%
              left_join(pop%>%
                          group_by(state, year)%>%
                          summarise(tot_pop = sum(pop)))%>%
              mutate(pct_pop = pop/tot_pop)%>%
              select(-pop, -tot_pop)%>%
              spread(race_ethn,pct_pop)%>%
              rename(loc_state = state))
  
fe_imp<-mice(fe,
             maxit = 10,
             m=10,
             seed = 1)

fe_complete<-mice::complete(fe_imp, action = "long")
  
### check time series on FE to start where it appears to stabilize
### looks like it levels out after 2005 - going to do 2006 -  2017

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
  group_by(year, race, sex, age, .imp)%>%
  summarise(deaths = n())%>%
  ungroup()

### merge data, create appropriate zeroes
dat<-left_join(pop_nat, fe_nat%>%
                 tidyr::complete(year, race, sex, age, .imp)%>%
                 mutate(deaths = ifelse(is.na(deaths), 0, deaths)))%>%
  filter(!(is.na(.imp)))

# 
# write.csv(dat, "./data/fe_pop_imputed.csv",
#           row.names = FALSE)

#### read in 2010 - 2016 mortality file
### layout is identical for focal vars on all files
#### fixed width file - docs at https://www.cdc.gov/nchs/data/dvs/Multiple_Cause_Record_Layout_2016.pdf

pos<-
  fwf_positions(
    c(69, 77, 154, 445, 484), #begin
    c(69, 78, 156, 446, 486), #end
    c("sex",
      "age_27",
      "cause_113",
      "race",
      "hisorgin") #colname
  )

files<-list("./data/VS16MORT.DUSMCPUB",
            "./data/VS15MORT.DUSMCPUB",
            "./data/VS14MORT.DUSMCPUB",
            "./data/VS13MORT.DUSMCPUB",
            "./data/VS12MORT.DUSMCPUB",
            "./data/VS11MORT.DUSMCPUB",
            "./data/VS10MORT.DUSMCPUB")

mort<-lapply(
  files, 
  function(x){
    read_fwf(x, pos)
  }
)
years<-2016:2010
for(i in 1:length(mort)){
  mort[[i]]$year<-years[[i]]
}

mort<-bind_rows(mort)

### convert 27 cat age into character

mort<-mort%>%
  mutate(
    age = 
      case_when(
        age_27 %in% c("01", "02") ~ "0",
        age_27 %in% c("03", "04", "05", "06") ~ "1-4",
        age_27 == "07" ~ "5-9",
        age_27 == "08" ~ "10-14",
        age_27 == "09" ~ "15-19",
        age_27 == "10" ~ "20-24",
        age_27 == "11" ~ "25-29",
        age_27 == "12" ~ "30-34",
        age_27 == "13" ~ "35-39",
        age_27 == "14" ~ "40-44",
        age_27 == "15" ~ "45-49",
        age_27 == "16" ~ "50-54",
        age_27 == "17" ~ "55-59",
        age_27 == "18" ~ "60-64",
        age_27 == "19" ~ "65-69",
        age_27 == "20" ~ "70-74",
        age_27 == "21" ~ "75-79",
        age_27 == "22" ~ "80-84",
        age_27 %in% c("23", "24", "25", "26") ~ "85+",
        age_27 == "27" ~ "Missing")
  )

### convert race into character

mort<-mort%>%
  mutate(
    race = case_when(
      race == "01" & hisorgin <200 ~ "white",
      hisorgin >200 & hisorgin<996 ~ "latino",
      race == "02" ~ "black",
      race == "03" ~ "amind",
      !(race%in%c("01", "02", "03")) ~ "asian"
    )
  )



### make age/gender/race total death rate, death by cause rate

mort<-mort%>%
  mutate(
    sex = case_when(
      sex=="M"~"Male",
      sex=="F"~"Female")
  )
  

total_mort<-mort%>%
  group_by(year, age, race, sex)%>%
  summarise(deaths = n())%>%
  filter(!(is.na(race)), !(is.na(age)), !(is.na(sex)))

mort_cause<-mort%>%
  group_by(year, age, race, sex, cause_113)%>%
  summarise(deaths = n())%>%
  arrange(age, race, sex, deaths)%>%
  filter(!(is.na(race)), !(is.na(sex)), !(is.na(age)))

### fill in zeroes

mort_cause<-mort_cause%>% 
  ungroup()%>%
  tidyr::complete(year, age, race, sex, cause_113, fill = list(deaths = 0))

### join pop data

#### START HERE - ADD IN ALL YEARS POP DATA FOR MERGE, POOLED AGE SPEC RATES

total_mort<-total_mort%>%
  left_join(pop_nat)

mort_cause<-mort_cause%>%
  left_join(pop_nat)


total_mort<-total_mort%>%
  ungroup()%>%
  mutate(race=
           case_when(
             race=="amind" ~ "American Indian/AK Native",
             race=="asian" ~ "Asian/Pacific Islander",
             race=="black" ~ "African American",
             race=="latino"~ "Latinx",
             race=="white" ~ "White"
           )
  )

mort_cause<-mort_cause%>%
  mutate(race=
           case_when(
             race=="amind" ~ "American Indian/AK Native",
             race=="asian" ~ "Asian/Pacific Islander",
             race=="black" ~ "African American",
             race=="latino"~ "Latinx",
             race=="white" ~ "White"
           )
  )
### output age/race/sex specific mortality counts total and by cause

write.csv(total_mort, "./data/total_mort.csv",
          row.names = FALSE)

write.csv(mort_cause, "./data/mort_cause.csv",
          row.names = FALSE)