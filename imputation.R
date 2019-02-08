#### Risk of being killed by police in the U.S. by age, race/ethnicity, and sex
#### Frank Edwards, Hedwig Lee, Michael Esposito
#### impute missing race/ethnicity data from fatal encounters
### based on surname and county population
rm(list=ls())
library(tidyverse)
library(mice)
library(wru)
library(jsonlite)


source("read_fe.r")
source("read_pop.r")

#### impute missing age, gender, race_ethn data
#### bind state population data (by year, 2017/18 not in SEER or mortality files)


#### CODE FROM police-mort repo

fe_new<-fe%>%
  mutate(Latitude = ifelse((id==11840)&(Latitude<1), 
                           Latitude + 47, 
                           ifelse((id == 11921)&(Latitude<1),
                                  Latitude + 41,
                                  ifelse((id == 12963)&(Latitude<1),
                                         Latitude + 38,
                                         ifelse((id==14073)&(Latitude<1),
                                                Latitude + 37,
                                                Latitude)))),
         Latitude = ifelse(id==20157, 32.7398191, Latitude),
         Longitude = ifelse(id==20157, -97.4412267, Longitude))

####### run script to link lat/long -> FIPS block
####### first check to see if crosswalk file is in directory, only run on new FE data
### API LIMIT requires file split
# 
# files<-list.files("./data")
# if(!("block_map.csv"%in%files)){
#   coords<-fe_new%>%
#     select(Latitude, Longitude)%>%
#     mutate("FIPS_block" = NA,
#            "FIPS_county" = NA,
#            "FIPS_state" = NA,
#            "STname" = NA,
#            "API_status" = NA)
#   if("block_map1.csv"%in%files){
#     completed<-read_csv("./data/block_map1.csv")
#     coords<-coords[nrow(completed):nrow(coords),]
#   }
#   for(i in 1:nrow(coords)){
#     url<-paste("https://geo.fcc.gov/api/census/block/find?latitude=",
#                coords[i, 1], 
#                "&longitude=", 
#                coords[i, 2], 
#                "&showall=true&format=json",
#                sep="")
#     
#     temp<-fromJSON(url)
#     print(i)
#     print(temp$status)
#     
#     coords[i, 3:7]<-c(temp$Block$FIPS,
#                       temp$County$FIPS,
#                       temp$State$FIPS,
#                       temp$State$code,
#                       temp$status)
#     
#   }
#   write.csv(coords, "./data/block_map2.csv", row.names=FALSE)
#   write.csv(rbind(completed, coords[2:nrow(coords),]), "./data/block_map.csv", row.names = FALSE)
# }

block_map<-read_csv("./data/block_map.csv",
                    col_types = "ddccccc")

fe_new<-bind_cols(fe_new, block_map)

#### get clean surnames
fe_new<-fe_new%>%
  mutate(name_mod = str_replace(fe_new$name, '[\"].*[\"]', ""))
fe_new<-fe_new%>%
  mutate(name_mod = ifelse(grepl("Name withheld", fe_new$name_mod),
                           NA, fe_new$name_mod))
fe_new<-fe_new%>%
  mutate(name_mod = ifelse(grepl("Jane Doe", fe_new$name_mod),
                           NA, fe_new$name_mod))
fe_new<-fe_new%>%
  mutate(name_mod = ifelse(grepl("John Doe", fe_new$name_mod),
                           NA, fe_new$name_mod))

fe_new<-fe_new%>%
  mutate(name_mod = ifelse(grepl("jr.", tolower(name_mod)), substr(name_mod, 1, nchar(name_mod)-4), name_mod),
         name_mod = ifelse(grepl("jr", tolower(name_mod)), substr(name_mod, 1, nchar(name_mod)-3), name_mod),
         name_mod = ifelse(grepl("sr.", tolower(name_mod)), substr(name_mod, 1, nchar(name_mod)-4), name_mod),
         name_mod = ifelse(grepl("II", name_mod), substr(name_mod, 1, nchar(name_mod)-3), name_mod),
         name_mod = ifelse(grepl("III", name_mod), substr(name_mod, 1, nchar(name_mod)-4), name_mod),
         name_mod = ifelse(grepl("IV", name_mod), substr(name_mod, 1, nchar(name_mod)-3), name_mod))

fe_new<-fe_new%>%
  mutate(name_mod = gsub("-" , " ", fe_new$name_mod), #for hyphenateds, take only the last name
         name_mod = trimws(name_mod, which="both"))

fe_new<-fe_new%>%
  mutate(surname = word(name_mod, -1))

### SELECT VARS FOR IMPUTATION
fe_imp_dat<-fe_new%>%
  mutate(year = year(death_date))%>%
  select(id, year, loc_state, surname, age, sex, race, 
         fe_cause_of_death, FIPS_county)%>%
  mutate(sex = ifelse(sex=="Male", 1, 0))

#### join to pop county pct data
pct_cnty_pop<-pop_cnty%>%
  mutate(FIPS_county = paste(st_fips, cnty_fips, sep=""))%>%
  group_by(year, FIPS_county, race)%>%
  summarise(pop = sum(pop))%>%
  left_join(pop_cnty%>%
              mutate(FIPS_county = paste(st_fips, cnty_fips, sep=""))%>%
              group_by(year, FIPS_county)%>%
              summarise(tot_pop = sum(pop)))%>%
  ungroup()%>%
  mutate(pct_pop = pop / tot_pop)%>%
  select(year, FIPS_county, race, pct_pop)

pct_cnty_wide<-pct_cnty_pop%>%
  group_by(FIPS_county, year)%>%
  spread(race, pct_pop)

## fix problem counties - 
## HI has no county dat for prior to 2000 (+3 in recode)
fe_imp_dat<-fe_imp_dat%>%
  mutate(FIPS_county = ifelse(substr(FIPS_county,1,2)=="15" & year < 2003, 
         "15900", FIPS_county))
# >% 
#   mutate(FIPS_county = ifelse(FIPS_county%in%
#                                 c("08001", "08059", "08123"), 
#                               "08014", FIPS_county)) # fix Broome recode


fe_imp_dat_join<-fe_imp_dat%>%
  left_join(pct_cnty_wide)

fe_imp_dat_join<-merge_surnames(fe_imp_dat_join)

### check county coverage: 
View(fe_imp_dat_join%>%filter(is.na(white)))
### recodes missing - because pop is lagged, this is tricky
### sub recodes with state averages
pop_st_avg<-fe_imp_dat_join%>%
  group_by(loc_state, year)%>%
  summarise_at(c("amind", "asian", "black", "latino", "white"), 
               mean, na.rm=TRUE)
names(pop_st_avg)[3:ncol(pop_st_avg)]<-paste(
  "st", names(pop_st_avg)[3:ncol(pop_st_avg)], 
  sep="_")

fe_imp_dat_join<-fe_imp_dat_join%>%
  left_join(pop_st_avg)%>%
  mutate(amind = ifelse(is.na(amind), st_amind, amind),
         asian = ifelse(is.na(asian), st_asian, asian),
         black = ifelse(is.na(black), st_black, black),
         latino = ifelse(is.na(latino), st_latino, latino),
         white = ifelse(is.na(white), st_white, white))


### format for mice
fe_imp_dat_join<-fe_imp_dat_join%>%
  select(id, year, age, sex, race, fe_cause_of_death,
         amind, asian, black, latino, p_whi, p_bla, 
         p_his, p_asi)%>%
  mutate_if(is.character, as.factor)%>%
  mutate(sex = factor(sex))
#white pct pop colinear with others, remove it and p_oth from imputation model
# these are constrained to sum to 1

#### impute!

fe_imp<-mice(fe_imp_dat_join,
             maxit = 1,
             m=5,
             seed = 1)
