rm(list=ls())
library(tidyverse)
library(wru)
library(jsonlite)

source("read_fe.r")
source("read_pop.r")

theme_set(theme_minimal())

### FIX geocoding mistakes in the data

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

###### run script to link lat/long -> FIPS block# 
# coords<-fe_new%>%
#   select(Latitude, Longitude)%>%
#   mutate("FIPS_block" = NA,
#          "FIPS_county" = NA,
#          "FIPS_state" = NA,
#          "STname" = NA,
#          "API_status" = NA)
# for(i in 1:nrow(coords)){
#   url<-paste("https://geo.fcc.gov/api/census/block/find?latitude=",
#              coords[i, 1],
#              "&longitude=",
#              coords[i, 2],
#              "&showall=true&format=json",
#              sep="")
#   
#   temp<-fromJSON(url)
#   print(i)
#   print(temp$status)
#   
#   coords[i, 3:7]<-c(temp$Block$FIPS,
#                     temp$County$FIPS,
#                     temp$State$FIPS,
#                     temp$State$code,
#                     temp$status)
#   
# }
# 
# write.csv(coords,
#           "./data/block_map.csv", row.names = FALSE)


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
  mutate(name_mod = ifelse(grepl("jr.", tolower(name_mod)), 
                           substr(name_mod, 1, nchar(name_mod)-4), name_mod),
         name_mod = ifelse(grepl("jr", tolower(name_mod)), 
                           substr(name_mod, 1, nchar(name_mod)-3), name_mod),
         name_mod = ifelse(grepl("sr.", tolower(name_mod)), 
                           substr(name_mod, 1, nchar(name_mod)-4), name_mod),
         name_mod = ifelse(grepl("II", name_mod), 
                           substr(name_mod, 1, nchar(name_mod)-3), name_mod),
         name_mod = ifelse(grepl("III", name_mod), 
                           substr(name_mod, 1, nchar(name_mod)-4), name_mod),
         name_mod = ifelse(grepl("IV", name_mod), 
                           substr(name_mod, 1, nchar(name_mod)-3), name_mod))

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
  spread(race, pct_pop)%>%
  ungroup()

fe_imp_dat_join<-fe_imp_dat%>%
  left_join(pct_cnty_wide)

fe_imp_dat_join<-merge_surnames(fe_imp_dat_join)

### check county coverage: 
### sub recodes / missings with state averages. about 30 cases

pop_st_avg<-fe_imp_dat_join%>%
  group_by(loc_state, year)%>%
  summarise_at(c("amind", "asian", "black", "latino", "white"), 
               mean, na.rm=TRUE)%>% #### manually add Hawaii 2000, entries in SEER are messed up for 1999/2000
  filter(!(year==2000 & loc_state=="HI"))

HI_2000<-pct_cnty_wide%>%filter(year==2000 & FIPS_county=="15900")%>%
  mutate(loc_state = "HI")%>%
  select(-FIPS_county)

pop_st_avg<-pop_st_avg%>%
  bind_rows(HI_2000)


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
         p_his, p_asi, p_oth)%>%
  mutate_if(is.character, as.factor)%>%
  mutate(sex = factor(sex))%>%
  mutate(year = factor(year))