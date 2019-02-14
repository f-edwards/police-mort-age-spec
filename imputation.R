#### Risk of being killed by police in the U.S. by age, race/ethnicity, and sex
#### Frank Edwards, Hedwig Lee, Michael Esposito
#### impute missing race/ethnicity data from fatal encounters
### based on surname and county population
# rm(list=ls())
library(tidyverse)
library(mice)
library(wru)
library(jsonlite)

source("read_fe.r")
source("read_pop.r")

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

write.csv(coords,
          "./data/block_map.csv", row.names = FALSE)


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
  select(year, FIPS_county, race, pct_pop)%>%
  mutate(year = year + 1)

pct_cnty_wide<-pct_cnty_pop%>%
  group_by(FIPS_county, year)%>%
  spread(race, pct_pop)

## fix problem counties - 
## HI has no county dat for prior to 2000 (+3 in recode)
fe_imp_dat<-fe_imp_dat%>%
  mutate(FIPS_county = ifelse(substr(FIPS_county,1,2)=="15" & year < 2001, 
                              "15900", FIPS_county))
# >% 
#   mutate(FIPS_county = ifelse(FIPS_county%in%
#                                 c("08001", "08059", "08123"), 
#                               "08014", FIPS_county)) # fix Broome recode


fe_imp_dat_join<-fe_imp_dat%>%
  left_join(pct_cnty_wide)

fe_imp_dat_join<-merge_surnames(fe_imp_dat_join)

### check county coverage: 
# View(fe_imp_dat_join%>%filter(is.na(white)))
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
         p_his, p_asi, p_oth)%>%
  mutate_if(is.character, as.factor)%>%
  mutate(sex = factor(sex))%>%
  mutate(year = factor(year))
#white pct pop colinear with others, remove it and p_oth from imputation model
# these are constrained to sum to 1

####################################################
#### impute!
####################################################
fe_imp_setup<-mice(fe_imp_dat_join,
             maxit = 0,
             m=1,
             seed = 1)
### set predictor matrix, disable id as predictor
preds<-fe_imp_setup$predictorMatrix
preds[1,]<-0
preds[,1]<-0
### default methods are good here

fe_imp<-mice(fe_imp_dat_join,
             maxit =20,
             m=10,
             seed = 1,
             predictorMatrix = preds)

saveRDS(fe_imp, "imputations.rds")
####################################################
## Diagnose!
####################################################
pdf("./vis/imp_trace.pdf")
plot(fe_imp)
dev.off()
pdf("./vis/imp_density_race.pdf")
densityplot(fe_imp, ~race)
dev.off()
pdf("./vis/imp_density_age.pdf")
densityplot(fe_imp, ~age)
dev.off()

### latinos see a dip in pct, amind and asian slight dip, 
### black incarease, white increase
### do more diagnostics with mice() vignettes

### fit probability of missing conditional on surname posterior probability - maybe 
### hispanic surnames getting id'd more often in original data?
### maybe Black has higher hit rate in original based on photos?

### method from https://stefvanbuuren.name/fimd/sec-diagnostics.html
### via https://doi.org/10.1002/sim.6926

fit_observed<-glm(is.na(race) ~ year + age + 
                    sex + fe_cause_of_death +
                    amind+ asian+ black+ latino+ 
                    p_whi+ p_bla+ 
                    p_his+ p_asi,
                  family = binomial,
                  data = fe_imp_dat_join)

library(texreg)
texreg(fit_observed, file = "./vis/missing_reg.tex")

### then look at predicted prob for latino / asian surname and latino / asian
### neighborhood

yhat<-predict(fit_observed, type = "response")
c.dat<-na.omit(fe_imp_dat_join%>%
                 select(-race))

hisp<-c.dat%>%
  select(p_his)%>%
  mutate(var = "P(Hispanic|Surname)",
         yhat = yhat)%>%
  rename(prob = p_his)

white<-c.dat%>%
  select(p_whi)%>%
  mutate(var = "P(White|Surname)",
         yhat = yhat)%>%
  rename(prob = p_whi)

black<-c.dat%>%
  select(p_bla)%>%
  mutate(var = "P(Black|Surname)",
         yhat = yhat)%>%
  rename(prob = p_bla)

asian<-c.dat%>%
  select(p_asi)%>%
  mutate(var = "P(Asian|Surname)",
         yhat = yhat)%>%
  rename(prob = p_asi)

other<-c.dat%>%
  mutate(prob = 1 - (p_whi + p_his + p_bla + p_asi))%>%
  mutate(var = "P(Other|Surname)",
         yhat = yhat)%>%
  select(prob, var, yhat)

plot_dat<-bind_rows(asian, black, hisp, other, white)

ggplot(plot_dat, aes (y = yhat, x = prob)) + 
  geom_point(pch=".")+
  facet_wrap(~var) + 
  xlab("P(Race|Surname)") + 
  ylab("Predicted probability of missing race data") + 
  ggsave("./vis/missing_surname.pdf")


###
### DO THIS FOR COUNTY POP AS WELL AND WRITE IT UP

hisp<-c.dat%>%
  select(latino)%>%
  mutate(var = "Percent Latinx",
         yhat = yhat)%>%
  rename(pct = latino)

amind<-c.dat%>%
  select(amind)%>%
  mutate(var = "Percent American Indian",
         yhat = yhat)%>%
  rename(pct = amind)

black<-c.dat%>%
  select(asian)%>%
  mutate(var = "Percent Asian/PI",
         yhat = yhat)%>%
  rename(pct = asian)

asian<-c.dat%>%
  select(black)%>%
  mutate(var = "Percent Black",
         yhat = yhat)%>%
  rename(pct = black)

white<-c.dat%>%
  mutate(pct = 1 - (amind + asian  +black + latino))%>%
  mutate(var = "Percent White",
         yhat = yhat)%>%
  select(pct, var, yhat)

plot_dat<-bind_rows(amind, asian, black, hisp,  white)

ggplot(plot_dat, aes (y = yhat, x = pct)) + 
  geom_point(pch=".")+
  facet_wrap(~var) + 
  xlab("County Population Composition") + 
  ylab("Predicted probability of missing race data") + 
  ggsave("./vis/missing_popcomp.pdf")

### ADD SOME MISSING X YEAR PLOTS
### ADD SOME MISSING X YEAR X RACE PLOTS

missing_ts<-fe_imp_dat_join%>%
  group_by(year)%>%
  summarise(`Missing race` = sum(is.na(race)),
            `Total cases` = n())%>%
  mutate(year = as.numeric(as.character(year)))%>%
  gather(key = "type", value = "count", -year)%>%
  filter(year<2018)

ggplot(missing_ts,
       aes(x = year,
           y = count,
           color = type)) + 
  geom_line() + 
  ylab("Year")+ 
  xlab("Cases") +
  theme(legend.title = element_blank()) +
  ggsave("./vis/missing_time_series.pdf")

### regress time series of killing counts on internet users
### h: digital news -> count
users<-read_csv("./data/ITNETUSERP2USA.csv")
users<-users%>%
  mutate(year = year(DATE))

ts_dat<-missing_ts%>%
  filter(type == "Total cases")%>%
  left_join(users)%>%
  rename(InternetUsage=ITNETUSERP2USA,
         FatalEncountersDeaths=count)

lm_check<-lm(FatalEncountersDeaths ~ InternetUsage,
             data = ts_dat)

texreg(lm_check, file = "./vis/internet_use_reg.tex")
### make a quick plot to show the trends
ts_dat<-ts_dat%>%
  select(year, FatalEncountersDeaths, InternetUsage)%>%
  mutate(InternetUsage = InternetUsage * 10)%>%
  gather(key = "var", value = "value", -year)

ggplot(ts_dat,
       aes(x = year, y = value, color = var)) + 
  geom_line() + 
  ylab("FE: Cases, Internet Usage: Users per 1,000 people") + 
  xlab("Year") + 
  theme(legend.title = element_blank()) + 
  ggsave("./vis/internet_use_ts.pdf")

#### OUTPUT IMPUTED

### read in imputed data and format for merge
dat<-mice::complete(read_rds("imputations.rds"),
                    action = "long")%>%
  select(.imp, id, year, age, sex, race, fe_cause_of_death)%>%
  mutate(year = as.numeric(as.character(year)),
         sex = as.numeric(sex),
         race = as.character(race),
         fe_cause_of_death = as.character(fe_cause_of_death))%>%
  mutate(sex = ifelse(sex==1, "Male", "Female"))%>%
  mutate(age = case_when(
    age<1 ~ "0",
    age>=1 & age<=4 ~ "1-4",
    age>=5 & age<=9 ~ "5-9",
    age>=10 & age<=14 ~ "10-14",
    age>=15 & age<=19 ~ "15-19",
    age>=20 & age<=24 ~ "20-24",
    age>=25 & age<=29 ~ "25-29",
    age>=30 & age<=34 ~ "30-34",
    age>=35 & age<=39 ~ "35-39",
    age>=40 & age<=44 ~ "40-44",
    age>=45 & age<=49 ~ "45-49",
    age>=50 & age<=54 ~ "50-54",
    age>=55 & age<=59 ~ "55-59",
    age>=60 & age<=64 ~ "60-64",
    age>=65 & age<=69 ~ "65-69",
    age>=70 & age<=74 ~ "70-74",
    age>=75 & age<=79 ~ "75-79",
    age>=80 & age<=84 ~ "80-84",
    age>=85  ~ "85+"))

### create age/sex/race/year/causeofdeath data

dat<-dat%>%
  group_by(.imp, year, age, sex, race, fe_cause_of_death)%>%
  summarise(deaths = n())%>%
  group_by(.imp, year, age, sex, race)%>%
  spread(fe_cause_of_death, deaths, fill = 0)%>%
  ungroup()

### join with national pop data

pop_nat<-read_csv("./data/pop_nat.csv")

dat<-left_join(dat, pop_nat)

dat<-dat%>%
  mutate(race=
           case_when(
             race=="amind" ~ "American Indian/AK Native",
             race=="asian" ~ "Asian/Pacific Islander",
             race=="black" ~ "African American",
             race=="latino"~ "Latinx",
             race=="white" ~ "White"
           )
  )%>%
  mutate(age = factor(age, levels = c(
    "0", "1-4", "5-9", "10-14", "15-19",
    "20-24", "25-29", "30-34", "35-39",  
    "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79",
    "80-84", "85+")))%>%
  arrange(.imp, year, sex, race, age)

write_csv(dat, "./data/fe_pop_imputed_00_18.csv")
