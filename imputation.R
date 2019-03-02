#### Risk of being killed by police in the U.S. by age, race/ethnicity, and sex
#### Frank Edwards, Hedwig Lee, Michael Esposito
#### impute missing race/ethnicity data from fatal encounters
### based on surname and county population
library(tidyverse)
library(mice)
library(xtable)

source("imputation_pre_process.R")
### drop cases prior to 2008
### based on EDA with NVSS, 2008-18 
### higher quality than 00-07 (possible censorship)
fe_imp_dat_join<-fe_imp_dat_join%>%
  filter(as.numeric(as.character(year))>2007)

### make table of missing values by variable
tab.out<-xtable(fe_imp_dat_join%>%
  summarise_all(funs(signif(sum(is.na(.))/n() * 100)),3)%>%
  select(age, sex, race, fe_cause_of_death)%>%
  rename(Age = age, Sex = sex, Race = race, `Cause of death` = fe_cause_of_death),
  caption = "Focal variables missing values in Fatal Encounters, percent of cases 2008 - 2018",
  label = "tab:pct_var",
  digits = 3)

print(tab.out, include.rownames = FALSE,
      file = "./vis/pct_var.tex")

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
             m=50,
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

### make better race plot
imp_dat<-fe_imp%>%
  mice::complete(action="long",include = TRUE)%>%
 group_by(.imp, race)%>%
  filter(!(is.na(race)))%>%
  summarise(n = n())%>%
  left_join(fe_imp%>%
              mice::complete(action="long",include = TRUE)%>%
              group_by(.imp)%>%
              filter(!(is.na(race)))%>%
              summarise(n_total = n()))%>%
  mutate(pct_race = n/n_total)
### make imputation range
imp_range<-imp_dat%>%
  filter(.imp!=0)%>%
  group_by(race)%>%
  summarise(ymin = min(pct_race),
            ymax = max(pct_race))

imp_dat<-imp_dat%>%
  filter(.imp==0)%>%
  left_join(imp_range)

ggplot(imp_dat,
       aes(x = race, y = pct_race,
           ymin = ymin, ymax = ymax)) + 
  geom_point(size = 2.5) + 
  geom_errorbar(color = 1, alpha = 0.8, width = 0.3) + 
  xlab("Race") + 
  ylab("Proportion of cases, excluding missing") + 
  ggsave("./vis/race_impute_pct.pdf")

### latinos see a dip in pct, amind and asian slight dip, 
### black incarease, white increase
### do more diagnostics with mice() vignettes

### fit probability of missing conditional on surname posterior probability - maybe 
### hispanic surnames getting id'd more often in original data?
### maybe Black has higher hit rate in original based on photos?

### method from https://stefvanbuuren.name/fimd/sec-diagnostics.html
### via https://doi.org/10.1002/sim.6926

#### OUTPUT IMPUTED

### read in imputed data and format for merge
dat<-mice::complete(read_rds("imputations.rds"),
                    action = "long", 
                    include = TRUE)%>%
  select(.imp, id, year, age, sex, race, fe_cause_of_death)%>%
  mutate(year = as.numeric(as.character(year)),
         sex = as.numeric(sex),
         race = as.character(race),
         fe_cause_of_death = as.character(fe_cause_of_death))%>%
  mutate(sex = ifelse(sex==2, "Male", "Female"))%>%
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
  ungroup()%>%
  tidyr::complete(.imp, year, age, sex, race, fe_cause_of_death,
                  fill = list(deaths=0))%>%
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

write_csv(dat%>%
            filter(.imp!=0), "./data/fe_pop_imputed_08_18.csv")

write_csv(dat,
          "./data/fe_pop_imputed_08_18_with_orig.csv")
