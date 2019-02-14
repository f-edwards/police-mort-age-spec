### make period lifetables 2010-2016 from fatal encounters data, sourced from main.r
## USES METHOD FROM http://data.princeton.edu/eco572/periodlt.html
rm(list=ls())
library(tidyverse)
library(mice)
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





#### adapt life table
#### get cumulative probability by year/age/sex/race

make_life_table<-function(nat_dat){
  nat_dat<-nat_dat%>%
    ungroup(nat_dat)
  ### create proportions of pop with outcome by age/year
  nat_dat<-nat_dat%>%
    mutate(m = deaths / pop)
  
  ### convert to probability 
  ### age_period (n) = 1 for all cases
  ### a = 0.5  within-period survival
  nat_dat<-nat_dat%>%
    mutate(q = 1 * m / (1 + (1 - 0.5) * m),
           p = 1 - q)
  ### make cumulative survival
  nat_dat<-nat_dat%>%
    mutate(lx = 1e5 * cumprod(c(1, p))[-nrow(nat_dat)])
  ### deaths
  nat_dat<-nat_dat%>%
    mutate(d = -c(diff(lx),0))
  ## person-years in each group
  nat_dat<-nat_dat%>%
    mutate(L = (lx - d) * 1 + d * 0.5,
           t = sum(L)- cumsum(L) + L)
  ## life expectancy
  nat_dat<-nat_dat%>%
    mutate(e = t/lx)
  ### cum prevalence
  nat_dat<-nat_dat%>%
    mutate(c = 1-lx/1e5)
  return(nat_dat)
}



### make pooled cross-period tables for age specific risk
imp_out<-list()
races<-unique(dat$race)
sexs<-unique(dat$sex)
k<-0
for(i in 1:length(unique(dat$.imp))){
  for(j in 1:length(races)){
    for(g in 1:length(sexs)){
      k<-k+1
      imp_out[[k]]<-make_life_table(dat%>%
                                      filter(.imp==i)%>%
                                      filter(race==races[j])%>%
                                      filter(sex==sexs[g])%>%
                                      group_by(.imp, race, sex, age)%>%
                                      summarise(pop = sum(pop), deaths=sum(deaths)))
    }
  }
}


fe_tables<-bind_rows(imp_out)