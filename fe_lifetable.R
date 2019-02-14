### make period lifetables 2010-2016 from fatal encounters data, sourced from main.r
## USES METHOD FROM http://data.princeton.edu/eco572/periodlt.html
rm(list=ls())
library(tidyverse)
library(mice)


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