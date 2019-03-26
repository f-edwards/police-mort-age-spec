### make period lifetables 2010-2016 from fatal encounters data, sourced from main.r
## USES METHOD FROM http://data.princeton.edu/eco572/periodlt.html
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
  ### age_period (n) != 1 for all cases
  ### assume mid-point average time of death for within-period deaths
  ### n = 1 if age == "0", 
  ### n = 10 (arbitrary, assumes death by 95) if age == "85+",
  ### n = 4 if age == "1-4"
  ### else n = 5 for all other age groups
  nat_dat<-nat_dat%>%
    mutate(n = case_when(
      age=="0" ~ 1,
      age=="1-4" ~ 4,
      age=="85+" ~ 10,
      ! age %in% c("0", "1-4", "85+") ~ 5
    ))
  ### assume deaths occur at age + 0.5 * n for all groups to obtain years lived
  nat_dat$nax<- nat_dat$n * 0.5
  
  nat_dat<-nat_dat%>%
    mutate(q = n * m / (1 + (n - nax) * m),
           p = 1 - q)
  ### make cumulative survival
  nat_dat<-nat_dat%>%
    mutate(lx = 1e5 * cumprod(c(1, p[-nrow(nat_dat)])))
  
  ### deaths
  nat_dat<-nat_dat%>%
    mutate(d = c(-diff(lx),lx[nrow(nat_dat)]))
  ## person-years in each group
  nat_dat<-nat_dat%>%
    mutate(L = (lx - d) * n + d * nax,
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