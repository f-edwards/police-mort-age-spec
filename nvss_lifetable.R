### lifetables and descriptives on NVSS
library(tidyverse)

nvss_dat<-read_csv("./data/mort_cause.csv")
pop<-read_csv("./data/pop_nat.csv")

nvss_dat<-nvss_dat%>%
  filter(age!="Missing")%>%
  left_join(pop)


########################
## LIFETABLE
# 
# make_life_table<-function(nat_nvss_dat){
#   nat_nvss_dat<-nat_nvss_dat%>%
#     ungroup(nat_nvss_dat)
#   nat_nvss_dat<-nat_nvss_dat%>%
#     mutate(m = deaths / pop)
#   
#   nat_nvss_dat<-nat_nvss_dat%>%
#     mutate(q = 1 * m / (1 + (1 - 0.5) * m),
#            p = 1 - q)
#   ### make cumulative survival
#   nat_nvss_dat<-nat_nvss_dat%>%
#     mutate(lx = 1e5 * cumprod(c(1, p))[-nrow(nat_nvss_dat)])
#   ### deaths
#   nat_nvss_dat<-nat_nvss_dat%>%
#     mutate(d = -c(diff(lx),0))
#   ## person-years in each group
#   nat_nvss_dat<-nat_nvss_dat%>%
#     mutate(L = (lx - d) * 1 + d * 0.5,
#            t = sum(L)- cumsum(L) + L)
#   ## life expectancy 
#   nat_nvss_dat<-nat_nvss_dat%>%
#     mutate(e = t/lx)
#   ### cum prevalence
#   nat_nvss_dat<-nat_nvss_dat%>%
#     mutate(c = 1-lx/1e5)
#   return(nat_nvss_dat)
# }
# 
# ### make pooled mortatlity tables
# tables<-list()
# races<-unique(nvss_dat$race)
# sexs<-unique(nvss_dat$sex)
# causes<-unique(nvss_dat$cause_50)
# k<-0
# for(h in 1:length(causes)){
#   for(j in 1:length(races)){
#     for(g in 1:length(sexs)){
#       k<-k+1
#       tables[[k]]<-make_life_table(nvss_dat%>%
#                                      filter(race==races[j])%>%
#                                      filter(sex==sexs[g])%>%
#                                      filter(cause_50==causes[h])%>%
#                                      group_by(race, sex, age, cause_50)%>%
#                                      summarise(pop = sum(pop), deaths=sum(deaths)))
#     }
#   }
# }
# 
# nvss_tables<-bind_rows(tables)