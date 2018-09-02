### MAKE POLICE KILLINGS LIFE TABLE
## USES METHOD FROM http://data.princeton.edu/eco572/periodlt.html
rm(list=ls()); gc()
library(tidyverse)
library(xtable)
library(lubridate)

theme_set(theme_minimal())

### read in child-level data with TPR variables
dat<-read_csv("./data/fe_pop_imputed.csv")

dat<-dat%>%
  mutate(race=
    case_when(
      race=="amind" ~ "American Indian/AK Native",
      race=="asian" ~ "Asian/Pacific Islander",
      race=="black" ~ "African American",
      race=="latino"~ "Latinx",
      race=="white" ~ "White"
    )
  )

make_life_table<-function(nat_dat){
  nat_dat<-nat_dat%>%
    ungroup(nat_dat)
  ### create proportions of pop with each TPR outcome by age/year
  nat_dat<-nat_dat%>%
    mutate(m = deaths / pop)
  
  ### convert to probability 
  ### age_period (n) = 1 for all cases
  ### a = 0.5 (avg age of TPR for ppl in year, within-period survival)
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
  ## life expectancy (time to TPR)
  nat_dat<-nat_dat%>%
    mutate(e = t/lx)
  ### cum prevalence
  nat_dat<-nat_dat%>%
    mutate(c = 1-lx/1e5)
  return(nat_dat)
}



### loop over imputations, make life tables
imp_out<-list()
races<-unique(dat$race)
years<-unique(dat$year)
genders<-unique(dat$gender)
k<-0
for(i in 1:length(unique(dat$.imp))){
    for(j in 1:length(races)){
      for(t in 1:length(years)){
        for(g in 1:length(genders)){
      k<-k+1
      print(k)
      imp_out[[k]]<-make_life_table(dat%>%
                                      filter(.imp==i)%>%
                                      filter(year==years[t])%>%
                                      filter(race==races[j])%>%
                                      filter(gender==genders[g]))
    }
    }
    }
  }

imp_tab<-bind_rows(imp_out)

### make pooled cross-period tables for age specific risk
imp_out<-list()
races<-unique(dat$race)
years<-unique(dat$year)
genders<-unique(dat$gender)
k<-0
for(i in 1:length(unique(dat$.imp))){
  for(j in 1:length(races)){
      for(g in 1:length(genders)){
        k<-k+1
        print(k)
        imp_out[[k]]<-make_life_table(dat%>%
                                        filter(.imp==i)%>%
                                        filter(race==races[j])%>%
                                        filter(gender==genders[g])%>%
                                        group_by(.imp, race, gender, age)%>%
                                        summarise(pop = sum(pop), deaths=sum(deaths)))
      }
    }
  }


imp_tab_pooled<-bind_rows(imp_out)


### make lifetime cumulative risk by race, year, gender
cumulative_range<-imp_tab%>%
  filter(age=="85+")%>%
  group_by(year, race, gender)%>%
  summarise(cmin=min(c), cmax=max(c), c=mean(c))%>%
  ungroup()

cumulative_pooled<-imp_tab_pooled%>%
  filter(age=="85+")%>%
  group_by(race, gender)%>%
  summarise(cmin=min(c), cmax=max(c), c=mean(c))%>%
  ungroup()

### make pooled age-specific risk
age_range<-imp_tab_pooled%>%
  ungroup()%>%
  group_by(race, gender, age)%>%
  summarise(qmin=min(q), qmax=max(q), q=mean(q))
### transform age var for better plotting, convert to numeric with last number
age_range<-age_range%>%
  mutate(age = ifelse(age=="0", 0,
                ifelse(age=="85+", 85,
                  ifelse(nchar(age)==3, as.integer(substr(age, 3, 3)),
                     as.integer(substr(age, 4, 5))))))%>%
  arrange(race, gender, age)

age_range_white<-age_range%>%
  ungroup()%>%
  filter(race=="White")

age_range_ineq<-age_range%>%
  ungroup()%>%
  filter(race!="White")%>%
  mutate(d = q/age_range_white$q)



### plots

ggplot(cumulative_range,
       aes(x=year, y=c *1e5, 
           ymin = cmin * 1e5, ymax = cmax * 1e5))+
  geom_line(aes(color=race))+
  geom_ribbon(alpha = 0.5, aes(fill = race))+
  xlab("Synthetic cohort year") + 
  ylab("Persons killed by police per 100,000 births") + 
  facet_wrap(~gender) + 
  theme(legend.title = element_blank()) + 
  ggsave("./vis/cumulative_risk_year.png", width = 5, height = 6)

### age specific for 2016, with intervals

ggplot(age_range,
       aes(x=age, y=q * 1e5, ymin=qmin * 1e5, 
           ymax=qmax * 1e5,
           color=race, 
           group = race))+
  geom_line()+
  geom_ribbon(aes(fill=race, color = NULL), alpha = 0.4)+
  facet_wrap(~gender)+
  xlab("Age")+
  ylab("Age-specific risk (per 100,000)") +  
  theme_minimal() + 
  theme(legend.title = element_blank()) + 
  ggsave("./vis/age_spec_prob.pdf", width = 8, height = 6)

#### pooled years, cumulative prob (expected deaths per 100k through 85yrs)

ggplot(data = cumulative_pooled,
       mapping =  aes(fill = gender,
           x = reorder(race, cmax),
           y = ifelse(gender=="Male",
                      -c*1e5, 
                      c*1e5),
           ymax = ifelse(gender=="Male",
                      -cmax*1e5, 
                      cmax*1e5),
           ymin = ifelse(gender=="Male",
                         -cmin*1e5, 
                         cmin*1e5))) + 
  geom_bar(stat = "identity") + 
  geom_linerange(size = 1) + 
  scale_y_continuous(limits = max(cumulative_pooled$cmax) * 1e5 * c(-1,1), 
                     labels = abs) +
  labs(fill = "Gender") + 
  ylab("People killed by police per 100,000 births") +
  xlab("") + 
  coord_flip() + 
  theme_minimal()+
  ggsave("./vis/pooled_lifetime.pdf", width = 8, height = 6)

white<-cumulative_pooled%>%
  filter(race=="White")
ineq<-cumulative_pooled%>%
  filter(race!="White")

ineq<-ineq%>%
  mutate(dmin = cmin / white$cmin,
         dmax = cmax / white$cmax,
         d = c  /white$c)

ggplot(ineq,
       aes(fill = gender,
           x = reorder(race,dmax),
           y = ifelse(gender == "Male",
                      -d, d), 
           ymax = ifelse(gender == "Male",
                         -dmax, dmax), 
           ymin = ifelse(gender == "Male",
                         -dmin, dmin))) +
  geom_bar(stat = "identity") + 
  geom_linerange(size = 1) + 
  scale_y_continuous(limits = max(ineq$dmax) *c(-1,1),
                     labels = abs) + 
  labs(fill = "Gender") + 
  ylab("Rate ratio (relative to white)") + 
  xlab("") + 
  coord_flip()+
  theme_minimal()
  ggsave("./vis/lifetime_ineq.pdf", width = 8, height = 6)

# ggplot(ineq,
#        aes(fill = race,
#            x = race,
#            y = d, 
#            ymax = dmax, 
#            ymin = dmin)) +
#   geom_col() + 
#   #geom_errorbar() + 
#   facet_wrap(~gender) + 
#   theme(axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         legend.title = element_blank()) + 
#   geom_hline(yintercept = 1, lty=2)+
#   ylab("Rate ratio (relative to white)") + 
#   xlab("") + 
#   ggsave("./vis/lifetime_ineq_web.png")