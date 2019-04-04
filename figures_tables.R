### Risk of being killed by police in the U.S. by age, 
##race/ethnicity, and sex
### Frank Edwards, Hedwig Lee, Michael Esposito
### see lifetable.r for details on life table method
rm(list=ls()); gc()
library(tidyverse)
library(xtable)
library(lubridate)
library(foreign)

source("lifetable.r")
source("lifetable_multiple.r")
nax<-read.dta("./data/kfnax.dta")

theme_set(theme_minimal())
### convert age to numeric
age_recode<-function(df){
  age_recode<-df%>%
    mutate(age = 
      case_when(
        age=="0" ~ "0",
        age=="1-4" ~ "1",
        age=="5-9" ~ "5",
        age=="85+" ~ "85",
        !age%in%c("0", "85+") ~ substr(age, 1, 2)
      ),
      age = as.numeric(age))
      return(age_recode)
}

### read and format data
pop<-read_csv("./data/pop_nat.csv")
fe_imp<-read_csv("./data/fe_pop_imputed_13_18.csv")
tot_mort<-read_csv("./data/total_mort.csv")%>%
    filter(age!="Missing")
fe_postpred<-readRDS("./data/post_pred_90.rds")%>%
  ungroup()%>%
  rename(age = age_group)%>%
  rename(median = .prediction,
         lower = .lower,
         upper = .upper)%>%
  select(age, race, sex, pop, 
         median, lower, upper)
### make a total race cat
fe_postpred<-fe_postpred%>%
  bind_rows(fe_postpred%>%
              group_by(age, sex)%>%
              summarise(median = sum(median), 
                        lower = sum(lower), 
                        upper = sum(upper), 
                        pop = sum(pop))%>%
              ungroup()%>%
              mutate(race = "Total"))

### recode age into numeric
pop<-age_recode(pop)
fe_imp<-age_recode(fe_imp)
tot_mort<-age_recode(tot_mort)
fe_postpred<-age_recode(fe_postpred)

tot_mort<-tot_mort%>%
  left_join(pop)
### make a total race cat
tot_mort<-tot_mort%>%
  bind_rows(tot_mort%>%
              group_by(year, age, sex)%>%
              summarise(deaths = sum(deaths),
                        pop = sum(pop))%>%
              mutate(race = "Total")%>%
              ungroup())

#################################################
## PERIOD LIFE TABLE, SINGE DECREMENT using NVSS
#################################################
## use 2017 data
## loop over race, sex
nvss_table_dat<-tot_mort%>%
  filter(year==2017)

race_i<-unique(nvss_table_dat$race)
sex_i<-unique(nvss_table_dat$sex)
nvss_lifetable<-list()
k<-0

for(i in race_i){
  for(j in sex_i){
    k<-k+1
    temp<-nvss_table_dat%>%
      filter(race==i, sex==j)%>%
      arrange(age)
    ### attach nAx from Keyfitz and Flieger (via Rodriguez)
    temp<-temp%>%
      left_join(nax)
    nvss_lifetable[[k]]<-make_life_table(temp, temp$deaths)
  }
}

nvss_lifetable<-bind_rows(nvss_lifetable)

#################################################
## Multiple decrement tables
#################################################
### Model simulations (focal table)
### scale to deaths at NVSS pop - convert 
fe_postpred<-fe_postpred%>%
  rename(pop_sim = pop)%>%
  left_join(nvss_lifetable%>%
              select(age, race, sex, pop))%>%
  mutate(median = median / pop_sim * pop,
         lower = lower / pop_sim * pop,
         upper = upper / pop_sim * pop)

fe_median<-list()
fe_lower<-list()
fe_upper<-list()
k<-0

for(i in race_i){
  for(j in sex_i){
    k<-k+1
    temp<-fe_postpred%>%
      filter(race==i, sex==j)%>%
      arrange(age)
    ### attach nAx from Keyfitz and Flieger (via Rodriguez)
    fe_median[[k]]<-make_life_table_multiple(temp%>%rename(deaths = median)
                                             , nvss_lifetable)
    fe_lower[[k]]<-make_life_table_multiple(temp%>%rename(deaths = lower), 
                                            nvss_lifetable)
    fe_upper[[k]]<-make_life_table_multiple(temp%>%rename(deaths = upper), 
                                            nvss_lifetable)
  }
}

fe_median<-bind_rows(fe_median)%>%
  select(age, race, sex, d_x, q_xi, d_i, ed, m, m_i, lx, L)%>%
  rename(q_med = q_xi, d_med = d_i, e_med = ed, m_med = m_i)
fe_lower<-bind_rows(fe_lower)%>%
  select(age, race, sex, q_xi, d_i, ed, m_i)%>%
  rename(q_low = q_xi, d_low = d_i, e_low = ed, m_low = m_i)
fe_upper<-bind_rows(fe_upper)%>%
  select(age, race, sex, q_xi, d_i, ed, m_i)%>%
  rename(q_hi = q_xi, d_hi = d_i, e_hi = ed, m_hi = m_i)

fe_post_tables<-fe_median%>%
  left_join(fe_lower)%>%
  left_join(fe_upper)

sample<-fe_post_tables%>%
  filter(race == "White", sex == "Male")%>%
  select(age, race, sex, d_x, d_med, lx)%>%
  mutate(age = as.integer(age))%>%
  rename(Age = age, Race = race, Sex = sex,
         `Total deaths` = d_x, 
         `Use-of-force deaths` = d_med,
         `Survivors in cohort` = lx)

library(xtable)
sample_out<-xtable(sample,
                   caption = 
                     "Excerpt of police use-of-force 
                   multiple decrement period lifetable, 
                   synthetic cohort of 100,000",
                   label = "tab:life_samp")
print.xtable(sample_out, 
             type = "latex",
             include.rownames = FALSE,
             caption.placement = "bottom",
             file = "./vis/lifetable_sample.tex")

### create race as factor for plot order
fe_post_tables<-fe_post_tables%>%
  mutate(race = factor(race, 
                       levels = c("African American",
                                  "American Indian/AK Native",
                                  "Asian/Pacific Islander",
                                  "Latinx",
                                  "White",
                                  "Total")))

cumulative_fe<-fe_post_tables%>%
  group_by(race, sex)%>%
  summarise(c_med = sum(d_med),
            c_low = sum(d_low),
            c_hi = sum(d_hi))


### life expectancy decomposition
ed<-fe_post_tables%>%
  select(age, race, sex, e_med)%>%
  left_join(nvss_lifetable%>%
              select(age, race, sex, e))%>%
  filter(age==0)%>%
  mutate(pct_change_e =( e_med / e - 1) * 100,
         days = (e_med - e) * 365)

#####################
### FOR APPX - CAUSE OF DEATH FIG BASED ON OBS LIFETABLES
### make each cat the sum of all prior cats

fe_obs_tabs<-fe_imp%>%
  mutate(vehicle = vehicle + officer_force,
         suicide = suicide + vehicle,
         other = other + suicide)%>%
  group_by(.imp, age, sex, race)%>%
  summarise(officer_force = sum(officer_force)/sum(pop),  # as multi-year avg
            other = sum(other)/sum(pop),
            suicide = sum(suicide)/sum(pop),
            vehicle = sum(vehicle)/sum(pop))%>%
  left_join(nvss_lifetable%>%
              select(age, race, sex, pop))%>%
  mutate(officer_force = officer_force * pop,
         other = other * pop,
         suicide = suicide * pop,
         vehicle = vehicle * pop)

force<-list()
other<-list()
suicide<-list()
vehicle<-list()
k<-0

for(h in 1:10){
  for(i in race_i){
    for(j in sex_i){
      k<-k+1
      temp<-fe_obs_tabs%>%
        filter(race==i, sex==j, .imp==h)%>%
        arrange(age)
      ### attach nAx from Keyfitz and Flieger (via Rodriguez)
      force[[k]]<-make_life_table_multiple(
        temp%>%rename(deaths = officer_force), 
        nvss_lifetable)
      other[[k]]<-make_life_table_multiple(
        temp%>%rename(deaths = other), 
        nvss_lifetable)
      suicide[[k]]<-make_life_table_multiple(
        temp%>%rename(deaths = suicide), 
        nvss_lifetable)
      vehicle[[k]]<-make_life_table_multiple(
        temp%>%rename(deaths = vehicle), 
        nvss_lifetable)
    }
  }
}
obs_tabs<-bind_rows(force)%>%
  select(.imp, age, sex, race, d_i)%>%
  mutate(cause = "Use of force")%>%
  bind_rows(bind_rows(other)%>%
              select(.imp, age, sex, race, d_i)%>%
              mutate(cause = "+Other"))%>%
  bind_rows(bind_rows(suicide)%>%
              select(.imp, age, sex, race, d_i)%>%
              mutate(cause="+Suicide"))%>%
  bind_rows(bind_rows(vehicle)%>%
              select(.imp, age, sex, race, d_i)%>%
              mutate(cause="+Vehicle"))

cumulative_cause<-obs_tabs%>%
  group_by(.imp, race, sex, cause)%>%
  summarise(d_i = sum(d_i))%>%
  group_by(race, sex, cause)%>%
  summarise(d_med = median(d_i),
            d_low = quantile(d_i, 0.1),
            d_hi = quantile(d_i, 0.9))%>%
  mutate(cause = factor(cause, levels = c(
    "Use of force", "+Vehicle", "+Suicide", "+Other")))

ggplot(data = cumulative_cause,
       mapping =  aes(x = reorder(race, d_med),
                      y = d_med,
                      fill = cause,
                      group = cause)) + 
  geom_bar(stat = "identity", position = "dodge", color = 1) + 
  # scale_y_continuous(limits = max(fe_all_tables_c$c) * c(-1,1),
  #                   labels = abs) +
  ylab("Lifetime risk of police-involved death, per 100,000") +
  xlab("") + 
  coord_flip() + 
  theme_minimal()+
  facet_wrap(~sex, ncol = 1, scales = "free") + 
  theme(legend.title = element_blank()) + 
  ggsave("./vis/death_type_c_new.pdf", width = 6, height = 6)

#################################################
## Visuals
#################################################

###########################
## AGE SPECIFIC RISK PLOTS
###########################

age_compare<-fe_post_tables%>%
  filter(age=="25")%>%
  select(age, race, sex, m_med, m_low, m_hi)%>%
  mutate_at(vars(m_med, m_low, m_hi), 
            funs(.*1e5))

pct_compare<-fe_post_tables%>%
  group_by(race, sex, age)%>%
  summarise(pct = sum(d_med)/sum(d_x) * 100)

ggplot(fe_post_tables%>%
         filter(age!="85", sex=="Male"),
       aes(x = age, y = m_med * 1e5, 
           ymin = m_low * 1e5, 
           ymax = m_hi * 1e5)) +
  geom_line() +
  facet_wrap(~race) +
  xlab("Age") +
  ylab("Death rate per 100,000, Male") + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  geom_errorbar(width = 0, alpha = 0.5) +
  geom_point(size = 0.5)+
  ggsave("vis/age_spec_prob_m.pdf", width = 6, height = 3.5)

ggplot(fe_post_tables%>%
         filter(age!="85", sex=="Female"),
       aes(x = age, y = m_med * 1e5, 
           ymin = m_low * 1e5, 
           ymax = m_hi * 1e5)) +
  geom_line() +
  facet_wrap(~race) +
  xlab("Age") +
  ylab("Death rate per 100,000, Female") + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  geom_errorbar(width = 0, alpha = 0.5) +
  geom_point(size = 0.5)+
  guides(col = guide_legend(override.aes = list(shape = 15, size = 5)))+
  ggsave("vis/age_spec_prob_f.pdf", width = 6, height = 3.5)

ggplot(fe_post_tables,
       aes(x=age,
           color = race,
           y=d_med/d_x * 100))+
  geom_line() +
  facet_wrap(~sex)+
  xlab("Age")+
  ylab("Police killings as percent of deaths") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  geom_point(size = 0.4)+
  ggsave("vis/age_pct.pdf", width = 6, height = 3.5)

ggplot(fe_post_tables%>%
         filter(sex == "Female"),
       aes(x=age,
           y=d_med/d_x * 100,
           ymin = d_low / d_x * 100,
           ymax = d_hi / d_x * 100))+
  geom_line() +
  facet_wrap(~race)+
  xlab("Age")+
  ylab("Police killings as percent of all deaths") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  geom_point(size = 0.4)+
  geom_errorbar(width = 0, alpha = 0.5) +
  ggsave("vis/age_pct_f.pdf", width = 6, height = 3.5)

#### pooled years, cumulative prob (expected deaths per 100k through 85yrs)

ggplot(data = cumulative_fe,
       mapping =  aes(fill = sex,
           x = reorder(race, c_med),
           y = ifelse(sex=="Male",
                      -c_med, 
                      c_med),
           ymax = ifelse(sex=="Male",
                      -c_hi, 
                      c_hi),
           ymin = ifelse(sex=="Male",
                         -c_low, 
                         c_low))) + 
  geom_bar(stat = "identity") + 
  geom_linerange(size = 1,  alpha = 0.5) + 
  scale_y_continuous(limits = max(cumulative_fe$c_hi) * c(-1,1), 
                     labels = abs) +
  labs(fill = "Sex") + 
  ylab("Lifetime risk of being killed by police, per 100,000") +
  xlab("") + 
  coord_flip() + 
  ggsave("./vis/pooled_lifetime_new.pdf", width = 6, height = 3.5)

white<-cumulative_fe%>%
  filter(race=="White")
ineq<-cumulative_fe%>%
  filter(race!="White",
         race!="Total")

ineq<-ineq%>%
  mutate(dmin = c_low / white$c_low,
         dmax = c_hi / white$c_hi,
         d = c_med  /white$c_med)

ggplot(ineq,
       aes(fill = sex,
           x = reorder(race,d),
           y = ifelse(sex == "Male",
                      -d, d), 
           ymax = ifelse(sex == "Male",
                         -dmax, dmax), 
           ymin = ifelse(sex == "Male",
                         -dmin, dmin))) +
  geom_bar(stat = "identity") + 
  geom_linerange(size = 1, alpha = 0.5) + 
  scale_y_continuous(limits = max(ineq$dmax) *c(-1,1),
                     labels = abs) + 
  labs(fill = "Sex") + 
  ylab("Mortality rate ratio (relative to white)") + 
  xlab("") + 
  coord_flip()+
  ggsave("./vis/lifetime_ineq.pdf", width = 6, height = 3.5)

saveRDS(nvss_lifetable, file = "nvss_lifetable.rds")

# #################################################
# ### FOR LEADING CAUSE COMPARISONS
# #################################################
# 
# cause<-nvss_dat%>%
#   group_by(age, race, sex, cause_50)%>%
#   summarise(q = sum(deaths)/sum(pop))%>%
#   ungroup()%>%
#   select(age, race, sex, cause_50, m)
# 
# #### CHECK RANKING METHOD FROM LAST PAPER
# 
# fe_cause<-age_range%>%
#   mutate(cause_50 = "Police homicide")%>%
#   select(age, race, sex, cause_50, m)
# 
# ### makes age-specific cause of death rank order 
# cause_rank<-cause%>%
#   bind_rows(fe_cause)%>%
#   group_by(age, race, sex)%>%
#   arrange(age, race, sex, desc(m))%>%
#   mutate(rank = rank(-m))
# 
# fe_rank<-cause_rank%>%
#   filter(cause_50=="Police homicide")
