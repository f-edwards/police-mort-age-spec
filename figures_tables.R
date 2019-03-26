### Risk of being killed by police in the U.S. by age, race/ethnicity, and sex
### Frank Edwards, Hedwig Lee, Michael Esposito
rm(list=ls()); gc()
library(tidyverse)
library(xtable)
library(lubridate)

source("lifetable.r")

theme_set(theme_minimal())

### read and format data
nvss_dat<-read_csv("./data/mort_cause.csv")
pop<-read_csv("./data/pop_nat.csv")
fe<-read_csv("./data/fe_pop_imputed_13_18.csv")
tot_mort<-read_csv("./data/total_mort.csv")### filter for matching years 

pop<-pop%>%
  mutate(race = 
           case_when(
             race=="amind" ~ "American Indian/AK Native",
             race=="black" ~ "African American",
             race=="asian" ~ "Asian/Pacific Islander",
             race=="latino" ~ "Latinx",
             race=="white" ~ "White"
           ))

nvss_dat<-nvss_dat%>%
  mutate(age = 
           ifelse(
             age == "Missing", "85+", age
           ))%>% # 6431 deaths over period are missing age. Assigning to 85+
  left_join(pop)

tot_mort<-tot_mort%>%
  left_join(pop)

#################################################
## Model predictions
#################################################
fe_postpred<-readRDS("./data/post_pred_90.rds")
fe_fit<-readRDS("./data/post_fit_90.rds")
### format for lifetable
fe_postpred<-fe_postpred%>%
  rename(median = .prediction,
         lower = .lower,
         upper = .upper,
         age = age_group)%>%
  select(year, age, race, sex, pop, 
         median, lower, upper)%>%
  mutate(.imp=1)

#### make posterior predictive lifetables


### make pooled cross-period tables for age specific risk
# imp_out<-list()
# races<-unique(dat$race)
# sexs<-unique(dat$sex)
# k<-0
# for(i in 1:length(unique(dat$.imp))){
#   for(j in 1:length(races)){
#     for(g in 1:length(sexs)){
#       k<-k+1
#       imp_out[[k]]<-make_life_table(dat%>%
#                                       filter(.imp==i)%>%
#                                       filter(race==races[j])%>%
#                                       filter(sex==sexs[g])%>%
#                                       group_by(.imp, race, sex, age)%>%
#                                       summarise(pop = sum(pop), deaths=sum(deaths)))
#     }
#   }
# }
# 
# 
# fe_tables<-bind_rows(imp_out)


dat<-fe_postpred%>%
  rename(deaths = median)
source("fe_lifetable.R")
post_tab<-fe_tables%>%
  select(race, sex, age, m, q, c)

dat<-fe_postpred%>%
  rename(deaths = upper)
source("fe_lifetable.R")
post_tab<-post_tab%>%
  left_join(fe_tables%>%
  select(race, sex, age, m, q, c)%>%
  rename(m_upper = m, q_upper = q, c_upper = c))

dat<-fe_postpred%>%
  rename(deaths = lower)
source("fe_lifetable.R")
post_tab<-post_tab%>%
  left_join(fe_tables%>%
              select(race, sex, age, m, q, c)%>%
              rename(m_lower = m, q_lower = q, c_lower = c))

#################################################
## Visuals
#################################################



######### mortality comparisons by age
deaths_age<-fe%>%
  filter(.imp==1)%>%
  group_by(age)%>%
  summarise(deaths = sum(officer_force + other + suicide + vehicle) / 
              length(unique(fe$year)))

nvss_age<-nvss_dat%>%
  group_by(age)%>%
  summarise(nvss_deaths = sum(deaths)/length(unique(nvss_dat$year)))

ratio<-left_join(deaths_age, nvss_age)%>%
  mutate(ratio = deaths / nvss_deaths * 1e2)

### total deaths ratio
ratio%>%summarise(ratio = sum(deaths) / sum(nvss_deaths) * 1e2)
##########################################
### Make life tables
##########################################
### note that lifetable scripts want a data.frame
### called dat with .imp, race, sex, age, deaths, and pop
######### For use of force deaths
dat<-fe%>%
  rename(deaths = officer_force)
source("fe_lifetable.R")

force_tables<-fe_tables%>%
  mutate(Type = "Force")

### for + vehicle
dat<-fe%>%
  mutate(deaths = officer_force + vehicle)
source("fe_lifetable.R")

force_vehicle_tables<-fe_tables%>%
  mutate(Type = "Force+Vehicles")

### for force + suicide deaths
dat<-dat%>%
  mutate(deaths = officer_force + suicide)

source("fe_lifetable.R")

suicide_tables<-fe_tables%>%
  mutate(Type = "Force+Suicide")

### for all deaths
dat<-dat%>%
  mutate(deaths = officer_force + vehicle + other + suicide)

source("fe_lifetable.R")

all_tables<-fe_tables%>%
  mutate(Type = "All Deaths")

fe_all_tables<-bind_rows(force_tables, 
                         force_vehicle_tables,
                         suicide_tables, 
                         all_tables)

fe_all_tables_c<-fe_all_tables%>%
  filter(age=="85+")%>%
  group_by(race, sex, Type)%>%
  summarise(cmin=quantile(c, 0.05)*1e5, 
            cmax=quantile(c, 0.95)*1e5, 
            c=mean(c)*1e5)%>%
  ungroup()

fe_all_tables_c$Type<-factor(fe_all_tables_c$Type,
                             levels = c("Force", "Force+Suicide",
                                        "Force+Vehicles",
                                        "All Deaths"))

ggplot(data = fe_all_tables_c,
       mapping =  aes(x = reorder(race, c),
                      y = c,
                      fill = Type,
                      group = Type)) + 
  geom_bar(stat = "identity", position = "dodge", color = 1) + 
  # scale_y_continuous(limits = max(fe_all_tables_c$c) * c(-1,1),
  #                   labels = abs) +
  ylab("Estimated lifetime risk") +
  xlab("") + 
  coord_flip() + 
  theme_minimal()+
  facet_wrap(~sex, ncol = 1, scales = "free") + 
  ggsave("./vis/death_type_c_new.pdf", width = 6, height = 6)

### make lifetime cumulative risk by race, year, sex
### for each fe data frame

fe_cumul_force<-force_tables%>%
  filter(age=="85+")%>%
  group_by(race, sex)%>%
  summarise(cmin=quantile(c, 0.05)*1e5, 
            cmax=quantile(c, 0.95)*1e5, 
            c=mean(c)*1e5)%>%
  ungroup()

### make pooled age-specific risk across imputations
age_range<-force_tables%>%
  group_by(race, sex, age)%>%
  summarise(qmin=quantile(q, 0.05), 
            qmax=quantile(q, 0.95), 
            q = mean(q),
            cmin = quantile(q, 0.05), 
            cmax = quantile(c, 0.95), 
            c = mean(c))

####################################################################################
### total mortality age/race/sex specific
####################################################################################
### MAIN ANALYSES USE FORCE DEATHS
### SET UP WITH TRAFFIC / SUICIDE IN APPX
fe_tables<-force_tables

#cause_mort<-read.csv("./data/mort_cause.csv", stringsAsFactors = FALSE)


####################################################################################
### plots
####################################################################################
### transform age var for better plotting, convert to numeric with last number

### MAKE AGE SPECIFIC PERIOD RISK TABLES

age_period_pct<-fe%>%
  group_by(age, sex, race, year, .imp)%>%
  summarise(officer_force = sum(officer_force))%>%
  left_join(tot_mort)%>%
  filter(!(is.na(pop)))%>% # remove 2018 FE data, NVSS 2018 not yet released
  group_by(age, sex, race, .imp)%>%
  summarise(ratio = sum(officer_force) / sum(deaths))%>%
  ungroup()%>%
  group_by(age, sex, race)%>%
  summarise(ratio_mean = mean(ratio), 
            ratio_lwr = quantile(ratio, 0.05),
            ratio_upr = quantile(ratio, 0.95))%>%
  arrange(desc(ratio_mean))%>%
  ungroup()

age_period<-fe%>%
  left_join(pop)%>%
  group_by(age, sex, race,.imp)%>%
  summarise(rate = sum(officer_force) / sum(pop))%>%
  ungroup()%>%
  group_by(age, sex, race)%>%
  summarise(q =mean(rate),
            q_lwr = quantile(rate, 0.05),
            q_upr = quantile(rate, 0.95))%>%
  ungroup()%>%
  arrange(desc(q))

age_period_pct<-age_period_pct%>%
  mutate(age = as.character(age)) %>%
  mutate(age = 
           case_when(
             age == "0" ~ "0",
             age =="85+" ~ "85",
             nchar(age)==3 ~ substr(age, 3, 3),
             nchar(age)==5 ~ substr(age, 4, 5)
           )
  )%>%
  mutate(age = as.numeric(age))%>%
  arrange(race, sex, age)

age_period<-age_period%>%
  mutate(age = as.character(age)) %>%
  mutate(age = 
           case_when(
             age == "0" ~ "0",
             age =="85+" ~ "85",
             nchar(age)==3 ~ substr(age, 3, 3),
             nchar(age)==5 ~ substr(age, 4, 5)
           )
  )%>%
  mutate(age = as.numeric(age))%>%
  arrange(race, sex, age)


###########################
## AGE SPECIFIC RISK PLOTS
###########################

age_period %>%
  ggplot(
    aes(
      x = age,
      y = q * 1e5, 
      ymin = q_lwr * 1e5, 
      ymax = q_upr * 1e5,
      color = race, 
      fill = race,
      group = race
    )
  ) +
  #geom_ribbon(aes(fill=race), color = 'grey100', alpha = 0.15, size = 1.25) +
  geom_line() +
  facet_wrap(~sex) +
  xlab("Age") +
  ylab("Risk of being killed by police (per 100,000)") + 
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  geom_errorbar(width = 0, alpha = 0.5) +
  geom_point(size = 0.4)+
  guides(col = guide_legend(override.aes = list(shape = 15, size = 5)))+
  ggsave("vis/age_spec_prob_new.pdf", width = 6, height = 3.5)


ggplot(age_period_pct,
       aes(x=age, 
           y=ratio_mean * 100, 
           ymin = ratio_lwr * 100,
           ymax = ratio_upr * 100,
           color=race, 
           group = race))+
  geom_line() +
  facet_wrap(~sex)+
  xlab("Age")+
  ylab("Police killings as percent of all deaths") +  
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  geom_errorbar(width = 0, alpha = 0.5) +
  geom_point(size = 0.4)+
  guides(col = guide_legend(override.aes = list(shape = 15, size = 5)))+
  ggsave("vis/age_pct_new.pdf", width = 6, height = 3.5)

#### pooled years, cumulative prob (expected deaths per 100k through 85yrs)

ggplot(data = fe_cumul_force,
       mapping =  aes(fill = sex,
           x = reorder(race, c),
           y = ifelse(sex=="Male",
                      -c, 
                      c),
           ymax = ifelse(sex=="Male",
                      -cmax, 
                      cmax),
           ymin = ifelse(sex=="Male",
                         -cmin, 
                         cmin))) + 
  geom_bar(stat = "identity") + 
  geom_linerange(size = 1,  alpha = 0.5) + 
  scale_y_continuous(limits = max(fe_cumul_force$cmax) * c(-1,1), 
                     labels = abs) +
  labs(fill = "Sex") + 
  ylab("People killed by police per 100,000 births") +
  xlab("") + 
  coord_flip() + 
  theme_minimal()+
  ggsave("./vis/pooled_lifetime_new.pdf", width = 6, height = 3.5)

white<-fe_cumul_force%>%
  filter(race=="White")
ineq<-fe_cumul_force%>%
  filter(race!="White")

ineq<-ineq%>%
  mutate(dmin = cmin / white$cmin,
         dmax = cmax / white$cmax,
         d = c  /white$c)

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
  theme_minimal() + 
  ggsave("./vis/lifetime_ineq_new.pdf", width = 6, height = 3.5)

write_csv(ineq, "./vis/lifetime_ineq.csv")  

#################################################
### FOR LEADING CAUSE COMPARISONS
#################################################

cause<-nvss_dat%>%
  group_by(age, race, sex, cause_50)%>%
  summarise(q = sum(deaths)/sum(pop))%>%
  ungroup()%>%
  select(age, race, sex, cause_50, q)

#### CHECK RANKING METHOD FROM LAST PAPER

fe_cause<-age_range%>%
  mutate(cause_50 = "Police homicide")%>%
  select(age, race, sex, cause_50, q)

### makes age-specific cause of death rank order 
cause_rank<-cause%>%
  bind_rows(fe_cause)%>%
  group_by(age, race, sex)%>%
  arrange(age, race, sex, desc(q))%>%
  mutate(rank = rank(-q))

fe_rank<-cause_rank%>%
  filter(cause_50=="Police homicide")
