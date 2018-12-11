### MAKE POLICE KILLINGS LIFE TABLE
## USES METHOD FROM http://data.princeton.edu/eco572/periodlt.html
rm(list=ls()); gc()
library(tidyverse)
library(xtable)
library(lubridate)

theme_set(theme_minimal())

### pull in fatal encounters derived life tables
### nvss derived life tables
source("fe_lifetable.R")
source("nvss_lifetable.R")


### make lifetime cumulative risk by race, year, sex
fe_cumul<-fe_tables%>%
  filter(age=="85+")%>%
  group_by(race, sex)%>%
  summarise(cmin=min(c)*1e5, cmax=max(c)*1e5, c=mean(c)*1e5)%>%
  ungroup()

### merged
compare<-fe_tables%>%
  group_by(.imp, race, sex)%>%
  summarise(fe_deaths = sum(deaths))%>%
  ungroup()%>%
  group_by(race, sex)%>%
  summarise(fe_min = min(fe_deaths),
            fe_max = max(fe_deaths))%>%
  left_join(nvss_tables%>%
              group_by(race, sex)%>%
              filter(cause_50 == "Legal intervention")%>% #select only legal intervention
              summarise(nvss_deaths = sum(deaths)))%>%
  mutate(ratio_min = fe_min / nvss_deaths,
         ratio_max = fe_max / nvss_deaths)

write_csv(compare, "./vis/compare.csv")

comp_tab<-compare%>%
  mutate(FE = paste("[",fe_min,", ",fe_max,"]", sep=""),
         ratio = paste("[",round(ratio_min,2),", ",round(ratio_max,2),"]", sep=""))%>%
  select(race, sex, FE, nvss_deaths, ratio)%>%
  arrange(sex, race)%>%
  mutate(sex = case_when(sex=="Male" ~ "M",
                         sex=="Female" ~ "F"))%>%
  rename(`Race / Ethnicity` = race, `Sex` = sex, `NVSS` = nvss_deaths,
         `FE / NVSS` = ratio)

comp_tab<-xtable(comp_tab,
                 caption = "People killed by police in the US by race and sex as recorded in Fatal Encounters and people killed by police as recorded in the National Vital Statistics System, 2010 - 2016. Uncertainty intervals calculated from multiple imputation of missing race/ethnicity in Fatal Encounters.")
print.xtable(comp_tab, 
             file = "./vis/comp_tab.tex",
             include.rownames = FALSE,
             caption.placement = "top")

### make pooled age-specific risk across imputations
age_range<-fe_tables%>%
  group_by(race, sex, age)%>%
  summarise(qmin=min(q), qmax=max(q), q = mean(q),
            cmin = min(c), cmax = max(c), c = mean(c))

####################################################################################
### total mortality age/race/sex specific
####################################################################################

tot_mort<-read.csv("./data/total_mort.csv", stringsAsFactors = FALSE)
#cause_mort<-read.csv("./data/mort_cause.csv", stringsAsFactors = FALSE)

### police deaths as pct of total
pol_deaths<-fe_tables%>%
  filter(.imp==1)%>%
  summarise(deaths = sum(deaths))

pct_tot<-pol_deaths / sum(tot_mort$deaths) * 1e5

### synthetic across years
tot_mort<-tot_mort%>%
  group_by(age, race, sex)%>%
  summarise(death.rt = sum(deaths) / sum(pop))%>%
  ungroup()

cause_mort<-nvss_tables%>%
  select(race, sex, age, cause_50, q, c)
### to make uncertainty intervals for table
fe_cause_min<-age_range%>%
  mutate(cause_50 = "Police homicide")%>%
  select(age, race, sex, cause_50, qmin, cmin)%>%
  rename(q = qmin, c = cmin)

fe_cause_max<-age_range%>%
  mutate(cause_50 = "Police homicide")%>%
  select(age, race, sex, cause_50, qmax, cmax)%>%
  rename(q = qmax, c = cmax)

### makes age-specific cause of death rank order MINIMUM
cause_mod_min<-cause_mort%>%
  mutate(sex = as.character(sex),
         race = as.character(race))%>%
  bind_rows(fe_cause_min)%>%
  group_by(age, race, sex)%>%
  arrange(age, race, sex, desc(q))%>%
  mutate(rank_agespec_min = rank(-q))%>%
  arrange(age, race, sex, desc(c))%>%
  mutate(rank_cumul_min = rank(-c))%>%
  rename(qmin=q, cmin=c)
### makes age-specific cause of death rank order MAXIMUM
cause_mod_max<-cause_mort%>%
  mutate(sex = as.character(sex),
         race = as.character(race))%>%
  bind_rows(fe_cause_max)%>%
  group_by(age, race, sex)%>%
  arrange(age, race, sex, desc(q))%>%
  mutate(rank_agespec_max = rank(-q))%>%
  arrange(age, race, sex, desc(c))%>%
  mutate(rank_cumul_max = rank(-c))%>%
  rename(qmax=q, cmax=c)

cause_mod<-cause_mod_min%>%
  left_join(cause_mod_max)%>%
  ungroup()

### make ranks NA when q or c == 0 respectively
cause_mod<-cause_mod%>%
  mutate(rank_agespec_min = ifelse(qmin==0, "-", rank_agespec_min),
         rank_agespec_max = ifelse(qmax==0, "-", rank_agespec_max),
         rank_cumul_min = ifelse(cmin==0, "-", rank_cumul_min),
         rank_cumul_max = ifelse(cmax==0, "-", rank_cumul_max))
### makes cumulative risk cause of death rank order
write_csv(cause_mod, "./vis/cause_of_death_ranks.csv")

### make appendix table for leading causes of death
pol_rank<-cause_mod%>%
  filter(cause_50!="Legal intervention")%>% # remove redundant category
  filter(cause_50 == "Police homicide")%>%
  mutate(age = factor(age, levels = c(
                        "0", "1-4", "5-9", "10-14", "15-19",
                        "20-24", "25-29", "30-34", "35-39",  
                       "40-44", "45-49", "50-54", "55-59",
                       "60-64", "65-69", "70-74", "75-79",
                       "80-84", "85+", "Missing")))%>%
  arrange(sex, race, age)%>%
  select(-cause_50)

### format intervals for table output
### single digit if min==max, otherwise range
pol_rank<-pol_rank%>%
  mutate(q = ifelse(
    qmin == qmax,
    as.character(prettyNum(qmin * 1e5, digits = 2)), 
    paste("[", 
          prettyNum(qmin * 1e5, digits=2),
          ", ",
          prettyNum(qmax * 1e5, digits=2),
          "]", 
          sep="")),
    c = ifelse(
      cmin == cmax,
      as.character(prettyNum(cmin * 1e5, digits = 2)), 
      paste("[", 
            prettyNum(cmin * 1e5, digits=2),
            ", ",
            prettyNum(cmax * 1e5, digits=2),
            "]", 
            sep="")),
    rank_agespec = ifelse(
      rank_agespec_min == rank_agespec_max,
      rank_agespec_min,
      paste("[",
            rank_agespec_max,
            ", ",
            rank_agespec_min,
            "]",
            sep="")),
      rank_cumul = ifelse(
        rank_cumul_min == rank_cumul_max,
        rank_cumul_min,
        paste("[",
              rank_cumul_max,
              ", ",
              rank_cumul_min,
              "]",
              sep=""))
    )%>%
  select(race, sex, age, q, c, rank_agespec, c, rank_cumul)

#excluding cumulative leading cause rank, it's hard to interpret
pol_rank<-pol_rank%>%
  rename(Age = age, Race = race, Sex = sex, `Age-specific police homicide rate` = q, 
         `Cumulative police homicide rate` = c, `Age-specific leading cause rank` = rank_agespec,
         `Cumulative leading cause rank` = rank_cumul)%>%
  mutate(Sex = case_when(
    Sex == "Female" ~ "F",
    Sex == "Male" ~ "M"
  ))
### write out table S3
write_csv(pol_rank, "./vis/pol_rank.csv")

print.xtable(
  xtable(
    pol_rank,
    caption = "Table S3. Police homicide rates by race and sex. Age-specific risk and cumulative risk of death per 100,000 population derived from 2010 - 2016 US synthetic life tables. Police homicide as a cumulative and age-specific leading cause of death relative to NCHS defined 50 rankable leading causes of death. Uncertainty intervals derived from multiple imputation of missing data on race/ethnicity and age in Fatal Encounters."
  ),
  file = "./vis/pol_rank.tex",
  include.rownames = FALSE,
  caption.placement = "top"
)

### join onto age specific
tot_mort<-tot_mort%>%
  left_join(age_range)%>%
  mutate(ratio=q/death.rt,
         ratio_min = qmin/death.rt,
         ratio_max = qmax/death.rt)%>%
  arrange(desc(ratio))%>%
  mutate(age = ifelse(age=="0", 0,
                      ifelse(age=="85+", 85,
                             ifelse(nchar(age)==3, as.integer(substr(age, 3, 3)),
                                    as.integer(substr(age, 4, 5))))))

write_csv(tot_mort, "./vis/tot_mort_pct.csv")

####################################################################################
### plots
####################################################################################

### transform age var for better plotting, convert to numeric with last number
age_range<-age_range%>%
  mutate(age = as.character(age)) %>%
  mutate(age = ifelse(age=="0", 0,
                      ifelse(age=="85+", 85,
                             ifelse(nchar(age)==3, as.integer(substr(age, 3, 3)),
                                    as.integer(substr(age, 4, 5))))))%>%
  arrange(race, sex, age)

age_range_white<-age_range%>%
  ungroup()%>%
  filter(race=="White")


### age specific for 2016, with intervals

ggplot(age_range,
       aes(x=age, y=q * 1e5, ymin=qmin * 1e5, 
           ymax=qmax * 1e5,
           color=race, 
           group = race))+
  geom_line()+
  geom_ribbon(aes(fill=race, color = NULL), alpha = 0.4)+
  facet_wrap(~sex)+
  xlab("Age")+
  ylab("Age-specific risk (per 100,000)") +  
  theme_minimal() + 
  guides(color = guide_legend(ncol=3))+
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  ggsave("./vis/age_spec_prob.pdf", width = 6, height = 6)

ggplot(age_range%>%
         filter(sex=="Female"),
       aes(x=age, y=q * 1e5, ymin=qmin * 1e5, 
           ymax=qmax * 1e5,
           color=race, 
           group = race))+
  geom_line()+
  geom_ribbon(aes(fill=race, color = NULL), alpha = 0.4)+
  facet_wrap(~sex)+
  xlab("Age")+
  ylab("Age-specific risk (per 100,000)") +  
  theme_minimal() + 
  guides(color = guide_legend(ncol=3))+
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  ggsave("./vis/age_spec_female.pdf", width = 6, height = 6)

ggplot(age_range%>%
         filter(sex=="Male"),
       aes(x=age, y=q * 1e5, ymin=qmin * 1e5, 
           ymax=qmax * 1e5,
           color=race, 
           group = race))+
  geom_line()+
  geom_ribbon(aes(fill=race, color = NULL), alpha = 0.4)+
  facet_wrap(~sex)+
  xlab("Age")+
  ylab("Age-specific risk (per 100,000)") +  
  theme_minimal() + 
  guides(color = guide_legend(ncol=3))+
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  ggsave("./vis/age_spec_male.pdf", width = 6, height = 6)

write.csv(age_range, "./vis/age_range.csv")

ggplot(tot_mort,
       aes(x=age, y=ratio * 100, 
           ymin = ratio_min * 100,
           ymax = ratio_max * 100,
           color=race, 
           group = race))+
  geom_line()+
  geom_ribbon(aes(fill=race, color = NULL), alpha = 0.4)+
  facet_wrap(~sex)+
  xlab("Age")+
  ylab("Police killings as percent of all deaths") +  
  theme_minimal() + 
  guides(color = guide_legend(ncol=3))+
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  ggsave("./vis/age_pct.pdf", width = 6, height = 6)

ggplot(tot_mort%>%
         filter(sex=="Female"),
       aes(x=age, y=ratio * 100, 
           ymin = ratio_min * 100,
           ymax = ratio_max * 100,
           color=race, 
           group = race))+
  geom_line()+
  geom_ribbon(aes(fill=race, color = NULL), alpha = 0.4)+
  xlab("Age")+
  ylab("Police killings as percent of all deaths") +  
  theme_minimal() + 
  guides(color = guide_legend(ncol=3))+
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  ggsave("./vis/age_pct_female.pdf", width = 6, height = 6)

ggplot(tot_mort%>%
         filter(sex=="Male"),
       aes(x=age, y=ratio * 100, 
           ymin = ratio_min * 100,
           ymax = ratio_max * 100,
           color=race, 
           group = race))+
  geom_line()+
  geom_ribbon(aes(fill=race, color = NULL), alpha = 0.4)+
  facet_wrap(~sex)+
  xlab("Age")+
  ylab("Police killings as percent of all deaths") +  
  theme_minimal() + 
  guides(color = guide_legend(ncol=3))+
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  ggsave("./vis/age_pct_male.pdf", width = 6, height = 6)

#### pooled years, cumulative prob (expected deaths per 100k through 85yrs)

ggplot(data = fe_cumul,
       mapping =  aes(fill = sex,
           x = reorder(race, cmax),
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
  geom_linerange(size = 1) + 
  scale_y_continuous(limits = max(fe_cumul$cmax) * c(-1,1), 
                     labels = abs) +
  labs(fill = "Sex") + 
  ylab("People killed by police per 100,000 births") +
  xlab("") + 
  coord_flip() + 
  theme_minimal()+
  ggsave("./vis/pooled_lifetime.pdf", width = 6, height = 6)

write_csv(fe_cumul, "./vis/pooled_lifetime.csv")

white<-fe_cumul%>%
  filter(race=="White")
ineq<-fe_cumul%>%
  filter(race!="White")

ineq<-ineq%>%
  mutate(dmin = cmin / white$cmin,
         dmax = cmax / white$cmax,
         d = c  /white$c)

ggplot(ineq,
       aes(fill = sex,
           x = reorder(race,dmax),
           y = ifelse(sex == "Male",
                      -d, d), 
           ymax = ifelse(sex == "Male",
                         -dmax, dmax), 
           ymin = ifelse(sex == "Male",
                         -dmin, dmin))) +
  geom_bar(stat = "identity") + 
  geom_linerange(size = 1) + 
  scale_y_continuous(limits = max(ineq$dmax) *c(-1,1),
                     labels = abs) + 
  labs(fill = "Sex") + 
  ylab("Mortality rate ratio (relative to white)") + 
  xlab("") + 
  coord_flip()+
  theme_minimal() + 
  ggsave("./vis/lifetime_ineq.pdf", width = 6, height = 6)

write_csv(ineq, "./vis/lifetime_ineq.csv")  


### make table 2

tab2<-ineq%>%
  bind_rows(white)%>%
  mutate(`Risk per 100,000` = 
           paste("[", round(cmin,2), ", ", round(cmax,2), "]", sep=""),
         `Risk ratio relative to white` = 
           paste("[", round(dmin, 2), ", ", round(dmax,2), "]", sep=""))%>%
  select(-cmin, -cmax, -c, -dmin, -dmax, -d)%>%
  mutate(sex = 
           case_when(sex == "Female" ~ "F",
                     sex == "Male" ~ "M"))%>%
  rename(Sex = sex, 
         `Race/ethnicity` = race)%>%
  arrange(Sex, `Race/ethnicity`)

tab2<-xtable(tab2,
                 caption = "Lifetime risk of being killed by police and racial/ethnic inequality in risk of being killed by police by Race/Ethnicity and sex in the US. Results from synthetic lifetables, 2010 - 2016. Uncertainty intervals calculated from multiple imputation of missing data on victim race/ethnicity.")
print.xtable(tab2, 
             file = "./vis/tab2.tex",
             include.rownames = FALSE,
             caption.placement = "top")





### new graphics


# ... version w/ a little distinct ribbons
age_range %>%
  ggplot(
    aes(
      x = age,
      y = q * 1e5, 
      ymin = qmin * 1e5, 
      ymax = qmax * 1e5,
      color = race, 
      group = race
    )
  ) +
  geom_ribbon(aes(fill=race), color = 'grey100', alpha = 0.15, size = 1.25) +
  geom_line(size = 1.25) + 
  #geom_point(size = 2, alpha = .5) +
  facet_wrap(~gender) +
  xlab("Age") +
  ylab("Age-specific risk (per 100,000)") + 
  theme_minimal() + 
  theme(legend.title = element_blank()) +
  scale_color_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2')

# ... or w/ real estimates marked and implied line smoothed
age_range %>%
  ggplot(
    aes(
      x = age,
      y = q * 1e5, 
      ymin = qmin * 1e5, 
      ymax = qmax * 1e5,
      color = race, 
      fill = race,
      group = race
    )
  ) +
  #geom_ribbon(aes(fill=race), color = 'grey100', alpha = 0.15, size = 1.25) +
  geom_line(stat = 'smooth', se = FALSE, method = 'loess', span = .5, alpha = .35, size = 1.25) +
  facet_wrap(~gender) +
  xlab("Age") +
  ylab("Age-specific risk (per 100,000)") + 
  theme_minimal() + 
  theme(legend.title = element_blank()) +
  scale_color_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  geom_errorbar(width = 0) +
  geom_point(size = 2.3, alpha = 1, pch = 21, color = 'grey10')


# ... can dodge the error bars a bit to side, if really want to dispaly uncertinty
age_range %>%
  ggplot(
    aes(
      x = age,
      y = q * 1e5, 
      ymin = qmin * 1e5, 
      ymax = qmax * 1e5,
      color = race, 
      fill = race,
      group = race
    )
  ) +
  #geom_ribbon(aes(fill=race), color = 'grey100', alpha = 0.15, size = 1.25) +
  geom_line(stat = 'smooth', se = FALSE, method = 'loess', span = .5, alpha = .5, size = 1.25) +
  facet_wrap(~gender) +
  xlab("Age") +
  ylab("Age-specific risk (per 100,000)") + 
  theme_minimal() + 
  theme(legend.title = element_blank()) +
  scale_color_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  geom_errorbar(width = 0, position = position_dodge(width = 3), size = 1.15, alpha = .7) +
  geom_point(size = 2.3, alpha = 1, pch = 21, color = 'grey100', position = position_dodge(width = 1.25))

  