#####################################
# deaths ~ f(age; race; gender)
# imputed models, .R
# last edit: 3/6 (M.E)
#####################################

library(tidyverse)
library(brms)
library(tidybayes)
library(RColorBrewer)
library(modelr)
library(tictoc)

setwd("~/Projects/pnas")
theme_set(theme_tidybayes())
options(mc.cores = parallel::detectCores())

cols = c(
	brewer.pal(8, 'Set2'), 
	brewer.pal(8, 'Set3'), 
	brewer.pal(8, 'Set1')
)

c_pal = scale_color_manual(values = cols[c(9, 11, 12:16)])
f_pal = scale_fill_manual( values = cols[c(9, 11, 12:16)])

# ... hmm, some errors in age cats; assumming that those are just mislabel;ed
dat = read_csv('./data/fe_pop_imputed_00_18.csv') %>%
	mutate(
		age_group = case_when(
			age == '4-Jan'  ~ '1-4',
			age == '9-May'  ~ '5-9',
			age == '14-Oct' ~ '10-14',
			TRUE ~ age
		),
		age_group = fct_relevel(age_group, "5-9", after = 2)
	) %>%
	filter(year %in% (2008:2018))

# ... fit via brms' built in imputation stuff
dfs = dat %>%
	nest(-.imp) %>%
	.$data

fit = brm_multiple(
	officer_force ~ race*sex + (race*sex | age_group) + (1 | year) + offset(I(log(pop))),
	data = dfs,
	family = 'negbinomial'
)

# fit = read_rds('fit.rds')

# ... check it out
round(fit$rhats, 2) %>% t()
fixef(fit)
ranef(fit)