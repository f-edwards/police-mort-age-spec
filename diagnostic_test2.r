#####################################
# deaths ~ f(age; race; gender)
# model diagnostics, .R
# last edit: 3/25 (M.E)
#####################################

library(tidyverse)
library(lubridate)
library(rstanarm)
library(tidybayes)
library(RColorBrewer)
library(modelr)
library(gridExtra)
library(brms)

setwd("~/projects/pnas")
theme_set(theme_tidybayes())
options(mc.cores = parallel::detectCores())

cols = c(
	brewer.pal(8, 'Set2'), 
	brewer.pal(8, 'Set3'), 
	brewer.pal(8, 'Set1')
)

c_pal = scale_color_manual(values = cols[c(9, 11, 12:16)])
f_pal = scale_fill_manual( values = cols[c(9, 11, 12:16)])

# ... pull in data
dat = read_csv('./data/fe_pop_imputed_13_18.csv') %>%
	mutate(
		age_group = fct_relevel(age, "5-9", after = 2),
		age_c = as.numeric(age_group)
	) %>%
	filter(.imp == 1)

# ... potential models
# ... baseline: simple, independent intercept shifts
m0 =  stan_glmer(
        officer_force ~ race + sex + (1 | age_group) + (1 | year),
        offset = log(pop),
        data = dat, 
        family = 'neg_binomial_2',
        QR = TRUE
    )

# ... m_1: race x gender interaction 
# ... m_2: race x gender effects depends on age group
# ... m_3: race x gender effects depends on year
m1 = update(m0, formula = officer_force ~ race*sex + (1 | age_group) + (1 | year))
m2 = update(m0, formula = officer_force ~ race*sex + (race*sex | age_group) + (1 | year), adapt_delta = .999)
m3 = update(m0, formula = officer_force ~ race*sex + (1 | age_group) + (race*sex | year), adapt_delta = .999)

# saveRDS(m0, 'm0.rds')
# saveRDS(m1, 'm1.rds')
# saveRDS(m2, 'm2.rds')
# saveRDS(m3, 'm3.rds')

# m0 = read_rds('m0.rds')
# m1 = read_rds('m1.rds')
# m2 = read_rds('m2.rds')
# m3 = read_rds('m3.rds')

# ... are any of these strongly preferred? 
# ... cv evidence 
loo0 = loo(m0)
loo1 = loo(m1)
loo2 = loo(m2)
loo3 = loo(m3)

compare_models(loo0, loo1) # ... race x gender interaction is informative
compare_models(loo1, loo2) # ... race x gender x age is informative over just r x g
compare_models(loo1, loo3) # ... just varying by year doesn't add anything

compare_models(loo0, loo1, loo2, loo3)

# ... and, stacking weight evidence
loo_model_weights(list(loo0, loo1, loo2, loo3))

# ... year variation in age processes?
# ... see that counts remain constant; some dip among latinx women
dat %>%
	group_by(race, sex, year) %>% 
	summarise(deaths = sum(officer_force)) %>%
    ggplot(., aes(x = year, y = deaths, group = race, color = race, fill = race)) +
    geom_smooth(method = 'loess', span = 10, se = FALSE, alpha = .15, size = 1.25) + 
    geom_point(pch = 21, color = 'grey100', size = 3, alpha = .75) +
    facet_wrap(~sex, scale = 'free_y') +
    c_pal +
    f_pal 

# ... but that patterns remain the same
# ... age x race x sex patterns are consistent across groups though, despite somewhat shifting counts
dat %>% 
    filter(sex == 'Male') %>%
    ggplot(., aes(x = age_group, y = officer_force, group = year, color = year)) +
    geom_smooth(se = FALSE) +
    facet_wrap(~race) +
    scale_x_discrete(breaks = c('25-29', '50-54'))

# ... gemale groups are a bit more heterogenous, but since counts are so small, may just be noise
dat %>% 
    filter(sex == 'Female') %>%
    ggplot(., aes(x = age_group, y = officer_force, group = year, color = year)) +
    geom_smooth(se = FALSE) +
    facet_wrap(~race) +
    scale_x_discrete(breaks = c('25-29', '50-54'))

# ... let's check it out
df_test = dat %>% filter(sex == 'Female', race == 'White')

t3 = stan_glmer(
    officer_force ~ age_group + (1 | year),
    offset = log(pop),
    data = df_test, 
    family = 'neg_binomial_2',
    adapt_delta = .95
)

t4 = stan_glmer(
    officer_force ~ age_group + (age_group | year),
    offset = log(pop),
    data = df_test, 
    family = 'neg_binomial_2',
    adapt_delta = .95
)

# ... strong evidence that interaction makes fits sig. better?
loo_t3 = loo(t3, k_threshold = .7)
loo_t4 = loo(t4, k_threshold = .7)

compare_models(loo_t3, loo_t4)

# ... any practical difference in predictions?
df_test %>%
    data_grid(age_group, year, race, pop = 100000) %>%
    add_fitted_draws(t4) %>%
    median_qi() %>%
    ggplot(., aes(x = age_group, y = .value, color = factor(year), group = year)) +
    geom_line(size = .75) +
    facet_wrap(~race)

# ... nope