#####################################
# deaths ~ f(age; race; gender)
# model results, .R
# last edit: 2/5 (M.E)
#####################################

library(tidyverse)
library(lubridate)
library(rstanarm)
library(tidybayes)
library(tidycensus)
library(RColorBrewer)
library(modelr)

setwd("~/Projects/pnas")
theme_set(theme_tidybayes())
options(mc.cores = parallel::detectCores())

cols = c(
	brewer.pal(8, 'Set2'), 
	brewer.pal(8, 'Set3'), 
	brewer.pal(8, 'Set1')
)

# ... attach imputed data 
dat = read_csv('./data/fe_pop_imputed.csv') 

# .... let's just go for first imputation for now (will stack)
df2 = dat %>%
	filter(.imp == 1) %>%
	mutate(
		age_group = fct_relevel(age, "5-9", after = 2),
		age_c = as.numeric(age_group)
	) 
# ... models
# ... baseline: simple, independent intercept shifts
m0 =  stan_glmer(
		deaths ~ race + sex + (1 | age_group) + (1 | year),
		offset = log(pop),
		data = df2, 
		family = 'neg_binomial_2'
	)

# ... m_1: race x gender interaction 
# ... m_2: race x gender effects depends on age group
# ... m_3: race x gender effects depends on year
m1 = update(m0, formula = deaths ~ race*sex + (1 | age_group) + (1 | year))
m2 = update(m0, formula = deaths ~ race*sex + (race + sex + race*sex | age_group) + (1 | year), adapt_delta = .9999)
m3 = update(m0, formula = deaths ~ race*sex + (1 | age_group) + (race + sex + race*sex | year))


m4 = update(m0, formula = deaths ~ race*sex + (race*sex | age_group:year), adapt_delta = .9999)


# ... maybe need a age x race x sex x year interaction? nah

# saveRDS(m0, 'm0.rds')
# saveRDS(m1, 'm1.rds')
# saveRDS(m2, 'm2.rds')
# saveRDS(m3, 'm3.rds')

# ... what do year effects look like?
m2 %>% 
	spread_draws(b[, group]) %>% 
	median_qi(.width = c(.5, .9)) %>% 
	separate(group, c("group", "value"), ":") %>%
	filter(group == 'year') %>%
	ggplot(., aes(x = value, y = b, ymin = .lower, ymax = .upper)) + 
	geom_hline(yintercept = 0, size = 1.25, alpha = .25, color = cols[8]) + 
	geom_errorbar(aes(group = .width, size = .width), width = 0, color = cols[8]) +
	geom_point(fill = cols[9], pch = 21, size = 3)  +
	scale_size(range = c(2, .25), breaks = c(.5, .90)) +
    coord_flip() +
    ylab(expression(beta)) 

# ... and predicted age x sex x race risks across the models
scens = df2 %>%
    data_grid(
        age_group,
        sex,
        race,
        pop = 100000000,
        year = 0
    ) 

pR = function(.sims, .mod){
	.sims %>% 
		add_predicted_draws(.mod) %>%
		median_qi(.width = .5) %>%
		ggplot(., 
           aes(x = age_group, 
               y = .prediction/1000,
               ymin = .lower/1000, ymax = .upper/1000,
               group = race, color = race, fill = race
           )
    ) + 
    geom_ribbon(color = 'grey100', alpha = .25) +
    geom_line(size = 1.45) +
    facet_wrap(~sex, scale = 'free_y') +
    scale_color_manual(values = cols[c(9, 12, 3, 8, 16)]) + 
    scale_fill_manual(values  = cols[c(9, 12, 3, 8, 16)]) +
    theme_tidybayes() +
    theme(
    	axis.text.x = element_text(angle = 90, vjust = 0.5),
    	legend.title = element_blank()
    )
}

# ... plot it out
pR(scens, m0)
pR(scens, m1)
pR(scens, m2)

df2 %>%
    data_grid(
        age_group,
        sex,
        race,
        pop = 100000000,
        year
    ) %>% 
    add_predicted_draws(m3) %>%
    filter(sex == 'Female') %>%
    median_qi(.width = .5) %>%
    ggplot(., aes(x = age_group, 
               y = .prediction/1000,
               ymin = .lower/1000, ymax = .upper/1000,
               group = year, color = factor(year), fill = year
           )
    ) +
    geom_line() +
    facet_wrap(~race, scale = 'free_y') +
    theme_tidybayes() +
    theme(
    	axis.text.x = element_text(angle = 90, vjust = 0.5),
    	legend.title = element_blank()
    ) +
    scale_color_brewer(palette = 'Dark2')

# ... are any of these strongly preferred? 
# ... CV evidence
loo0 = loo(m0)
loo1 = loo(m1)
loo2 = loo(m2)
loo3 = loo(m3)

compare_models(loo0, loo1) # ... race x gender interaction is informative
compare_models(loo1, loo2) # ... race x gender x age is informative over just r x g
compare_models(loo1, loo3) # ... year interaction doesn't do much over r x g only model

# ... and, stacking weight evidence
# ... wow, ok
loo_model_weights(list(loo0, loo1, loo2, loo3))

# ... maybe better to use fitted draws here 
# ... folks might 'get' uncertainty a bit more?
scens %>%
    add_fitted_draws(m2) %>%
    mutate(.value = .value/1000) %>%
    median_qi(.width = .5) %>%
    ggplot(., 
           aes(x = age_group, 
               y = .value,
               ymin = .lower, ymax = .upper,
               group = race, color = race, fill = race
           )
    ) + 
    geom_ribbon(color = 'grey100', alpha = .5) +
    geom_line(size = 1.45) +
    facet_wrap(~sex, scale = 'free_y') +
    scale_color_manual(values = cols[c(9, 12, 3, 8, 16)]) + 
    scale_fill_manual(values  = cols[c(9, 12, 3, 8, 16)]) +
    theme_tidybayes()

# ... can probably be more efficent with age
# .... tossing away info; try as numeric

# ... check if can use standard polynomials to get this at atkk
m0_c = stan_glmer(
		deaths ~ race*sex*poly(age_c, 3) + (1 | year),
		offset = log(pop),
		data = df2, 
		family = 'neg_binomial_2'
	) 

#m00_c    = update(m0_c, formula = deaths ~ race*sex + poly(age_c,2) + (1 | year))
#m000_c   = update(m0_c, formula = deaths ~ race*sex + poly(age_c,3) + (1 | year))
#m0000_c  = update(m0_c, formula = deaths ~ race*sex + poly(age_c,4) + (1 | year))
#m00000_c = update(m0_c, formula = deaths ~ race*sex + poly(age_c,5) + (1 | year))#

#loo_l = loo(m0_c)
#loo_s = loo(m00_c)
#loo_c = loo(m000_c)
#loo_q = loo(m0000_c)
#loo_i = loo(m00000_c)#

#compare_models(loo_l, loo_s)
#compare_models(loo_s, loo_c)
#compare_models(loo_c, loo_q) # ... looks like improvments stop after 4th order
#compare_models(loo_q, loo_i)#

## ... let's fit using age^4 as base
#m1_c = update(m0_c, formula = deaths ~ race*sex + poly(age_c, 4) + (1 | year))
#m2_c = update(m0_c, formula = deaths ~ race*sex*poly(age_c, 4)   + (1 | year))
##m3_c = update(m0_c, formula = deaths ~ race*sex + poly(age_c, 4) + (race + sex + race*sex | year))
##m4_c = update(m0_c, formula = deaths ~ race*sex*poly(age_c, 4)  + (race + sex + race*sex | year))#

#l1 = loo(m1_c)
#l2 = loo(m2_c)


# ... does a spline work
# ... offset argument failing in rstanarm? 
m_gam = stan_gamm4(
    deaths ~ s(age_c) + race + sex,
    data = df2,
    random = ~ (1 | year),
    offset = log(pop),
    family = 'neg_binomial_2'
)



### IN PROGRESS
# ... what if treat year as just a fixed paramter?

# ... what's the shape look like in the data?
df2 %>% 
    ggplot(., aes(x = year, y = deaths)) +
    geom_smooth(method = 'loess', span = 1, se = TRUE, fill = 'grey1') 

df2 %>% 
    ggplot(., aes(x = year, y = deaths, group = race, color = race, fill = race)) +
    geom_smooth(method = 'loess', span = 1, se = TRUE, alpha = .15) + 
    facet_wrap(~sex, scale = 'free_y')

# ... looks like increase mostly among whites?
df2 %>% 
    ggplot(., aes(x = year, y = deaths, group = race, color = race, fill = race)) +
    geom_smooth(method = 'lm', span = 1, se = TRUE, alpha = .15) + 
    facet_wrap(~sex, scale = 'free_y')

# ... age patterns seem consistent acorss years; outsiide of small groups
df2 %>% 
    filter(sex == 'Male') %>%
    ggplot(., aes(x = age_c, y = deaths, group = year, color = year)) +
    geom_smooth(se = FALSE) +
    facet_wrap(~race, scale = 'free_y')

# ... female pop much more noisey
df2 %>% 
    filter(sex == 'Female') %>%
    ggplot(., aes(x = age_c, y = deaths, group = year, color = year)) +
    geom_smooth(se = FALSE) +
    facet_wrap(~race, scale = 'free_y')


# ... observed data look like 2rd degree term works well enough (can test this if really concened)
# ... let's walk through the same modeling exercise 
m0_y = stan_glmer(
    deaths ~ race + sex + poly(year, 2) + (1 | age_group),
    offset = log(pop),
    data = df2,
    family = 'neg_binomial_2'
)    

# ... m_1: race x gender interaction 
# ... m_2: race x gender effects depends on age group
# ... m_3: race x gender effects depends on year
# ... m_4: race x age x gender x year effects all depedent on one another
m1_y = update(m0_y, formula = deaths ~ race*sex + poly(year, 2) + (1 | age_group))
m2_y = update(m0_y, formula = deaths ~ race*sex + poly(year, 2) + (race*sex | age_group))
m3_y = update(m0_y, formula = deaths ~   race*sex*poly(year, 2) + (1 | age_group))
m4_y = update(m0_y, formula = deaths ~   race*sex*poly(year, 2) + (race*sex*poly(year, 2) | age_group))

# saveRDS(m0_y, 'm0_y.rds')
# saveRDS(m1_y, 'm1_y.rds')
# saveRDS(m2_y, 'm2_y.rds')
# saveRDS(m3_y, 'm3_y.rds')
# saveRDS(m4_y, 'm4_y.rds')

df3 = df2 %>% mutate(year2 = year - 2015)

m0_y2 = stan_glmer(
    deaths ~ race + sex + year + I(year^2) + (1 | age_group),
    offset = log(pop),
    data = df2,
    family = 'neg_binomial_2',
    QR = TRUE
)

# ... maybe could center as well? 







# ... hmm, maybe orthogonal polynomials are slowing this down

# ... and predicted age x sex x race risks across the models
scens2 = df2 %>%
    data_grid(
        age_group,
        sex,
        race,
        pop = 100000000,
        year
    ) 

scens2 %>%
    add_predicted_draws(m1_y) %>%
    median_qi(.width = .5) %>%
    filter(sex == 'Male') %>%
    ggplot(.,
        aes(
            x = age_group,
            y = .prediction/1000,
            group = year,
            color = year
        )) + 
    geom_line(size = .75) +
    facet_wrap(~race) +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank()
    )

scens2 %>%
    filter(year == 2016) %>%
        add_predicted_draws(m1_y) %>%
        median_qi(.width = .5) %>%
        ggplot(., 
           aes(x = age_group, 
               y = .prediction/1000,
               ymin = .lower/1000, ymax = .upper/1000,
               group = race, color = race, fill = race
           )
    ) + 
    geom_ribbon(color = 'grey100', alpha = .25) +
    geom_line(size = 1.45) +
    facet_wrap(~sex, scale = 'free_y') +
    scale_color_manual(values = cols[c(9, 12, 3, 8, 16)]) + 
    scale_fill_manual(values  = cols[c(9, 12, 3, 8, 16)]) +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank()
    )