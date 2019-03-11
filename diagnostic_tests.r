#####################################
# deaths ~ f(age; race; gender)
# model diagnostics, .R
# last edit: 3/6 (M.E)
#####################################

library(tidyverse)
library(lubridate)
library(rstanarm)
library(tidybayes)
library(RColorBrewer)
library(modelr)
library(gridExtra)

setwd("~/Projects/pnas")
theme_set(theme_tidybayes())
options(mc.cores = parallel::detectCores())

cols = c(
	brewer.pal(8, 'Set2'), 
	brewer.pal(8, 'Set3'), 
	brewer.pal(8, 'Set1')
)

# ... attach imputed data 
# ... using 2010-2016 here
dat = read_csv('./data/fe_pop_imputed.csv') 

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
m2 = update(m0, formula = deaths ~ race*sex + (race*sex | age_group) + (1 | year), adapt_delta = .9999)
m3 = update(m0, formula = deaths ~ race*sex + (1 | age_group) + (race*sex | year))

m4 = update(m0, formula = deaths ~ race*sex + (race*sex | age_group) + (race*sex | year))

m0 = read_rds('m0.rds')
m1 = read_rds('m1.rds')
m2 = read_rds('m2.rds')
m3 = read_rds('m3.rds')

# ... are any of these strongly preferred? 
# ... cv evidence 
loo0 = loo(m0)
loo1 = loo(m1)
loo2 = loo(m2)
loo3 = loo(m3)

compare_models(loo0, loo1) # ... race x gender interaction is informative
compare_models(loo1, loo2) # ... race x gender x age is informative over just r x g
compare_models(loo1, loo3) # ... year interaction doesn't do much over r x g only model
compare_models(loo1, loo4)

# ... and, stacking weight evidence
loo_model_weights(list(loo0, loo1, loo2, loo3))


# ... are patterns stable across time
df2 %>% 
    ggplot(., aes(x = year, y = deaths, group = race, color = race, fill = race)) +
    geom_smooth(method = 'loess', span = 2, se = FALSE, alpha = .15) + 
    facet_wrap(~sex, scale = 'free_y')

# ... looks like increase across time is mostly among whites, particularly men
df2 %>% 
    ggplot(., aes(x = year, y = deaths, group = race, color = race, fill = race)) +
    geom_smooth(method = 'lm', span = 1, se = TRUE, alpha = .15) + 
    facet_wrap(~sex, scale = 'free_y')

# ... age patterns seem consistent across years, outside of small groups
df2 %>% 
    filter(sex == 'Male') %>%
    ggplot(., aes(x = age_c, y = deaths, group = year, color = year)) +
    geom_smooth(se = FALSE) +
    facet_wrap(~race)

# ... female pop much more noisy (makes sense; much smaller counts)
df2 %>% 
    filter(sex == 'Female') %>%
    ggplot(., aes(x = age_c, y = deaths, group = year, color = year)) +
    geom_smooth(se = FALSE) +
    facet_wrap(~race)

# ... let's test this first in way that mirrors original set up first
stratR = function(.sim_data){

    # ... fit independent model
    m0 = .sim_data %>%
        stan_glmer(
            deaths ~ 1 + (1 | year) + (1 | age_group),
            offset = log(pop),
            data = .,
            family = 'neg_binomial_2',
            adapt_delta = .99
        )

    # ... fit conditional model
    m1 = .sim_data %>%
        stan_glmer(
            deaths ~ 1 + (1 | year) + (1 | age_group) + (1 | year:age_group),
            offset = log(pop),
            data = .,
            family = 'neg_binomial_2',
            adapt_delta = .99
        )

    # ... get fit measures
    loo_m0 = loo(m0, k_threshold = 0.7)
    loo_m1 = loo(m1, k_threshold = 0.7)
    loo_comp = compare_models(loo_m0, loo_m1)
    m_weights = loo_model_weights(list(loo_m0, loo_m1))

    # ... and fitted + predicted values
    scen = .sim_data %>%
        data_grid(
            age_group,
            pop = 100000000,
            year
        ) 

    fit_m0 = scen %>% add_fitted_draws(m0)
    fit_m1 = scen %>% add_fitted_draws(m1)

    pred_m0 = scen %>% add_predicted_draws(m0)
    pred_m1 = scen %>% add_predicted_draws(m1)

    # ... plot of estimated values across years
    fit_plot = bind_rows(
        fit_m0 %>% median_qi(.width = .90) %>% mutate(model = 'm0'),
        fit_m1 %>% median_qi(.width = .90) %>% mutate(model = 'm1')
    ) %>%
    ggplot(.,
        aes(x = age_group,
            y = .value/1000, 
            group = model, 
            color = model)
    ) + 
    geom_line() +
    facet_wrap(~year) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 


    # ... plot draws from predictive draws
    pred_plot = pred_m0 %>%
            filter(year %in% c(2010, 2013, 2016)) %>%
            ggplot(., 
                aes(x = age_group, y = .prediction/1000, group = year)
            ) +
            geom_count(alpha = .25, color = cols[8]) +
            geom_boxplot(aes(group = age_group), outlier.shape = NA, fill = cols[9]) +
            facet_wrap(~year) +
            theme(
                axis.text.x = element_text(angle = 90, vjust = 0.5),
                legend.position = 'bottom'
            ) 

    # ... just return everything
    return(
        list(
            m0 = m0,
            m1 = m1, 
            loo_0 = loo_m0, 
            loo_1 = loo_m1,
            loo_comp = loo_comp,
            m_weights = m_weights,
            fit_m0 = fit_m0,
            fit_m1 = fit_m1,
            pred_m0 = pred_m0,
            pred_m1 = pred_m1, 
            fit_plot = fit_plot,
            pred_pot = pred_plot
        )
    )
}

sim_df = tibble(
            group_id = c('bm', 'wm', 'lm', 'am', 'nm', 'bf', 'wf', 'lf', 'af', 'nf'),
            group_data = list(
                 df2 %>% filter(race == 'black',  sex == 'Male'), 
                 df2 %>% filter(race == 'white',  sex == 'Male'),
                 df2 %>% filter(race == 'latino', sex == 'Male'),
                 df2 %>% filter(race == 'asian',  sex == 'Male'),
                 df2 %>% filter(race == 'amind',  sex == 'Male'),
                 df2 %>% filter(race == 'black',  sex == 'Female'),
                 df2 %>% filter(race == 'white',  sex == 'Female'),
                 df2 %>% filter(race == 'latino', sex == 'Female'),
                 df2 %>% filter(race == 'asian',  sex == 'Female'),
                 df2 %>% filter(race == 'amind',  sex == 'Female')
            )
        ) %>%
        mutate(group_res = map(group_data, ~stratR(.x)))
save_rds(sim_df, 'sims.rds')       

# ... maybe a spline interaction works just as well/gives a year interaction a better chance?
library(brms)

df3 = df2 %>% 
    mutate(year_f = factor(year)) %>% 
    filter(race == 'asian',  sex == 'Female')

# ... set K to max 
m_test00 = brm(
    deaths ~ s(age_c) + s(year, k = 7) + offset(I(log(pop))),
    data = df3,
    family = "negbinomial",
    control = list(adapt_delta = 0.9999999, max_treedepth = 15),
    iter = 2000
)

m_test10 = brm(
    deaths ~ t2(age_c, year) + offset(I(log(pop))),
    data = df3,
    family = "negbinomial",
    control = list(adapt_delta = 0.99999, max_treedepth = 15),
    iter = 2000
)

pa = df3 %>%
    data_grid(age_c, year, pop = 100000000) %>%
    add_fitted_draws(m_test00) %>%
    median_qi(.width = .9) %>%
    mutate(model = 'M_11') %>%
    ggplot(.,
           aes(
            x = age_c, 
            y = .value/1000, 
            group = year, 
            color = factor(year),
            fill = factor(year)
        )
    ) + 
    geom_line(size = 1.25) +
    scale_color_manual(values = cols[c(9, 11, 12, 13, 14, 15, 16)]) +
    scale_fill_manual(values = cols[c(9, 11, 12, 13, 14, 15, 16)]) +
    ylab('estimated risk, per 100k') +
    xlab('age interval') +
    facet_wrap(~model) + 
    theme(legend.title = element_blank())

pb = df3 %>%
    data_grid(age_c, year, pop = 100000000) %>%
    add_fitted_draws(m_test10) %>%
    median_qi(.width = .9) %>%
    mutate(model = 'M_21') %>%
    ggplot(.,
           aes(
            x = age_c, 
            y = .value/1000, 
            group = year, 
            color = factor(year),
            fill = factor(year)
        )
    ) + 
    geom_line(size = 1.25) +
    scale_color_manual(values = cols[c(9, 11, 12, 13, 14, 15, 16)]) +
    scale_fill_manual(values = cols[c(9, 11, 12, 13, 14, 15, 16)]) +
    ylab('estimated risk, per 100k') +
    xlab('age interval') +
    facet_wrap(~model) + 
    theme(legend.title = element_blank())

# .. . honestly looks like can rep w/ just a year intercept and age spline
l00 = loo(m_test00, reloo = TRUE)
l10 = loo(m_test10, reloo = TRUE)

plot(l00)
plot(l10)

compare_ic(l00, l10)
(ws2 = loo_model_weights(list(l00, l10)))
test2 = df3 %>% data_grid(age_c, year, pop = 100000000)

pc = pp_average(
    m_test00, m_test10, 
    weights = ws2, 
    method = 'fitted',
    newdata = test2
    ) %>% 
    tbl_df() %>%
    bind_cols(test2) %>%
    mutate(model = 'combined') %>%
    ggplot(., 
        aes(y  = Estimate/1000, 
            #ymin = Q2.5, ymax = Q97.5,
            x = age_c, 
            group = year, 
            color = factor(year), fill = factor(year)
        )
    ) +
    #geom_ribbon() +
    geom_line(size = 1.25) +
    scale_color_manual(values = cols[c(9, 11, 12, 13, 14, 15, 16)]) +
    scale_fill_manual(values = cols[c(9, 11, 12, 13, 14, 15, 16)]) +
    facet_wrap(~model) +
    theme(legend.title = element_blank()) +
    ylab('estimated risk, per 100k') +
    xlab('age interval') 

# ... seems like fairly stable; a bit of variance for groups like black women but largely washed out after combining models


# ... plot out bf group for example
grid.arrange(pa, pb, pc, ncol = 2, layout_matrix = rbind(c(1,2),c(3)))