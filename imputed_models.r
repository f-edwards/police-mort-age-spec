#####################################
# deaths ~ f(age; race; gender)
# imputed models, .R
# last edit: 2/23 (M.E)
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
		)
	) %>%
	mutate(age_group = fct_relevel(age_group, "5-9", after = 2))

# ... fit using brms' built in function
dfs = dat %>%
	nest(-.imp) %>%
	.$data

tic()
fit = brm_multiple(
	officer_force ~ race*sex + (race*sex | age_group) + (1 | year) + offset(I(log(pop))),
	data = dfs,
	family = 'negbinomial'#,
	#iter = 500
)
toc()

fit = read_rds('fit.rds')

# ... check it out
round(fit$rhats, 2) %>% t()
fixef(fit)
ranef(fit)

# ... plot it out
# ... year effects
yr_scen = dfs[[1]] %>%
	data_grid(
		year, 
		age_group = '30-34',
		race = 'White',
		sex = 'Male',
		pop = 100000
	) 

yr_scen %>%
	add_fitted_draws(fit) %>%
	median_qi(.width = .75) %>%
	ggplot(., 
		aes(x = year, 
			y = .value, 
			ymin = .lower, 
			ymax = .upper,
		)
	) +
	geom_vline(xintercept = 2010, alpha = .25, color = cols[12], size = 4) +
	geom_errorbar(width = 0, color = cols[8]) +
	geom_point(fill = cols[13], size = 2.5, pch = 21)

# ... age x sex gender effects
demo_scen = dfs[[1]] %>%
	data_grid(
		year = 0, 
		age_group,
		race,
		sex,
		pop = 100000
	) 
	
demo_scen %>%
	add_fitted_draws(fit, allow_new_levels = TRUE) %>%
	median_qi(.width = .5) %>%
	ggplot(., aes(
		x = age_group, y = .value, 
		color = race, fill = race, group = race
		)
	) +
	#geom_ribbon(alpha = .25, color = 'grey100') +
	geom_line(size = 1) +
	facet_wrap(~sex, scale = 'free_y') +
    c_pal +
    f_pal + 
    theme(
    	axis.text.x = element_text(angle = 90, vjust = 0.5),
    	legend.title = element_blank()
    )

# ... or, if want rep of (fittedddd) uncert. in each group
demo_scen %>%
	filter(sex == 'Male') %>%
	add_fitted_draws(fit, allow_new_levels = TRUE) %>%
	median_qi(.width = c(.5, .9)) %>%
	ggplot(., aes(
		x = age_group, y = .value, 
		color = race, fill = race, group = race,
		ymin = .lower, ymax = .upper
		)
	) +
	geom_ribbon(color = 'grey100', aes(group = .width, alpha = .width)) +
	geom_line(size = 1) +
	facet_grid(~race) +
    c_pal +
    f_pal + 
    theme(
    	axis.text.x = element_text(angle = 90, vjust = 0.5),
    	legend.position = 'none'
    ) +
    scale_alpha(range = c(.75, .25), breaks = c(.5, .9))

demo_scen %>%
	filter(sex == 'Female') %>%
	add_fitted_draws(fit, allow_new_levels = TRUE) %>%
	median_qi(.width = c(.5, .9)) %>%
	ggplot(., aes(
		x = age_group, y = .value, 
		color = race, fill = race, group = race,
		ymin = .lower, ymax = .upper
		)
	) +
	geom_ribbon(color = 'grey100', aes(group = .width, alpha = .width)) +
	geom_line(size = 1) +
	facet_grid(~race) +
    c_pal +
    f_pal + 
    theme(
    	axis.text.x = element_text(angle = 90, vjust = 0.5),
    	legend.position = 'none'
    ) +
    scale_alpha(range = c(.75, .25), breaks = c(.5, .9))


# ... more importantly, uncertanity in predictions 
demo_scen %>%
	mutate(pop = 100000*10) %>%
	filter(sex == 'Female') %>%
	add_predicted_draws(fit, allow_new_levels = TRUE) %>%
	median_qi(.width = c(.5, .9)) %>%
	ggplot(., aes(
		x = age_group, y = .prediction/10, 
		color = race, fill = race, group = race,
		ymin = .lower/10, ymax = .upper/10
		)
	) +
	geom_ribbon(color = 'grey100', aes(group = .width, alpha = .width)) +
	geom_line(size = 1) +
	facet_grid(~race) +
    c_pal +
    f_pal + 
    scale_x_discrete(
    	breaks = c('0', '25-29', '50-54', '75-79'),
    	labels = c(0, 25, 50, 75)
    ) +
    theme(
    	#axis.text.x = element_text(angle = 90, vjust = 0.5),
    	legend.position = 'none'
    ) +
    scale_alpha(range = c(.75, .25), breaks = c(.5, .9)) 

demo_scen %>%
	mutate(pop = 100000*100) %>%
	filter(sex == 'Male') %>%
	add_predicted_draws(fit, allow_new_levels = TRUE) %>%
	median_qi(.width = c(.5, .9)) %>%
	ggplot(., aes(
		x = age_group, y = .prediction/100, 
		color = race, fill = race, group = race,
		ymin = .lower/100, ymax = .upper/100
		)
	) +
	geom_ribbon(color = 'grey100', aes(group = .width, alpha = .width)) +
	geom_line(size = 1) +
	facet_grid(~race) +
    c_pal +
    f_pal + 
    scale_x_discrete(
    	breaks = c('0', '25-29', '50-54', '75-79'),
    	labels = c(0, 25, 50, 75)
    ) +
    theme(
    	#axis.text.x = element_text(angle = 90, vjust = 0.5),
    	legend.position = 'none'
    ) +
    scale_alpha(range = c(.75, .25), breaks = c(.5, .9)) 