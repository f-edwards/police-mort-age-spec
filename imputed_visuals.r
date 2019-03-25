#####################################
# deaths ~ f(age; race; gender)
# imputed model visuals, .R
# last edit: 3/25 (M.E)
#####################################

sim = dfs[[1]] %>% mutate(age_group = fct_relevel(age, '5-9', after = 2))

# ... plot it out
# ... year effects
dfs[[1]] %>%
	data_grid(
		year, 
		age_group = '30-34',
		race = 'White',
		sex = 'Male',
		pop = 100000
	) %>%
	add_fitted_draws(fit) %>%
	median_qi(.width = .75) %>%
	ggplot(., 
		aes(x = year, y = .value, ymin = .lower, ymax = .upper)
	) +
	geom_errorbar(width = 0, color = cols[8]) +
	geom_point(fill = cols[13], size = 2.5, pch = 21) + 
	coord_flip()

# ... get posterior sims
demo_scen = sim %>%
	data_grid(
		year = 0,
		age_group,
		race,
		sex,
		pop = 100000
	)

# ... fitted 
post_fit = demo_scen %>%
	mutate(pop = 100000) %>%
	add_fitted_draws(fit, allow_new_levels = TRUE) %>%
	median_qi(.width = .9)

# ... predicted
post_pred = demo_scen %>%
	mutate(pop = 100000*1000) %>%
	add_predicted_draws(fit, allow_new_levels = TRUE) %>%
	median_qi(.width = c(.5,.9))

# ... plot of fitted values
post_fit %>%
	ggplot(., 
		aes(x = age_group, y = .value, color = race, fill = race, group = race)
	) +
	geom_line(size = 1) +
	facet_wrap(~sex, scale = 'free_y') +
	c_pal +
	f_pal +
	theme(
    	axis.text.x = element_text(angle = 90, vjust = 0.5),
    	legend.title = element_blank()
    ) +
    ylab('risk of being killed by police, per 100k pop.') +
    xlab('age')

# ... plot of predicted values
# ... men 
post_pred %>%
	filter(sex == 'Male') %>%
	ggplot(., aes(
		x = age_group, y = .prediction/1000, 
		color = race, fill = race, group = race,
		ymin = .lower/1000, ymax = .upper/1000
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
    theme(legend.position = 'none') +
    scale_alpha(range = c(.75, .25), breaks = c(.5, .9)) +
    ylab('predicted police-related mortality rate, per 100,000') +
    xlab('age')

# ... women 
post_pred %>%
	filter(sex == 'Female') %>%
	ggplot(., aes(
		x = age_group, y = .prediction/1000, 
		color = race, fill = race, group = race,
		ymin = .lower/1000, ymax = .upper/1000
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
    theme(legend.position = 'none') +
    scale_alpha(range = c(.75, .25), breaks = c(.5, .9)) +
    ylab('predicted police-related mortality rate, per 100,000') +
    xlab('age')

# ... exports for team
#post_fit_90 = demo_scen %>%
#	mutate(pop = 100000) %>%
#	add_fitted_draws(fit, allow_new_levels = TRUE) %>%
#	median_qi(.width = .9)#

#post_pred_90 = demo_scen %>%
#	mutate(pop = 100000*1000) %>%
#	add_predicted_draws(fit, allow_new_levels = TRUE) %>%
#	median_qi(.width = .9)#

#write_rds(post_fit_90,  'post_fit_90.rds')
#write_rds(post_pred_90, 'post_pred_90.rds')