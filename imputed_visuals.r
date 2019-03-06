#####################################
# deaths ~ f(age; race; gender)
# imputed model visuals, .R
# last edit: 3/6 (M.E)
#####################################

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
    ) +
    ylab('rate of being killed by police, per 100k pop') +
    xlab('age')

# ... predicted values
# ... men
demo_scen %>%
	mutate(pop = 100000*10) %>%
	filter(sex == 'Male') %>%
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
    theme(legend.position = 'none') +
    scale_alpha(range = c(.75, .25), breaks = c(.5, .9)) +
    ylab('predicted police-related mortality rate, per 100,000') +
    xlab('age')

# ... women
demo_scen %>%
	mutate(pop = 100000*1000) %>%
	filter(sex == 'Female') %>%
	add_predicted_draws(fit, allow_new_levels = TRUE) %>%
	median_qi(.width = c(.5, .9)) %>%
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
    scale_alpha(range = c(.75, .25), breaks = c(.5, .9)) 