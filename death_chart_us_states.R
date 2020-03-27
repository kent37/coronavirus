# Growth chart for US states only
death_us = read_csv(here::here('data/covid_deaths_usafacts.csv')) %>% 
  select(-matches('X\\d+')) # Remove extra columns

min_death_cases = 5
last_date = names(death_us) %>% tail(1)
death_by_state = death_us %>% clean_state(min_death_cases)

# For each state, make a data series that
# starts at min_death_cases cases
affected = by_day_since_min(death_by_state, State, min_death_cases)

# How many states?
length(unique(affected$State))

min_days = 5
# Only states with >=min_days days >= min_death_cases
# No cruise ships
to_plot_us_death = affected %>% 
  filter(!State %in% c('Diamond Princess', "Grand Princess"),
         NumDays>=min_days)

death_chart_us = growth_chart_base(to_plot_us_death, State, 'darkred') +
  labs(x=death_chart_x(min_death_cases), y='',
       title='Coronavirus deaths by state',
       subtitle=death_chart_subtitle(min_death_cases),
       caption=usafacts_credit(last_date))

death_us_totals = totals_chart_base(death_by_state, State, 
                                    last_date, min_death_cases) +
  labs(x='Reported deaths (log scale)', y='',
       title='Reported coronavirus deaths by US state',
       subtitle=str_glue(
         'Showing states with {min_death_cases} or more deaths'),
       caption=usafacts_credit(last_date))

selected_us_death_base_plot = 
  selected_item_base(to_plot_us_death, selected_states, State) +
  labs(x=death_chart_x(min_death_cases), y='Reported deaths',
       title='Reported coronavirus deaths by US state',
       caption=usafacts_credit(last_date)) 

selected_us_death_log_plot = selected_us_death_base_plot +
  scale_y_log10(labels=scales::comma)

selected_us_death_plot = selected_us_death_base_plot +
  scale_y_continuous(labels=scales::comma)
