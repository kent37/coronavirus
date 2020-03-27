# Growth chart for US states only

conf_us = read_csv(here::here('data/covid_confirmed_usafacts.csv')) %>% 
  select(-matches('X\\d+')) # Remove extra columns

min_state_cases = 10
last_date = names(conf_us) %>% tail(1)
by_state = conf_us %>% clean_state(min_state_cases)

# For each state, make a data series that
# starts at min_state_cases cases
state_cases = by_day_since_min(by_state, State, min_state_cases) 

# How many countries?
length(unique(state_cases$State))

min_days = 5
# Only countries with >=min_days days >= min_state_cases
# No cruise ships
to_plot_us = state_cases %>% 
  filter(!State %in% c('Diamond Princess', "Grand Princess"),
         NumDays>=min_days)

growth_chart_us = growth_chart_base(to_plot_us, State, 'darkred') +
  labs(x=case_chart_x(min_state_cases), y='',
       title='Coronavirus reported cases by state',
       subtitle=case_chart_subtitle(min_state_cases),
       caption=usafacts_credit(last_date))

growth_us_totals = totals_chart_base(by_state, State, 
                              last_date, min_state_cases) +
  labs(x='Reported cases (log scale)', y='',
       title='Reported coronavirus cases by US state',
        subtitle=case_chart_subtitle(min_state_cases),
       caption=usafacts_credit(last_date)) 

selected_states_base_plot = 
  selected_item_base(to_plot_us, selected_states, State) +
    labs(x=case_chart_x(min_state_cases), y='Reported cases',
       title='Coronavirus reported cases by state',
       subtitle=case_chart_subtitle(min_state_cases),
       caption=usafacts_credit(last_date))

selected_states_log_plot = selected_states_base_plot +
  scale_y_log10(labels=scales::comma)

selected_states_plot = selected_states_base_plot +
  scale_y_continuous(labels=scales::comma)
