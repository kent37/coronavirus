# Growth chart for US states only

by_state = read_and_clean_covid_tracking() %>% 
  select(-Deaths, Count=Cases)
min_state_cases = 10
last_date = max(by_state$Date)

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

growth_chart_us = growth_chart_base(to_plot_us, State, 'darkred',
                                    highlight_states) +
  labs(x=case_chart_x(min_state_cases), y='',
       title='Coronavirus reported cases by state',
       subtitle=case_chart_subtitle(min_state_cases),
       caption=covid_tracking_credit(last_date))

us_daily_df = to_plot_us %>% with_sliding_window(State, 5)
daily_chart_us = new_cases_base(us_daily_df, State, 'darkred', 
                             highlight_states, 5) +
  labs(x=case_chart_x(min_country_cases),
       y='Daily new cases',
       title='New reported cases by US state',
       subtitle='Five day average of daily reported cases',
       caption=covid_tracking_credit(last_date))

new_vs_all_us_chart = new_vs_count_base(us_daily_df, State, 'darkred',
                                     highlight_states) +
  labs(x='Total reported cases', y='Daily new cases', 
       title='New reported cases vs all cases by US state',
       subtitle='Five day average of daily reported cases vs all cases, log-log scale',
       caption=covid_tracking_credit(last_date))

growth_us_totals = totals_chart_base(by_state, State, 
                              last_date, min_state_cases) +
  labs(x='Reported cases (log scale)', y='',
       title='Reported coronavirus cases by US state',
       subtitle=str_glue(
         'Showing states with {min_state_cases} or more cases'),
       caption=covid_tracking_credit(last_date)) 

selected_states_base_plot = 
  selected_item_base(to_plot_us, selected_states, State) +
    labs(x=case_chart_x(min_state_cases), y='Reported cases',
       title='Coronavirus reported cases by state',
       subtitle=case_chart_subtitle(min_state_cases),
       caption=covid_tracking_credit(last_date))

selected_states_log_plot = selected_states_base_plot +
  my_y_log10()

selected_states_plot = selected_states_base_plot +
  scale_y_continuous(labels=scales::comma)
