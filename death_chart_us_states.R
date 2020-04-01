# Death chart for US states only
death_by_state = read_and_clean_covid_tracking() %>% 
  select(-Cases, Count=Deaths)
min_state_cases = 10
last_date = max(death_by_state$Date)

min_death_cases = 5

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

death_chart_us = growth_chart_base(to_plot_us_death, State, 'darkred',
                                    highlight_states) +
  labs(x=death_chart_x(min_death_cases), y='',
       title='Coronavirus deaths by state',
       subtitle=death_chart_subtitle(min_death_cases),
       caption=covid_tracking_credit(last_date))

us_death_df = to_plot_us_death %>% 
  with_sliding_window(State, state_window)
daily_deaths_us = new_cases_base(us_death_df, State, 'darkred', 
                             highlight_states, state_window) +
  labs(x=case_chart_x(min_country_cases),
       y='Daily deaths',
       title='Daily reported deaths by US state',
       subtitle=str_glue(
         '{state_window_str} day average of daily reported deaths'),
       caption=covid_tracking_credit(last_date))

new_vs_all_deaths_us_chart = new_vs_count_base(us_death_df, State, 'darkred') +
  labs(x='Total reported deaths', y='Daily new deaths', 
       title='New reported deaths vs all deaths by US state',
       subtitle=str_glue(
         '{state_window_str} day average of daily reported deaths ',
         'vs all deaths, log-log scale'),
       caption=covid_tracking_credit(last_date))

total_deaths_us = death_by_state %>% 
  filter(Date==ymd(last_date)) %>% 
  pull(Count) %>% 
  sum(na.rm=TRUE)

death_us_totals = totals_chart_base(death_by_state, State, 
                                    last_date, min_death_cases) +
  labs(x='Reported deaths (log scale)', y='',
       title='Reported coronavirus deaths by US state',
       subtitle=str_glue(
         'Showing states with {min_death_cases} or more deaths\n',
         'US total deaths: {scales::comma(total_deaths_us)}'),
       caption=covid_tracking_credit(last_date))

selected_us_death_base_plot = 
  selected_item_base(us_death_df, selected_states, State, Sliding) +
  labs(x=death_chart_x(min_death_cases), y='Reported deaths',
       subtitle=selected_death_chart_subtitle(state_window_str, min_death_cases),
       title='Daily coronavirus deaths, selected US states',
       caption=covid_tracking_credit(last_date)) 

selected_us_death_log_plot = selected_us_death_base_plot +
  my_y_log10()

selected_us_death_plot = selected_us_death_base_plot +
  scale_y_continuous(labels=scales::comma)
